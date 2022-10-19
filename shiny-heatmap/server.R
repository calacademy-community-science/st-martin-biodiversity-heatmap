#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# Read in spatial data
occ.sf <- 
  st_read("data/stmartin_gbif.gpkg") %>% 
  select(phylum, class, order, family, genus, species, 
         geom, year) %>% 
  filter(species != "")

island.sf <- st_read("data/st_martin_land.gpkg")
island_bbox.area <- 
  island.sf %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_area()

border.sf <- st_read("data/st_martin_border.gpkg") %>% st_union()

proposed_protected.sf <- st_read("data/proposed_boundaries.gpkg") %>% st_union()



shinyServer(function(input, output) {
  
  ###### leaflet basemap ######
  output$heatmap <- renderLeaflet({
    
    # Make Basemap
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery',
                       options = providerTileOptions(opacity = .6)) %>% 
      setView(-63.06017551131934, 18.06793379171563, zoom = 12.49) %>% 
      hideGroup("Political Border") %>% 
      hideGroup("Proposed Protected Areas")
    
  }) # end leaflet map
  
  
  
  ###### Filter occurrence data reactively ######
  filteredData <- reactive({
    # Filter for taxa
    occ_taxa.sf <- 
      occ.sf %>% 
      filter(if_any(c(phylum, class), ~. %in% input$clade)) %>%
      mutate(obs_count = 1) # every row is one obs, used for aggregation in raster
    
    # Change spatial resolution
    sp_resolution <- (input$sp_resolution %>% as.numeric()) * 1000^2 # convert sq km to sq m

    n_cells <- (island_bbox.area / sp_resolution) %>% as.numeric()
    
    # Rasterize bounding box then convert back to sf
    template.sf <- st_as_stars(st_bbox(island.sf),
                               n = n_cells, 
                               values = 0) %>% 
      st_as_sf() %>% # back to sf
      mutate(GRID_ID = 1:n())
    
    occ_grid_long.df <- 
      occ_taxa.sf %>% 
      st_join(template.sf)
    
    # Assign grid number from template to each point, then group by grid and 
    # get different taxonomic counts from within
    occ_grid_short.df <- 
      occ_grid_long.df %>% 
      group_by(GRID_ID) %>% 
      mutate(OBS_COUNT = n(),
             SP_COUNT = n_distinct(species),
             GEN_COUNT = n_distinct(genus),
             FAM_COUNT = n_distinct(family)) %>% 
      ungroup() %>% 
      tibble() %>% 
      select(GRID_ID, OBS_COUNT, SP_COUNT, GEN_COUNT, FAM_COUNT) %>% 
      distinct()
    
    # Join counts back to the grid sf
    heatmap.sf <- 
      template.sf %>% 
      left_join(occ_grid_short.df, by = "GRID_ID") %>% 
      filter(!is.na(OBS_COUNT)) # remove NA polygons entirely
    
    return(list(
      "heatmap_sf" = heatmap.sf,
      "long_grid_df" = occ_grid_long.df
    ))
  })
  
  
  
  
  #### Add the shapes to the base map #####
  observe({
    heatmap.sf_reactive <- filteredData()[["heatmap_sf"]]
    if(nrow(heatmap.sf_reactive) > 0){ # Make sure there's data
      
      # Hover Labels
      grid_labels <- 
        with(heatmap.sf_reactive,
             sprintf("<strong>Observations:</strong> %s<br/><strong>Species:</strong> %s<br/> <strong>Genera:</strong> %s<br/> <strong>Families:</strong> %s",
                     OBS_COUNT, SP_COUNT,
                     GEN_COUNT, FAM_COUNT) %>%
               lapply(htmltools::HTML))
      
      #' Okay so we have to make an initial color ramp of discrete colors to then
      #' skew it with the 'bias' parameter to then feed it to colorNumeric for use
      #' in leaflet
      #' inspired by https://stackoverflow.com/questions/49126405/how-to-set-up-asymmetrical-color-gradient-for-a-numerical-variable-in-leaflet-in
      #' 
      pre_colors <- colorRampPalette(colors = c("midnightblue", "chartreuse"))(20)
      # pre_colors <- colorRampPalette(colors = c("black", "orangered"))(20)
      trans_colors <- colorRampPalette(colors = pre_colors,
                              bias = 2)(30)

      pal <- colorNumeric(trans_colors,
                          domain = heatmap.sf_reactive[, input$data_type][[1]],
                          na.color = "#00000000")
      
      leafletProxy("heatmap", data = heatmap.sf_reactive) %>%
        clearShapes() %>%
        addPolygons(label = grid_labels,
                    color = ~pal(heatmap.sf_reactive[, input$data_type][[1]]),
                    stroke = T,
                    weight = .5,
                    opacity = input$opacity,
                    fillOpacity = input$opacity,
                    layerId = ~GRID_ID,
                    highlightOptions = highlightOptions(
                      weight = 2,
                      fillOpacity = 0,
                      bringToFront = TRUE,
                      opacity = input$opacity
                    ),
                    labelOptions = labelOptions(
                      style = list("font-weight" = "normal", padding = "3px 8px"),
                      textsize = "15px",
                      direction = "auto")
        ) %>%
        # Border
        addPolylines(data = border.sf, group = "Political Border", 
                     weight = 3, color = "black", opacity = 1) %>%
        addPolygons(data = proposed_protected.sf, group = "Proposed Protected Areas",
                    fill = NA, weight = 3, color = "brown", opacity = 1) %>% 
        clearControls() %>%
        addLegend("bottomright", pal = pal,
                  values = ~heatmap.sf_reactive[, input$data_type][[1]],
                  title = "Count",
                  opacity = 1) %>% 
        # This shows and hides some of the different layers
        addLayersControl(
          overlayGroups = c("Political Border", "Proposed Protected Areas"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } else { # Clear data if nothing selected
      
      leafletProxy("heatmap") %>%
        clearShapes()
    }
  }) # end observe
  
  #### CLick for species list #####
  
  observeEvent(input$heatmap_shape_click$id, {
    hide(id = "species_placeholder")
  })
  
  output$species_list <- renderText({
    filteredData()[["long_grid_df"]] %>% 
      filter(GRID_ID == input$heatmap_shape_click$id) %>%
      arrange(species) %>% 
      pull(species) %>% 
      unique() %>% 
      # Add hyperlink to inaturalist
      paste0('<a href="https://www.inaturalist.org/taxa/',
             str_replace(., " ", "-"),'" target="_blank">', ., "</a>") %>% 
      paste(collapse = "<br>") %>% 
      HTML()
  }) %>% 
    bindEvent(input$heatmap_shape_click)
  
})
