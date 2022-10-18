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
         geom, year)

island.sf <- st_read("data/st_martin_land.gpkg")
island_bbox.area <- 
  island.sf %>% 
  st_bbox() %>% 
  st_as_sfc() %>% 
  st_area()

border.sf <- st_read("data/st_martin_border.gpkg") %>% st_union()

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  
  ###### leaflet basemap ######
  output$heatmap <- renderLeaflet({
    
    # Make Basemap
    leaflet() %>%
      addProviderTiles('Esri.WorldImagery',
                       options = providerTileOptions(opacity = .6)) %>% 
      setView(-63.06017551131934, 18.06793379171563, zoom = 12.49) %>% 
      hideGroup("Political Border")
    
  }) # end leaflet map
  
  
  
  
  
  ###### Filter occurrence data reactively ######
  
  filteredData <- reactive({
    # Filter for taxa
    occ_taxa.sf <- 
      occ.sf %>% 
      filter(if_any(c(phylum, class), ~. %in% input$clade)) %>%
      # filter(if_any(c(phylum, class), ~. %in% "")) %>%
      mutate(obs_count = 1) # every row is one obs, used for aggregation in raster
    
    # Change spatial resolution
    sp_resolution <- (input$sp_resolution %>% as.numeric()) * 1000000 # convert sq km to sq m
    # sp_resolution <- (.2 ) * 1000000 # convert sq km to sq m
    
    n_cells <- (island_bbox.area / sp_resolution) %>% as.numeric()
    
    # Rasterize bounding box then convert back to sf
    template.sf <- st_as_stars(st_bbox(island.sf),
                               n = n_cells, 
                               values = 0) %>% 
      st_as_sf() %>% # back to sf
      mutate(GRID_ID = 1:n())
    
    # Assign grid number from template to each point, then group by grid and 
    # get different taxonomic counts from within
    occ_grid.df <- 
      occ_taxa.sf %>% 
      st_join(template.sf) %>% 
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
      left_join(occ_grid.df, by = "GRID_ID") %>% 
      filter(!is.na(OBS_COUNT)) # remove NA polygons entirely
  })
  
  
  
  
  #### Add the shapes to the base map #####
  observe({
    if(nrow(filteredData()) > 0){ # Make sure there's data
      
      # Hover Labels
      grid_labels <- 
        with(filteredData(),
             sprintf("<strong>Observations:</strong> %s<br/><strong>Species:</strong> %s<br/> <strong>Genera:</strong> %s<br/> <strong>Families:</strong> %s",
                     OBS_COUNT, SP_COUNT,
                     GEN_COUNT, FAM_COUNT) %>%
               lapply(htmltools::HTML))
      
      
      pal <- colorNumeric("viridis",
                          domain = filteredData()[, input$data_type][[1]],
                          na.color = "#00000000")
      
      leafletProxy("heatmap", data = filteredData()) %>%
        clearShapes() %>%
        addPolygons(label = grid_labels,
                    color = ~pal(filteredData()[, input$data_type][[1]]),
                    stroke = T,
                    weight = .5,
                    opacity = input$opacity,
                    fillOpacity = input$opacity,
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
        clearControls() %>%
        addLegend("bottomright", pal = pal,
                  values = ~filteredData()[, input$data_type][[1]],
                  title = "Count",
                  opacity = 1) %>% 
        # This shows and hides some of the different layers
        addLayersControl(
          overlayGroups = c("Political Border"),
          options = layersControlOptions(collapsed = FALSE)
        )
      
    } else { # Clear data if nothing selected
      
      leafletProxy("heatmap") %>%
        clearShapes()
    }
  }) # end observe
  
})
