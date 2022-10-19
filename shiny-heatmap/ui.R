#' NatureCheck Invert Data Explorer
#' 
#' ################## UI FILE ################## 
#' 
#' Author: Avery Hill
#' email: ahill@calacademy.org
#' 
#' ---------- libraries ---------- 
#' 

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(stars)
library(shinyjs)
library(RColorBrewer)

# Read in phylogenetic info for checkboxes
target_clades <- list("Birds" = "Aves", 
                      "Reptiles" = "Reptilia",
                      "Mammals" = "Mammalia",
                      "Slugs/Snails" = "Gastropoda",
                      "Arthropods" = "Arthropoda",
                      "Plants" = "Tracheophyta"
)

taxa.df <- 
  read_csv2("data/taxon_info.csv") %>% 
  filter(if_any(c(phylum, class), ~. %in% unlist(target_clades)))



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Biodiversity Heatmap for St. Martin"),
  column(
    width = 3,
    fluidRow(
      class="well",
      selectizeInput(inputId = "clade",
                     label = h3("Select Clade(s):"),
                     choices = target_clades,
                     selected = "Aves",
                     multiple = T),
      br(),
      
      selectInput("data_type", h3("What do you want to see?"),
                  choices = list("Total Observations" = "OBS_COUNT", 
                                 "Total Species" = "SP_COUNT",
                                 "Total Genera" = "GEN_COUNT",
                                 "Total Families" = "FAM_COUNT"), 
                  selected = "SP_COUNT"),
      br(),
      
      sliderInput(inputId = "sp_resolution",
                  label = h3(HTML("Spatial Resolution (km<sup>2</sup>)")),
                  min = .025, max = 1, value = .2,
                  step = .025
      ),
      
      sliderInput(inputId = "opacity",
                  label = h3(HTML("Layer Opacity")),
                  min = 0, max = 1, value = .85
      ),
    ),
    
    # Species list
    fluidRow(
      useShinyjs(), # Need this to hide the species_placeholder
      class = "well",
      style = "overflow-y:scroll; max-height: 15vh;", # Add scrollbar to species list
      span(id = "species_placeholder",
           "Click on a grid cell to show species list here", 
           style = "font-size:12px; text-align: center; font-style: italic;"),
      span(htmlOutput("species_list"), 
           style = "font-size:12px; text-align: left; font-style: italic;")
    )
  ),
  column(
    width = 9,
    leafletOutput("heatmap", height="90vh") %>% 
      shinycssloaders::withSpinner(type = 8, # Loading animation
                                   color = "darkgreen", 
                                   size = .75)
    )
  # end main body fluidRow
))
