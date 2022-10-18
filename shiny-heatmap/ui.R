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

# Read in phylogenetic info for checkboxes
target_phyla <- c("Arthropoda", "Chordata", "Tracheophyta", 
                  "Annelida", "Bryophyta")
target_classes <- c("Reptilia", "Aves", "Mammalia", "Gastropoda")
target_clades <- c(target_phyla, target_classes)

taxa.df <- 
  read_csv2("data/taxon_info.csv") %>% 
  filter(phylum %in% target_phyla)

# A list object to set up multicols for checkboxGroupInput
tweaks <- 
  list(tags$head(
    tags$style(
      HTML(
        ".checkbox-inline { 
                    margin-left: 0px;
                    margin-right: 10px;
          }
         .checkbox-inline+.checkbox-inline {
                    margin-left: 0px;
                    margin-right: 10px;
          }
        "
      )
    ) 
  ))


# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Biodiversity Heatmap for St. Martin"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      tweaks,
      checkboxGroupInput(inputId = "clade",
                         label = h3("Select Clade: "),
                         choices = target_clades,
                         selected = "Tracheophyta",
                         inline = T),
      br(),

      radioButtons("data_type", h3("What do you want to see?"),
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
                  min = 0, max = 1, value = .7
      ),
    ),
    
    mainPanel(
      width = 9,
      leafletOutput("heatmap", height="90vh") %>% 
        shinycssloaders::withSpinner(type = 8, # Loading animation
                                     color = "darkgreen", 
                                     size = .75)
    )
  )
  # end main body fluidRow
))
