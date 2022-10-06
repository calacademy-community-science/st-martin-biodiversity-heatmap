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


target_phyla <- c("Arthropoda", "Chordata", "Tracheophyta", 
                  "Mollusca", "Annelida", "Bryophyta")
target_classes <- c("Reptilia", "Aves", "Mammalia")
target_clades <- c(target_phyla, target_classes)

taxa.df <- 
  read_csv2("data/taxon_info.csv") %>% 
  filter(phylum %in% target_phyla)



# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Biodiversity Heatmap for St. Martin"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(inputId = "clade",
                         label = h3("Select Clade: "),
                         choices = target_clades,
                         selected = "Tracheophyta",
                         inline = F),
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
                  min = .05, max = 1, value = .2,
                  step = .05
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