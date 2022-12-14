#' Alpha-diversity heatmap for St Martin
#' 
#' Author: Avery Hill
#' email: ahill@calacademy.org
#' 
#' last update:
#' 
#' In this script we download occurrence data for St Martin using GBIF
#' 

library(sf)
library(tidyverse)
library(rgbif)
library(leaflet)

# downloaded from https://data.humdata.org/dataset/cod-ab-maf? and https://data.humdata.org/dataset/cod-ab-sxm
# and stitched together in GBIF
# interestingly, the french side was vectorized from a raster and has weird resolution and edges
island.sf <- 
  st_read("../data/perimeters/st_martin_land.gpkg") %>% 
  st_transform(4326) # convert to WGS 84

# # Simplify input polygons and add some buffer zone to it
island_simple.sf <-
  island.sf %>%
  # st_buffer(0) %>% # 1km buffer
  st_simplify(dTolerance = 50) %>% # reduce
  st_cast("MULTIPOLYGON")

ggplot() +
  geom_sf(data = island.sf) +
  geom_sf(data = island_simple.sf, alpha = .5, color = 'red')

# Convert to a well known text format of the polygon and check validity
island.wkt <- 
  island_simple.sf$geom %>% 
  st_as_text() %>%
  check_wkt()

# # Get the taxon keys for gastropods and arthropods
# gastro_key <- name_suggest(q="Gastropoda", rank = "CLASS")[["data"]][["key"]]
# arthro_key <- name_suggest(q="Arthropoda", rank = "PHYLUM")[["data"]][["key"]]

# Query from GBIF
stmartin_query.info <- 
  occ_download(
    # pred_in("taxonKey", c(gastro_key, arthro_key)), 
               # pred("datasetKey", "50c9509d-22c7-4a22-a47d-8c48425ef4a7"), # iNat dataset key
               pred("geometry", island.wkt),
               pred("hasCoordinate", TRUE),
               format = "SIMPLE_CSV")

occ_download_wait(stmartin_query.info)

dir.create("../data/occurrence")
gbif.df <- occ_download_get(stmartin_query.info,
                                  path = "../data/occurrence",
                                  overwrite = T) %>%
  occ_download_import()

gbif.sf <- 
  gbif.df %>% 
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326, remove = F)

# How does it look? good
ggplot() + 
  geom_sf(data = island.sf) +
  geom_sf(data = gbif.sf %>% sample_n(5000), alpha = .5)

write_sf(gbif.sf, "../data/occurrence/stmartin_gbif_raw.gpkg")


## Going to make a little dataframe of phylogenetic information for the shiny app
gbif.sf <- st_read("../data/occurrence/stmartin_gbif_raw.gpkg")

phylo.df <- gbif.sf %>% tibble() %>% select(phylum:species) %>% distinct()
write_csv2(phylo.df, "../data/taxon_info.csv")

## Here is a trimmed down version specificallly for the shiny app
gbif.sf <- st_read("../data/occurrence/stmartin_gbif_raw.gpkg")
# Has 60500 observations

gbif.sf %>% filter(!is.na(coordinateUncertaintyInMeters)) %>% nrow()
# Only 15822 have coordinate uncertainty measured

# Let's go with the 60,000 records and just filter out fossils and materials (may be repeat samples)
gbif_cert.sf <- 
  gbif.sf %>% 
  filter(#coordinateUncertaintyInMeters < 1000,
         !(basisOfRecord %in% c("FOSSIL_SPECIMEN", "MATERIAL_SAMPLE")))
# 13000 have less than 1000 uncertainty

write_sf(gbif_cert.sf,"../data/stmartin_gbif.gpkg")

# Messing around: 
gbif_omitted.sf <- gbif.sf %>% 
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters > 1000)

leaflet() %>%
  addProviderTiles('Esri.WorldImagery',
                   options = providerTileOptions(opacity = .6)) %>% 
  setView(-63.06017551131934, 18.06793379171563, zoom = 12.49) %>% 
  addPolygons(data = gbif_omitted.sf)

ggplot() + 
  geom_sf(data = gbif_omitted.sf)
st_precision(gbif_omitted.sf)


getDecimalPlaces <- function(x) {min(which( x*10^(0:20)==floor(x*10^(0:20)) )) - 1}

gbif_omitted.sf %>% 
  mutate(LONLAT_DECRES = getDecimalPlaces(decimalLatitude)) %>% 
  pull(LONLAT_DECRES) %>% 
  table()

gbif_omitted.sf %>% pull(basisOfRecord) %>% table()

ggplot() + 
  geom_sf(data = island_simple.sf) +
  geom_sf(data = gbif_omitted.sf %>% filter(basisOfRecord == "PRESERVED_SPECIMEN"))
