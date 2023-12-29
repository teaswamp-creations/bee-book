library(plotly)
suppressPackageStartupMessages(library(tidyverse))


# make species map
gbif <- read_delim("../../files/0037810-231120084113126.csv", delim='\t') %>%
  mutate(date = ymd(date(eventDate)), Year = year(date),
         Month = lubridate::month(date, label=T),
         lat=decimalLatitude, lon=decimalLongitude) %>%
  drop_na(date, lat, lon, genus, occurrenceID) %>%
  separate_wider_delim(species, ' ', names=c('Genus', 'Species'), too_many = 'merge')



make_species_map <- function(species){
  if (!(species %in% gbif$Species)){
    return("no GBIF data to display")
  }
  
  gbif %>% 
    filter(Species == species) %>%
    plot_ly() %>%
    add_trace(
      lat = ~lat, lon =~lon,
      mode = 'markers', type = 'scattermapbox',
      color = ~Year,
      text = ~paste('Species:', Genus, Species, '\nObserved:', stamp("March 1, 1999")(date)),
      size=10,
      opacity=1,
      colors='Blues',
      name=""
    ) %>%
    layout(mapbox = list(
      style = 'carto-positron', show_legend = T,
      #zoom=7, center = list(lon = -123, lat = 49)),
      zoom=3.8, center = list(lon = -127, lat = 50)),
      title = 'GBIF observations in the Pacific Maritime region') %>%
    return()
}

# make buttons 