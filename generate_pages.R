library(tidyverse)
library(htmltools)
library(googlesheets4)

# read spreadsheet data
gs4_auth('galiwatch.info@gmail.com')
df <- read_sheet("1c4N54O_x8a_Wfoe3tct0gryId2wG0ze-HA-Ckms4dy4", sheet = 'Bee families of BC') %>%
  mutate(`Image link` = replace_na(`Image link`, '/files/no_bee.png'))


# make species map
gbif <- read_delim("../../files/0004981-231120084113126.csv", delim='\t') %>%
  mutate(date = ymd(date(eventDate)), Year = year(date),
         Month = lubridate::month(date, label=T),
         lat=decimalLatitude, lon=decimalLongitude) %>%
  drop_na(date, lat, lon, genus, occurrenceID) %>%
  separate_wider_delim(species, ' ', names=c('Genus', 'Species'), too_many = 'merge')


# make species page
make_page <- function(row){
  parent <- gsub(' ', '_', paste0(tolower(row['Genus']),'_', tolower(row['Species'])))
  if (!dir.exists(file.path('bees', parent))){dir.create(file.path('bees', parent))}
  contents <- paste0(
    '---\ntitle: ',
    row['Genus'], ' ', row['Species'],
    '\nsubtite: ', row['Common name'], 
    '\nengine: knitr',
    '\ncategories:\n  - ', row['Family'], 
    '\nfreeze: auto',
    '\n---\n',
    '[![`r emo::ji("copyright")` ', row['Observer'], '. ', row['Observation date'], '.](', row['Image link'], '){height=300}](', row['Observation Link'], ')',
    ifelse(row[['Species']] %in% gbif$Species, paste0('\n\n```{r warning=F, message=F, echo=F}\nsource("../../page_functions.R")\nmake_species_map("', row["Species"], '")\n```'),
           '\n\nNo GBIF data to display')
    )
  write(contents, file.path('bees', parent, 'index.qmd'))
}

apply(df, FUN=make_page, MARGIN=1)
