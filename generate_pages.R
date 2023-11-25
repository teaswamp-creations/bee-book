library(tidyverse)
library(htmltools)
library(googlesheets4)

# read spreadsheet data
gs4_auth('galiwatch.info@gmail.com')
df <- read_sheet("1c4N54O_x8a_Wfoe3tct0gryId2wG0ze-HA-Ckms4dy4", sheet = 'Bee families of BC') %>%
  mutate(`Image link` = replace_na(`Image link`, '/files/no_bee.png'))
# generate species data
make_page <- function(row){
  parent <- gsub(' ', '_', paste0(tolower(row['Genus']),'_', tolower(row['Species'])))
  if (!dir.exists(file.path('bees', parent))){dir.create(file.path('bees', parent))}
  contents <- paste0(
    '---\ntitle: ',
    row['Genus'], ' ', row['Species'],
    '\nengine: knitr',
    '\ncategories:\n  - ', row['Family'], 
    '\nfreeze: True',
    '\n---\n',
    '(', row['Common name'], ')\n\n',
    '[![`r emo::ji("copyright")` ', row['Observer'], '. ', row['Observation date'], '.](', row['Image link'], '){height=300}](', row['Observation Link'], ')'
  )
  write(contents, file.path('bees', parent, 'index.qmd'))
}

apply(df, FUN=make_page, MARGIN=1)