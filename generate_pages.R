library(tidyverse)
library(htmltools)
library(googlesheets4)

# read spreadsheet data
gs4_auth('galiwatch.info@gmail.com')
df <- read_sheet("1c4N54O_x8a_Wfoe3tct0gryId2wG0ze-HA-Ckms4dy4", sheet = 'Andrena PM')

# set genus folder
dir <- 'Andrena'

# generate species data
make_page <- function(row, parent='./'){
  print(row)
  contents <- paste0(
    h1(paste(row['Genus'], row['Species'])),
    p(paste0('(', row['Common name'], ')')),
    br(),
    paste0('[![© ', row['iNat account'], '. ', row['Date'], '.](', 
           row['Image link'], ')](', row['Reference link'], ')')
  )
  write(contents, paste0(tolower(parent),'/',row['Species'],'.qmd'))
}

apply(df, FUN=make_page, MARGIN=1, parent=dir)
