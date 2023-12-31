library(tidyverse)
library(htmltools)
library(googlesheets4)


# spreadsheet data
gs4_auth('galiwatch.info@gmail.com')
df <- read_sheet("1c4N54O_x8a_Wfoe3tct0gryId2wG0ze-HA-Ckms4dy4", sheet = 'Bee families of BC') %>%
  mutate(physical_record = (str_sub(Record,-2,-1) =="PM") | (Record=="Galiano life-list")) %>%
  mutate(Observer = ifelse(is.na(`Image link`), 'Cait Harrigan', Observer)) %>%
  mutate(`Image link` = replace_na(`Image link`, '/files/no_bee.png'))

# gbif data
gbif <- read_delim("files/0037810-231120084113126.csv", delim = "\t") %>%
  mutate(date = ymd(date(eventDate)), Year = year(date),
         Month = lubridate::month(date, label=T),
         lat=decimalLatitude, lon=decimalLongitude) %>%
  drop_na(date, lat, lon, genus, occurrenceID) %>%
  separate_wider_delim(species, ' ', names=c('Genus', 'Species'), too_many = 'merge') %>%
  mutate(Species=ifelse(Species=='nr', NA, Species))


# read spreadsheet data
#gs4_auth('galiwatch.info@gmail.com')
#df <- read_sheet("1c4N54O_x8a_Wfoe3tct0gryId2wG0ze-HA-Ckms4dy4", sheet = 'Bee families of BC') %>%
#  mutate(`Image link` = replace_na(`Image link`, '/files/no_bee.png'))


# make species map
#gbif <- read_delim("files/0037810-231120084113126.csv", delim='\t') %>%
#  mutate(date = ymd(date(eventDate)), Year = year(date),
#         Month = lubridate::month(date, label=T),
#         lat=decimalLatitude, lon=decimalLongitude) %>%
#  drop_na(date, lat, lon, genus, occurrenceID) %>%
#  separate_wider_delim(species, ' ', names=c('Genus', 'Species'), too_many = 'merge')

# make evidence badges 
make_badges <- function(row){
  b <- ''
  if(row['Sheffield']=='Y'){
    b <- paste(b, '<span class="sh-badge">SH list</span>')
  } 
  if(row['physical_record']==T){
    b <- paste(b, '<span class="pe-badge">PM Evidence</span>')
  }
  if(row['Record']=='Galiano life-list'){
    b <- paste(b, '<span class="gali-badge">Galiano</span>')
  }
  b
}

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
    '\ndescription: ', make_badges(row),
    '\n---\n',
    '![`r emo::ji("copyright")` ', row['Observer'], '.](', row['Image link'], '){height=300}', 
    '\n\n', ifelse(!is.na(row['Summary note']), row['Summary note'], ''), 
    '\n\n', ifelse(!(row['Species'] %in% gbif$Species), 'No GBIF observations in Pacific Maritime to display.',
                   paste0('\n\n```{r warning=F, message=F, echo=F}\nsource("../../page_functions.R")\nmake_species_map("', row["Species"], '")\n```')
                   ),
    '\n<br><br>\n<center>\n',
    '\n\n<a href="', row['Map Link'], '"><button type="button" class="btn btn-secondary">See all ', row['Genus'], ' ', row['Species'], 
    ' in BC <i class="fa-solid fa-arrow-up-right-from-square"></i> </button></a>',
    '\n</center>'
    )
  write(contents, file.path('bees', parent, 'index.qmd'))
}

inline_summary <- function(df){
  g <- df %>% 
    group_by(Genus) %>%
    summarize(n=n()) 
  paste0(dim(g)[1], " genuses, ", sum(g$n), " species (", 
         paste(apply(g, FUN=function(r){paste0(r["Genus"], ', ', trimws(r["n"]))}, MARGIN=1), collapse = "; "), 
         ")")
}

table_summary <- function(df){
  r = ifelse(nrow(df)>10,3,1)
  paste(
    df %>%
      filter((Sheffield=='Y') & (physical_record==F)) %>%
      mutate(Species = paste(Genus, Species)) %>% 
      select(Species) %>%
      mutate(name = 1:nrow(.) %% r,
             row=rep(1:(ceiling(nrow(.)/r)), each=r)[1:nrow(.)]) %>%
      pivot_wider(values_from=Species) %>%
      select(-c(row)) %>%
      kable(caption = paste0("Bee species on Sheffield and Heron’s list but with no physical record (", sum(!is.na(.)), " species)"), rownames=F, col.names = NULL) %>%
      kable_styling(bootstrap_options = "striped", full_width = T) %>%
      column_spec(1:r, italic=T),
    
    df %>%
      filter((Sheffield=='Y') & (physical_record==T)) %>%
      mutate(Species = paste(Genus, Species)) %>% 
      select(Species) %>%
      mutate(name = 1:nrow(.) %% r,
             row=rep(1:(ceiling(nrow(.)/r)), each=r)[1:nrow(.)]) %>%
      pivot_wider(values_from=Species) %>%
      select(-c(row)) %>%
      kable(caption = paste0("Bee species on Sheffield and Heron’s list and with physical records (", sum(!is.na(.)), " species)"), rownames=F, col.names = NULL) %>%
      kable_styling(bootstrap_options = "striped", full_width = T) %>%
      column_spec(1:r, italic=T),
    df %>%
      filter((Sheffield=='N') & (physical_record==T)) %>%
      mutate(Species = paste(Genus, Species)) %>% 
      select(Species, `Summary note`) %>%
      kable(caption = paste0("Bee species with physical records, but not on Sheffield and Heron’s list (", sum(!is.na(.)), " species)"), rownames=F) %>%
      kable_styling(bootstrap_options = "striped", full_width = T) %>%
      column_spec(1, italic=T)
  ) 
}

# run this line to regenerate bee pages
#apply(df, FUN=make_page, MARGIN=1)
