library(tidyverse)
library(htmltools)
library(googlesheets4)

places <- read_csv('files/place_names.txt') %>% 
  arrange(desc(nchar(name))) %>%
  pull(name) %>% 
  unique()

format_place <- function(x, lookup){
  for(i in lookup){
    x <- gsub(i, paste0('<span class="hl-place-name">', i, '</span>'), x)
  }
  x
}

# spreadsheet data
#gs4_auth('galiwatch.info@gmail.com')
df <- readxl::read_xlsx("files/Native bees of BC - PM_3rd edition.xlsx") %>%
  filter(`Bee book list` == 'Y') %>%
  separate(Observer, into=c('Observer', 'obs_link'), '\\(') %>%
  mutate(physical_record = (str_sub(Record,-2,-1) =="PM") | (Record=="Galiano life-list"),
         Observer = ifelse(is.na(`Image link`), 'Cait Harrigan', Observer),
         `Image link` = replace_na(`Image link`, '/files/no_bee.png'),
         `Common name` = ifelse(`Common name` == paste(Genus, Species), NA, `Common name`),
         obs_link = gsub("\\)", "", obs_link)
         ) %>%
  mutate(`Abr. note` = format_place(`Abr. note`, places)) 


# gbif data
gbif <- read_delim("files/0050043-251120083545085.csv", delim = "\t") %>%
  separate_wider_delim(eventDate, delim='/', names = c('eventDate', NA), too_few = "align_start") %>%
  mutate(eventDate = parse_date_time(eventDate, orders = c("ymd", "ymd HM", 'ymd HMS', "ym", "y")),
         date = ymd(date(eventDate)), Year = year(date),
         Month = lubridate::month(date, label=T),
         lat=decimalLatitude, lon=decimalLongitude) %>%
  drop_na(date, lat, lon, occurrenceID) %>%
  separate_wider_delim(species, ' ', names=c('Genus', 'Species'), too_many = 'merge', too_few = 'align_start') %>%
  mutate(Species=ifelse(Species=='nr', NA, Species)) 

# make badges 
make_badges <- function(row){
  b <- ''
  if(trimws(row['KQ_Van_Isle']) > 0){
    b <- paste(b, '<span class="vanc-isl-badge">Vancouver Island</span>')
  } 
  if(trimws(row['KQ_LM']) > 0){
    b <- paste(b, '<span class="lower-mainland-badge">Lower Mainland</span>')
  }
  if(trimws(row['KQ_C&NW']) > 0){
    b <- paste(b, '<span class="coast-badge">Coast</span>')
  }
  if(trimws(row['KQ_Galiano']) > 0){
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
    ifelse(is.na(row['Common name']), '', paste0('\nsubtitle: ', row['Common name'])),
    '\nengine: knitr',
    '\ncategories:\n  - ', row['Family'], 
    '\nfreeze: auto',
    '\ndescription: ', make_badges(row),
    '\nimage: ', row['Image link'],
    '\n---\n',
    ifelse(is.na(row['obs_link']), 
           paste0('![`r emo::ji("copyright")` ', row['Observer'], '.](', row['Image link'], '){height=300}'),
           paste0('![`r emo::ji("copyright")` [', row['Observer'], '](', row['obs_link'], ')', '.](', row['Image link'], '){height=300}')
           ),
    '\n\n', '## Observation notes  \n', ifelse(!is.na(row['Abr. note']), row['Abr. note'], ''), 
    '\n\n', '## GBIF observations  \n', ifelse(!(row['Species'] %in% gbif$Species), 'No GBIF observations in Pacific Maritime to display.',
                   paste0('\n\n```{r warning=F, message=F, echo=F}\nsource("../../page_functions.R")\nmake_species_map("', row["Species"], '")\n```')
                   ),
    '\n<br><br>\n<center>\n',
    '\n\n<a href="', row['Map Link'], '"><button type="button" class="btn btn-secondary">See all ', row['Genus'], ' ', row['Species'], 
    ' <i class="fa-solid fa-arrow-up-right-from-square"></i> </button></a>',
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
      kable(caption = paste0("Bee species on Sheffield and Heron's list but with no physical record (", sum(!is.na(.)), " species)"), rownames=F, col.names = NULL) %>%
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
      kable(caption = paste0("Bee species on Sheffield and Heron's list and with physical records (", sum(!is.na(.)), " species)"), rownames=F, col.names = NULL) %>%
      kable_styling(bootstrap_options = "striped", full_width = T) %>%
      column_spec(1:r, italic=T),
    df %>%
      filter((Sheffield=='N') & (physical_record==T)) %>%
      mutate(Species = paste(Genus, Species)) %>% 
      select(Species, `Summary note`) %>%
      kable(caption = paste0("Bee species with physical records, but not on Sheffield and Heron's list (", nrow(.), " species)"), rownames=F) %>%
      kable_styling(bootstrap_options = "striped", full_width = T) %>%
      column_spec(1, italic=T)
  ) 
}

# run this line to regenerate bee pages
print(dim(df))
apply((df), FUN=make_page, MARGIN=1)
