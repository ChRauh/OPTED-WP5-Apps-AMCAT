############################################################
# Project:  OPTED WP5
# Task:     Prepare normalisation data for PLS-Words app
# Author:   Christian Rauh (29.08.2023)
############################################################

# The issue
# Total number of parliamentary speeches varies heavily over time, parties, or speakers
# Aggregated data in PLS-words should be normalised to this variation to allow consistent comparisons 
# -> relative share of speeches matching the query
# This file prepares the respective normalisation data to have it statically available in the app
# (rather than extracting and re-calculating it every time)


# Packages ####
library(tidyverse)


# Prepare PLS data ####

# Get file lists (local paths HP)
data.path <- "D:/Dropbox/OPTED datasets" # HP home
# data.path <- "C:/Users/rauh/Dropbox/OPTED datasets" # WZB
files <- list.files(path = data.path, recursive = T, full.names = T) %>% 
  as.data.frame() %>% 
  rename(file = 1) %>% 
  filter(str_detect(file, ("\\.(RDS|rds)"))) %>% 
  mutate(type = str_extract(file, "_[a-zA-Z]*?_") %>% 
           str_remove_all("_") %>% tolower(),
         country = str_extract(file, "OPTED datasets/[A-Za-z ]*?/") %>% 
           str_remove_all("OPTED datasets/") %>% str_remove_all("/"))

# Full parliament names
files$parliament <- NA
files$parliament[files$country == "Austria"] <- "Austria: Nationalrat"
files$parliament[files$country == "Croatia"] <- "Croatia: Hrvatski sabor"
files$parliament[files$country == "Czech Republic"] <- "Czech Republic: Poslanecká sněmovna"
files$parliament[files$country == "Denmark"] <- "Denmark: Folketing"
files$parliament[files$country == "EP"] <- "EU: European Parliament"
files$parliament[files$country == "Germany"] <- "Germany: Bundestag"
files$parliament[files$country == "Hungary"] <- "Hungary: Országgyűlés"
files$parliament[files$country == "Spain"] <- "Spain: Congreso de los Diputados"

# Index names (the names that will be used on the AMCAT server)
files$index <- paste0(files$type, "_", tolower(files$country))
files$index[files$index== "speeches_czech republic"] <- "speeches_cz"
files$index[files$index== "laws_czech republic"] <- "laws_cz"
files$index[files$index== "bills_czech republic"] <- "bills_cz"


# Remove duplicated data
files <- files %>% filter(!(str_detect(file, fixed("/old/"))))

# files <- files %>%  filter(country != "Czech Republic") # Remove CZ for now - party in speeches missing
# files <- files %>%  filter(country != "Croatia") # Remove Croatia for now - law metadata missing



# Speeches ####

speeches_files <- files %>%  filter(type == "speeches")

# Assemble speech data

speeches <- data.frame()
for (i in 1:nrow(speeches_files)) {
  
  print(speeches_files$file[i])
  
  # Load data
  df <- read_rds(speeches_files$file[i])
  
  # Make missing metadata explicit
  if(!"party" %in% colnames(df)) {
    df$party <- NA
  }
  if(!"speech_procedure_ID" %in% colnames(df)) {
    df$speech_procedure_ID <- NA
  }
  if(!"speech_ID" %in% colnames(df)) {
    df$speech_ID <- 1:nrow(df) # running number, assuming data is order chronologically
  }
  
  # Harmonize the data
  df <- df %>%
    select(speech_ID, date, text, speaker, party, agenda, speech_procedure_ID) %>%
    mutate(date = as.Date(date)) %>%
    mutate(title = paste0(as.character(date), " - ", speech_ID, "; ", speaker, " (", party, ")")) %>%
    mutate(speaker_party = paste0(speaker, " (", party,")")) %>%
    mutate(index = speeches_files$index[i]) %>%
    mutate(parliament = speeches_files$parliament[i])
  
  # Append to target
  speeches <- rbind(speeches, df)
}

rm(df)
gc()

summary(speeches)

# Observations
table(speeches$parliament)

# Correct some issues
speeches$party[speeches$party == "-"] <- NA
speeches$party[speeches$party == "NA"] <- NA

# Variables for aggregation (in PLS-Words app) should not contain missings on AMCAT servers (creates HTTP 500 error in query_aggregate())
speeches$party[is.na(speeches$party)] <- "Party unknown"
speeches$speaker[is.na(speeches$speaker)] <- "Speaker unknown"
speeches$speaker_party[is.na(speeches$speaker_party)] <- "Speaker unknown"

# Some fields may never be empty on AMCAT servers (2 cases)
speeches <- speeches %>%
  filter(!is.na(date)) %>%
  filter(!is.na(title)) %>%
  filter(!is.na(text))


# Normalization data for PLS-Words app ####

docs <- speeches

ndocs <- docs %>% 
  group_by(index) %>% 
  summarise(speeches = n()) %>% 
  ungroup()

# Monthly number of speeches 
docs$month <- as.character(docs$date) %>% 
  str_remove("-[0-9]{2}$")
ndocs.monthly <- docs %>% 
  group_by(index, month) %>% 
  summarise(speeches = n()) %>% 
  ungroup() %>% ungroup()

# Complete monthly series
# some months may be missing if there were no parliamentary speeches but should appear in the plot
complete.months <- data.frame(NULL) # generate complete series here
indices <- unique(speeches$index)
for(i in 1:length(indices)) {
  print(i)
  print(indices[i])
  minmonth <- paste0(min(ndocs.monthly$month[ndocs.monthly$index ==  indices[i]]), "-01") # First day of first month with observations
  maxmonth <- paste0(max(ndocs.monthly$month[ndocs.monthly$index ==  indices[i]]), "-01") # First day of last month with observations
  current <- seq.Date(as.Date(minmonth), as.Date(maxmonth), by = "month") %>% 
    as.character() %>% 
    str_remove("-01$") %>% 
    as.data.frame() %>% 
    rename(month = 1) %>% 
    mutate(index = indices[i])
  
  complete.months <- rbind(complete.months, current)
  
}
monthly.speeches <- complete.months %>% # Merge with monthly speech numbers ...
  left_join(ndocs.monthly, by = c("index", "month")) %>% 
  rename(monthly.speeches =  speeches)
monthly.speeches$monthly.speeches[is.na(monthly.speeches$monthly.speeches)] <- 0 # ... and set missing months to a (true) zero
write_rds(monthly.speeches, "./PLS-words-AMCAT/Data/monthly.speeches.rds") # Export


# Number of speeches by speaker
speaker.speeches <- docs %>% 
  group_by(index, speaker) %>% 
  summarise(s.speeches = n()) %>% 
  ungroup() %>% ungroup()
write_rds(speaker.speeches, "./PLS-words-AMCAT/Data/speaker.speeches.rds") # Export


# Number of speeches by party
party.speeches <- docs %>% 
  group_by(index, party) %>% 
  summarise(party.speeches = n()) %>% 
  ungroup() %>% ungroup()
write_rds(party.speeches, "./PLS-words-AMCAT/Data/party.speeches.rds") # Export

