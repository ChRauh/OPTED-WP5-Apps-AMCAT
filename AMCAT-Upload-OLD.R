############################################################
# Project:  OPTED WP5
# Task:     Prepare and submit ParlLawSpeech data (PLS)
#           to a dedicated AMCAT server
# Author:   Christian Rauh (29.08.2023)
############################################################

# Raw data description: https://chrauh.github.io/ParlLawSpeechTutorials


# Packages ####
library(tidyverse)
library(amcat4r)
amcat_login("https://opted.amcat.nl/amcat") # [e-mail address of registered user]


# Prepare PLS data ####

# Get file lists (local paths HP)
# data.path <- "D:/Dropbox/OPTED datasets" # HP home
data.path <- "C:/Users/rauh/Dropbox/OPTED datasets" # WZB
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

# Consistent encoding (at least declared)
for (col in c("title", "text", "speaker", "party", "speaker_party")){
  Encoding(speeches[[col]]) <- "UTF-8"}


# Some fields may never be empty on AMCAT servers (2 cases)
speeches <- speeches %>%
  filter(!is.na(date)) %>%
  filter(!is.na(title)) %>%
  filter(!is.na(text))



# Normalization data for PLS-Words app
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

# Complete monthly series, some months may be missing if there were no parliamentary speeches 
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

rm(speaker.speeches, party.speeches, monthly.speeches, ndocs, ndocs.monthly)
gc()



# Create speech indices on AMCAT server
# Note login above

for (i in 1:nrow(speeches_files)) {
  delete_index(speeches_files$index[i]) # Clean up beforehand ...
}


for (i in 1:nrow(speeches_files)) {
  print(i)
  create_index(speeches_files$index[i], description = paste0("OPTED-WP5: Parliamentary speeches | ", speeches_files$parliament[i]))
  # set_fields(index = speeches_files$index[i], 
  #            list(speaker = "keyword",
  #                 party = "keyword",
  #                 speaker_party = "keyword")) # required for query_aggregate, must be set before upload!
}

help <- list_indexes()


# Only columns used in the OPTED apps
speeches <- speeches %>% select(c(date, title, text, speaker, party, speaker_party, index))


# Upload
start_speeches <- Sys.time()
for (i in 1:nrow(speeches_files)) {
  print(paste0(i, ": ", speeches_files$index[i]))
  upload_documents(speeches_files$index[i], speeches %>% filter(index == speeches_files$index[i]), chunk_size = 50L)
}
end_speeches <- Sys.time()
end_speeches - start_speeches # 2.13 hours

gc()




# Laws ####

laws_files <- files %>%  filter(type == "laws")

# Assemble laws data

laws <- data.frame()
for (i in 1:nrow(laws_files)) {
  
  print(laws_files$file[i])
  
  # Load data
  df <- read_rds(laws_files$file[i]) 
  
  # Make missing metadata explicit
  if(!"adoption_date" %in% colnames(df)) {
    df$adoption_date <- NA
  }
  if(!"law_ID" %in% colnames(df)) {
    df$law_ID <- NA
  }
  if(!"speech_procedure_ID" %in% colnames(df)) {
    df$speech_procedure_ID <- NA
  }
  
  # Harmonize the data
  df <- df %>% 
    select(law_ID, adoption_date, law_text, speech_procedure_ID) %>% 
    rename(date = adoption_date,
           text = law_text) %>% 
    mutate(date = as.Date(date)) %>% 
    mutate(title = paste0(as.character(date), " - ", law_ID, "; ", speech_procedure_ID)) %>% 
    mutate(index = laws_files$index[i]) %>% 
    mutate(parliament = laws_files$parliament[i])
  
  # Append to target
  laws <- rbind(laws, df)
}

rm(df)
gc()

summary(laws)

# Some fields may never be empty on AMCAT servers
laws <- laws %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(title)) %>%
  filter(!is.na(text))


# Create speech indices on AMCAT server
# Note login above

# for (i in 1:nrow(laws_files)) {
#   delete_index(laws_files$index[i]) # Clean up beforehand ...
# }

for (i in 1:nrow(laws_files)) {
  print(i)
  create_index(laws_files$index[i], description = paste0("OPTED-WP5: Parliamentary laws | ", laws_files$parliament[i]))
}

list_indexes()


# Upload
start_laws <- Sys.time()
for (i in 1:nrow(laws_files)) {
  print(paste0(i, ": ", laws_files$index[i]))
  upload_documents(laws_files$index[i], laws %>% filter(index == laws_files$index[i]))
}
end_laws <- Sys.time()
end_laws - start_laws # 4 mins




# Bills ####

bills_files <- files %>%  filter(type == "bills") %>% 
  filter(!(country %in% c("Germany", "Austria"))) # Date issues in these for now

# Assemble bills data

bills <- data.frame()
for (i in 1:nrow(bills_files)) {
  
  print(bills_files$file[i])
  
  # Load data
  df <- read_rds(bills_files$file[i]) 
  
  # Make missing metadata explicit
  if(!"initiation_date" %in% colnames(df)) {
    df$initiation_date <- NA
  }
  if(!"bill_ID" %in% colnames(df)) {
    df$bill_ID <- NA
  }
  if(!"speech_procedure_ID" %in% colnames(df)) {
    df$speech_procedure_ID <- NA
  }
  
  # Harmonize the data
  df <- df %>% 
    select(bill_ID, initiation_date, bill_text, speech_procedure_ID) %>% 
    rename(date = initiation_date,
           text = bill_text) %>% 
    mutate(date = as.Date(date)) %>% # Brakes for Dates in German
    mutate(title = paste0(as.character(date), " - ", bill_ID, "; ", speech_procedure_ID)) %>% 
    mutate(index = bills_files$index[i]) %>% 
    mutate(parliament = bills_files$parliament[i])
  
  # Append to target
  bills <- rbind(bills, df)
}

rm(df)
gc()

summary(bills)


# Some fields may never be empty on AMCAT servers (I'm killing full countries here - raw data errors!)
bills <- bills %>% 
  filter(!is.na(date)) %>% 
  filter(!is.na(title)) %>%
  filter(!is.na(text))

# Create speech indices on AMCAT server
# Note login above

# for (i in 1:nrow(bills_files)) {
#   delete_index(bills_files$index[i]) # Clean up beforehand ...
# }


for (i in 1:nrow(bills_files)) {
  print(i)
  create_index(bills_files$index[i], description = paste0("OPTED-WP5: Parliamentary bills | ", bills_files$parliament[i]))
}

list_indexes()


# Upload
start_bills <- Sys.time()
for (i in 1:nrow(bills_files)) {
  print(paste0(i, ": ", bills_files$index[i]))
  upload_documents(bills_files$index[i], bills %>% filter(index == bills_files$index[i]))
}
end_bills <- Sys.time()
end_bills - start_bills # 



