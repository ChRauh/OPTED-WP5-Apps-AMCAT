############################################################
# Project:  OPTED WP5
# Task:     Prepare and submit ParlLawSpeech data (PLS)
#           to a dedicated AMCAT server
# Author:   Christian Rauh (25.08.2023)
############################################################

# Raw data description: https://chrauh.github.io/ParlLawSpeechTutorials


# Packages ####
library(tidyverse)
library(amcat4r)
amcat_login("https://opted.amcat.nl/amcat") # [e-mail address of registered user]


# Prepare PLS data ####

# Get file lists (local paths HP)
files <- list.files(path = "D:/Dropbox/OPTED datasets", recursive = T, full.names = T) %>% 
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

# Some fields are required on AMCAT servers
speeches <- speeches %>%
  filter(!is.na(date)) %>%
  filter(!is.na(title)) %>%
  filter(!is.na(text))



# Create speech indices on AMCAT server
# Note login above

for (i in 1:nrow(speeches_files)) {
  delete_index(speeches_files$index[i]) # Clean up beforehand ...
}


for (i in 1:nrow(speeches_files)) {
  print(i)
  create_index(speeches_files$index[i], description = paste0("OPTED-WP5: Parliamentary speeches | ", speeches_files$parliament[i]))
}

list_indexes()


# Upload
start_speeches <- Sys.time()
for (i in 1:nrow(speeches_files)) {
  print(paste0(i, ": ", speeches_files$index[i]))
  upload_documents(speeches_files$index[i], speeches %>% filter(index == speeches_files$index[i]))
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

# Some fields are required on AMCAT servers
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


# Some fields are required on AMCAT servers (I'm killing full countries here - raw data errors!)
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



