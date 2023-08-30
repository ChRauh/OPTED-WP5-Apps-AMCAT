
# The issue: Upload of the (rather large) DK speech data repeatedly fails at different stages of the proces (HTTP 500)
# Here I upload speeches individually, catching errors  and repeat upload once as an alternative to bulk download



# Packages ####
library(tidyverse)
library(amcat4r)
amcat_login("https://opted.amcat.nl/amcat") # [e-mail address of registered user]

# Denmark
speeches <- read_rds("C:/Users/rauh/Dropbox/OPTED datasets/Denmark/Corpus_speeches_denmark.RDS") %>%
  select(speech_ID, date, text, speaker, party, agenda, speech_procedure_ID) %>%
  mutate(date = as.Date(date)) %>%
  mutate(title = paste0(as.character(date), " - ", speech_ID, "; ", speaker, " (", party, ")")) %>%
  mutate(speaker_party = paste0(speaker, " (", party,")")) %>%
  mutate(index = "speeches_denmark") %>%
  mutate(parliament = "Folketing (Denmark)")

# Some fields are required on AMCAT servers
speeches <- speeches %>%
  filter(!is.na(date)) %>%
  filter(!is.na(title)) %>%
  filter(!is.na(text))


# Clean up beforehand ...
delete_index("speeches_denmark") # Clean up beforehand ...

# Re-create index
create_index("speeches_denmark", description = paste0("OPTED-WP5: Parliamentary speeches | ", "Folketing (Denmark)"))
set_fields(index = "speeches_denmark",
           list(speaker = "keyword",
                party = "keyword",
                speaker_party = "keyword")) #


# Upload marker
speeches$upl <- T

# Upload each row individually ----
for (i in 1:nrow(speeches)) {
  tryCatch({
    print(paste0(i, ": ", round((i/nrow(speeches))*100, 2), "%"))
    upload_documents(index = "speeches_denmark", 
                     documents <- speeches[i, ])
  }, 
  error=function(e){
    speeches$upl[i] <<- F # Mark failures in data
    cat("ERROR :",conditionMessage(e),"\n")
    })
}


# Save errors 
write_rds(speeches %>%  select(c(speech_ID, upl)), "errors.rds")


# Repeat for failed uploads ----

speeches <- speeches %>% filter(!upl)
speeches$upl <- T

for (i in 1:nrow(speeches)) {
  tryCatch({
    print(paste0(i, ": ", round((i/nrow(speeches)))*100, "%"))
    upload_documents(index = "speeches_denmark", 
                     documents <- speeches[i, ])
  }, 
  error=function(e){
    speeches$upl[i] <<- F # Mark failures in data
    cat("ERROR :",conditionMessage(e),"\n")
  })
}



