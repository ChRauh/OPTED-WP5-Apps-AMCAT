##################################################################
# Project:  OPTED WP5 Apps based on AMCAT server
# TasK:     Generate satic background data to be used in the apps
# Author:   @ChRauh / 23.08.2023
#################################################################

# The issues
# I. Data retrieved from AMCAT datbase needs to be normalized in various ways. Rather than repeating request to the database, store data to be normalized against once
# II. Time series extracted from AMCAT are incomplete if there are no query hits in a certain month, for example. Full time series needed as basis


# Packages
library(tidyverse)
library(amcat4r)

# Connect to database
host <- "https://parliaments.opted.eu/api/"
amcat_login(host) 


# All indices on server ###
indices <- list_indexes()


# For the app it is decisive that each of those comes with an identical set of fields (columns)
fields <- data.frame(NULL)
for (i in 1:nrow(indices)) {
  print(i)
  current <- get_fields(indices$name[i])
  current$index <- indices$name[i]
  fields <- rbind(fields, current)
}

fields2 <- table(fields$index, fields$name) %>% 
  as.data.frame() %>% 
  pivot_wider(names_from = "Var2", values_from = "Freq")

field.av <- colSums(fields2[2:ncol(fields2)]) %>% 
  as.data.frame() %>% 
  rename(indices = 1) %>% 
  rownames_to_column(var = "field")

# Fields available in all indices
field.av$field[field.av$indices == nrow(indices)]




# Get all documents for each index - basis ####

docs <- data.frame(NULL)
for(i in 1:nrow(indices)) {
  print(i)
  current <- amcat4r::query_documents(index = indices$name[i],  # Define which index on host
                                   queries = NULL,      # All docs
                                   fields = c("date", "speaker", "party"),       
                                   scroll = "15m",      # Should suffice, I hope
                                   per_page = 10000,       # Results per page: 1 means number of pages equals number of speeches (slow)
                                   max_pages = Inf) %>% 
    mutate(index = indices$name[i],
           date = as.character(date))
  docs <- plyr::rbind.fill(docs, current) # fills missing columns with NAs, currently CZ has a party field that is apprently empty
}

table(docs$party, useNA = "ifany")

docs$party[docs$party == "-"] <- NA
docs$party[docs$party == "NA"] <- NA


# Number of docs/speeches per parliament ####
# In the app, this is statically stored in source code

ndocs <- docs %>% 
  group_by(index) %>% 
  summarise(speeches = n()) %>% 
  ungroup()



# Monthly number of speeches ####

docs$month <- as.character(docs$date) %>% 
  str_remove("-[0-9]{2}$")

ndocs.monthly <- docs %>% 
  group_by(index, month) %>% 
  summarise(speeches = n()) %>% 
  ungroup() %>% ungroup()

# Complete monthly series ####
# Some months may be missing if there were no parliamentary speeches 

# For correct plots, generate complete series here
complete.months <- data.frame(NULL)
for(i in 1:nrow(indices)) {
  
  minmonth <- paste0(min(ndocs.monthly$month[ndocs.monthly$index ==  indices$name[i]]), "-01") # First day of first month with observations
  maxmonth <- paste0(max(ndocs.monthly$month[ndocs.monthly$index ==  indices$name[i]]), "-01") # First day of last month with observations
  
  current <- seq.Date(as.Date(minmonth), as.Date(maxmonth), by = "month") %>% 
    as.character() %>% 
    str_remove("-01$") %>% 
    as.data.frame() %>% 
    rename(month = 1) %>% 
    mutate(index = indices$name[i])
  
  complete.months <- rbind(complete.months, current)
  
}

# Merge with monthly speech numbers ...
monthly.speeches <- complete.months %>% 
  left_join(ndocs.monthly, by = c("index", "month")) %>% 
  rename(monthly.speeches =  speeches)

# ... and set missing months to a (true) zero
monthly.speeches$monthly.speeches[is.na(monthly.speeches$monthly.speeches)] <- 0

# Export
write_rds(monthly.speeches, "./PLS-words-AMCAT/Data/monthly.speeches.rds")



# Number of speeches by speaker ####

speaker.speeches <- docs %>% 
  group_by(index, speaker) %>% 
  summarise(s.speeches = n()) %>% 
  ungroup() %>% ungroup()

# Export
write_rds(speaker.speeches, "./PLS-words-AMCAT/Data/speaker.speeches.rds")




# Number of speeches by party ####

party.speeches <- docs %>% 
  group_by(index, party) %>% 
  summarise(party.speeches = n()) %>% 
  ungroup() %>% ungroup()

# Export
write_rds(party.speeches, "./PLS-words-AMCAT/Data/party.speeches.rds")



# TO DOs
# breaks when keywords are non-existent - check "arbeitslos" in spain
# Reverse party match - parties currently get dropped when there are no query hits


# Check whther we can quickly query hits
nrow(query_documents("speeches_germany",
                queries = "text:(Kommission)",
                fields = ".id", 
                per_page = 100,
                scroll = "5m",
                max_pages = Inf))


# # Logic of time series data in PLS-words
#
# hits <-    amcat4r::query_aggregate(index = "speeches_austria",  # Define which index on host - user.parliament()
#                                     queries = "migra*",      # Query
#                                     axes = list(list(name="date",       # Axes for aggregation (note the correct nesting of lists)
#                                                      field="date",
#                                                      interval="month"))) %>%
#   rename(month = date_month) %>%
#   mutate(month = str_remove(as.character(month), "-[0-9]{2}$")) # AMCAT returns daily date (first) here
#
#
# # Add to full range of monthly speeches
# full <- monthly.speeches %>%
#   filter(index == "speeches_austria") %>% # Only for user chosen parliament
#   left_join(hits, by = "month")
# full$n[is.na(full$n)] <- 0 # True zeros - no hits in this month
#
# full <- full %>%
#   mutate(share = n/monthly.speeches)
# full$share[is.na(full$share)] <- 0 # True zeros - no hits in this month
#
# # Calculate moving averages and turn into long shape for plotting
# long <- full %>%
#   select(-c(monthly.speeches, index)) %>%
#   mutate(share.ma = stats::filter(share, rep(1,5), sides = 2)/5) %>%
#   pivot_longer(2:4) %>%
#   mutate(series = ifelse(name == "n", "Monthly Count",
#                          ifelse(name == "share", "Monthly share", "Moving average (5 months)"))) %>%
#   select(-name) %>%
#   arrange(month, series)


# # Logic of party data in PLS-words
# 
# # AMCAT query
# hits <- amcat4r::query_aggregate(index = "speeches_austria",  # Define which index on host - user.parliament()
#                                  queries = "migra*",      # Query
#                                  axes = list(list(name="party",       # Axes for aggregation (note the correct nesting of lists)
#                                                   field="party"))) %>%
#   mutate(index = "speeches_austria")
# hits <- hits %>% filter(!(party %in% c(NA, "-", "fraktionslos", "ohne Klubzugehörigkeit", "independent")))
# 
# 
# # Add normalisation data and calculate share
# # hits <- hits %>%
# #   left_join(party.speeches, by = c("index", "party")) %>%
# #   mutate(share = n/party.speeches) %>%
# #   arrange(share)
# # hits$party2 <- factor(hits$party, levels = hits$party) # Plot ordering
# 
# # Add normalisation data and calculate share
# # While ensuring that all parties in parliament are included (even if none of the speeches matched the query)
# hits2 <- party.speeches %>% filter(index == "speeches_austria") %>% 
#   left_join(hits, by = c("index", "party")) %>% 
#   filter(!(party %in% c(NA, "-", "fraktionslos", "ohne Klubzugehörigkeit", "independent"))) %>% 
#   arrange(share)
# 
# hits2$n[is.na(hits2$n)] <- 0 # True zeros
# hits2$party2 <- factor(hits2$party, levels = hits2$party) # Plot ordering
# 
# # Plot
# ggplot(hits2, aes(y = party2, x = share))+
#   geom_col(width = .7, fill = "#0063a6") +
#   geom_vline(xintercept = mean(hits$share), linetype = "dashed")+
#   scale_x_continuous(labels = scales::percent)+
#   labs(title = "Keywords by party of speaker",
#        subtitle = paste("Parliament: ", user.parliament(), ". Keywords: ", input$words, sep = ""),
#        x = "Share of parliamentary speeches\nthat match the query\n",
#        y = "")+
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.text = element_text(color = "black"))




# # Logic of speaker data in PLS-words
# 
# # AMCAT query
# hits <- amcat4r::query_aggregate(index = "speeches_austria",  # Define which index on host - user.parliament()
#                                  queries = "migra*",      # Query
#                                  axes = list(list(name="speaker",       # Axes for aggregation (note the correct nesting of lists)
#                                                   field="speaker"))) %>% 
#   mutate(index = "speeches_austria")
# 
# # Add normalisation data and calculate shares
# hits <- hits %>% 
#   left_join(speaker.speeches, by = c("index", "speaker")) %>% 
#   mutate(share = n/s.speeches) %>% 
#   arrange(share)
# hits$speaker2 <- factor(hits$speaker, levels = hits$speaker) # Plot ordering
# 
# 
# 
# 
# # Plot
# ggplot(head(hits %>% arrange(-share), 25), aes(y = speaker2, x = share))+
#   geom_col(fill = "#0063a6", width = .7)+
#   # geom_vline(xintercept = mean(hits$share), linetype = "solid", color = "red")+
#   scale_x_continuous(expand = expansion(mult = c(0, 0.1)))+
#   labs(title = "Keyword usage by individual speakers (Top 25, in relative terms)",
#        subtitle = paste("Parliament: ", user.parliament(), ". Keywords: ", paste(user.words(), collapse = ", "), sep = ""),
#        x = "Share of keywords\namong all words spoken in parliament (%)\n",
#        y = "")+
#   theme_bw()+
#   theme(legend.position = "none",
#         axis.text = element_text(color = "black"))



