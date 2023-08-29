# Packages
library(tidyverse)
library(amcat4r)

# Connect to database
# host <- "https://parliaments.opted.eu/api/" # Vienna server
host <- "https://opted.amcat.nl/amcat" # AMS server - [registered e-mail, stored locally]
amcat_login(host) 


# query_documents returns NAs in date field
test <-
  amcat4r::query_documents(index = "speeches_germany",  # Define which index on host
                         queries = NULL,      # All docs
                         fields = c("date", "speaker", "party"),       
                         scroll = "15m",      # Should suffice, I hope
                         per_page = 10000,       # Results per page: 1 means number of pages equals number of speeches (slow)
                         max_pages = Inf)

sum(is.na(test$date)) == nrow(test) # TRUE !? Same result for other speeches_* indices


# But "date" field apparently not empty on server
test2 <-    amcat4r::query_aggregate(index = "speeches_germany",  # Define which index on host - user.parliament()
                                    queries = NULL,      # Query
                                    axes = list(list(name="date",       # Axes for aggregation (note the correct nesting of lists)
                                                     field="date",
                                                     interval="month")))

sum(is.na(test2$date_month)) # 0
min(test2$date_month) # Correct
max(test2$date_month) # Correct



########
str(speeches$date)
sum(is.na(speeches$date))
sum(str_detect(speeches$date, "-"))



#######


test <-    amcat4r::query_aggregate(index = "speeches_austria",  # Define which index on host - user.parliament()
                                    queries = "migration",      # Query
                                    axes = list(list(name="date",       # Axes for aggregation (note the correct nesting of lists)
                                                     field="date",
                                                     interval="month"))) %>% 
  rename(month = date_month) %>% 
  mutate(month = str_remove(as.character(month), "-[0-9]{2}$")) # AMCAT returns daily date (first) here 


test2 <- amcat4r::query_aggregate(index = "speeches_austria",  # Define which index on host - user.parliament()
                                 queries = "migration",      # Query
                                 axes = list(list(name="party",       # Axes for aggregation (note the correct nesting of lists)
                                                  field="party"))) %>% 
  mutate(index = "speeches_austria")

get_fields("speeches_austria")



# reprex for aggregation error

# Data with NA in one non-required field
my_data <- data.frame(date = rep(as.Date("2020-01-01"), 6),
                      title = paste0("Title ", seq(1:6)),
                      text = paste0("bla bla blub ", seq(1:6)),
                      aggregator = c("A", "A", "A", "B", "B", NA))

create_index("my_index", description = "My Index")
upload_documents("my_index", my_data)

# Query aggregated along variable with NA -> HTTP 500 error
query_aggregate(index = "my_index",
                queries = "text:blub",
                axes = list(list(name="aggregator",
                                 field="aggregator")))


# Working without the NA
delete_index("my_index")
my_data <- data.frame(date = rep(as.Date("2020-01-01"), 6),
                      title = paste0("Title ", seq(1:6)),
                      text = paste0("bla bla blub ", seq(1:6)),
                      aggregator = factor(c("A", "A", "A", "B", "B", "B")))
create_index("my_index", description = "My Index")
set_fields(index = "my_index", list(aggregator = "keyword")) # Before upload
upload_documents("my_index", my_data)
query_aggregate(index = "my_index",
                queries = NULL,
                axes = list(list(name="aggregator",
                                 field="aggregator")))
get_fields("my_index")
set_fields(index = "my_index", fields = list(aggregator = "keyword"))
set_fields(index = "my_index", list(aggregator = "keyword"))
help <- list_indexes()


query_aggregate("my_index",
                                axes = list(list(field="text")))


library(amcat4r)
host <- "https://parliaments.opted.eu/api/"
amcat_login(host) # MUST BE DONE ON THE SHINY SERVER AND ENCRYPTED THERE!


list_indexes()
get_fields("speeches_austria") # Keywords!



#########


for (col in c("speaker", "party", "speaker_party")){
  Encoding(speeches[[col]]) <- "UTF-8"}

# Ideas why upload breaks

# Special symbols in Keyowrds - no, also breaks without setting certain columns to keyword
# Chuck size in upload (set to 50 instead of 100) - breaks at same percent value - not it


# in ebody[["detail"]][[1]][["msg"]] : subscript out of bounds
# Error in ebody[["detail"]][[1]][["loc"]] : subscript out of bounds
# Error in `httr2::req_perform()`:
#   ! HTTP 400 Bad Request.
# • There was an error parsing the body: 
#   Run `rlang::last_trace()` to see where the error occurred.
# > rlang::last_trace()
# <error/httr2_http_400>
#   Error in `httr2::req_perform()`:
#   ! HTTP 400 Bad Request.
# • There was an error parsing the body: 
#   ---
#   Backtrace:
#   ▆
# 1. └─amcat4r::upload_documents(...)
# 2.   └─amcat4r:::request(...)
# 3.     └─amcat4r:::request_response(...)
# 4.       └─httr2::req_perform(req)
