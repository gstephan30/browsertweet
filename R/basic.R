## Setup keys
library(rtweet)
library(dplyr)
library(purrr)
library(tidyr)

source("R/functions.R")
api_key <- setup_keys("api_key")
api_key_secret <- setup_keys("api_key_secret")
access_token <- setup_keys("access_token")
access_token_secret <- setup_keys("access_token_secret")
app_name <- setup_keys("app_name")

token <- create_token(app = app_name,
                      consumer_key = api_key,
                      consumer_secret = api_key_secret,
                      access_token = access_token,
                      access_secret = access_token_secret)


interests <- tribble(
  ~interest_name, ~query, ~type,
  "epi & ds", "epidemiology 'data science'", "tweets",
  "rstats", "#rstats", "tweets",
  "dq", 'journal data quality', "tweets",
  "ije", "IJEeditorial", "user",
  "AmJEpi", "AmJEpi", "user",
  "JClinEpi", "JClinEpi", "user"
)

get_recent_tweet <- function(interest_name, query, type) {
  print(paste0("Fetching: ", interest_name))
  
  if (type == "tweets") {
    rec <- search_tweets(query, n = 2, include_rts = FALSE)
  }
  if (type == "user") {
    rec <- get_timeline(query, n = 1)
  }
   rec <- rec %>% 
     mutate(interest_name = interest_name) %>% 
     select(interest_name, everything())
  
  return(rec)
}


data <- NULL
for (i in 1:nrow(interests)) {
  data[[i]] <- get_recent_tweet(pull(interests[i, 1]),
                                pull(interests[i, 2]),
                                pull(interests[i, 3]))
}
data <- data %>% 
  bind_rows()
  
  
db <- check_db()
  #### check if new
if (nrow(db) != 0) {
  
  new <- db %>% 
    bind_rows(data) %>% 
    select(screen_name, status_id) %>% 
    # is the discovered tweet new?
    inner_join(data) %>% 
    add_count(status_id, name = "ind") %>% 
    filter(ind == 1)
    
  
  if (nrow(new) != 0) {
    for (k in seq_along(new$screen_name)) {
      paste0("https://twitter.com/",
             new$screen_name[k],
             "/status/",
             new$status_id[k]) %>%
        browseURL()
    }
  }
}  

db <- db %>%
  bind_rows(data) %>%
  distinct(across(status_id), .keep_all = TRUE)
  
print("saving")
  
saveRDS(db, file = "data/db.rds")

