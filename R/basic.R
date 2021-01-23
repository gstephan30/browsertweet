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
  #"epi & ds", "epidemiology 'data science'", "tweets",
  "rstats", "#rstats", "tweets",
  #"dq", 'journal data quality', "tweets",
  #"ije", "IJEeditorial", "user",
  #"AmJEpi", "AmJEpi", "user",
  #"JClinEpi", "JClinEpi", "user"
)

get_recent_tweet <- function(interest_name, query, type) {
  print(paste0("Fetching: ", interest_name))
  
  if (type == "tweets") {
    rec <- search_tweets(query, n = 1, include_rts = FALSE)
  }
  if (type == "user") {
    rec <- get_timeline(query, n = 1)
  }
   rec <- rec %>% 
     mutate(interest_name = interest_name) %>% 
     select(interest_name, everything())
  
  return(rec)
}


k <- 50
while (k != 0) {
  print(k)
  
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
      select(one_of("interest_name", "user_id", "status_id", "screen_name")) %>%
      anti_join(
        data %>%
          select(interest_name, user_id, status_id, screen_name))
    found <- nrow(new)
    
    if (found != 0) {
      paste0(
        "https://twitter.com/", 
        new$screen_name, 
        "/status/", 
        new$status_id
      ) %>% 
        browseURL()
    }
    
  }
  
  db <- db %>% 
    bind_rows(data) %>% 
    distinct(across(status_id), .keep_all = TRUE)
  
  print("saving")
  
  saveRDS(db, file = "data/db.rds")
  k <- k - 1
}



post_tweet("fetching #rstats")


db %>% 
  select(interest_name, user_id, status_id, text) %>% 
  group_split(interest_name) %>% 
  map(distinct)


db %>% 
  add_count(status_id) %>% select(n) %>% 
  filter()
  
######

data$user_id
data$status_id
data %>% 
  select(insterst_name, user_id, status_id)

if (nrow(data) != 0){
  tweet_id <- data$status_id
  #source <- data$user_id
  
  filter_test <- db %>% 
    bind_rows(data) %>% 
    filter(status_id == tweet_id)
  
  if (nrow(filter_test) == 2) {
    print("No new tweet found.")
  } else {
    print("new tweet found")
    
    if (!is.na(filter_test$urls_expanded_url[[1]][1])) {
      print("open link within tweet")
      
      browseURL(filter_test$urls_expanded_url[[1]][1])    
    } else {
      print("no link found, open tweet")
      paste0(
        "https://twitter.com/", 
        filter_test$screen_name, 
        "/status/", 
        filter_test$status_id
      ) %>% 
        browseURL()
      
    }
    
    db <- db %>% bind_rows(data) 
    saveRDS(db, file = "data/db.rds")
    
  }
}


