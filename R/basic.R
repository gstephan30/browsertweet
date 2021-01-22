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


db <- check_db()

interests <- tribble(
  ~insterst_name, ~query, ~type,
  "epi & ds", 'epidemiology "data science"', "tweets",
  #"rstats", "#rstats", "tweets",
  "dq", 'journal data quality', "tweets",
  "ije", "IJEeditorial", "user",
  "AmJEpi", "AmJEpi", "user",
  "JClinEpi", "JClinEpi", "user"
)

data <- interests %>% 
  mutate(twitter = case_when(type == "tweets" ~ map(query, search_tweets, n = 1, include_rts = FALSE),
                             type == "user" ~ map(query, get_timeline, n = 1),
                             TRUE ~ list("NA"))) %>% 
  unnest_wider(twitter)

db <- check_db()
db <- db %>% 
  bind_rows(data)

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


