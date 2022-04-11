# This is a script I wrote to scrape live scores for golfers competing in the 2022 Masters Tournament
# which fed a leaderboard created in googlesheets to track the scores of entrants in a contest among a group of friends

# load required packages
  library(rvest)
  library(tidyverse)
  library(curl)
  library(Rcpp)
  library(googlesheets4)
  library(googledrive)
  library(dplyr)
  library(lubridate)
  library(taskscheduleR)

  # define url data is to be scraped from
  url <- "https://www.espn.com/golf/leaderboard"
  
  # read page html into R
  scoreboard_page <- read_html(url) 
  
  # extract desired data columns using Rvest html_nodes function
  # CSS selectors reference: (https://www.w3schools.com/cssref/css_selectors.asp)
  
  player_names <- scoreboard_page %>% html_nodes("a.leaderboard_player_name") %>% html_text()
  overall_scores_to_par <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(5)") %>% html_text() %>% gsub("E", "0",.) %>% as.numeric()
  today_scores_to_par <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(6)") %>% html_text() %>% gsub("E", "0",.) %>% as.numeric()
  thru <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(7)") %>% html_text() 
  r1 <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(8)") %>% html_text()%>% gsub("E", "0",.) %>% as.numeric()
  r2 <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(9)") %>% html_text()%>% gsub("E", "0",.) %>% as.numeric()
  r3 <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(10)") %>% html_text()%>% gsub("E", "0",.) %>% as.numeric()
  r4 <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(11)") %>% html_text()%>% gsub("E", "0",.) %>% as.numeric()
  tot <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(12)") %>% html_text() %>% gsub("E", "0",.) %>% as.numeric()
  last_updated <- Sys.time()-hours(4)
  
  #combine columns into one table
  leaderboard <- tibble::tibble(last_updated, player_names, overall_scores_to_par, today_scores_to_par, thru , r1, r2, r3, r4, tot)
  
  # define url of the google sheet in which to write the data (the sheet sharing settings must be set to 'Anyone on the internet with this link can edit')
  ss2 <- "https://docs.google.com/spreadsheets/d/12dh0CS_UoRKVnOWgq8Qaw5rng5CnOydYN46QecH9YlE/edit?usp=sharing"
  
  # disable R from attempting to use authentication credentials - we want this to run non-interactively
  drive_deauth()
  
  # write data in the appropriate sheet and range
  range_write(leaderboard, ss = ss2, sheet = "Frankie Live Scoring", range = "A1:J92")
  
  # script is then scheduled to run every minute using the taskscheduleR AddIn for RStudio
  # future development to consider: deploy as RShiny app to avoid need to keep local machine running 
  
  

