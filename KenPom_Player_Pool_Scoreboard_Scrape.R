
## This is an R script I wrote to track entrant score in a contest I ran for the NCAA Men's Basketball tournament in 2023. 
## It leverages rvest to login to kenpom.com and scrape game box scores and write the player stats by game to a google sheet
## (https://docs.google.com/spreadsheets/d/1fzMpvaa6aO20lepu3dnwScj32jBtKeWaJ6iZEaVgTZk/edit#gid=642843034)
## The google sheet then takes this data and displays it in a format which scores the entries and displays is as a scoreboard


library(rvest)
library(tidyverse)
library(tibble)
library(stringr)
library(data.table)
library(googlesheets4)
library(googledrive)
library(scales)
library(lubridate)


# login into kenpom (you need a valid account to use this script)

login <- "https://kenpom.com" 
pgsession<-session(login)
pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 1st form
filled_form<-html_form_set(pgform, email="#########", password="#####")
session_submit(pgsession, filled_form)

# scrape NCAA tournament game box scores from kenpom

# define tournament schedule
date <- ymd(c('2023-03-16','2023-03-17','2023-03-18','2023-03-19','2023-03-23','2023-03-24','2023-03-25','2023-03-26','2023-04-01', '2023-04-03'))
round <- c('First Round','First Round','Second Round','Second Round','Sweet 16','Sweet 16','Elite 8','Elite 8','Final 4','Championship')

schedule <- tibble(date,round)

# get current date

sys_date <- ymd(c(rep(Sys.Date(),10)))

sys_date_time <- Sys.time()

# trim down schedule date list to avoid 404 errors when calling links that don't exist yet
tourney_days_to_date <- schedule %>% filter(schedule$date == sys_date | schedule$date < sys_date)

## sample url for testing fanmatch_url <- "https://kenpom.com/fanmatch.php?d=2023-03-15"

## define a function to get the list of urls of game box scores that are complete and available
scrape_completed_box_score_links <- function(date,round){
  
  login <- "https://kenpom.com" 
  pgsession<-session(login)
  pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 1st form
  filled_form<-html_form_set(pgform, email="#######", password="######")
  session_submit(pgsession, filled_form)

  fanmatch_url <-  paste0("https://kenpom.com/fanmatch.php?d=",date)
      
  page <-session_jump_to(pgsession, fanmatch_url)
  completed_games_links <- page %>% html_nodes('tbody tr td:nth-of-type(3) a[href*="box"]') %>% html_attr("href") %>% paste0("https://kenpom.com/",.)

  dates_links <- tibble(date,round,completed_games_links) 
}

## run the function for each date in the tournament that has happened thus far
dates_rounds_links <- map2_dfr(tourney_days_to_date$date,tourney_days_to_date$round,scrape_completed_box_score_links) %>% filter(completed_games_links != "https://kenpom.com/")

# ## define a function to get the player stats by game
scrape_game_box <- function(date, game_url) {
  
  login <- "https://kenpom.com" 
  pgsession<-session(login)
  pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 1st form
  filled_form<-html_form_set(pgform, email="f.cocuzza@gmail.com", password="threenonymousdonor")
  session_submit(pgsession, filled_form)
  
  page <- session_jump_to(pgsession, game_url)

  player_name <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(3)") %>%  html_text() %>% str_replace(.,"\n","") %>% str_replace(.,"MVP","") %>% tibble("player_name"=.) %>% filter(.,str_detect(player_name,"TOTAL",negate = TRUE))
  player_points <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(7)") %>%  html_text()  %>% tibble("points"=.)  %>%  filter(.,str_detect(points,"\\.",negate = TRUE))
  player_2PM_2PA <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(8)") %>%  html_text()  %>% tibble("TWPMA"=.)  %>%  filter(.,str_detect(TWPMA,"\\.",negate = TRUE))
  player_2PM <- str_split(player_2PM_2PA$TWPMA,"-",simplify=TRUE)[,1]
  player_2PA <- str_split(player_2PM_2PA$TWPMA,"-",simplify=TRUE)[,2]
  player_3PM_3PA <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(9)") %>%  html_text()  %>% tibble("THPMA"=.)  %>%  filter(.,str_detect(THPMA,"\\.",negate = TRUE))
  player_3PM <- str_split(player_3PM_3PA$THPMA,"-",simplify=TRUE)[,1]
  player_3PA <- str_split(player_3PM_3PA$THPMA,"-",simplify=TRUE)[,2] 
  player_FTM_FTA <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(10)") %>%  html_text()  %>% tibble("FTMA"=.)  %>%  filter(.,str_detect(FTMA,"\\.",negate = TRUE))
  player_FTM <- str_split(player_FTM_FTA$FTMA,"-",simplify=TRUE)[,1]
  player_FTA <- str_split(player_FTM_FTA$FTMA,"-",simplify=TRUE)[,2] 
  player_OR <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(11)") %>%  html_text()  %>% tibble("OR"=.)  %>%  filter(.,str_detect(OR,"\\.",negate = TRUE))
  player_DR <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(12)") %>%  html_text()  %>% tibble("DR"=.)  %>%  filter(.,str_detect(DR,"\\.",negate = TRUE))
  player_AST <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(13)") %>%  html_text()  %>% tibble("AST"=.)  %>%  filter(.,str_detect(AST,"\\.",negate = TRUE))
  player_TO <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(14)") %>%  html_text()  %>% tibble("TO"=.)  %>%  filter(.,str_detect(TO,"\\.",negate = TRUE))
  player_BLK <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(15)") %>%  html_text()  %>% tibble("BLK"=.)  %>%  filter(.,str_detect(BLK,"\\.",negate = TRUE))
  player_STL <- page %>% html_nodes("table.box-table tbody tr:not(.bottom):not(.bottom2) td:nth-of-type(16)") %>%  html_text()  %>% tibble("STL"=.)  %>%  filter(.,str_detect(STL,"\\.",negate = TRUE))
  
  #bind all stats together
  player_game_stats <- tibble(date, game_url, player_name,player_points,player_2PM,player_2PA,player_3PM,player_3PA,player_FTM,player_FTA,player_OR,player_DR,player_AST,player_TO,player_BLK,player_STL)  

  }

## run the function for box score url that has been scraped thus far
dates_rounds_links_stats <- map2_dfr(dates_rounds_links$date,dates_rounds_links$completed_games_links,scrape_game_box) 

## convert stats to number format for easy post-processing
dates_rounds_links_stats$points = as.numeric(dates_rounds_links_stats$points)
dates_rounds_links_stats$player_2PM = as.numeric(dates_rounds_links_stats$player_2PM)
dates_rounds_links_stats$player_2PA = as.numeric(dates_rounds_links_stats$player_2PA)
dates_rounds_links_stats$player_3PM = as.numeric(dates_rounds_links_stats$player_3PM)
dates_rounds_links_stats$player_3PA = as.numeric(dates_rounds_links_stats$player_3PA)
dates_rounds_links_stats$player_FTM = as.numeric(dates_rounds_links_stats$player_FTM)
dates_rounds_links_stats$player_FTA = as.numeric(dates_rounds_links_stats$player_FTA)
dates_rounds_links_stats$OR = as.numeric(dates_rounds_links_stats$OR)
dates_rounds_links_stats$DR = as.numeric(dates_rounds_links_stats$DR)
dates_rounds_links_stats$AST = as.numeric(dates_rounds_links_stats$AST)
dates_rounds_links_stats$TO = as.numeric(dates_rounds_links_stats$TO)
dates_rounds_links_stats$BLK = as.numeric(dates_rounds_links_stats$BLK)
dates_rounds_links_stats$STL = as.numeric(dates_rounds_links_stats$STL)

## join on additional schedule fields
dates_rounds_links_stats <- dates_rounds_links_stats %>% full_join(schedule, by=c('date'='date')) %>% full_join(schedule, by=c('date'='date')) %>% distinct()

## get list of teams that have are 'still alive' in the tournament
page<-session_jump_to(pgsession, login)
kp_team_names <- page %>% html_nodes('tr.tourney td:nth-of-type(2) a') %>% html_text() %>% tibble()

## write data to a respective google sheet ranges
sheet_url <- "https://docs.google.com/spreadsheets/d/1fzMpvaa6aO20lepu3dnwScj32jBtKeWaJ6iZEaVgTZk/edit#gid=0"
##gs4_auth(scope = "https://www.googleapis.com/auth/drive")

##drive_auth(token = gs4_token())
# write data in the appropriate sheet and range

range_write(dates_rounds_links_stats, ss = sheet_url, sheet = "Player Stats", range = "A1:Z10000")
range_write(kp_team_names, ss = sheet_url, sheet = "Teams_Alive", range = "A1:Z10000")
range_write(tibble(sys_date_time), ss = sheet_url, sheet = "Last_Update", range = "A1")



###### references materials below
#https://rawgit.com/jennybc/googlesheets/master/vignettes/managing-auth-tokens.html#how-to-completely-avoid-reading-this-document
#https://towardsdatascience.com/using-r-and-python-in-google-sheets-formulas-b397b302098
