library(rvest)
library(tidyverse)
library(tibble)
library(stringr)
library(data.table)
library(googlesheets4)
library(googledrive)


# Login to kenpom.com

login <- "https://kenpom.com" 

pgsession<-session(login)
pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 1st form
filled_form<-html_form_set(pgform, email="####", password="#####")
session_submit(pgsession, filled_form)

# Define function to scrape list of box score links for the primary team-year

team_name <- "Villanova"
year <- 2023

get_games_list <- function(team_name,year) {
  
  url <- paste0("https://kenpom.com/team.php?team=",team_name,"&y=",as.character(year))
  page<-session_jump_to(pgsession, url)
  
  opp_names <- page %>% html_nodes("table#schedule-table tr:not(tr.un)")  %>% html_nodes("td:nth-of-type(4) a") %>% html_text()
  opp_links <- page %>% html_nodes("table#schedule-table tr:not(tr.un)") %>% html_nodes("td:nth-of-type(4) a") %>% html_attr("href") %>%           paste0("https://kenpom.com/",.)
  
  game_links <- page %>% html_nodes("table#schedule-table tr:not(tr.un)") %>% html_nodes("td:nth-of-type(5) a") %>% html_attr("href") %>% paste0("https://kenpom.com/",.)
  
  game_tbl <<- data.frame(team_name,opp_names,opp_links,game_links)
  opp_list_distinct <<- distinct(data.frame(game_tbl$game_links))
  names(opp_list_distinct)[names(opp_list_distinct) == "opp_tbl.opp_list_links"] <<- "opp_names"
  year <<- year
  team_master <<- data.frame(team_name,opp_names,game_links)
}

get_games_list("Villanova", 2023)

##game_tbl
##opp_list_distinct

# Define function to scrape list of box score links for opponents 

##team_name <- "Delaware+St."
##team_url <- "https://kenpom.com/team.php?team=Delaware+St."
##year <- 2023


get_opponents_game_list <- function(team_url) {
  
  page <- session_jump_to(pgsession, paste0(team_url,"&y=",as.character(year)))
  team_xml <- page %>% html_nodes("h5") %>% html_text()  
  start_name <- unlist(gregexpr(" ",team_xml))[1]+1
  end_name <- tail(unlist(gregexpr(" [(]", team_xml)), n=1)-1
  team_name_opp <- substring(team_xml,start_name, end_name)
  
  opp_opp_rk <- page %>% html_nodes("table#schedule-table tr:not(tr.un)") %>% html_nodes("td:nth-of-type(3)") %>% html_text()
  opp_opp_game_links <- page %>% html_nodes("table#schedule-table tr:not(tr.un)") %>% html_nodes("td:nth-of-type(5) a") %>% html_attr("href") %>% paste0("https://kenpom.com/",.)
  
  opp_tbl_clean <- data.frame(opp_opp_rk,opp_opp_game_links) %>% filter(opp_opp_rk != "NR")
  
  opp_opp_names <- page %>% html_nodes("table#schedule-table tr:not(tr.un)") %>% html_nodes("td:nth-of-type(4) a") %>% html_text()
  
  opp_tbl <- data.frame(team_name_opp,opp_opp_names ,opp_tbl_clean$opp_opp_game_links)
  
  assign(paste0(get("team_name_opp"),"_games_list"),opp_tbl,envir = .GlobalEnv)
}

#  Loop function to scrape the game links for all of primary team's opponents

for (x in game_tbl$opp_links) {
  get_opponents_game_list(x)
  Sys.sleep(0.0001)
}

mget_list <- paste0(c(game_tbl$opp_names),"_games_list")
opp_master <- do.call(rbind,mget(mget_list))

names(opp_master)[names(opp_master) == "team_name_opp"] <- "team_name"
names(opp_master)[names(opp_master) == "opp_opp_names"] <- "opp_names"
names(opp_master)[names(opp_master) == "opp_tbl_clean.opp_opp_game_links"] <- "game_links"

# combine primary team game links and opponents game links into one df
game_list <<- rbind(team_master,opp_master)
rownames(game_list) <- NULL

game_url<-"https://kenpom.com/box.php?g=320"

scrape_game_box <- function(game_url) {
  
  page <- session_jump_to(pgsession, game_url)

game_number <-  str_split(game_url ,"=",simplify=TRUE)[2]  #sslect all tr's whose class is not bottom or bottom 2?
team_1 <- page %>% html_nodes("span.teamnav:nth-of-type(1) b a") %>%  html_text()
team_1_ftm_fta <- page %>% html_nodes("table.box-table") %>% '[['(1)  %>% html_nodes("tbody tr:not(.bottom):not(.bottom2)")  %>% tail(1) %>%  html_nodes("td:nth-of-type(10) b") %>%  html_text()
team1_ftm <- str_split(team_1_ftm_fta,"-",simplify=TRUE)[1]
team1_fta <- str_split(team_1_ftm_fta,"-",simplify=TRUE)[2]
team_1_data <- data.frame("team_1_name"=team_1,"team_1_ftm"=team1_ftm,"team_1_fta"=team1_fta)

team_2 <- page %>% html_nodes("span.teamnav:nth-of-type(2) b a") %>%  html_text()
team_2_ftm_fta <- page %>% html_nodes("table.box-table") %>% '[['(2)  %>% html_nodes("tbody tr:not(.bottom):not(.bottom2)")  %>% tail(1) %>%  html_nodes("td:nth-of-type(10) b") %>%  html_text()
team2_ftm <- str_split(team_2_ftm_fta,"-",simplify=TRUE)[1]
team2_fta <- str_split(team_2_ftm_fta,"-",simplify=TRUE)[2]
team_2_data <- data.frame("team_2_name"=team_2,"team_2_ftm"=team2_ftm,"team_2_fta"=team2_fta)

game_stats <- cbind(team_1_data,team_2_data)
assign(paste0("game_",game_number,"_stats"),game_stats,envir = .GlobalEnv)
}

#test scrape_game_box(game_url)

game_ft_data <- map_df(game_list$game_links,scrape_game_box)

output <- distinct(tibble(game_list,game_ft_data))



# Write to google sheet

sheet_url <- "https://docs.google.com/spreadsheets/d/1O5E3FnIoCzyhDpB2qLY9Tgf-aH-uqi2J0kHWqV_6NQ4/edit#gid=0"


gs4_auth(scope = "https://www.googleapis.com/auth/drive")
drive_auth(token = gs4_token())

# write data in the appropriate sheet and range
range_write(output, ss = sheet_url, sheet = "FT Data", range = "A1:P100000")


#  https://github.com/yusuzech/r-web-scraping-cheat-sheet/blob/master/README.md#rvest7.11
# https://rvest.tidyverse.org/reference/html_nodes.html
# https://stackoverflow.com/questions/43598427/r-how-to-extract-items-from-xml-nodeset
# https://www.marsja.se/how-to-transpose-a-dataframe-or-matrix-in-r-with-the-t-function/
# https://googlesheets4.tidyverse.org/articles/drive-and-sheets.html

# start_date <- '2022-11-07'
# end_date <- '2023-02-27'
# date <- '2022-12-26'
