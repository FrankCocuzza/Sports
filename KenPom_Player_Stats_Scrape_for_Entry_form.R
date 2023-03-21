library(rvest)
library(tidyverse)
library(tibble)
library(stringr)
library(data.table)
library(googlesheets4)
library(googledrive)
library(scales)
libr


# login into kenpom

login <- "https://kenpom.com" 
pgsession<-session(login)
pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 1st form
filled_form<-html_form_set(pgform, email="#####", password="######")
session_submit(pgsession, filled_form)


# scrape links of teams in tournament

team_name <- "Penn+St."
year <- 2023

url <- paste0("https://kenpom.com/team.php?team=",team_name,"&y=",as.character(year))

page<-session_jump_to(pgsession, login)
kp_team_names <- page %>% html_nodes('tr.tourney td:nth-of-type(2) a') %>% html_text()

kp_team_links <- page %>% html_nodes('tr.tourney td:nth-of-type(2) a') %>% html_attr("href") %>% as.character() %>% paste0("https://kenpom.com/",.) %>% tibble(kp_team_links=.)

kp_team_seed <-  page %>% html_nodes('tr.tourney td:nth-of-type(2) span.seed') %>% html_text()
kp_teams_links <- tibble(kp_team_seed,kp_team_names,kp_team_links)
kp_team_seed <- tibble(kp_team_seed,kp_team_names)

# get players from kenpom page

##team <- "Houston"
##url <- "https://kenpom.com/team.php?team=Houston"

scrape_player_info <- function(team,url){

page<-session_jump_to(pgsession, url)


kenpom_player_names <- page %>% html_nodes('tr.player a[href*="player.php"]') %>% html_text()
kenpom_player_links <- page %>% html_nodes('tr.player a[href*="player.php"]') %>% html_attr("href") %>% as.character() %>% paste0("https://kenpom.com/",.) 
team_games <-  page %>% html_nodes('h5 span:nth-of-type(2)')%>% html_text() %>% str_split(.,"-",simplify=TRUE) %>% gsub("\\(","",.) %>% gsub("\\)","",.) %>% as.numeric() %>% sum()
player_game_count <- page %>% html_nodes('tr.player td.G') %>% html_text()
game_count_str_lengths <- page %>% html_nodes('tr.player td.G') %>% as.character() %>% str_length(.)
injury_flag <- case_when(game_count_str_lengths > 25 ~1, TRUE~0)
##CBBR_player_url <- kenpom_player_names %>% str_replace_all(" ","-") %>% paste0("https://www.sports-reference.com/cbb/players/",.,"-1.html") %>% tolower() %>% str_replace_all("'","")
kenpom_player_tbl <- data.frame(team,team_games,url,kenpom_player_names,kenpom_player_links,player_game_count,injury_flag)
}

kenpom_players <- map2_dfr(kp_teams_links$kp_team_names,kp_teams_links$kp_team_links,scrape_player_info)



##player_url <- "https://kenpom.com/player.php?p=45371"

### drill down to player game logs to get REB, AST, TO, STL data
scrape_player_logs <- function(player_url){

  page<-session_jump_to(pgsession, player_url)

  player_pts <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(14)') %>% html_text() %>% as.numeric()
  player_2P_FGM <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(15)') %>% html_text() %>% str_split(.,"-",simplify=TRUE) %>% .[,1] %>% as.numeric()
  player_2P_FGA <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(15)') %>% html_text() %>% str_split(.,"-",simplify=TRUE) %>% .[,2] %>% as.numeric()
  player_3P_FGM <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(16)') %>% html_text() %>% str_split(.,"-",simplify=TRUE) %>% .[,1] %>% as.numeric()
  player_3P_FGA <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(16)') %>% html_text() %>% str_split(.,"-",simplify=TRUE) %>% .[,2] %>% as.numeric()
  player_FTM <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(17)') %>% html_text() %>% str_split(.,"-",simplify=TRUE) %>% .[,1] %>% as.numeric()
  player_FTA <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(17)') %>% html_text() %>% str_split(.,"-",simplify=TRUE) %>% .[,2] %>% as.numeric()
  player_OR <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(18)') %>% html_text() %>% as.numeric()
  player_DR <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(19)') %>% html_text() %>% as.numeric()
  player_A <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(20)') %>% html_text() %>% as.numeric()
  player_TO <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(21)') %>% html_text() %>% as.numeric()
  player_BLK <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(22)') %>% html_text() %>% as.numeric()
  player_STL <- page %>% html_nodes('table#schedule-table') %>% '[['(1) %>% html_nodes('tr td:nth-of-type(23)') %>% html_text() %>% as.numeric()
  
  kenpom_player_logs <- data.frame(player_url,player_pts,player_2P_FGM,player_2P_FGA,player_3P_FGM,player_3P_FGA,player_FTM,player_FTA,player_OR,player_DR,player_A,player_TO,player_BLK,player_STL)
  kenpom_player_season_stats <- kenpom_player_logs %>% group_by(player_url ) %>%  
  summarize(
  'PTS' = sum(player_pts),
  '2PM' = sum(player_2P_FGM),
  '2PA' = sum(player_2P_FGA),
  '3PM' = sum(player_3P_FGM),
  '3PA' = sum(player_3P_FGA),
  'FTM' = sum(player_FTM),
  'FTA' = sum(player_FTA),
  'OR' = sum(player_OR),
  'DR' = sum(player_DR),
  'A' = sum(player_A),
  'TO' = sum(player_TO),
  'BLK' = sum(player_BLK),
  'STL' = sum(player_STL)
  )
}

kenpom_player_season_stats_agg <- map_dfr(kenpom_players$kenpom_player_links,scrape_player_logs)
kenpom_player_stats <- kenpom_players %>% full_join(kenpom_player_season_stats_agg, by=c('kenpom_player_links'='player_url'))
kenpom_player_stats <- kenpom_player_stats %>% full_join(kp_team_seed, by=c('team'='kp_team_names'))
kenpom_player_team <- tibble(player = kenpom_player_stats$kenpom_player_names, team =kenpom_player_stats$team)


sheet_url <- "https://docs.google.com/spreadsheets/d/14JKtkbfOIabnRtmEo8dV5U2H0XiAsEj8Soyxr_bNJ6c/edit#gid=0"
gs4_auth(scope = "https://www.googleapis.com/auth/drive")

drive_auth(token = gs4_token())
# write data in the appropriate sheet and range
range_write(kenpom_player_team , ss = sheet_url, sheet = "Player-Team", range = "A1:X1000")


# scrape NCAA tournament game box scores from kenpom



login <- "https://kenpom.com" 
pgsession<-session(login)
pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 1st form
filled_form<-html_form_set(pgform, email="f.cocuzza@gmail.com", password="threenonymousdonor")
session_submit(pgsession, filled_form)


# define tournament schedule
date <- ymd(c('2023-03-15','2023-03-16','2023-03-18','2023-03-18','2023-03-23','2023-03-24','2023-03-25','2023-03-26','2023-04-01', '2023-04-03'))
round <- c('First Round','First Round','Second Round','Second Round','Sweet 16','Sweet 16','Elite 8','Elite 8','Final 4','Championship')

schedule <- tibble(date,round)

# get current date

sys_date <- ymd(
                paste0(year(Sys.time()),"-0",month(Sys.time()),"-",if (day(Sys.time())>9) {day(Sys.time())} else {paste0(0,day(Sys.time()))})
                )

sys_date <- c(rep(sys_date,10))

# trim down schedule date list to avoid 404 errors when calling links that don't exist yet
tourney_days_to_date <- schedule %>% filter(schedule$date == sys_date | schedule$date < sys_date)

fanmatch_url <- "https://kenpom.com/fanmatch.php?d=2023-03-15"

scrape_completed_box_score_links <- function(date,round){

fanmatch_url <-  paste0("https://kenpom.com/fanmatch.php?d=",date)
    
page <-session_jump_to(pgsession, fanmatch_url)
completed_games_links <- page %>% html_nodes('tbody tr td:nth-of-type(3) a[href*="box"]') %>% html_attr("href") %>% paste0("https://kenpom.com/",.)

dates_links <- tibble(date,round,completed_games_links) 
}

dates_rounds_links <- map2_dfr(tourney_days_to_date$date,tourney_days_to_date$round,scrape_completed_box_score_links) %>% filter(completed_games_links != "https://kenpom.com/")










































































# explore scraping this seasonal player stats from CBB reference for the entry form

url <- "https://www.sports-reference.com/cbb/players/erik-stevenson-1.html"

scrape_player_stats <- function(url){

page <- session(url)
stat_labels <- page %>% html_nodes("tr[id='players_per_game.2023'] td") %>% html_attr("data-stat")
stats <- page %>% html_nodes("tr[id='players_per_game.2023'] td") %>% html_text()
stats_comb <- tibble(stat_labels,stats)
stats_wide <- pivot_wider(stats_comb, names_from = stat_labels,values_from = stats)

}


CBBR_stats <- map_df(CBB_links$CBBR_player_url,scrape_player_stats)




sheet_url <- "https://docs.google.com/spreadsheets/d/14JKtkbfOIabnRtmEo8dV5U2H0XiAsEj8Soyxr_bNJ6c/edit?usp=sharing"


gs4_auth(scope = "https://www.googleapis.com/auth/drive")
drive_auth(token = gs4_token())

# write data in the appropriate sheet and range
range_write(CBB_links, ss = sheet_url, sheet = "Sheet1", range = "A1:P100000")






# Login to kenpom.com

login <- "https://kenpom.com" 

pgsession<-session(login)
pgform<-html_form(pgsession)[[1]]  #in this case the submit is the 1st form
filled_form<-html_form_set(pgform, email="f.cocuzza@gmail.com", password="threenonymousdonor")
session_submit(pgsession, filled_form)

# Define function to scrape list of box score links for the primary team-year

team_name <- "Villanova"
year <- 2023

get_games_list <- function(team_name,year) {
  
  url <- paste0("https://kenpom.com/team.php?team=",team_name,"&y=",as.character(year))
  page<-session_jump_to(pgsession, url)
  
  opp_names <- page %>% html_nodes("table#schedule-table tr:not(tr.un)")  %>% html_nodes("td:nth-of-type(4) a") %>% html_text()
  opp_links <- page %>% html_nodes("table#schedule-table tr:not(tr.un)") %>% html_nodes("td:nth-of-type(4) a") %>% html_attr("href") %>%  paste0("https://kenpom.com/",.)
  
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