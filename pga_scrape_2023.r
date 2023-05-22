
#remotes::install_github("MarkEdmondson1234/googleCloudRunner")

library(tidyverse)
library(rvest)
library(googlesheets4)
library(googledrive)
library(lubridate)
library(data.table)
library(scales)
library(gargle)
#library(googleCloudRunner)


# define url where data is to be scraped from
url <- "https://www.espn.com/golf/leaderboard?tournamentId=401465523"

# read page html into R
scoreboard_page <- read_html(url) 

# extract desired data columns using Rvest html_nodes function
# CSS selectors reference: (https://www.w3schools.com/cssref/css_selectors.asp)


col_1 <- scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(1):not([colspan])") %>% html_text()
col_2  <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(2):not([colspan])") %>% html_text()
col_3  <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(3):not([colspan])") %>% html_text()
col_4  <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(4)") %>% html_text()
col_5  <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(5)") %>% html_text()
col_6  <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(6)") %>% html_text()
col_7 <-scoreboard_page%>% html_nodes("td.Table__TD:nth-of-type(7)") %>% html_text()
col_8 <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(8)") %>% html_text()
col_9 <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(9)") %>% html_text()
col_10 <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(10)") %>% html_text()
col_11 <-scoreboard_page %>% html_nodes("td.Table__TD:nth-of-type(11)") %>% html_text()

col_names <-scoreboard_page %>% html_nodes("th.Table__TH") %>% html_text()
leaderboard <- tibble::tibble(col_1, col_2,col_3,col_4,col_5,col_6,col_7,col_8,col_9,col_10,col_11)
colnames(leaderboard) <- col_names

last_updated <- tibble(Sys.time()-hours(4))

# define url of the google sheet in which to write the data (update google sheet sharing settings to 'Anyone on the internet with this link can edit')
ss2 <- "https://docs.google.com/spreadsheets/d/1R6200-of_TtZCueALtWBTSPeqnpY-Lqn0UkLBcxQGVI/edit?usp=sharing"

##### Run this part just once, the first time you run it for the day

# Manually authenticate in browser after running this:
# token <- gs4_auth()

# Then save the authentication token in a .secrets folder in the working directory
# saveRDS(token, file = "~/.secrets1/googlesheets_token.rds")

#####

# write data in the appropriate sheet and range
range_write(leaderboard, ss = ss2, sheet = "Frankie Live Scoring", range = "A1:P200")

range_write(last_updated , ss = ss2, sheet = "Frankie Live Scoring", range = "A205")

##### end

##### These are the commands I use to build the .yaml file and start the 'build' on Google CloudBuild
# This is not part of the script that is to be run regularly

# my_yaml <- cr_build_yaml(
#  steps = c(
#    cr_buildstep_r("pga_scrape_2023.R", name = "tidyverse",r_source = "local", prefix = "rocker/")
#  )
# )
# cr_build_write(my_yaml, file = "cloudbuild.yaml")
#
# itworks <- cr_build("cloudbuild.yaml", launch_browser = TRUE)



