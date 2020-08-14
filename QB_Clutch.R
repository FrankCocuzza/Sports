#Frank Cocuzza
#Defining a 'clutch' metric for 2018 NFL Quarterbacks


library(tidyverse)
setwd("C:/Users/Frank/Desktop/Villanova MSA/MSA 8310 - Advanced Business Applications/Module 2 - Sports Analytics")

## Before getting into the PBP data for specific QB's, we need to rank team defenses
## reliably to adjust for opponent strength

## To save time, (and because I was not having success with some of the packages
## for solving nonlinear differential equations in r) I'm using football outsiders'
## DVOA pass defense ratings. (https://www.footballoutsiders.com/info/methods)
## For reference, an average DVOA for a defense is 0, a good
## defense will have a negative DVOA, and a bad defense will have a positive DVOA

# Let's read in 2018 DVOA pass defense data, pulled from the football outsiders website archive
DVOA_def_2018 <- read_csv("DVOA_Pass_Def_2018.csv") %>%
  mutate(PASS_DVOA_num=as.numeric(sub("%","",PASS_DVOA))/100) %>%
  select("defteam", "PASS_DVOA_num") 
DVOA_def_2018 <- as.data.frame(DVOA_def_2018)
head(DVOA_def_2018)

## Now let's read in the 2018 PBP data set and pare it down to the variables that we need
## to develop the clutch metric

pbp_2018 <- read.csv("reg_pbp_2018.csv")

passer_2018 <- pbp_2018 %>% 
  filter(!is.na(play_type) & !is.na(posteam) & !is.na(epa) & !is.na(passer_player_name)) %>%
  select(passer_player_name, defteam, epa, wp, score_differential)
head(passer_2018)

## Now we can append the DVOA values onto the PBP data and adjust the epa values
## for strength of defense faced. We want to scale the epa UP if the DVOA value is negative
## and scale it DOWN if it is positive. This rewards performances against good pass defenses
## and penalizes performances against poor pass defenses

adj_passer_2018 <- left_join(passer_2018, DVOA_def_2018, by="defteam") %>% 
                   mutate(adj_epa = (1-PASS_DVOA_num)*epa) %>%
                   filter(!is.na(adj_epa))
adj_passer_2018$passer_player_name <- as.character(adj_passer_2018$passer_player_name)

##
subset(adj_passer_2018,passer_player_name == "A.Rodgers")
head(adj_passer_2018)


## One last step. We now need to filter out siutations where the point differential
## is greater than 11 points and the win probability is greater than 75% or less than 25%.
## This is how we defined a clutch situation in the summary document.
## We will filter out QB's with 75 or fewer total 'clutch' plays so that we have a 
## sizable sample to work with.

adj_QB_pass_clutchEPA_2018 <- adj_passer_2018 %>% 
  group_by(passer_player_name) %>%
  summarize(adj_clutch_epa = mean(adj_epa[wp>0.25 & wp<0.75 & score_differential<=11
                                          & score_differential >= -11]),
            total_plays = sum(n())) %>%
  filter(total_plays>75)

head(adj_QB_pass_clutchEPA_2018)
as.data.frame(adj_QB_pass_clutchEPA_2018)

## Now we can summarize the top 10 QB's in a table & generate some visualizations

top_10 <- adj_QB_pass_clutchEPA_2018[order(adj_QB_pass_clutchEPA_2018$adj_clutch_epa,
                                           decreasing = TRUE),] %>%
          top_n(10,adj_clutch_epa) %>%
          select(passer_player_name, adj_clutch_epa)
top_10

install.packages("ggrepel")
library(ggrepel)

plot <- ggplot(adj_QB_pass_clutchEPA_2018, aes(x=total_plays, y=adj_clutch_epa)) +
        geom_point(aes(alpha = 0.5, color = "dodgerblue4")) +
        geom_text_repel(data = adj_QB_pass_clutchEPA_2018 %>% top_n(10,adj_clutch_epa),
                        aes(label = passer_player_name), 
                        box.padding = 1.0) +
        coord_cartesian(xlim =c(75, 800), ylim = c(-0.5, 0.5)) +
        theme(legend.position = "none") +
        labs(x="Total Clutch Plays", y="Adjusted Clutch EPA (per play)")
        







