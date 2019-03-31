library(tidyverse)
library(rvest)

template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv")

accronyms_pg = read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")
accronyms = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)

fullnames = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_Accronyms = cbind(FullName = fullnames, Accronym = accronyms) %>%
  as_tibble(.) %>% 
  bind_rows(., c(FullName = "Mighty Ducks of Anaheim", Accronym = "MDA")) 

rm(accronyms_pg, accronyms, fullnames)

getData = function(year){
data = read_csv(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/MoneyPuck/Team Stats/", year, ".csv", sep = "")) %>% .[,2:ncol(.)] %>% filter(., situation == "5on5") %>%
  mutate(CF = shotsOnGoalFor + missedShotsFor + blockedShotAttemptsFor) %>%
  mutate(CA = shotsOnGoalAgainst + missedShotsAgainst + blockedShotAttemptsAgainst) %>%
  mutate(CF_Per60Team = CF/iceTime * 60) %>%
  mutate(CA_Per60Team = CA/iceTime * 60) %>%
  mutate(xGF.60 = xGoalsFor/iceTime * 60) %>%
  mutate(xGA.60 = xGoalsAgainst/iceTime * 60) %>%
  mutate(PenaltiesDrawn = penaltiesFor/iceTime * 60) %>%
  mutate(PenaltiesTaken = penaltiesAgainst/iceTime * 60) %>%
  mutate(CF._Team = CF/(CF+CA)) %>%
  mutate(ShotPercentage = goalsFor/shotAttemptsFor) %>%
  mutate(SavePercentage = 1 - goalsAgainst/shotAttemptsAgainst) %>%
  mutate(PDO = ShotPercentage + SavePercentage) %>%
  select(season, name, CF._Team, CF_Per60Team, CA_Per60Team, xGF.60, xGA.60, PDO, PenaltiesDrawn, PenaltiesTaken)

}

allData = bind_rows(lapply(2009:2018, getData))
