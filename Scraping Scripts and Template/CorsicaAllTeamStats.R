library(tidyverse)
library(rvest)

template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") %>%
  mutate_all(funs(str_replace(., "Mighty Ducks of Anaheim", "Anaheim Ducks"))) %>%
  mutate_all(funs(str_replace(., "Phoenix Coyotes", "Arizona Coyotes")))


accronyms_pg = read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")
accronyms = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)

fullnames = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_Accronyms = cbind(FullName = fullnames, Team = accronyms) %>%
  as_tibble(.) %>% 
  bind_rows(., c(FullName = "Mighty Ducks of Anaheim", Team = "MDA")) %>%
  mutate(FullName = ifelse(FullName == "St. Louis Blues", "St Louis Blues", FullName))

rm(accronyms_pg, accronyms, fullnames)

getData = function(year){
  
  data = read_csv(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/All Team Stats/", year,".csv", sep = "")) %>%
          mutate_if(is.character, funs(str_replace(., "L.A", "LAK"))) %>%
          mutate_if(is.character, funs(str_replace(., "N.J", "NJD"))) %>%
          mutate_if(is.character, funs(str_replace(., "S.J", "SJS"))) %>%
          mutate_if(is.character, funs(str_replace(., "T.B", "TBL")))
  
  bind_cols(tibble(Year = rep(year, nrow(data))), data)
  
}

processData = function(team.1, team.2, highest.seed, year, data){
  
  data = data %>% 
            filter(., Year == year)
  
  teamAcc = c(lookup_Accronyms$Team[which(lookup_Accronyms$FullName == team.1)], lookup_Accronyms$Team[which(lookup_Accronyms$FullName == team.2)])
  highestseedAcc = teamAcc[which(teamAcc == lookup_Accronyms$Team[which(lookup_Accronyms$FullName == highest.seed)])]
  
  bind_cols(CF._Team = data$"CF%"[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$"CF%"[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])],
  CF_Per60Team = data$"CF/60"[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$"CF/60"[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])],
  CA_Per60Team = data$"CA/60"[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$"CA/60"[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])],
  xGF.60 = data$"xGF/60"[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$"xGF/60"[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])],
  xGA.60 = data$"xGA/60"[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$"xGA/60"[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])],
  PDO = data$PDO[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$PDO[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])],
  PenaltiesTaken = data$PENT[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$PENT[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])],
  PenaltiesDrawn = data$PEND[which(data$Team == teamAcc[which(teamAcc == highestseedAcc)])] - data$PEND[which(data$Team == teamAcc[which(teamAcc != highestseedAcc)])])
  
}

allData = bind_rows(lapply(2008:2018, getData))

final = bind_rows(tibble(CF._Team = rep(NA,30), CF_Per60Team = rep(NA, 30), CA_Per60Team = rep(NA, 30)), mapply(processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = allData), SIMPLIFY = FALSE))

setwd("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets")

write_csv(final, "CorsicaAllTeamStats.csv")
