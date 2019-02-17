library(tidyverse)
library(rvest)

template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") %>%
              mutate(Team1 = ifelse(Team1 == "St Louis Blues", "St. Louis Blues", Team1)) %>%
              mutate(Team2 = ifelse(Team2 == "St Louis Blues", "St. Louis Blues", Team2)) %>%
              mutate(Highest.Seed = ifelse(Highest.Seed == "St Louis Blues", "St. Louis Blues", Highest.Seed))
  

accronyms_pg = read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")
accronyms = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)
fullnames = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_Accronyms = cbind(FullName = fullnames, Accronym = accronyms) %>% as_tibble(.) %>% bind_rows(., c(FullName = "Mighty Ducks of Anaheim", Accronym = "MDA")) %>% bind_rows(., c(FullName = "St Louis Blues", Accronym = "STL"))  
lookup_Accronyms$Accronym = ifelse(lookup_Accronyms$Accronym == "VGK", "VEG", lookup_Accronyms$Accronym)                                                                       

rm(accronyms_pg, accronyms, fullnames)

#The function below uses the formula given by http://hockeyanalytics.com/2016/07/elo-ratings-for-the-nhl/ to actually calculate ELO ratings. This is quite complex and so refer 
#to the page for actual details.

calculateM = function(goal.difference_home, ELO_Home, ELO_Visitor){
  
  M = max(1,log(abs(goal.difference_home-0.0085*(ELO_Home - ELO_Visitor + 35)) + exp(1) -1))
  ifelse(is.na(M), stop(paste("List of Parameters:",goal.difference_home, ELO_Home, ELO_Visitor, sep = " ")), M)
}

calculateEH = function(ELO_Home, ELO_Visitor){
  
  EH = 1/(1 + 10^(-(ELO_Home - ELO_Visitor + 35) / 400))
  ifelse(is.na(EH), stop("NA"), EH)
}

getData = function(year, last.games = 0){
  
  teamnames = read_html(paste("https://www.hockey-reference.com/leagues/NHL_",year,"_standings.html", sep="")) %>%
                html_nodes("#standings td.left") %>%
                html_text(.)
  
  trackELO = tibble(Team = teamnames) %>%
                bind_cols(., ELORating = rep(1500, nrow(.)), Year = rep(year, nrow(.)))
  
  rm(teamnames)
  
  schedule_and_results = read_html(paste("https://www.hockey-reference.com/leagues/NHL_",year,"_games.html", sep="")) 
          
  visitor = schedule_and_results %>%
                html_nodes("#games .left+ td.left") %>%
                html_text(.) 
  
  data  = schedule_and_results %>%
                html_nodes("#games td:nth-child(4)") %>%
                html_text(.) %>%
                bind_cols(Visitor = visitor, Home = .)
  
  rm(visitor)
  data = schedule_and_results %>%
                html_nodes("#games .right:nth-child(3)") %>%
                html_text(.) %>%
                as.numeric(.) %>%
                bind_cols(data, Goals_Visitor = .)
  
  data = schedule_and_results %>%
                html_nodes("#games .right:nth-child(5)") %>%
                html_text(.) %>%
                as.numeric(.) %>%
                bind_cols(data, Goals_Home = .) 
  
  data = schedule_and_results %>%
                html_nodes("#games td.center") %>%
                html_text(.) %>%
                bind_cols(data, SOorOTIndicator = .) %>%
                mutate(Outcome.Home = ifelse(SOorOTIndicator == "SO", 0.5, 
                                      ifelse(Goals_Home > Goals_Visitor, 1,0))) %>%
                filter(.,!is.na(Goals_Visitor))
  
constant = ifelse(last.games == 0, 1, nrow(data)-last.games)
  
  for (i in constant:nrow(data)){
    
    visitorELO = trackELO$ELORating[which(trackELO$Team == data$Visitor[i])]
    homeELO = trackELO$ELORating[which(trackELO$Team == data$Home[i])]
    goal.difference = data$Goals_Home[i] - data$Goals_Visitor[i]
    
    ELOChange = 8 * 1 * calculateM(goal.difference_home = goal.difference, ELO_Home = homeELO, ELO_Visitor = visitorELO) * (data$Outcome.Home[i] - calculateEH(ELO_Home = homeELO, ELO_Visitor = visitorELO))

    if(is.na(ELOChange)){
      paste("Iteration:", i, sep = " ")
    }
    
    trackELO$ELORating[which(trackELO$Team == data$Visitor[i])] = visitorELO + -ELOChange
    trackELO$ELORating[which(trackELO$Team == data$Home[i])] = homeELO + ELOChange
  }
  
  rm(i)
  
  out = trackELO
  
}

frames_delay = lapply(seq(2006, 2018, 1), FUN = getData, last.games = 400)
data_delay = bind_rows(frames_delay)

frames = lapply(seq(2006, 2018, 1), FUN = getData, last.games = 0)
data = bind_rows(frames)

rm(frames, frames_delay)

processData = function(team.1, team.2, highest.seed, data, year){
  
  data = data %>% filter(., Year == year)
  
  team_ELO = c(data$ELORating[which(data$Team == team.1)], data$ELORating[which(data$Team == team.2)])

  out = as.numeric(team_ELO[which(c(team.1,team.2) == highest.seed)] - team_ELO[which(c(team.1, team.2) != highest.seed)]) 
  
}

template = template %>% rowwise %>% 
  mutate(ELORating = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = data, year = Year)) %>%
  mutate(ELORating_Q4 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = data_delay, year = Year))

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(template[, 7:8], "ELORatings_January25_2019.csv")