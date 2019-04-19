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

getData_ESPN = function(year){
  
  mainpage = read_html(paste("http://www.espn.com/nhl/stats/rpi/_/season/",year, sep=""))
  secondary.page = read_html(paste("http://www.espn.com/nhl/statistics/team/_/stat/special-teams/sort/powerPlayPct/year/",year,"/seasontype/2/split/142", sep = ""))

  teams = mainpage %>%
    html_nodes("td:nth-child(2)") %>%
    html_text(.) %>%
    .[2:length(.)] %>%
    tibble(Team = .) %>%
    mutate(Team = ifelse(Team == "Detroit", "Detroit Red Wings",Team)) %>%
    mutate(Team = ifelse(Team == "Ottawa", "Ottawa Senators",Team)) %>%
    mutate(Team = ifelse(Team == "Dallas", "Dallas Stars",Team)) %>%
    mutate(Team = ifelse(Team == "Buffalo", "Buffalo Sabres",Team)) %>%
    mutate(Team = ifelse(Team == "Carolina", "Carolina Hurricanes",Team)) %>%
    mutate(Team = ifelse(Team == "Calgary", "Calgary Flames",Team)) %>%
    mutate(Team = ifelse(Team == "Atlanta", "Atlanta Thrashers", Team)) %>%
    mutate(Team = ifelse(Team == "Nashville", "Nashville Predators", Team)) %>%
    mutate(Team = ifelse(Team == "Anaheim" & year <= 2006, "Mighty Ducks of Anaheim", Team)) %>%
    mutate(Team = ifelse(Team == "Anaheim", "Anaheim Ducks",Team)) %>%
    mutate(Team = ifelse(Team == "San Jose", "San Jose Sharks", Team)) %>%
    mutate(Team = ifelse(Team == "Colorado", "Colorado Avalanche",Team)) %>%
    mutate(Team = ifelse(Team == "Philadelphia", "Philadelphia Flyers",Team)) %>%
    mutate(Team = ifelse(Team == "Montreal", "Montreal Canadiens", Team)) %>%
    mutate(Team = ifelse(Team == "New Jersey", "New Jersey Devils",Team)) %>%
    mutate(Team = ifelse(Team == "Edmonton", "Edmonton Oilers",Team)) %>%
    mutate(Team = ifelse(Team == "NY Rangers", "New York Rangers",Team)) %>%
    mutate(Team = ifelse(Team == "Vancouver", "Vancouver Canucks",Team)) %>%
    mutate(Team = ifelse(Team == "Los Angeles", "Los Angeles Kings",Team)) %>%
    mutate(Team = ifelse(Team == "Toronto", "Toronto Maple Leafs",Team)) %>%
    mutate(Team = ifelse(Team == "Tampa Bay", "Tampa Bay Lightning",Team)) %>%
    mutate(Team = ifelse(Team == "Winnipeg" & year <= 2011, "Atlanta Thrashers",Team)) %>%
    mutate(Team = ifelse(Team == "Minnesota", "Minnesota Wild",Team)) %>%
    mutate(Team = ifelse(Team == "Arizona" & year <= 2014, "Phoenix Coyotes",Team)) %>%
    mutate(Team = ifelse(Team == "Florida", "Florida Panthers",Team)) %>%
    mutate(Team = ifelse(Team == "Boston", "Boston Bruins",Team)) %>%
    mutate(Team = ifelse(Team == "NY Islanders", "New York Islanders",Team)) %>%
    mutate(Team = ifelse(Team == "Columbus", "Columbus Blue Jackets",Team)) %>%
    mutate(Team = ifelse(Team == "Washington", "Washington Capitals",Team)) %>%
    mutate(Team = ifelse(Team == "Chicago", "Chicago Blackhawks",Team)) %>%
    mutate(Team = ifelse(Team == "Pittsburgh", "Pittsburgh Penguins",Team)) %>%
    mutate(Team = ifelse(Team == "St. Louis", "St Louis Blues",Team)) %>%
    mutate(Team = ifelse(Team == "Vegas", "Vegas Golden Knights", Team)) %>%
    mutate(Team = ifelse(Team == "Arizona", "Arizona Coyotes", Team)) %>%
    mutate(Team = ifelse(Team == "Winnipeg", "Winnipeg Jets", Team))
  
  teams.2 = secondary.page %>%
    html_nodes("td:nth-child(2)") %>%
    html_text(.) %>%
    .[. != "TEAM"] %>%
    tibble(Team = .) %>%
    mutate(Team = ifelse(Team == "Detroit", "Detroit Red Wings",Team)) %>%
    mutate(Team = ifelse(Team == "Ottawa", "Ottawa Senators",Team)) %>%
    mutate(Team = ifelse(Team == "Dallas", "Dallas Stars",Team)) %>%
    mutate(Team = ifelse(Team == "Atlanta", "Atlanta Thrashers", Team)) %>%
    mutate(Team = ifelse(Team == "Buffalo", "Buffalo Sabres",Team)) %>%
    mutate(Team = ifelse(Team == "Carolina", "Carolina Hurricanes",Team)) %>%
    mutate(Team = ifelse(Team == "Calgary", "Calgary Flames",Team)) %>%
    mutate(Team = ifelse(Team == "Nashville", "Nashville Predators", Team)) %>%
    mutate(Team = ifelse(Team == "Anaheim" & year <= 2006, "Mighty Ducks of Anaheim", Team)) %>%
    mutate(Team = ifelse(Team == "Anaheim", "Anaheim Ducks",Team)) %>%
    mutate(Team = ifelse(Team == "San Jose", "San Jose Sharks", Team)) %>%
    mutate(Team = ifelse(Team == "Colorado", "Colorado Avalanche",Team)) %>%
    mutate(Team = ifelse(Team == "Philadelphia", "Philadelphia Flyers",Team)) %>%
    mutate(Team = ifelse(Team == "Montreal", "Montreal Canadiens", Team)) %>%
    mutate(Team = ifelse(Team == "New Jersey", "New Jersey Devils",Team)) %>%
    mutate(Team = ifelse(Team == "Edmonton", "Edmonton Oilers",Team)) %>%
    mutate(Team = ifelse(Team == "NY Rangers", "New York Rangers",Team)) %>%
    mutate(Team = ifelse(Team == "Vancouver", "Vancouver Canucks",Team)) %>%
    mutate(Team = ifelse(Team == "Los Angeles", "Los Angeles Kings",Team)) %>%
    mutate(Team = ifelse(Team == "Toronto", "Toronto Maple Leafs",Team)) %>%
    mutate(Team = ifelse(Team == "Tampa Bay", "Tampa Bay Lightning",Team)) %>%
    mutate(Team = ifelse(Team == "Winnipeg" & year <= 2011, "Atlanta Thrashers",Team)) %>%
    mutate(Team = ifelse(Team == "Minnesota", "Minnesota Wild",Team)) %>%
    mutate(Team = ifelse(Team == "Arizona" & year <= 2014, "Phoenix Coyotes",Team)) %>%
    mutate(Team = ifelse(Team == "Florida", "Florida Panthers",Team)) %>%
    mutate(Team = ifelse(Team == "Boston", "Boston Bruins",Team)) %>%
    mutate(Team = ifelse(Team == "NY Islanders", "New York Islanders",Team)) %>%
    mutate(Team = ifelse(Team == "Columbus", "Columbus Blue Jackets",Team)) %>%
    mutate(Team = ifelse(Team == "Washington", "Washington Capitals",Team)) %>%
    mutate(Team = ifelse(Team == "Chicago", "Chicago Blackhawks",Team)) %>%
    mutate(Team = ifelse(Team == "Pittsburgh", "Pittsburgh Penguins",Team)) %>%
    mutate(Team = ifelse(Team == "St. Louis", "St Louis Blues",Team)) %>%
    mutate(Team = ifelse(Team == "Vegas", "Vegas Golden Knights", Team)) %>%
    mutate(Team = ifelse(Team == "Arizona", "Arizona Coyotes", Team)) %>%
    mutate(Team = ifelse(Team == "Winnipeg", "Winnipeg Jets", Team))
    
  RPI = mainpage %>% 
    html_nodes(".sortcell") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(RPI = .) 
  
  ESPNPower = mainpage %>%
    html_nodes("td:nth-child(9)") %>%
    .[2:length(.)] %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(ESPNPower = .) %>%
    mutate(ESPNPower = ifelse(ESPNPower == "null", NA, ESPNPower))
  
  PenaltyKill_PostAllStar = secondary.page %>% 
    html_nodes("td:nth-child(10)") %>%
    html_text(.) %>%
    .[!is.na(.)] %>%
    .[. != "PCT"] %>% 
    as.numeric(.) %>%
    bind_cols(teams.2, PenaltyKill_PostAllStar = .)

  data = bind_cols(tibble(Year = rep(year, nrow(teams))), teams, ESPNPower, RPI) %>% left_join(., PenaltyKill_PostAllStar, by = "Team")
  
}

processData = function(team.1, team.2, highest.seed, data, year){
  data = data %>% filter(., Year == year)
  
  team_ESPN = c(data$ESPNPower[which(data$Team == team.1)], data$ESPNPower[which(data$Team == team.2)])
  team_RPI = c(data$RPI[which(data$Team == team.1)], data$RPI[which(data$Team == team.2)])
  team_PK = c(data$PenaltyKill_PostAllStar[which(data$Team == team.1)], data$PenaltyKill_PostAllStar[which(data$Team == team.2)])
  
  list(ESPNPower = as.numeric(team_ESPN[which(c(team.1,team.2) == highest.seed)] - team_ESPN[which(c(team.1, team.2) != highest.seed)]),
             RPI = as.numeric(team_RPI[which(c(team.1,team.2) == highest.seed)] - team_RPI[which(c(team.1, team.2) != highest.seed)]),
       PenaltyKill_PostAllStar = as.numeric(team_PK[which(c(team.1,team.2) == highest.seed)] - team_PK[which(c(team.1, team.2) != highest.seed)]))
}

allYears = bind_rows(lapply(seq(2006, 2019,1), FUN = getData_ESPN)) 
allYears$PenaltyKill_PostAllStar = ifelse(allYears$PenaltyKill_PostAllStar == 0, NA, allYears$PenaltyKill_PostAllStar)

template = template %>% rowwise %>% 
  mutate(ESPNPower = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$ESPNPower) %>%
  mutate(RPI = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$RPI) %>%
  mutate(PenaltyKill_PostAllStar = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$PenaltyKill_PostAllStar)

setwd("C:/Users/Brayden/Documents/GitHub/NHLPLayoffs/Required Data Sets")
write_csv(template[,7:ncol(template)], "ESPNStats.csv")