library(tidyverse)
library(rvest)

template = read.csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv", na.strings = FALSE, stringsAsFactors = FALSE)

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

getData_nhl = function(year){
  
  mainpage = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/NHL HTML Renders/Hits and Blocks/NHL.com - Stats ",year,".html", sep=""))
  
  TeamName = mainpage %>%
    html_nodes(".rt-td:nth-child(2)") %>%
    html_text(.) %>%
    gsub("é", "e",.) %>%
    gsub("\\.", "",.) 
  
  Hits = mainpage %>%
    html_nodes(".rt-td:nth-child(10)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  Blocks = mainpage %>%
    html_nodes(".rt-td:nth-child(11)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  FaceoffWinPercentage = mainpage %>%
    html_nodes(".rt-td:nth-child(18)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  GiveAways = mainpage %>%
    html_nodes(".rt-td:nth-child(13)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  TakeAways = mainpage %>%
    html_nodes(".rt-td:nth-child(14)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  data = tibble(Year = rep(year, length(TeamName)),Team = TeamName, BlocksatES = Blocks, HitsatES = Hits, FaceoffWinPercentage = FaceoffWinPercentage, 
                GiveAways = GiveAways, TakeAways = TakeAways) %>%
          mutate(Team = ifelse(Team == "Anaheim Ducks" & year <= 2006, "Mighty Ducks of Anaheim", Team))
  
}

processData = function(team.1, team.2, highest.seed, data, year){
  
  data = data %>% filter(., Year == year)
  
  team_Hits = c(data$HitsatES[which(data$Team == team.1)], data$HitsatES[which(data$Team == team.2)])
  team_Blocks = c(data$BlocksatES[which(data$Team == team.1)], data$BlocksatES[which(data$Team == team.2)])
  team_FaceoffWinPercentage = c(data$FaceoffWinPercentage[which(data$Team == team.1)], data$FaceoffWinPercentage[which(data$Team == team.2)])
  team_GiveAways = c(data$GiveAways[which(data$Team == team.1)], data$GiveAways[which(data$Team == team.2)])
  team_TakeAways = c(data$TakeAways[which(data$Team == team.1)], data$TakeAways[which(data$Team == team.2)])
  
  list(HitsatES = as.numeric(team_Hits[which(c(team.1,team.2) == highest.seed)] - team_Hits[which(c(team.1, team.2) != highest.seed)]),
       BlocksatES = as.numeric(team_Blocks[which(c(team.1,team.2) == highest.seed)] - team_Blocks[which(c(team.1, team.2) != highest.seed)]),
       FaceoffWinPercentage = as.numeric(team_FaceoffWinPercentage[which(c(team.1,team.2) == highest.seed)] - team_FaceoffWinPercentage[which(c(team.1, team.2) != highest.seed)]),
       GiveAways = as.numeric(team_GiveAways[which(c(team.1,team.2) == highest.seed)] - team_GiveAways[which(c(team.1, team.2) != highest.seed)]),
       TakeAways = as.numeric(team_TakeAways[which(c(team.1,team.2) == highest.seed)] - team_TakeAways[which(c(team.1, team.2) != highest.seed)]))
}

allData = lapply(2006:2018, FUN = getData_nhl) %>% bind_rows(.)

final = bind_rows(mapply(processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = allData),
               SIMPLIFY = FALSE))

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(final, "NHLOfficialStatsJanuary25th.csv")