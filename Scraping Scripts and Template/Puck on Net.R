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

getData_pon = function(year){
  
    year2 = year - 1
    
    mainpage = read_html(paste("http://www.puckon.net/fenwick.php?s=",year2,"-09-01&e=",year,"-06-30&f=0&ld=1&l=82&p=0", sep=""))
    last20page_main = read_html(paste("https://www.puckon.net/?s=",year2,"-08-01&e=",year,"-06-30&f=0&ld=-1&l=20&p=0", sep = ""))
    last20page_Goals = read_html(paste("https://www.puckon.net/goals.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    last20page_misses = read_html(paste("https://www.puckon.net/misses.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    last20page_blocks = read_html(paste("https://www.puckon.net/blocks.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    last20page_hits = read_html(paste("https://www.puckon.net/hits.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    
    teams = mainpage %>%
      html_nodes("td:nth-child(1)") %>%
      html_text(.) %>%
      gsub("\\.", "",.) %>%
      as_tibble(.) %>%
      set_names(., "Accronym") %>%
      mutate(Accronym = ifelse(Accronym == "LA", "LAK",Accronym)) %>%
      mutate(Accronym = ifelse(Accronym == "NJ", "NJD", Accronym)) %>%
      mutate(Accronym = ifelse(Accronym == "SJ", "SJS", Accronym)) %>%
      mutate(Accronym = ifelse(Accronym =="TB", "TBL", Accronym)) 

    Fenwick = mainpage %>% 
      html_nodes("td:nth-child(6)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(Fenwick = .) 

    Fenwick_Last20 = last20page_main %>%
      html_nodes("td:nth-child(10)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(Fenwick_Last20 = .)
    
    Corsi_Last20 = last20page_main %>%
      html_nodes("td:nth-child(6)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(Corsi_Last20 = .)
    
    SOG_Last20 = last20page_main %>%
      html_nodes("td:nth-child(14)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(SOG_Last20 = .)
    
    rm(last20page_main)
    
    GoalsPercentage_Last20 = last20page_Goals %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(GoalsPercentage_Last20 =.)
    
    rm(last20page_Goals)
    
    MissesPercentage_Last20 = last20page_misses %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(MissesPercentage_Last20 = .)
  
    rm(last20page_misses)
    
    BlocksPercentage_Last20 = last20page_blocks %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(BlocksPercentage_Last20 = .)
    
    rm(last20page_blocks)
    
    HitsPercentage_Last20 = last20page_hits %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(HitsPercentage_Last20 = .)
    
    rm(last20page_hits)
    
    data = left_join(teams,lookup_Accronyms, by = "Accronym") %>%
           bind_cols(.,tibble(Year = rep(year, nrow(teams))),teams,Fenwick, Fenwick_Last20, Corsi_Last20, SOG_Last20, GoalsPercentage_Last20, MissesPercentage_Last20, BlocksPercentage_Last20, HitsPercentage_Last20) %>%
           mutate(FullName = ifelse(FullName == "St. Louis Blues", "St Louis Blues", FullName)) %>%
           mutate(FullName = ifelse(FullName == "Anaheim Ducks" & Year <= 2006, "Mighty Ducks of Anaheim", FullName))
 
}

processData = function(team.1, team.2, highest.seed, data, year){
  data = data %>% filter(., Year == year)
  
  team_Fenwick = c(data$Fenwick[which(data$Team == team.1)], data$Fenwick[which(data$Team == team.2)])
  team_FenwickLast20 = c(data$Fenwick_Last20[which(data$Team == team.1)], data$Fenwick_Last20[which(data$Team == team.2)])
  team_CorsiLast20 = c(data$Corsi_Last20[which(data$Team == team.1)], data$Corsi_Last20[which(data$Team == team.2)])
  team_SOGLast20 = c(data$SOG_Last20[which(data$Team == team.1)], data$SOG_Last20[which(data$Team == team.2)])
  team_GoalsPercentageLast20 = c(data$GoalsPercentage_Last20[which(data$Team == team.1)], data$GoalsPercentage_Last20[which(data$Team == team.2)])
  team_MissesPercentageLast20 = c(data$MissesPercentage_Last20[which(data$Team == team.1)], data$MissesPercentage_Last20[which(data$Team == team.2)])
  team_BlocksPercentageLast20 = c(data$BlocksPercentage_Last20[which(data$Team == team.1)], data$BlocksPercentage_Last20[which(data$Team == team.2)])
  team_HitsPercentageLast20 = c(data$HitsPercentage_Last20[which(data$Team == team.1)], data$HitsPercentage_Last20[which(data$Team == team.2)])
  
  list(Fenwick = as.numeric(team_Fenwick[which(c(team.1,team.2) == highest.seed)] - team_Fenwick[which(c(team.1, team.2) != highest.seed)]),
       Fenwick_Last20 = as.numeric(team_FenwickLast20[which(c(team.1,team.2) == highest.seed)] - team_FenwickLast20[which(c(team.1, team.2) != highest.seed)]),
       Corsi_Last20 = as.numeric(team_CorsiLast20[which(c(team.1,team.2) == highest.seed)] - team_CorsiLast20[which(c(team.1, team.2) != highest.seed)]),
       SOG_Last20 = as.numeric(team_SOGLast20[which(c(team.1,team.2) == highest.seed)] - team_SOGLast20[which(c(team.1, team.2) != highest.seed)]),
       GoalsPercentageLast20 = as.numeric(team_GoalsPercentageLast20[which(c(team.1,team.2) == highest.seed)] - team_GoalsPercentageLast20[which(c(team.1, team.2) != highest.seed)]),
       MissesPercentageLast20 = as.numeric(team_MissesPercentageLast20[which(c(team.1,team.2) == highest.seed)] - team_MissesPercentageLast20[which(c(team.1, team.2) != highest.seed)]),
       BlocksPercentageLast20 = as.numeric(team_BlocksPercentageLast20[which(c(team.1,team.2) == highest.seed)] - team_BlocksPercentageLast20[which(c(team.1, team.2) != highest.seed)]),
       HitsPercentageLast20 = as.numeric(team_HitsPercentageLast20[which(c(team.1,team.2) == highest.seed)] - team_HitsPercentageLast20[which(c(team.1, team.2) != highest.seed)]))
}

allYears = bind_rows(lapply(seq(2006, 2018,1), FUN = getData_pon)) %>% rename(Team = FullName)

template = template %>% rowwise %>% 
  mutate(Fenwick = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$Fenwick) %>%
  mutate(Fenwick_Last20 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$Fenwick_Last20) %>% 
  mutate(Corsi_Last20 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$Corsi_Last20) %>% 
  mutate(SOG_Last20 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$SOG_Last20) %>% 
  mutate(GoalsPercentage_Last20 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$GoalsPercentageLast20) %>% 
  mutate(MissesPercentage_Last20 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$MissesPercentageLast20) %>% 
  mutate(BlocksPercentage_Last20 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$BlocksPercentageLast20) %>% 
  mutate(HitsPercentage_Last20 = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allYears, year = Year)$HitsPercentageLast20)

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(template[, 7:ncol(template)], "FenwickScores.csv")