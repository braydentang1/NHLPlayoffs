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
  bind_rows(., c(FullName = "Mighty Ducks of Anaheim", Team = "MDA")) 

rm(accronyms_pg, accronyms, fullnames)


getData = function(year){
  
  data = read_csv(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Game Score/", year, ".csv", sep=""), na = "--") %>%
          .[,2:ncol(.)] %>%
    mutate(TradedPlayer = ifelse(grepl("/", Team) == TRUE, 1,0)) %>%
    mutate(Team = gsub(" ", "", Team, fixed = TRUE)) %>%
    mutate_if(is.numeric, funs(ifelse(is.infinite(.), 0,.))) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0,.))) %>%
    mutate_if(is.numeric, funs(ifelse(is.nan(.),0,.))) %>%
    select(., -Position, -Season)
  
  AllTradedPlayers = data[data$TradedPlayer == 1,]
  
  AllTradedPlayers_Team1 = AllTradedPlayers %>% mutate(Team = sub("/.*", "", AllTradedPlayers$Team))
  AllTradedPlayers_Team2 = AllTradedPlayers %>% mutate(Team = sub(".*/", "", AllTradedPlayers$Team))
  AllTradedPlayers_Team3 = AllTradedPlayers %>% mutate(Team = sub(".*/ *(.*?) */.*", "\\1", AllTradedPlayers$Team))
  AllTradedPlayers_Team3 = AllTradedPlayers_Team3[-which(grepl("/", AllTradedPlayers_Team3$Team) == TRUE), ]
  
  combined = rbind(AllTradedPlayers_Team1, AllTradedPlayers_Team2, AllTradedPlayers_Team3)
  rm(list = ls(pattern = "AllTradedPlayers"))
  
  data = bind_rows(data[-which(data$TradedPlayer == 1),], combined)
  rm(combined)
  
  data$Team = as.factor(data$Team)
  
  #removed "GS", "CF.", "Rel.CF." since they are already included in Full Data
  data = data %>%
              group_by(Team) %>%
              summarise_at(funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE), min(.,na.rm=TRUE), max(., na.rm=TRUE)), .vars = c("iCF/60", "P1", "P1/60", "GS", "GS/60", "CF", "Rel CF%",
                                                                                                                  "ixGF/60", "ZSR", "CF% QoT", "CF% QoC")) %>%
              mutate(Team = as.character(Team))
  
  bind_cols(data, tibble(Year = rep(year, nrow(data))))
}

allCombined = bind_rows(lapply(2008:2018, getData)) %>%
                mutate(Team = ifelse(Team == "L.A", "LAK", Team)) %>%
                mutate(Team = ifelse(Team == "N.J", "NJD", Team)) %>%
                mutate(Team = ifelse(Team == "S.J", "SJS", Team)) %>%
                mutate(Team = ifelse(Team == "T.B", "TBL", Team)) 

findMatch = function(team.1, team.2, stat, data, highest.seed){
  
  data = data %>%
                left_join(., lookup_Accronyms, by = "Team") %>%
                mutate(FullName = ifelse(FullName == "St. Louis Blues", "St Louis Blues", FullName)) 
  
  tmp = unlist(c(data[, names(data) %in% c(stat)][which(data$FullName == team.1),], data[, names(data) %in% c(stat)][which(data$FullName == team.2),]))
  tmp[which(c(team.1, team.2) == highest.seed)] - tmp[which(c(team.1, team.2) != highest.seed)] 
}

processData = function(team.1, team.2, highest.seed, data, year){
  
  data = data %>% 
          filter(., Year == year)
  
  team_vec = as_tibble(unlist(lapply(colnames(data)[2:(ncol(data)-1)], FUN = findMatch, team.1 = team.1, team.2 = team.2, data = data, highest.seed = highest.seed))) %>%
    rownames_to_column(.) %>%
    spread(rowname, value) 
  
  colnames(team_vec) = colnames(data)[2:(ncol(data)-1)]
  team_vec
  
}

final = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = allCombined), SIMPLIFY = FALSE)) 

setwd("C:/Users/Brayden/Documents/Github/NHLPlayoffs/Required Data Sets")
write_csv(final, "CorsicaGameScoreStats.csv")