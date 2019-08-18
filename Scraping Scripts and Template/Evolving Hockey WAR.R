library(tidyverse)
library(rvest)

template = read_csv("/home/brayden/GitHub/NHLPlayoffs/Scraping Scripts and Template/Templates/Template.csv") %>%
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
  
  data = read_csv(paste("/home/brayden/GitHub/NHLPlayoffs/Evolving Hockey/Player Stats WAR/", year, ".csv", sep = "")) 
  
  data[data == "S.J"] = "SJS"
  data[data == "L.A"] = "LAK"
  data[data == "T.B"] = "TBL"
  data[data == "N.J"] = "NJD"
  
  TOI_byteam = data %>% 
            mutate(Team = as.factor(Team)) %>%
            group_by(Team) %>%
            summarize_at(funs(sum(., na.rm = TRUE)), .vars = "TOI_all")
  
  data = data %>%
            mutate(Team = as.factor(Team)) %>%
            left_join(., TOI_byteam, by = "Team") %>%
            group_by(Team) %>%
            summarize_at(funs(mean(., na.rm = TRUE), median(., na.rm = TRUE), max(., na.rm = TRUE), sd(., na.rm = TRUE)), .vars = c("WAR", "GAR")) %>%
            mutate(Team = as.character(Team))
  
  abc = bind_cols(Year = rep(year, nrow(data)), data)
          
  
}

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
  
  team_vec = as_tibble(unlist(lapply(colnames(data)[3:ncol(data)], FUN = findMatch, team.1 = team.1, team.2 = team.2, data = data, highest.seed = highest.seed))) %>%
    rownames_to_column(.) %>%
    mutate(rowname = colnames(data)[3:ncol(data)]) %>%
    spread(rowname, value) 
  
  team_vec
  
}

allCombined = bind_rows(lapply(2008:2019, FUN = getData))

setwd("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets")
final = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = allCombined), SIMPLIFY = FALSE)) %>%
        write_csv(., "EvolvingHockey_WAR.csv")

