library(tidyverse)
library(rvest)

template = read.csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv", na.strings = FALSE, stringsAsFactors = FALSE)
                

getData_nst = function(year){
  
  year2 = year - 1
  
  mainpage = read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=",year2,year,"&thruseason=",year2,year,"&stype=2&sit=5v5&score=all&rate=y&team=all&loc=B&gpf=410&fd=&td=", sep=""))
  
  teams = mainpage %>%
    html_nodes(".lh") %>%
    html_text(.) %>%
    .[2:length(.)]
  
  SCF = mainpage %>% 
          html_nodes("td:nth-child(25)") %>%
          html_text(.) %>%
          as.numeric(.) 
  
  SCA = mainpage %>%
          html_nodes("td:nth-child(24)") %>%
          html_text(.) %>%
          as.numeric(.) 
  
  HighDangerSC_Percent = mainpage %>%
          html_nodes("td:nth-child(33)") %>%
          html_text(.) %>%
          as.numeric(.)
  
  HDCA = mainpage %>% 
          html_nodes("td:nth-child(35)") %>%
          html_text(.) %>%
          as.numeric(.)
  
  data = tibble(Year = rep(year, length(teams)), Team = teams, SCF = SCF,
                SCA = SCA, HighDangerSC_Percent = HighDangerSC_Percent, HDCA = HDCA)
  
}


findMatch = function(team.1, team.2, stat, data, highest.seed){
  
  tmp = unlist(c(data[, names(data) %in% c(stat)][which(data$Team == team.1),], data[, names(data) %in% c(stat)][which(data$Team == team.2),]))
  tmp[which(c(team.1, team.2) == highest.seed)] - tmp[which(c(team.1, team.2) != highest.seed)]
  
}

processData = function(team.1, team.2, highest.seed, year, data){
  
  data = data %>% filter(., Year == year)
  
  team_vec = as_tibble(unlist(lapply(colnames(data)[3:ncol(data)], FUN = findMatch, team.1 = team.1, team.2 = team.2, data = data, highest.seed = highest.seed))) %>%
    rownames_to_column(.) %>%
    mutate(rowname = colnames(data)[3:ncol(data)]) %>%
    spread(rowname, value) 
  
  team_vec
  
}

allData = lapply(seq(2008,2019,1), FUN = getData_nst) %>%
              bind_rows(.)

final = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year,
               MoreArgs = list(data = allData), SIMPLIFY = FALSE)) %>%
        select_if(~sum(!is.na(.)) > 0) 

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(final, "SCFScores.csv")