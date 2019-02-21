library(tidyverse)
library(rvest)

template = read.csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv", na.strings = FALSE, stringsAsFactors = FALSE)
                

getData_nst = function(year){
  
  year2 = year - 1
  
  mainpage = read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=",year2,year,"&thruseason=",year2,year,"&stype=2&sit=5v5&score=all&rate=n&team=all&loc=B&gpf=410&fd=&td=", sep=""))
  
  teams = mainpage %>%
    html_nodes(".lh") %>%
    html_text(.) %>%
    .[2:length(.)]
  
  SCF = mainpage %>% 
          html_nodes("td:nth-child(25)") %>%
          html_text(.) %>%
          as.numeric(.) 
  
  data = tibble(Year = rep(year, length(teams)), Team = teams, SCF = SCF)
  
}

processData = function(team.1, team.2, highest.seed, data, year){
  data = data %>% filter(., Year == year)
  
  team_SCF = c(data$SCF[which(data$Team == team.1)], data$SCF[which(data$Team == team.2)])
  if(any(is.na(team_SCF))){
    NA
  }else{
  as.numeric(team_SCF[which(c(team.1,team.2) == highest.seed)] - team_SCF[which(c(team.1, team.2) != highest.seed)])}
}

allData = lapply(seq(2008,2018,1), FUN = getData_nst) %>%
              bind_rows(.)

template = template %>% 
  rowwise %>% 
  mutate(SCF = processData(team.1 = Team1, team.2 = Team2, highest.seed = Highest.Seed, data = allData, year = Year)) 

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(template[,7], "SCFScores.csv")