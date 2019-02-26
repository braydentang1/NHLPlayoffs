library(tidyverse)
library(rvest)

getData = function(year){

main = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Odds HTML Renders/", year, " pg1.html", sep=""))
main2 = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Odds HTML Renders/", year, " pg2.html", sep=""))

teams = main %>% 
          html_nodes(".table-participant") %>%
          html_text(.) %>%
          tibble(Teams = .) 

teams2 = main2 %>%
          html_nodes(".table-participant") %>%
          html_text(.) %>%
          tibble(Teams = .)

teams_fin = bind_rows(teams, teams2)

playoff_indicator = main2 %>%
          html_nodes(".ico-event-info") %>%
          html_text(.) 

playoff_indicator = c(playoff_indicator, rep(NA, nrow(teams2) - length(playoff_indicator))) 
playoff_indicator = c(rep(1, nrow(teams)),playoff_indicator)

rm(teams, teams2)

playoff_indicator = ifelse(!is.na(playoff_indicator), 1,0) %>% tibble(Playoff.Indicator = .)

odds = main %>%
          html_nodes(".table-score+ .odds-nowrp :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(Odds.HighestSeed =.)

odds2 = main2 %>%
          html_nodes(".table-score+ .odds-nowrp :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(Odds.HighestSeed =.)

odds.Highest_fin = bind_rows(odds, odds2)

odds = main %>% 
          html_nodes(".odds-nowrp:nth-child(6) :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(OddsLowestSeed = .)
odds2 = main2 %>%
          html_nodes(".odds-nowrp:nth-child(6) :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(OddsLowestSeed = .)

Odds.Lowest_fin = bind_rows(odds, odds2)

rm(odds, odds2)

combined = bind_cols(tibble(Year = rep(year, nrow(playoff_indicator))),teams_fin, playoff_indicator, odds.Highest_fin, Odds.Lowest_fin) %>%
                filter(Playoff.Indicator != 0)

rm(main, main2, odds.Highest_fin, Odds.Lowest_fin, playoff_indicator, teams_fin)

combined %>% select(-Playoff.Indicator)
          
}

processData = function(year, team.1, team.2, data){
  
  data = data %>% filter(Year == year)
  
  string1 = paste(team.1, "-", team.2, sep =" ")
  string2 = paste(team.2, "-", team.1, sep = " ")
  
  first_game = data[max(which(grepl(paste(c(string1, string2), collapse = "|"), data$Teams))),]
  
  first_game$Odds.HighestSeed - first_game$OddsLowestSeed
  
}


template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") 
template[template == "St Louis Blues"] = "St. Louis Blues"
template[template == "Mighty Ducks of Anaheim"] = "Anaheim Ducks"
template[template == "Phoenix Coyotes"] = "Arizona Coyotes"
template[template == "Atlanta Thrashers"] = "Winnipeg Jets"

#Note: the function call below sends a warning because on OddsPortal the actual odds are missing! But, these values are not important as we only 
#take the first game odds

allData = bind_rows(lapply(2006:2018, FUN = getData))

template = template %>% 
                rowwise %>%
                mutate(VegasOpeningOdds = processData(year = Year, team.1 = Team1, team.2 = Team2, data = allData))

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(template[,7], "VegasOddsOpening.csv")
