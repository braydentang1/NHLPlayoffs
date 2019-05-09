library(tidyverse)
library(rvest)
library(RSelenium)

rd = rsDriver(browser = c("chrome"), chromever = "74.0.3729.6")
rem_dr = rd[["client"]]

rd2 = rsDriver(port = 5000L, browser = c("chrome"), chromever = "74.0.3729.6")
rem_dr2 = rd2[["client"]]

getData = function(year){

rem_dr$navigate(paste("https://www.oddsportal.com/hockey/usa/nhl-",year-1,"-",year,"/results/#/", sep = ""))  
main = read_html(rem_dr$getPageSource()[[1]])

#I tried navigating on the page, but it appears that it doesn't work (it keeps parsing only the first page, not the second)
rem_dr2$navigate(paste("https://www.oddsportal.com/hockey/usa/nhl-",year-1,"-",year,"/results/#/page/2/", sep = ""))  
main2 = read_html(rem_dr2$getPageSource()[[1]])

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
          tibble(Odds.LowestSeed = .)
odds2 = main2 %>%
          html_nodes(".odds-nowrp:nth-child(6) :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(Odds.LowestSeed = .)

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
  
  first_game$Odds.HighestSeed - first_game$Odds.LowestSeed
  
}

#Kind of a hacky way to do this. Set the round argument to what is needed. Page 1 = quarters, page 2= semis, page 3=finals, page 4=stanley cup.
getData.current = function(year, round){

  if(round == "quarter-finals"){
  
  page = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Odds HTML Renders/", year, " pg1.html", sep = ""))
  
  }else if(round == "semi-finals"){
    
  page = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Odds HTML Renders/", year, " pg2.html", sep = ""))
    
  }else if(round == "finals"){
    
  page = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Odds HTML Renders/", year, " pg3.html", sep = ""))
  
  }else{
    
  page = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Odds HTML Renders/", year, " pg4.html", sep = ""))
    
  }
  teams = page %>% 
    html_nodes(".table-participant a:nth-child(3)") %>%
    html_text(.) %>%
    tibble(Teams = .) 
  
  odds = page %>%
    html_nodes(".table-participant+ .odds-nowrp a") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(Odds.HighestSeed =.)
  
  odds2 = page %>%
    html_nodes(".odds-nowrp~ .odds-nowrp+ .odds-nowrp:nth-child(5) a") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(Odds.LowestSeed =.)
  
  combined = bind_cols(tibble(Year = rep(year, nrow(teams))), teams, odds, odds2)
  
  
}


template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") 
template[template == "St Louis Blues"] = "St. Louis Blues"
template[template == "Mighty Ducks of Anaheim"] = "Anaheim Ducks"
template[template == "Phoenix Coyotes"] = "Arizona Coyotes"
template[template == "Atlanta Thrashers"] = "Winnipeg Jets"

#Note: the function call below sends an error because on OddsPortal the actual odds are missing! But, these values are not important as we only 
#take the first game odds

allData = bind_rows(lapply(2006:2018, FUN = getData)) %>%
          bind_rows(lapply(2019, FUN = getData.current, round = "quarter-finals")) %>%
          bind_rows(lapply(2019, FUN = getData.current, round = "semi-finals")) %>%
          bind_rows(lapply(2019, FUN = getData.current, round = "finals"))

template = template %>% 
                rowwise %>%
                mutate(VegasOpeningOdds = processData(year = Year, team.1 = Team1, team.2 = Team2, data = allData))

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(template[,7], "VegasOddsOpening.csv")


