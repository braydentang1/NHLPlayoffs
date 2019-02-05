require(tidyverse)
require(rvest)

template = read_csv("C:/Users/Brayden/Documents/NHLModel/Scraping Scripts and Template/Template.csv")

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

#CAPS Sensitive!
#Input winner_FullTeamName/loser_FullTeamName as ex: "Ottawa Senators" or "St Louis Blues" (no period!)
#Input highest_seed as  ex: "Tampa Bay Lightning"
#Input conference as "western or eastern"
#Input round as "quarter-finals, "semi-finals", "finals", "or stanley-cup-final"
#Input year as 2006 (number)

getData = function(year){
  
  main = read_html(paste("C:/Users/Brayden/Documents/NHLModel/Hockey Reference/",year,".html", sep=""))
  
  team_name = main %>%
              html_nodes("#stats tbody .left") %>%
              html_text(.) %>%
              str_replace(.,"[*]", "") %>%
              str_replace(., "[.]", "") %>%
              tibble(Team = .)
  
  average_age = main %>%
              html_nodes("#stats tbody .left+ .right") %>%
              html_text(.) %>%
              as.numeric(.) %>%
              tibble(AverageAge = .)
  
  games_played = main %>%
    html_nodes("#stats tbody .right:nth-child(4)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(Wins = .) 
  
  wins = main %>%
    html_nodes("#stats tbody .right:nth-child(5)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(Wins = .) %>%
    transmute(RegularSeasonWinPercentage = Wins/games_played$Wins)
  
  otl = main %>%
    html_nodes("#stats tbody .right:nth-child(7)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(OTL = .) 
  
  Points = main %>%
    html_nodes("#stats tbody .right:nth-child(8)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(Points = .) 
  
  PointsPercentage = main %>%
    html_nodes("#stats tbody .right:nth-child(9)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(PointsPercentageofMax = .) 
  
  GoalsFor = main %>%
    html_nodes("#stats tbody .right:nth-child(10)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(GoalsFor = .)
  
  GoalsAgainst = main %>%
    html_nodes("#stats tbody .right:nth-child(11)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(GoalsAgainst = .)
  
  GoalDifferential = tibble(GoalDiff = GoalsFor$GoalsFor - GoalsAgainst$GoalsAgainst)
  
  SRS = main %>%
    html_nodes("#stats tbody .right:nth-child(14)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(SRS = .)
  
  SOS = main %>%
    html_nodes("#stats tbody .right:nth-child(15)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(SOS = .)
  
  PenaltyinMinsPG = main %>%
    html_nodes("tbody .right:nth-child(27)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(PenaltyMinsPG = .)
  
  PowerPlayGoals = main %>%
    html_nodes("tbody .right:nth-child(19)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(PowerPlayGoals = .)
  
  PowerPlayOppurtunities = main %>%
    html_nodes("tbody .right:nth-child(20)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(PowerPlayOppurtunities =.)
  
  PowerPlayPercentage = main %>%
    html_nodes("tbody .right:nth-child(21)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(PowerPlayPercentage =.)
  
  PenaltyKillPercentage = main %>%
    html_nodes("tbody .right:nth-child(24)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(PenaltyKillPercentage =.)
  
  SOG = main %>%
    html_nodes("tbody .right:nth-child(29)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(SOG =.)
  
  ShotPercentage = main %>%
    html_nodes("tbody .right:nth-child(30)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(ShotPercentage =.)
  
  bind_cols(tibble(Year = as.numeric(rep(year, nrow(team_name)))), team_name, average_age, wins, otl, Points, PointsPercentage, GoalsFor, GoalsAgainst, GoalDifferential, SRS, SOS, PenaltyinMinsPG, PowerPlayGoals,
            PowerPlayOppurtunities, PowerPlayPercentage, PenaltyKillPercentage, SOG, ShotPercentage)
}

findMatch = function(team.1, team.2, stat, data, highest.seed){
  tmp = unlist(c(data[, names(data) %in% c(stat)][which(data$Team == team.1),], data[, names(data) %in% c(stat)][which(data$Team == team.2),]))
  tmp[which(c(team.1, team.2) == highest.seed)] - tmp[which(c(team.1, team.2) != highest.seed)] 
}
  
processData = function(team.1, team.2, highest.seed, year, data){
  
  data = data %>% filter(., Year == year)
  
  team_vec = as_tibble(unlist(lapply(colnames(data)[3:ncol(data)], FUN = findMatch, team.1 = team.1, team.2 = team.2, data = data, highest.seed = highest.seed))) %>%
                  rownames_to_column(.) %>%
                  spread(rowname, value) 

}

allData = bind_rows(lapply(2006:2018, FUN = getData))

teams = template %>% transmute(Series = paste(Team1, "vs.", Team2, sep = " "))

final = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = allData), SIMPLIFY = FALSE)) %>%
        bind_cols(teams, .)
rm(teams)

setwd("C:/Users/Brayden/Documents/NHLModel/Required Data Sets")
write_csv(final[,2:ncol(final)], "HockeyReference1.csv")