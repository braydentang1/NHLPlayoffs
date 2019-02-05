require(tidyverse)
require(rvest)
require(doParallel)
require(foreach)

template = read.csv("C:/Users/Brayden/Documents/NHLModel/Scraping Scripts and Template/Template.csv", na.strings = FALSE, stringsAsFactors = FALSE)

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

getData = function(year, team.1, team.2, conference, Round, highest_seed, result_known = TRUE){

if(any(Round %in% c("quarter-finals", "semi-finals", "finals", "stanley-cup-final")) ==  FALSE){
  stop("Invalid round format.")
}
  
team1 = tolower(team.1) %>%
    gsub(" ", "-", .)
team2 = tolower(team.2) %>%
    gsub(" ", "-",.)

teamsCombined = c(team1, team2) %>%
                    .[order(.)]

if(year >= 2014){
  
  if(Round == "quarter-finals"){
    Round = "first-round"
  }else if(Round == "semi-finals"){
    Round = "second-round"
  }else if(Round =="finals"){
    Round = "conference-finals"
  }else{
    Round = "stanley-cup-final"
  }
}

if(conference == "eastern" | conference == "western"){
      if(year < 2014){
          mainpage = read_html(paste("https://www.hockey-reference.com/playoffs/",year,"-",teamsCombined[1],"-vs-",teamsCombined[2],"-",conference,"-conference-",Round,".html", sep = ""))}
      else{
          mainpage = read_html(paste("https://www.hockey-reference.com/playoffs/",year,"-",teamsCombined[1],"-vs-",teamsCombined[2],"-",conference,"-",Round,".html", sep = ""))}
}else{
  mainpage = read_html(paste("https://www.hockey-reference.com/playoffs/",year,"-",teamsCombined[1],"-vs-",teamsCombined[2],"-stanley-cup-final.html", sep=""))
}

Winner = mainpage %>%
  html_nodes("br+ span a") %>%
  html_text(.) 
Loser = mainpage %>% 
  html_nodes("#content span+ span a") %>%
  html_text(.)

teamWinAcc = as.character(lookup_Accronyms[which(Winner == lookup_Accronyms$FullName),2])
teamLoseAcc = as.character(lookup_Accronyms[which(Loser == lookup_Accronyms$FullName), 2])
highest_seedAcc = as.character(lookup_Accronyms[which(highest_seed == lookup_Accronyms$FullName),2])

winnerpage = read_html(paste("https://www.hockey-reference.com/teams/",teamWinAcc,"/",year,".html", sep=""))
loserpage = read_html(paste("https://www.hockey-reference.com/teams/",teamLoseAcc,"/",year,".html",sep=""))
winnergames = read_html(paste("https://www.hockey-reference.com/teams/",teamWinAcc,"/",year,"_games.html", sep= ""))
losergames = read_html(paste("https://www.hockey-reference.com/teams/",teamLoseAcc,"/",year,"_games.html", sep = ""))

if(result_known == TRUE){
  ResultProper = ifelse(Winner == highest_seed, "W","L")
}else{
     ResultProper = NA
}

teamsCombined_orig = c(team.1, team.2) %>% .[order(.)]
constant = ifelse(highest_seed == teamsCombined_orig[1], -1, 1)

rm(teamsCombined_orig)

WinLoss = mainpage %>% 
            html_nodes(".score+ .datapoint") %>%
            html_text(.) %>%
            sub("^[^.]*", "", .) %>%
            as.numeric(.)

H2Hstats = mainpage %>%
            html_nodes("h2+ .game_summaries .winner td:nth-child(1) a") %>%
            html_text(.) %>%
            table() %>%
            as_tibble(.) 

if(nrow(H2Hstats) != 0){
  
H2Hstats = H2Hstats %>%
            set_names(c("FullName", "Wins")) %>%
            mutate(WinRatio = Wins/sum(Wins)) %>%
            left_join(., lookup_Accronyms, by = "FullName") %>%
            mutate(HighestSeed = ifelse(Accronym == highest_seedAcc, 1,0)) %>%
            select(., HighestSeed, WinRatio)
rm(highest_seedAcc)
}

if(nrow(H2Hstats) == 0){
  H2H = NA
}else if (any(H2Hstats$HighestSeed == 1) & nrow(H2Hstats) == 2){
  H2H = H2Hstats$WinRatio[which(H2Hstats$HighestSeed == 1)] - H2Hstats$WinRatio[which(H2Hstats$HighestSeed ==0)]
}else if(any(H2Hstats$HighestSeed == 1) & nrow(H2Hstats == 1)){
  H2H = 1
}else{
  H2H = -1
}

rm(H2Hstats)

SRS = mainpage %>%
          html_nodes(".datapoint:nth-child(4)") %>%
          html_text(.) %>%
          sub("SRS: ", "", .) %>%
          sub("\\(.*", "", .) %>%
          as.numeric(.)

Goals = mainpage %>%
          html_nodes(".datapoint:nth-child(5)") %>%
          html_text(.) %>%
          sub("Goals: ", "",.) %>%
          sub("\\(.*", "",.) %>%
          as.numeric(.)

GoalsAgainst = mainpage %>%
          html_nodes(".datapoint:nth-child(6)") %>%
          html_text(.) %>%
          sub("Goals Against: ", "",.) %>%
          sub("\\(.*", "",.) %>%
          as.numeric(.)

PenaltyMins = mainpage %>%
          html_nodes(paste("#skaters-",teamWinAcc, " tfoot .right:nth-child(10) , #skaters-",teamLoseAcc, " tfoot .right:nth-child(10)", sep = "")) %>%
          html_text(.) %>%
          as.numeric(.)


PowerPlay = mainpage %>%
          html_nodes(paste("#skaters-",teamWinAcc, " tfoot .right:nth-child(12) , #skaters-", teamLoseAcc, " tfoot .right:nth-child(12)", sep = "")) %>%
          html_text(.) %>%
          as.numeric(.)

SOG = mainpage %>%
         html_nodes(paste("#skaters-",teamLoseAcc, " tfoot .right:nth-child(18) , #skaters-",teamWinAcc, " tfoot .right:nth-child(18)", sep="")) %>%
         html_text(.) %>%
         as.numeric(.)

ShotPercentage = mainpage %>%
         html_nodes(paste("#skaters-",teamLoseAcc, " tfoot .right:nth-child(19) , #skaters-", teamWinAcc, " tfoot .right:nth-child(19)", sep = "")) %>%
         html_text(.) %>%
         as.numeric(.)

GoalieStatswinner = winnerpage %>%
        html_nodes("#goalies tbody .right:nth-child(22) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(12)") %>%
        html_text(.) %>%
        as.numeric(.) %>%
        as_tibble(.) %>%
        mutate(Stat = rep(c("SavePercentage", "Minutes","GPS"), length(.$value)/3)) %>%
        mutate(Goalie = rep(seq_len(length(.$value)/3), each = 3)) %>%
        spread(., Stat, value) %>%
        mutate(WeightedGPS = GPS * Minutes / sum(.$Minutes)) %>%
        mutate(WeightedGoalieSavePercentage = Minutes * SavePercentage / sum(Minutes))

GoalieStatsloser = loserpage %>%
  html_nodes("#goalies tbody .right:nth-child(22) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(12)") %>%
  html_text(.) %>%
  as.numeric(.) %>%
  as_tibble(.) %>%
  mutate(Stat = rep(c("SavePercentage", "Minutes","GPS"), length(.$value)/3)) %>%
  mutate(Goalie = rep(seq_len(length(.$value)/3), each = 3)) %>%
  spread(., Stat, value) %>%
  mutate(WeightedGPS = GPS * Minutes / sum(.$Minutes)) %>%
  mutate(WeightedGoalieSavePercentage = Minutes * SavePercentage / sum(Minutes))

WeightedGoalieSavePercentage = c(sum(GoalieStatswinner$WeightedGoalieSavePercentage),sum(GoalieStatsloser$WeightedGoalieSavePercentage))
WeightedGPS = c(sum(GoalieStatswinner$WeightedGPS), sum(GoalieStatsloser$WeightedGPS))

rm(GoalieStatsloser, GoalieStatswinner)

PlayerPoints = mainpage %>% 
    html_nodes(paste("#skaters-",teamLoseAcc, " tfoot .right:nth-child(8) , #skaters-",teamWinAcc, " tfoot .right:nth-child(8)", sep = "")) %>%
    html_text(.) %>%
    as.numeric(.)

SOSwinner = winnerpage %>%
      html_nodes("#team_stats tr:nth-child(1) .right:nth-child(12)") %>%
      html_text(.) %>%
      as.numeric(.)
SOSloser = loserpage %>%
      html_nodes("#team_stats tr:nth-child(1) .right:nth-child(12)") %>%
      html_text(.) %>%
      as.numeric(.)
SOS = c(SOSwinner, SOSloser)
rm(SOSwinner,SOSloser)

OTLwinner = winnerpage %>%
        html_nodes("#team_stats tr:nth-child(1) .right:nth-child(6)") %>%
        html_text(.) %>%
        as.numeric(.)
OTL = loserpage %>%
        html_nodes("#team_stats tr:nth-child(1) .right:nth-child(6)") %>%
        html_text(.) %>%
        as.numeric(.) %>%
        c(OTLwinner,.)
rm(OTLwinner)

RecordsOverTime = winnergames %>%
        html_nodes("tr:nth-child(86) .center+ .right , tr:nth-child(62) .center+ .right , tr:nth-child(41) .center+ .right , tr:nth-child(20) .center+ .right") %>%
        html_text(.) %>%
        as.numeric(.) %>%
        as_tibble(.) %>%
        set_names("Wins") %>%
        mutate(Difference = ifelse(is.na(Wins - lag(Wins, 1)), Wins, Wins - lag(Wins, 1))) %>%
        mutate(Record = Difference / 20)

RecordsOverTime$Record[4] = RecordsOverTime$Difference[4]/22
WinnerRecordOverTime = as.numeric(RecordsOverTime$Record)
rm(RecordsOverTime)

RecordsOverTime = losergames %>%
  html_nodes("tr:nth-child(86) .center+ .right , tr:nth-child(62) .center+ .right , tr:nth-child(41) .center+ .right , tr:nth-child(20) .center+ .right") %>%
  html_text(.) %>%
  as.numeric(.) %>%
  as_tibble(.) %>%
  set_names("Wins") %>%
  mutate(Difference = ifelse(is.na(Wins - lag(Wins, 1)), Wins, Wins - lag(Wins, 1))) %>%
  mutate(Record = Difference / 20)

RecordsOverTime$Record[4] = RecordsOverTime$Difference[4]/22
LoserRecordOverTime = as.numeric(RecordsOverTime$Record)
rm(RecordsOverTime)

PointsWinner = winnerpage %>%
                  html_nodes(".prevnext+ p") %>%
                  html_text(.) %>%
                  sub(".* \\(", "", .) %>%
                  sub(" points).*", "",.) %>%
                  as.numeric(.)

Points = loserpage %>%
  html_nodes(".prevnext+ p") %>%
  html_text(.) %>%
  sub(".* \\(", "", .) %>%
  sub(" points).*", "",.) %>%
  as.numeric(.) %>%
  c(PointsWinner,.)

rm(PointsWinner)

PowerPlayPercentageWinner = winnerpage %>%
  html_nodes("#team_stats tr:nth-child(1) .right:nth-child(16)") %>%
  html_text(.) %>%
  as.numeric(.)

PowerPlayPercentage = loserpage %>%
  html_nodes("#team_stats tr:nth-child(1) .right:nth-child(16)") %>%
  html_text(.) %>%
  as.numeric(.) %>%
  c(PowerPlayPercentageWinner,.)

rm(PowerPlayPercentageWinner)

PenaltyKillPercentageWinner = winnerpage %>%
    html_nodes("#team_stats tr:nth-child(1) .right:nth-child(19)") %>%
    html_text(.) %>%
    as.numeric(.) 

PenaltyKillPercentage = loserpage %>%
  html_nodes("#team_stats tr:nth-child(1) .right:nth-child(19)") %>%
  html_text(.) %>%
  as.numeric(.) %>%
  c(PenaltyKillPercentageWinner,.)

rm(PenaltyKillPercentageWinner)

tibble(ResultProper, RegularSeasonWinPercentage = constant*diff(WinLoss), Points = constant*diff(Points),
                       H2H = H2H, SRS = constant*diff(SRS), Goals = constant*diff(Goals), GoalsAgainst = constant*diff(GoalsAgainst),
                       Penalties = constant*diff(PenaltyMins), PowerPlay = constant*diff(PowerPlay), PowerPlayPercentage = constant*diff(PowerPlayPercentage),
                       SOG = constant*diff(SOG), ShotPercentage = constant*diff(ShotPercentage), WeightedGoalieSavePercentage = constant*diff(WeightedGoalieSavePercentage),
                       WeightedGPS = constant*diff(WeightedGPS), PlayerPoints = constant*diff(PlayerPoints), OTL = constant*diff(OTL), 
                       PenaltyKillPercentage = constant*diff(PenaltyKillPercentage), Q1Record = WinnerRecordOverTime[1] - LoserRecordOverTime[1],
                       Q2Record = WinnerRecordOverTime[2] - LoserRecordOverTime[2], Q3Record = WinnerRecordOverTime[3] - LoserRecordOverTime[3],
                       Q4Record = WinnerRecordOverTime[4] - LoserRecordOverTime[4], SDRecord = sd(WinnerRecordOverTime) - sd(LoserRecordOverTime),
                       SOS = constant*diff(SOS))

}

cluster = makeCluster(detectCores())
registerDoParallel(cluster)
setwd("C:/Users/Brayden/Documents/NHLModel/Status")
data = foreach(j = 1:nrow(template), .combine = rbind, .packages = c("tidyverse", "rvest")) %dopar% {
  write.table(j,paste("Row_", j, ".txt", sep=""))
  getData(year = template$Year[j], team.1  = template$Team1[j], team.2 = template$Team2[j],
          conference = template$Conference[j], Round = template$Round[j], highest_seed = template$Highest.Seed[j], result_known = TRUE)
}
stopCluster(cluster)
