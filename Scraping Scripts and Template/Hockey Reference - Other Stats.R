library(rvest)
library(tidyverse)
library(RSelenium)

template = read_csv("/home/brayden/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv")

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

rem_dr = remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
rem_dr$open()

grabPageofMatchUp = function(year, team.1, team.2, Round, conference){

  if(any(Round %in% c("quarter-finals", "semi-finals", "finals", "stanley-cup-final")) ==  FALSE){
    stop("Invalid round format.")
  }
  
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

  teamsCombined = combine(team.1, team.2) %>%
                    tolower(.) %>% 
                    str_replace_all(., coll(" "), "-") %>%
                    .[order(.)]
  
  if(conference == "eastern" | conference == "western"){
    if(year < 2014){
      mainpage = read_html(paste("https://www.hockey-reference.com/playoffs/",year,"-",teamsCombined[1],"-vs-",teamsCombined[2],"-",conference,"-conference-",Round,".html", sep = ""))}
    else{
      mainpage = read_html(paste("https://www.hockey-reference.com/playoffs/",year,"-",teamsCombined[1],"-vs-",teamsCombined[2],"-",conference,"-",Round,".html", sep = ""))}
  }else{
    mainpage = read_html(paste("https://www.hockey-reference.com/playoffs/",year,"-",teamsCombined[1],"-vs-",teamsCombined[2],"-stanley-cup-final.html", sep=""))
  }
  mainpage
}

grabPageandGamesofSpecificTeam = function(team, year, lookup_Accronyms){
  teamAcc = as.character(lookup_Accronyms[which(lookup_Accronyms$FullName == team),2])
  
  list(page = read_html(paste("https://www.hockey-reference.com/teams/",teamAcc,"/",year,".html", sep="")),
       games = read_html(paste("https://www.hockey-reference.com/teams/",teamAcc,"/",year,"_games.html", sep= "")),
       year = year)
  
}


calculateH2H = function(mainpage, highest.seed, process = FALSE){
  
  paste(highest.seed)
  
  teamNames = mainpage %>%
    html_nodes("#content span a") %>%
    html_text(.) %>%
    str_remove(., "[.]") %>%
    tibble(FullName = .)
  
  if(any(teamNames$FullName == highest.seed) == FALSE){
    stop("Highest seed does not match any teams being pulled from main page.")
  }
  
  H2Hstats = mainpage %>%
    html_nodes("h2+ .game_summaries .winner td:nth-child(1) a") %>%
    html_text(.) %>%
    table() 
  
  if(length(H2Hstats) != 0){
    
    H2Hstats = H2Hstats %>%
      as_tibble(.) %>%
      set_names(c("FullName", "Wins")) %>%
      mutate(WinRatio = Wins/sum(Wins)) %>%
      mutate(FullName = str_remove(FullName, "[.]")) %>%
      right_join(., teamNames, by = "FullName") %>%
      replace_na(., list(Wins = 0, WinRatio = 0))
    
  }else {
    H2Hstats = NA
  }
  
  ifelse(process == TRUE & any(!is.na(H2Hstats)), H2Hstats$WinRatio[H2Hstats$FullName == highest.seed],
         ifelse(process == TRUE & any(is.na(H2Hstats)), H2Hstats, H2Hstats))
  
}

calculateGoalieStats = function(teamPage, returnGoalieSavePercentage = TRUE){

teamPage = teamPage$page
    
main = teamPage %>%
    html_nodes("#goalies tbody .right:nth-child(22) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(12)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    as_tibble(.) %>%
    mutate(Stat = rep(c("SavePercentage", "Minutes","GPS"), length(.$value)/3)) %>%
    mutate(Goalie = rep(seq_len(length(.$value)/3), each = 3)) %>%
    spread(., Stat, value) %>%
    mutate(WeightedGPS = GPS * Minutes / sum(.$Minutes)) %>%
    mutate(WeightedGoalieSavePercentage = Minutes * SavePercentage / sum(Minutes))

WeightedGoalieSavePercentage = sum(main$WeightedGoalieSavePercentage, na.rm = TRUE)
WeightedGPS = sum(main$WeightedGPS, na.rm = TRUE)

ifelse(returnGoalieSavePercentage == TRUE, WeightedGoalieSavePercentage, WeightedGPS)

  
}

calculateRecordOverTime = function(teamPage){
  
  year = as.numeric(teamPage$year)
  teamPage = teamPage$games
  
if(year != 2013){
  
  RecordsOverTime = teamPage %>%
    html_nodes("tr:nth-child(86) .center+ .right , tr:nth-child(62) .center+ .right , tr:nth-child(41) .center+ .right , #games tr:nth-child(20) .center+ .right") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    as_tibble(.) %>%
    set_names("Wins") %>%
    mutate(Difference = ifelse(is.na(Wins - lag(Wins, 1)), Wins, Wins - lag(Wins, 1))) %>%
    mutate(Record = Difference / 20)
  
  RecordsOverTime$Record[4] = RecordsOverTime$Difference[4]/22
  }else{
  
    RecordsOverTime = teamPage %>%
      html_nodes("tr:nth-child(50) .center+ .right , tr:nth-child(37) .center+ .right , tr:nth-child(25) .center+ .right , tr:nth-child(12) .center+ .right") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      as_tibble(.) %>%
      set_names("Wins") %>%
      mutate(Difference = ifelse(is.na(Wins - lag(Wins, 1)), Wins, Wins - lag(Wins, 1))) %>%
      mutate(Record = Difference / 12)
  
  }
  RecordsOverTime = RecordsOverTime %>% 
    select(Record) %>%
    slice(., 1:4) %>%
    bind_cols(Quarter = c("Q1Record", "Q2Record", "Q3Record", "Q4Record"), Record =.) %>%
    spread(., key = Quarter, value = Record) %>%
    bind_cols(tibble(Year = year), .)
  RecordsOverTime
}

calculatePlayerPoints = function(teamPage){
  
teamPage = teamPage$page

PlayerPoints = teamPage %>%
    html_nodes("#skaters tfoot .right:nth-child(8)") %>%
    html_text(.) %>%
    as.numeric(.)

PlayerPoints
}

getTeamNames = function(year){
  
  rem_dr$navigate(paste("https://www.hockey-reference.com/leagues/NHL_",year,".html", sep = ""))
  
  main = read_html(rem_dr$getPageSource()[[1]]) %>%
  html_nodes("#stats tbody .left") %>%
  html_text(.) %>%
  str_remove(.,"[*]") %>%
  str_remove(., "[.]") %>%
  tibble(Year = rep(year, length(.)), Team = .)
  
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

getWinner = function(pages, year, highest.seed, team.1, team.2){
  
  mainpage = pages[[year - 2005]]

  allPlayoffTeams = mainpage %>% 
              html_nodes("#all_playoffs td:nth-child(3)") %>%
              html_text(.) %>%
              str_remove(., "[.]") %>%
              .[str_detect(., "over")] %>%
              .[str_detect(., team.1) & str_detect(.,team.2)] %>%
              str_split_fixed(., "over", n=2)
  
  if(nrow(allPlayoffTeams) == 0){
    NA
  } else{
    
    allPlayoffTeams = allPlayoffTeams %>% 
      as_tibble(.) %>%
      set_names(c("Winner", "Loser")) %>%
      mutate(Winner = str_trim(Winner),
             Loser = str_trim(Loser)) 
    
    ifelse(any(allPlayoffTeams$Winner == highest.seed), "W", "L")
    
  }
}

getPastNumberofGames = function(year){
  
  mainpage = read_html(paste("https://www.hockey-reference.com/playoffs/NHL_",year,".html", sep = ""))
  
  allPlayoffTeams = mainpage %>% 
    html_nodes("#all_playoffs td:nth-child(3)") %>%
    html_text(.) %>%
    str_remove(., "[.]") %>%
    .[str_detect(., "over")] %>%
    as_tibble(.) %>%
    rename(Teams = value) %>%
    mutate(Teams = str_trim(Teams, side = "right"))
  
  game.series = mainpage %>%
    html_nodes("#all_playoffs td:nth-child(2)") %>%
    html_text(.) %>%
    .[str_detect(., "-")] %>%
    str_split_fixed(., "-", n=2) %>%
    as_tibble(.) %>%
    rename(Winning.Game.Count = V1, Losing.Game.Count = V2) %>%
    mutate(Winning.Game.Count = as.numeric(Winning.Game.Count)) %>%
    mutate(Losing.Game.Count = as.numeric(Losing.Game.Count)) %>%
    filter(Winning.Game.Count == 4) %>%
    transmute(TotalGamesPlayed = rowSums(.)) %>%
    bind_cols(Year = rep(year, nrow(.)), allPlayoffTeams, .)
  
}

processData.numberofGames = function(team.1, team.2, round, highest.seed, year, data){
  
  if(round == "quarter-finals"){
    0
  }else{
  
  team1.games = data %>% filter(Year == year) %>% .[str_detect(.$Teams, team.1),]
  team2.games = data %>% filter(Year == year) %>% .[str_detect(.$Teams, team.2),]
  
  if(which(c(team.1, team.2) == highest.seed) == 1){
    highest.seed.games = team1.games
    lowest.seed.games = team2.games
  }else{
    highest.seed.games = team2.games
    lowest.seed.games = team1.games
  }
  
    if(round == "semi-finals"){
      
      highest.seed.games$TotalGamesPlayed[nrow(highest.seed.games)] - lowest.seed.games$TotalGamesPlayed[nrow(lowest.seed.games)]
      
    }else if(round == "finals"){
      
      highest.seed.games$TotalGamesPlayed[nrow(highest.seed.games) - 1] - lowest.seed.games$TotalGamesPlayed[nrow(lowest.seed.games) - 1]
      
    }else{
      
      highest.seed.games$TotalGamesPlayed[nrow(highest.seed.games) - 2] - lowest.seed.games$TotalGamesPlayed[nrow(lowest.seed.games) - 2]
      
    }
  
  }
  
}

getPastNumberofGames = bind_rows(lapply(2006:2019, FUN = getPastNumberofGames))
givePastNumberofGames = unlist(mapply(FUN = processData.numberofGames, year = template$Year, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, round = template$Round, MoreArgs = list(data = getPastNumberofGames), SIMPLIFY = FALSE))

allData = bind_rows(lapply(2006:2019, FUN = getTeamNames)) 

allTeamPages = mapply(FUN = grabPageandGamesofSpecificTeam, team = allData$Team, year = allData$Year, MoreArgs = list(lookup_Accronyms = lookup_Accronyms), SIMPLIFY = FALSE)
allWinners = lapply(2006:2019, function(year){read_html(paste("https://www.hockey-reference.com/playoffs/NHL_",year, ".html", sep = ""))})
giveWinners = unlist(mapply(FUN = getWinner, year = template$Year, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, MoreArgs = list(pages = allWinners), SIMPLIFY = FALSE))

rm(allWinners)

final = tibble(WeightedGoalieSavePercntage = unlist(lapply(allTeamPages, FUN = calculateGoalieStats, returnGoalieSavePercentage = TRUE))) %>%
            bind_cols(., WeightedGPS = unlist(lapply(allTeamPages, FUN = calculateGoalieStats, returnGoalieSavePercentage = FALSE)), 
                         PlayerPoints = unlist(lapply(allTeamPages, FUN = calculatePlayerPoints))) %>%
            bind_cols(allData,.) %>%
            mutate(PlayerPoints = ifelse(Year == 2013, PlayerPoints/48, PlayerPoints/82))

RecordsOverTime = bind_rows(lapply(allTeamPages, FUN = calculateRecordOverTime)) %>%
                  bind_cols(Team = allData$Team, .) %>%
                  rowwise %>%
                  mutate(SDRecord = sd(c(Q1Record, Q2Record, Q3Record, Q4Record)))

allH2Hpages = mapply(grabPageofMatchUp, year = template$Year, team.1 = template$Team1, team.2 = template$Team2, Round = template$Round, conference = template$Conference, SIMPLIFY = FALSE)
giveH2H = tibble(H2H = unlist(mapply(calculateH2H, mainpage = allH2Hpages, highest.seed = template$Highest.Seed, process = TRUE, SIMPLIFY = FALSE)))

rm(allData, allH2Hpages, allTeamPages)
gc()

allStats = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = final), SIMPLIFY = FALSE)) %>%
           bind_cols(tibble(ResultProper = giveWinners, PastSeriesGameCount = givePastNumberofGames),., giveH2H) %>%
           bind_cols(bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = RecordsOverTime), SIMPLIFY = FALSE)))

rm(RecordsOverTime, final, giveH2H)

setwd("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(allStats, "HockeyReference2.csv")
