library(tidyverse)
library(rvest)

template = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv") 

startdates = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Time Related Features.csv") %>% filter(Year >= 2008)

getData.nst.Time = function(year, round, start, end, powerplay = FALSE){
  
  if(powerplay == FALSE){
  page = read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=",year-1,year,"&thruseason=",year-1,year,"&stype=3&sit=sva&score=all&rate=y&team=all&loc=B&gpf=410&fd=",start,"&td=",end, sep ="")) 
  }else{
  page = read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=",year-1,year,"&thruseason=",year-1,year,"&stype=3&sit=pp&score=all&rate=y&team=all&loc=B&gpf=410&fd=",start,"&td=",end, sep ="")) 
  }
  
  teamnames = page %>%
    html_nodes(".lh") %>% 
    html_text(.) %>%
    .[-str_detect(., "Team")]
  
  if(year < 2010){
  
  corsi_for = page %>%
    html_nodes("td:nth-child(7)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  corsi_against = page %>%
    html_nodes("td:nth-child(8)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  fenwick_for = page %>%
    html_nodes("td:nth-child(10)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  fenwick_against = page %>%
    html_nodes("td:nth-child(11)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  scf = page %>%
    html_nodes("td:nth-child(19)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  sca = page %>%
    html_nodes("td:nth-child(20)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  hdcf = page %>%
    html_nodes("td:nth-child(30)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  hdca = page %>%
    html_nodes("td:nth-child(31)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  hdsv = page %>%
    html_nodes("td:nth-child(40)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  savepercentage = page %>%
    html_nodes("td:nth-child(64)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  pdo = page %>%
    html_nodes("td:nth-child(65)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  xGF = rep(NA, length(teamnames))
  xGA = rep(NA, length(teamnames))
  
  
  } else{
    
    corsi_for = page %>%
      html_nodes("td:nth-child(7)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    corsi_against = page %>%
      html_nodes("td:nth-child(8)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    fenwick_for = page %>%
      html_nodes("td:nth-child(10)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    fenwick_against = page %>%
      html_nodes("td:nth-child(11)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    scf = page %>%
      html_nodes("td:nth-child(22)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    sca = page %>%
      html_nodes("td:nth-child(23)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    hdcf = page %>%
      html_nodes("td:nth-child(33)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    hdca = page %>%
      html_nodes("td:nth-child(34)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    hdsv = page %>%
      html_nodes("td:nth-child(43)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    savepercentage = page %>%
      html_nodes("td:nth-child(67)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    pdo = page %>%
      html_nodes("td:nth-child(68)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    #Extra stats introduced in 2010:
    
    xGF = page %>%
      html_nodes("td:nth-child(19)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    xGA = page %>%
      html_nodes("td:nth-child(20)") %>%
      html_text(.) %>%
      as.numeric(.)
    
  }
  
  if(powerplay == FALSE){
  tibble(Year = rep(year, length(teamnames)), Up.To.Round = rep(round, length(teamnames)), Team = teamnames, CF.Playoff = corsi_for, CA.Playoff = corsi_against,
         FenFor.Playoff = fenwick_for, FenAga.Playoff = fenwick_against, SCF.Playoff = scf, SCA.Playoff = sca, HDSV.Playoff = hdsv, HDCF.Playoff = hdcf, HDCA.Playoff = hdca, SavePercentage.Playoff = savepercentage,
         PDO.Playoff = pdo, xGF.Playoff = xGF, xGA.Playoff = xGA)
  }else{
    tibble(Year = rep(year, length(teamnames)), Up.To.Round = rep(round, length(teamnames)), Team = teamnames, CF.Playoff.PP = corsi_for, CA.Playoff.PP = corsi_against,
           FenFor.Playoff.PP = fenwick_for, FenAga.Playoff.PP = fenwick_against, SCF.Playoff.PP = scf, SCA.Playoff.PP = sca, HDSV.Playoff.PP = hdsv, HDCF.Playoff.PP = hdcf,
           HDCA.Playoff.PP = hdca, SavePercentage.Playoff.PP = savepercentage,
           PDO.Playoff.PP = pdo, xGF.Playoff.PP = xGF, xGA.Playoff.PP = xGA)
  }
}

#If you run this too many times, expect your IP address to be blocked on NaturalStatTrick for 24 hours because the site can't handle too much traffic.
#allData = bind_rows(mapply(getData.nst.Time, year = startdates$Year, round = startdates$Round, start = startdates$Start, end = startdates$End, powerplay = FALSE, SIMPLIFY = FALSE))
#Sys.sleep(500)
#allData.powerplay = bind_rows(mapply(getData.nst.Time, year = startdates$Year, round = startdates$Round, start = startdates$Start, end = startdates$End, powerplay = TRUE, SIMPLIFY = FALSE))

#write_csv(allData, "TimeData.csv")
#write_csv(allData.powerplay, "TimeData_PowerPlay.csv")

allData = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Raw Time Features Data/TimeData.csv")
allData.powerplay = read_csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Raw Time Features Data/TimeData_PowerPlay.csv")

findMatch = function(team.1, team.2, stat, data, highest.seed, round){
  
  if(round == "quarter-finals"){
    0
  } else if (round =="semi-finals"){
    
  tmp.data = data %>% filter(., Up.To.Round == "quarter-finals")
  tmp = unlist(c(tmp.data[, names(tmp.data) %in% c(stat)][which(tmp.data$Team == team.1),], tmp.data[, names(tmp.data) %in% c(stat)][which(tmp.data$Team == team.2),]))
  tmp[which(c(team.1, team.2) == highest.seed)] - tmp[which(c(team.1, team.2) != highest.seed)]
  
  } else if (round == "finals") {
    
    tmp.data = data %>% filter(., Up.To.Round == "semi-finals")
    tmp = unlist(c(tmp.data[, names(tmp.data) %in% c(stat)][which(tmp.data$Team == team.1),], tmp.data[, names(tmp.data) %in% c(stat)][which(tmp.data$Team == team.2),]))
    tmp[which(c(team.1, team.2) == highest.seed)] - tmp[which(c(team.1, team.2) != highest.seed)]
    
  } else {
    
    tmp.data = data %>% filter(., Up.To.Round == "finals")
    tmp = unlist(c(tmp.data[, names(tmp.data) %in% c(stat)][which(tmp.data$Team == team.1),], tmp.data[, names(tmp.data) %in% c(stat)][which(tmp.data$Team == team.2),]))
    tmp[which(c(team.1, team.2) == highest.seed)] - tmp[which(c(team.1, team.2) != highest.seed)]
  }
}

processData = function(year, team.1, team.2, highest.seed, round, data){
  
  if(year <= 2007){
    
    if(round == "quarter-finals"){
      
      tmp = as_tibble(matrix(c(rep(0, ncol(data)- 3)), ncol = ncol(data) - 3))
      colnames(tmp) = colnames(data)[4:ncol(data)]
      
      tmp
      
    } else{
      
      tmp = as_tibble(matrix(c(rep(NA, ncol(data)- 3)), ncol = ncol(data) - 3))
      colnames(tmp) = colnames(data)[4:ncol(data)]
      
      tmp
    }
    
  } else {
    
    data = data %>% filter(., Year == year)
    
    team_vec = as_tibble(unlist(lapply(colnames(data)[4:ncol(data)], FUN = findMatch, team.1 = team.1, team.2 = team.2, data = data, highest.seed = highest.seed, round = round))) %>%
      rownames_to_column(.) %>%
      mutate(rowname = colnames(data)[4:ncol(data)]) %>%
      spread(rowname, value) 
    
  }
}

final = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, round = template$Round, MoreArgs = list(data = allData), SIMPLIFY = FALSE)) %>%
        bind_cols(.,bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, round = template$Round, MoreArgs = list(data = allData.powerplay), SIMPLIFY = FALSE)))

#remove the renaming after being unbanned from NST.
final = final[, !names(final) %in% c("HDSV.Playoff.PP")] %>% rename(HDCA.Playoff.PP = HDCA.Playoff1, SavePercentage.Playoff.PP = SavePercentage.Playoff1)

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(final, "TimeRelatedPlayoffFeatures.csv")