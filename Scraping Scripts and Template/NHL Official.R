library(tidyverse)
library(rvest)

template = read.csv("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Scraping Scripts and Template/Template.csv", na.strings = FALSE, stringsAsFactors = FALSE)

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

getData_nhl_HitsandBlocks = function(year){
  
  mainpage = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/NHL HTML Renders/Hits and Blocks/NHL.com - Stats ",year,".html", sep=""))
  
  TeamName = mainpage %>%
    html_nodes(".rt-td:nth-child(2)") %>%
    html_text(.) %>%
    gsub("é", "e",.) %>%
    gsub("\\.", "",.) 
  
  Hits = mainpage %>%
    html_nodes(".rt-td:nth-child(10)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  Blocks = mainpage %>%
    html_nodes(".rt-td:nth-child(11)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  FaceoffWinPercentage = mainpage %>%
    html_nodes(".rt-td:nth-child(18)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  GiveAways = mainpage %>%
    html_nodes(".rt-td:nth-child(13)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  TakeAways = mainpage %>%
    html_nodes(".rt-td:nth-child(14)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  if(year == 2013){
    Hits = Hits/48
    Blocks = Blocks/48
    GiveAways = GiveAways/48
    TakeAways = TakeAways/48
  }else{
    Hits = Hits/82
    Blocks = Blocks/82
    GiveAways = GiveAways/82
    TakeAways = TakeAways/82
  }
  
  data = tibble(Year = rep(year, length(TeamName)),Team = TeamName, BlocksatES = Blocks, HitsatES = Hits, FaceoffWinPercentage = FaceoffWinPercentage, 
                GiveAways = GiveAways, TakeAways = TakeAways) %>%
          mutate(Team = ifelse(Team == "Anaheim Ducks" & year <= 2006, "Mighty Ducks of Anaheim", Team))
  
}

getData_nhl_LeadingandTrailing = function(year){
  
  mainpage = read_html(paste("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/NHL HTML Renders/Leading and Trailing/",year,".html", sep=""))
  
  TeamName = mainpage %>%
    html_nodes(".rt-td:nth-child(2)") %>%
    html_text(.) %>%
    gsub("é", "e",.) %>%
    gsub("\\.", "",.) 
  
  WinPercent_Lead1P = mainpage %>%
    html_nodes(".rt-td:nth-child(13)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  WinPercent_Lead2P = mainpage %>%
    html_nodes(".rt-td:nth-child(17)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  WinPercent_Trail1P = mainpage %>%
    html_nodes(".rt-td:nth-child(21)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  WinPercent_Trail2P = mainpage %>%
    html_nodes(".rt-td:nth-child(25)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  OT_Losses_Lead1P = mainpage %>%
    html_nodes(".rt-td:nth-child(12)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  OT_Losses_Lead2P = mainpage %>%
    html_nodes(".rt-td:nth-child(16)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  if(year == 2013){
    OT_Losses_Lead1P = OT_Losses_Lead1P/48
    OT_Losses_Lead2P = OT_Losses_Lead2P/48
  }else{
    OT_Losses_Lead1P = OT_Losses_Lead1P/82
    OT_Losses_Lead2P = OT_Losses_Lead2P/82
  }
  
  
  data = tibble(Year = rep(year, length(TeamName)),
                Team = TeamName,
                WinPercent_Lead1P = WinPercent_Lead1P,
                WinPercent_Lead2P = WinPercent_Lead2P, 
                WinPercent_Trail1P = WinPercent_Trail1P, 
                WinPercent_Trail2P = WinPercent_Trail2P,
                OT_Losses_Lead1P = OT_Losses_Lead1P,
                OT_Losses_Lead2P = OT_Losses_Lead2P) %>%
    mutate(Team = ifelse(Team == "Anaheim Ducks" & year <= 2006, "Mighty Ducks of Anaheim", Team))
  
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

allData = lapply(2006:2018, FUN = getData_nhl_HitsandBlocks) %>%
          bind_rows(.) %>%
          left_join(., bind_rows(lapply(2006:2018, FUN = getData_nhl_LeadingandTrailing)), by = c("Year", "Team")) 
          

final = bind_rows(mapply(processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = allData),
               SIMPLIFY = FALSE))

setwd("C:/Users/Brayden/Documents/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(final, "NHLOfficialStats.csv")