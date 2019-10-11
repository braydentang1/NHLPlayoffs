library(tidyverse)
library(rvest)

template = read_csv("/home/brayden/GitHub/NHLPlayoffs/Scraping Scripts and Template/Templates/Template.csv")

accronyms_pg = read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")
accronyms = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)
fullnames = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_Accronyms = cbind(FullName = fullnames, Accronym = accronyms) %>%
                   as_tibble(.) %>% 
                   bind_rows(., c(FullName = "Mighty Ducks of Anaheim", Accronym = "MDA")) 

rm(accronyms_pg, accronyms, fullnames)

getData_pon = function(year){
  
  #' Pulls data from Puck on Net, namely, stats from the last 20 games of the regular season.
  #'
  #' @param year an integer: the year of NHL Playoffs to pull data from.
  #'
  #' @return
  #' A tibble that contains stats from the last 20 games played.
  #'
  #' @export
  #'
    year2 = year - 1
    
    mainpage = read_html(paste("http://www.puckon.net/fenwick.php?s=",year2,"-09-01&e=",year,"-06-30&f=0&ld=1&l=82&p=0", sep=""))
    last20page_main = read_html(paste("https://www.puckon.net/?s=",year2,"-08-01&e=",year,"-06-30&f=0&ld=-1&l=20&p=0", sep = ""))
    last20page_Goals = read_html(paste("https://www.puckon.net/goals.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    last20page_misses = read_html(paste("https://www.puckon.net/misses.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    last20page_blocks = read_html(paste("https://www.puckon.net/blocks.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    last20page_hits = read_html(paste("https://www.puckon.net/hits.php?f=0&s=",year2,"-08-01&e=",year,"-06-30&l=-20&p=0", sep=""))
    
    teams = mainpage %>%
      html_nodes("td:nth-child(1)") %>%
      html_text(.) %>%
      gsub("\\.", "",.) %>%
      as_tibble(.) %>%
      set_names(., "Accronym") %>%
      mutate(Accronym = ifelse(Accronym == "LA", "LAK",Accronym)) %>%
      mutate(Accronym = ifelse(Accronym == "NJ", "NJD", Accronym)) %>%
      mutate(Accronym = ifelse(Accronym == "SJ", "SJS", Accronym)) %>%
      mutate(Accronym = ifelse(Accronym =="TB", "TBL", Accronym)) 

    Fenwick = mainpage %>% 
      html_nodes("td:nth-child(6)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(Fenwick = .) 

    Fenwick_Last20 = last20page_main %>%
      html_nodes("td:nth-child(10)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(Fenwick_Last20 = .)
    
    Corsi_Last20 = last20page_main %>%
      html_nodes("td:nth-child(6)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(Corsi_Last20 = .)
    
    SOG_Last20 = last20page_main %>%
      html_nodes("td:nth-child(14)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(SOG_Last20 = .)
    
    rm(last20page_main)
    
    GoalsPercentage_Last20 = last20page_Goals %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(GoalsPercentage_Last20 =.)
    
    rm(last20page_Goals)
    
    MissesPercentage_Last20 = last20page_misses %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(MissesPercentage_Last20 = .)
  
    rm(last20page_misses)
    
    BlocksPercentage_Last20 = last20page_blocks %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(BlocksPercentage_Last20 = .)
    
    rm(last20page_blocks)
    
    HitsPercentage_Last20 = last20page_hits %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(HitsPercentage_Last20 = .)
    
    rm(last20page_hits)
    
    data = left_join(teams,lookup_Accronyms, by = "Accronym") %>%
           bind_cols(.,tibble(Year = rep(year, nrow(teams))),teams,Fenwick, Fenwick_Last20, Corsi_Last20, SOG_Last20, GoalsPercentage_Last20, MissesPercentage_Last20, BlocksPercentage_Last20, HitsPercentage_Last20) %>%
           mutate(FullName = ifelse(FullName == "St. Louis Blues", "St Louis Blues", FullName)) %>%
           mutate(FullName = ifelse(FullName == "Anaheim Ducks" & Year <= 2006, "Mighty Ducks of Anaheim", FullName))
 
}

findMatch = function(team.1, team.2, stat, data, highest.seed){
  
  #' Finds the two relevant teams playing each other in the raw dataset provided by getData_nhl_HitsandBlocks or getData_nhl_LeadingandTrailing,
  #'   and calculates the difference in a statistic from the perspective of the higher seed.
  #'
  #' @param team.1 character string; a team competing against team.2 in a particular NHL series
  #' @param team.2 a team competing against team.1 in a particular NHL series
  #' @param stat character string; a column name found in the raw data given by the argument data to compute the differencing
  #' @param data the raw dataset provided by getData_nhl_HitsandBlocks or getData_nhl_LeadingandTrailing
  #' @param highest.seed character string; gives the highest seed among team.1 or team.2. The highest seed is defined as the team that starts the series at home.
  #'
  #' @return
  #' A numeric value that gives the difference in a statistic, from the higher seeds perspective.
  #'
  #' @export
  #'
  
  tmp = unlist(c(data[, names(data) %in% c(stat)][which(data$Team == team.1),], data[, names(data) %in% c(stat)][which(data$Team == team.2),]))
  tmp[which(c(team.1, team.2) == highest.seed)] - tmp[which(c(team.1, team.2) != highest.seed)] 
}

processData = function(team.1, team.2, highest.seed, data, year, start_col = 5L){
  
  #' Processes the raw data from the function getData_pon to be the differences in stats between two teams from the highest seeds perspective.
  #'  Starts processing at column 5 by default.
  #'
  #' @param team.1 a particular team in a NHL playoff series, playing against team.2
  #' @param team.2 a particular team in a NHL playoff series, playing against team.1
  #' @param highest.seed the highest seed between team.1 and team.2
  #' @param the raw data resulting from the function getData_pon
  #' @param year the year of the NHL playoffs for the series played between team.1 and team.2
  #' @param start_col a vector of length one that gives the starting column index to start processing from. All columns from the given column index and onwards are processed. Default = 5L.
  #'
  #' @return
  #'  A list of the processed stats given in the raw dataset.
  #'
  #' @export
  #'
  
  data = data %>% filter(., Year == year)
  
  team_vec = as_tibble(unlist(lapply(colnames(data)[start_col:ncol(data)], FUN = findMatch, team.1 = team.1, team.2 = team.2, data = data, highest.seed = highest.seed))) %>%
    rownames_to_column(.) %>%
    mutate(rowname = colnames(data)[start_col:ncol(data)]) %>%
    spread(rowname, value) 
  
  team_vec
}



allYears = bind_rows(lapply(seq(2006, 2019,1), FUN = getData_pon)) %>% rename(Team = FullName)

finalData = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year, MoreArgs = list(data = allYears), SIMPLIFY = FALSE))
  
setwd("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(finalData, "FenwickScores.csv")