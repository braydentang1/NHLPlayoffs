library(tidyverse)
library(rvest)

template = read_csv("/home/brayden/GitHub/NHLPlayoffs/Scraping Scripts and Template/Templates/Template.csv")
                
getData_nst = function(year){
  
  #' Pulls data from NaturalStatTrick, mostly specialized stats.
  #'
  #' @param year an integer: the year of NHL Playoffs to pull data from.
  #'
  #' @return
  #' A tibble that contains stats on specialized stats for all teams during a particular NHL regular season.
  #'
  #' @export
  #'
  
  year2 = year - 1
  
  mainpage = read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=",year2,year,"&thruseason=",year2,year,"&stype=2&sit=sva&score=all&rate=y&team=all&loc=B&gpf=410&fd=&td=", sep=""))
  
  teams = mainpage %>%
    html_nodes(".lh") %>%
    html_text(.) %>%
    .[2:length(.)]
  
  SCF = mainpage %>% 
          html_nodes("td:nth-child(26)") %>%
          html_text(.) %>%
          as.numeric(.) 
  
  SCA = mainpage %>%
          html_nodes("td:nth-child(27)") %>%
          html_text(.) %>%
          as.numeric(.) 
  
  HDCF = mainpage %>%
          html_nodes("td:nth-child(34)") %>%
          html_text(.) %>%
          as.numeric(.)
  
  HDCA = mainpage %>% 
          html_nodes("td:nth-child(35)") %>%
          html_text(.) %>%
          as.numeric(.)
  
  data = tibble(Year = rep(year, length(teams)), Team = teams, SCF = SCF,
                SCA = SCA, HDCF = HDCF, HDCA = HDCA)
  
}


findMatch = function(team.1, team.2, stat, data, highest.seed){
  
  #' Finds the two relevant teams playing each other in the raw dataset provided by getData_nhl_HitsandBlocks or getData_nhl_LeadingandTrailing,
  #'   and calculates the difference in a statistic from the perspective of the higher seed.
  #'
  #' @param team.1 character string; a team competing against team.2 in a particular NHL series
  #' @param team.2 character string; a team competing against team.1 in a particular NHL series
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

processData = function(team.1, team.2, highest.seed, year, data, start_col = 3L){
  
  #' Processes the dataset for team.1 and team.2 for a particular dataset. 
  #' Starts processing at column 3 of data by default.
  #'
  #' @param team.1 character string; a team competing against team.2 in a particular NHL series
  #' @param team.2 character string; a team competing against team.1 in a particular NHL series
  #' @param stat character string; a column name found in the raw data given by the argument data to compute the differencing
  #' @param data the raw dataset provided by getData_nhl_HitsandBlocks or getData_nhl_LeadingandTrailing
  #' @param highest.seed character string; gives the highest seed among team.1 or team.2. The highest seed is defined as the team that starts the series at home.
  #' @param start_col a vector of length one that gives the starting column index to start processing from. All columns from the given column index and onwards are processed. Default = 3L.
  #' 
  #' @return
  #' A numeric value that gives the difference in a statistic, from the higher seeds perspective.
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

allData = lapply(seq(2008,2019,1), FUN = getData_nst) %>%
              bind_rows(.)

final = bind_rows(mapply(FUN = processData, team.1 = template$Team1, team.2 = template$Team2, highest.seed = template$Highest.Seed, year = template$Year,
               MoreArgs = list(data = allData), SIMPLIFY = FALSE)) %>%
        select_if(~sum(!is.na(.)) > 0) 

setwd("/home/brayden/GitHub/NHLPlayoffs/Required Data Sets")
write_csv(final, "SCFScores.csv")