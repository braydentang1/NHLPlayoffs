library(tidyverse)
library(rvest)

template <- read_csv("src/scraping/templates/template.csv")
                
get_data_nst <- function(year) {
  
  #' Pulls data from NaturalStatTrick, mostly specialized stats.
  #'
  #' @param year an integer: the year of NHL Playoffs to pull data from.
  #'
  #' @return
  #' A tibble that contains stats on specialized stats for all teams during a particular NHL regular season.
  #'
  #' @export
  #'
  
  year2 <- year - 1
  
  main_page <- read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=", year2, year,"&thruseason=", year2, year,"&stype=2&sit=sva&score=all&rate=y&team=all&loc=B&gpf=410&fd=&td=", sep=""))
  
  teams <- main_page %>%
    html_nodes(".lh") %>%
    html_text(.) %>%
    .[2:length(.)]
  
  scf <- main_page %>% 
          html_nodes("td:nth-child(26)") %>%
          html_text(.) %>%
          as.numeric(.) 
  
  sca <- main_page %>%
          html_nodes("td:nth-child(27)") %>%
          html_text(.) %>%
          as.numeric(.) 
  
  hdcf <- main_page %>%
          html_nodes("td:nth-child(34)") %>%
          html_text(.) %>%
          as.numeric(.)
  
  hdca <- main_page %>% 
          html_nodes("td:nth-child(35)") %>%
          html_text(.) %>%
          as.numeric(.)
  
  tibble(year = rep(year, length(teams)), team = teams, scf = scf,
                sca = sca, hdcf = hdcf, hdca = hdca)
  
}

find_match <- function(team1, team2, stat, data, highest_seed) {
  
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
  
  tmp <- unlist(c(data[, names(data) %in% c(stat)][which(data$team == team1), ], data[, names(data) %in% c(stat)][which(data$team == team2), ]))
  tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)]
  
}

process_data <- function(team1, team2, highest_seed, year_of_play, data, start_col = 3L) {
  
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
  
  data <- data %>% filter(., year == year_of_play)
  
  team_vec <- as_tibble(unlist(lapply(colnames(data)[start_col:ncol(data)], FUN = find_match, team1 = team1, team2 = team2, data = data, highest_seed = highest_seed))) %>%
    rownames_to_column(.) %>%
    mutate(rowname = colnames(data)[start_col:ncol(data)]) %>%
    spread(rowname, value) 
  
  team_vec
  
}


all_data <- map_df(2008:2019, get_data_nst)
write_csv(all_data, "data/raw/2008-2019_naturalstattrick_raw.csv")

final <- pmap_dfr(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, ..4, data = all_data)) %>%
  select_if(~sum(!is.na(.)) > 0) 

write_csv(final, "data/processed/2008-2019_naturalstattrick_scf.csv")