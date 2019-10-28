library(tidyverse)
library(rvest)

template <- read_csv("src/scraping/templates/template.csv") 

start_dates <- read_csv("src/scraping/templates/time-related-features.csv") %>% filter(Year >= 2008)

get_data_nst_time <- function(year, round, start, end, event = FALSE) {

  #' Retrieves data from NaturalStatTrick in a "walk-through" fashion through the playoffs (to prevent data leakage).
  #'
  #' @param year an integer: the year of playoffs. Example: 2009 for the 2009 NHL Playoffs.
  #' @param round either "quarter-finals", "semi-finals", "finals" or "stanley-cup-final"
  #' @param start starting date of the playoffs
  #' @param end the ending date of a particular round
  #' @param event the context of the data during the game. Must be either "penaltykill" or "powerplay". By default, assumes score and venue adjusted data during regular 5v5 play.
  #'
  #' @return
  #' A tibble that contains relevant playoff data from NaturalStatTrick.
  #'
  #' @export
  #'
  
  if (event == "penaltykill") {
    
  page <- read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=", year-1, year, "&thruseason=", year-1, year, "&stype=3&sit=pk&score=all&rate=y&team=all&loc=B&gpf=410&fd=", start, "&td=", end, sep ="")) 
  
  } else if (event == "powerplay") {
    
  page <- read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=", year-1, year,"&thruseason=", year-1, year,"&stype=3&sit=pp&score=all&rate=y&team=all&loc=B&gpf=410&fd=", start, "&td=", end, sep ="")) 
  
  } else {
    
    page <- read_html(paste("https://www.naturalstattrick.com/teamtable.php?fromseason=", year-1, year,"&thruseason=", year-1, year,"&stype=3&sit=sva&score=all&rate=y&team=all&loc=B&gpf=410&fd=", start, "&td=", end, sep ="")) 
  
    }
  
  team_names <- page %>%
    html_nodes(".lh") %>% 
    html_text(.) %>%
    .[-str_detect(., "Team")]
  
  corsi_for <- page %>%
      html_nodes("td:nth-child(7)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    corsi_against <- page %>%
      html_nodes("td:nth-child(8)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    fenwick_for <- page %>%
      html_nodes("td:nth-child(10)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    fenwick_against <- page %>%
      html_nodes("td:nth-child(11)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    scf <- page %>%
      html_nodes("td:nth-child(22)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    sca <- page %>%
      html_nodes("td:nth-child(23)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    hdcf <- page %>%
      html_nodes("td:nth-child(33)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    hdca <- page %>%
      html_nodes("td:nth-child(34)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    hdsv <- page %>%
      html_nodes("td:nth-child(43)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    save_percentage <- page %>%
      html_nodes("td:nth-child(67)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    pdo <- page %>%
      html_nodes("td:nth-child(68)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    xgf <- page %>%
      html_nodes("td:nth-child(19)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    xga <- page %>%
      html_nodes("td:nth-child(20)") %>%
      html_text(.) %>%
      as.numeric(.)
    
    temp <- tibble(year = rep(year, length(team_names)), up_to_round = rep(round, length(team_names)),
           team = team_names, cf_playoff = corsi_for,
           ca_playoff = corsi_against, fen_for_playoff = fenwick_for,
           fen_aga_playoff = fenwick_against, scf_playoff = scf, 
           sca_playoff = sca, hdsv_playoff = hdsv,
           hdcf_playoff = hdcf, hdca_playoff = hdca, 
           save_percentage_playoff = save_percentage, pdo_playoff = pdo, 
           xgf_playoff = xgf, xga_playoff = xga)
    
  if (event == "penaltykill") {
    
    colnames(temp)[4:ncol(temp)] <- paste(colnames(temp[4:ncol(temp)]), "_pk", sep = "") 
    
  } else if (event == "powerplay") {
    
    colnames(temp)[4:ncol(temp)] <- paste(colnames(temp[4:ncol(temp)]), "_pp", sep = "") 
    
  }
    temp
}

#If you run this too many times, expect your IP address to be blocked on NaturalStatTrick for 24 hours because the site can't handle too much traffic.
all_data <- pmap_dfr(list(start_dates$Year, start_dates$Round, start_dates$Start, start_dates$End), ~get_data_nst_time(year = ..1, round = ..2, start = ..3, end = ..4, event = FALSE))
Sys.sleep(500)

all_data_powerplay <- pmap_dfr(list(start_dates$Year, start_dates$Round, start_dates$Start, start_dates$End), ~get_data_nst_time(year = ..1, round = ..2, start = ..3, end = ..4, event = "powerplay"))
Sys.sleep(500)

all_data_penaltykill <- pmap_dfr(list(start_dates$Year, start_dates$Round, start_dates$Start, start_dates$End), ~get_data_nst_time(year = ..1, round = ..2, start = ..3, end = ..4, event = "penaltykill"))

write_csv(all_data, "data/raw/2008-2019_naturalstattrick_raw-time-features-data/time_data.csv")
write_csv(all_data_powerplay, "data/raw/2008-2019_naturalstattrick_raw-time-features-data/time_data_power-play.csv")
write_csv(all_data_penaltykill, "data/raw/2008-2019_naturalstattrick_raw-time-features-data/time_data_penallty-kill.csv")

#allData = read_csv("/home/brayden/GitHub/NHLPlayoffs/Scraping Scripts and Template/Raw Time Features Data/TimeData.csv")
#allData.powerplay = read_csv("/home/brayden/GitHub/NHLPlayoffs/Scraping Scripts and Template/Raw Time Features Data/TimeData_PowerPlay.csv")
#allData.penaltykill = read_csv("/home/brayden/GitHub/NHLPlayoffs/Scraping Scripts and Template/Raw Time Features Data/TimeData_PenaltyKill.csv")

find_match <- function(team1, team2, stat, data, highest_seed, round) {
  
  #' Parses the raw dataset given by the function "getData.nst.time", finds the two teams that are playing each other during a round in the playoffs, and differences their stats.
  #'
  #' @param team1 a team that is playing against team2 in a particular round of the playoffs
  #' @param team2 a second team that is playing against team1 in a particular round of the playoffs
  #' @param stat the particular statistic (i.e. the column name) in data
  #' @param data the "raw" dataset given by the function get_data_nst_time
  #' @param highest_seed specifies which of team1 or team2 is the highest seed. 
  #'  The highest seed is defined in terms of who starts the series at home. The name of the team must match one of team1 or team2.
  #' @param round the round of which the playoff series between team1 and team2 is being played. 
  #'  Can be "quarter-finals", "semi-finals", "finals" or "stanley-cup-final".
  #'
  #' @return
  #' A numeric value that provides the difference between the highest seed and the lower seed between team1 and team2,
  #'  for a particular column found in data.
  #'
  #' @export 
  #'
  
  if (round == "quarter-finals") {
    0
  } else if (round =="semi-finals") {
    
  tmp_data <- data %>% filter(., up_to_round == "quarter-finals")
  tmp <- unlist(c(tmp_data[, names(tmp_data) %in% c(stat)][which(tmp_data$team == team1), ], tmp_data[, names(tmp_data) %in% c(stat)][which(tmp_data$team == team2), ]))
  tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)]
  
  } else if (round == "finals") {
    
    tmp_data <- data %>% filter(., up_to_round == "semi-finals")
    tmp <- unlist(c(tmp_data[, names(tmp_data) %in% c(stat)][which(tmp_data$team == team1),], tmp_data[, names(tmp_data) %in% c(stat)][which(tmp_data$team == team2), ]))
    tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)]
    
  } else {
    
    tmp_data <- data %>% filter(., up_to_round == "finals")
    tmp <- unlist(c(tmp_data[, names(tmp_data) %in% c(stat)][which(tmp_data$team == team1),], tmp_data[, names(tmp_data) %in% c(stat)][which(tmp_data$team == team2), ]))
    tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)]
    
  }
}

process_data = function(year_of_play, team1, team2, highest_seed, round, data, start_col = 4L) {
  
  #' A wrapper around the function findMatch that processes the data into a usable form. For quarter-final matchups, sets all values to 0 since there are no prior playoff games for that particular season.
  #'   Starts processing data at column 4 by default.
  #'
  #' @param year an integer: the year of the NHL Playoffs
  #' @param team1 character string of the name of a particular team playing in the NHL playoffs, playing against team2
  #' @param team2 character string the name of a particular team playing in the NHL playoffs, playing against team.1
  #' @param highest_seed character string representing the highest seed amongst team.1 or team.2. 
  #'  The highest seed is defined as the team that starts the playoff series at home.
  #' @param round the round of the playoff series between team.1 and team.2
  #' @param data the dataset that should be the result of a call to getData.nst.time
  #' @param start_col a vector of length one that gives the starting column index to start processing from.
  #'  All columns from the given column index and onwards are processed. Default = 4L.
  #'
  #' @return
  #' A tibble that provides the processed data for a particular matchup of the NHL Playoffs.
  #'
  #' @export
  #'
  
  if (year_of_play <= 2007) {
    
    if (round == "quarter-finals") {
      
      tmp <- as_tibble(matrix(c(rep(0, ncol(data)- 3)), ncol = ncol(data) - 3))
      colnames(tmp) <- colnames(data)[start_col:ncol(data)]
      
      tmp
      
    } else {
      
      tmp <- as_tibble(matrix(c(rep(NA, ncol(data)- 3)), ncol = ncol(data) - 3))
      colnames(tmp) <- colnames(data)[start_col:ncol(data)]
      
      tmp
      
    }
    
  } else {
    
    data <- data %>% filter(., year == year_of_play)
    
    team_vec <- as_tibble(unlist(lapply(colnames(data)[start_col:ncol(data)], FUN = find_match, team1 = team1, team2 = team2, data = data, highest_seed = highest_seed, round = round))) %>%
      rownames_to_column(.) %>%
      mutate(rowname = colnames(data)[start_col:ncol(data)]) %>%
      spread(rowname, value) 
    
  }
}

final <- pmap_dfr(list(template$Year, template$Team1, template$Team2, template$Highest.Seed, template$Round), ~process_data(year_of_play = ..1, team1 = ..2, team2 = ..3, highest_seed = ..4, round = ..5, data = all_data)) %>%
  bind_cols(., pmap_dfr(list(template$Year, template$Team1, template$Team2, template$Highest.Seed, template$Round), ~process_data(year_of_play = ..1, team1 = ..2, team2 = ..3, highest_seed = ..4, round = ..5, data = all_data_powerplay))) %>%
  bind_cols(., pmap_dfr(list(template$Year, template$Team1, template$Team2, template$Highest.Seed, template$Round), ~process_data(year_of_play = ..1, team1 = ..2, team2 = ..3, highest_seed = ..4, round = ..5, data = all_data_penaltykill)))

#Remove these variables because they aren't meaningful in the context.
final <- final %>%
  select(-hdsv_playoff_pp, -cf_playoff_pk, -scf_playoff_pk, -hdcf_playoff_pk, -pdo_playoff_pk, -xgf_playoff_pk)

write_csv(final, "data/processed/2008-2019_naturalstattrick_time-related.csv")