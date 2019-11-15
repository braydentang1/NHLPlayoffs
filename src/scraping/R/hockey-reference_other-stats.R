library(rvest)
library(tidyverse)
library(RSelenium)

template <- read_csv("src/scraping/templates/template.csv")

accronyms_pg <- read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")

accronyms <- accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)

full_names <- accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_accronyms <- cbind(full_name = full_names, accronym = accronyms) %>%
  as_tibble(.) %>%
  bind_rows(., c(full_name = "Mighty Ducks of Anaheim", accronym = "MDA")) %>%
  bind_rows(., c(full_name = "St Louis Blues", accronym = "STL"))  

lookup_accronyms$accronym <- ifelse(lookup_accronyms$accronym == "VGK", "VEG", lookup_accronyms$accronym)                                                                       

rm(accronyms_pg, accronyms, full_names)

rem_dr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
rem_dr$open()

grab_page_of_matchup <- function(year, team1, team2, round, conference) {

  #' Grabs the hockeyreference.com web page of a particular playoff matchup.
  #'
  #' @param year integer: the year of NHL playoffs for a particular playoff matchup
  #' @param team1 character string of a specific team that played against team2 in the NHL playoffs
  #' @param team2 character string of a specific team that played against team1 in the NHL playoffs
  #' @param round character string that gives the round of the playoff series between team1 and team2. Should be either "quarter-finals", "semi-finals", "finals", or "stanley-cup-final".
  #' @param conference character string that gives the conference of play. Should be "western" or "eastern"
  #' 
  #' @return
  #' A list that outlines the html page pulled from hockeyreference, given by read_html of rvest.
  #'
  #' @export
  #' 
  
  
  if (any(round %in% c("quarter-finals", "semi-finals", "finals", "stanley-cup-final")) ==  FALSE) {
    stop("Invalid round format.")
  }
  
  if (year >= 2014) {
    
    if (round == "quarter-finals") {
      round <- "first-round"
    } else if (round == "semi-finals") {
      round <- "second-round"
    } else if (round =="finals") {
      round <- "conference-finals"
    } else {
      round <- "stanley-cup-final"
    }
  }

  teams_combined <- combine(team1, team2) %>%
                    tolower(.) %>% 
                    str_replace_all(., coll(" "), "-") %>%
                    .[order(.)]
  
  if (conference == "eastern" | conference == "western") {
    if (year < 2014) {
      main_page <- read_html(paste("https://www.hockey-reference.com/playoffs/", year, "-", teams_combined[1], "-vs-", teams_combined[2], "-", conference ,"-conference-", round, ".html", sep = ""))
      } else {
      main_page <- read_html(paste("https://www.hockey-reference.com/playoffs/", year, "-", teams_combined[1], "-vs-", teams_combined[2], "-", conference, "-", round, ".html", sep = ""))}
  } else {
    main_page <- read_html(paste("https://www.hockey-reference.com/playoffs/", year, "-", teams_combined[1], "-vs-", teams_combined[2], "-stanley-cup-final.html", sep = ""))
  }
  main_page
}

grab_page_and_games_specific_team <- function(team, year, lookup_accronyms) {
  
  #' Grabs the main page of a team from hockey reference for a particular year. 
  #'
  #' @param team character string of a NHL team that played in the NHL regular season for the year.
  #' @param year year of data to pull from
  #' @param lookup_accronyms a lookup table that finds accronyms for a particular team in the NHL regular season. See the README file in the repo for details.
  #'
  #' @return
  #' A list that provides the main html webpage (resulting from a call to read_html of rvest), the game page, and the year for a particular team during the NHL regular season.
  #'
  #' @export
  #' 
  
  team_acc <- as.character(lookup_accronyms[which(lookup_accronyms$full_name == team), 2])
  
  list(
    page = read_html(paste("https://www.hockey-reference.com/teams/", team_acc, "/", year, ".html", sep = "")),
    games = read_html(paste("https://www.hockey-reference.com/teams/", team_acc, "/", year, "_games.html", sep = "")),
    year = year)
  
}


calculate_H2H <- function(main_page, highest_seed, process = TRUE) {

  #' Calculates the head to head during the regular season between two teams in a series, using a matchup page resulting from
  #'  a call to grab_page_of_matchup.
  #'  
  #' @param main_page html page resulting from a call to grabPageofMatchUp
  #' @param highest_seed 
  #'  character vector of length one that gives 
  #'  the highest seed between two teams in a series. The highest seed is defined as the team that starts the playoff series at home.
  #' @param process Logical vector of length one. 
  #'  If TRUE, will provide the win rate against the lower seeded team during the regular season.
  #'  If FALSE, will provide the raw data table for both teams involved in a playoff series. Default = TRUE.
  #' 
  #' @return
  #' A numeric value representing the head to head win rate against the other team, from the higher seeds perspective during the regular season, or
  #' a tibble containing the head to head record during the regular season for both teams involved in a playoff series.
  #'
  #' @export
  #' 
  
  team_names <- main_page %>%
    html_nodes("#content span a") %>%
    html_text(.) %>%
    str_remove(., "[.]") %>%
    tibble(full_name = .)
  
  if (any(team_names$full_name == highest_seed) == FALSE) {
    stop("Highest seed does not match any teams being pulled from main page.")
  }
  
  H2H_stats <- main_page %>%
    html_nodes("h2+ .game_summaries .winner td:nth-child(1) a") %>%
    html_text(.) %>%
    table() 
  
  if (length(H2H_stats) != 0) {
    
    H2H_stats <- H2H_stats %>%
      as_tibble(.) %>%
      set_names(c("full_name", "wins")) %>%
      mutate(win_ratio = wins/sum(wins)) %>%
      mutate(full_name = str_remove(full_name, "[.]")) %>%
      right_join(., team_names, by = "full_name") %>%
      replace_na(., list(wins = 0, win_ratio = 0))

  } else {
    H2H_stats <- NA
  }
  ifelse(process == TRUE & any(!is.na(H2H_stats)), H2H_stats$win_ratio[H2H_stats$full_name == highest_seed], H2H_stats)
}

calculate_goalie_stats <- function(team_page, return_goalie_save_percentage = TRUE) {

  #' Calculates weighted goalie statistics (by playing time) for a specified team during the NHL regular season.
  #'
  #' @param team_page a list containing html attributes as a result of calling function grabPageandGamesofSpecificTeam
  #' @param return_goalie_save_percentage a logical. Should the goalie save percentage be returned? If FALSE, will return the weighted goalie point share instead.
  #'
  #' @return
  #' A numeric value representing the head to head win rate against the other team, from the higher seeds perspective during the regular season, or
  #' a tibble containing the head to head record during the regular season for both teams involved in a playoff series.
  #'  
  #' @export
  #'
  
main <- team_page$page %>%
    html_nodes("#goalies tbody .right:nth-child(22) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(15) , #goalies tbody .right:nth-child(12)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    as_tibble(.) %>%
    mutate(stat = rep(c("save_percentage", "minutes", "GPS"), length(.$value)/3)) %>%
    mutate(goalie = rep(seq_len(length(.$value)/3), each = 3)) %>%
    spread(., stat, value) %>%
    mutate(weighted_GPS = GPS * minutes / sum(.$minutes)) %>%
    mutate(weighted_goalie_save_percentage = minutes * save_percentage / sum(minutes))

weighted_goalie_save_percentage <- sum(main$weighted_goalie_save_percentage, na.rm = TRUE)
weighted_GPS <- sum(main$weighted_GPS, na.rm = TRUE)

ifelse(return_goalie_save_percentage == TRUE, weighted_goalie_save_percentage, weighted_GPS)

}

calculate_record_over_time <- function(team_page) {
  
  #' Calculates quarterly records over time for a specific NHL team as provided in a given HTML page.
  #'
  #' @param team_page A HTML page saved resulting from the function grab_page_and_games_specific_team 
  #'
  #' @return A numeric vector that provides the cumulative records over time by quarter.
  #' @export
  #'
  
  year <- as.numeric(team_page$year)
  team_page <- team_page$games
  
if (year != 2013) {
  
  records_over_time <- team_page %>%
    html_nodes("tr:nth-child(86) .center+ .right , tr:nth-child(62) .center+ .right , tr:nth-child(41) .center+ .right , #games tr:nth-child(20) .center+ .right") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    as_tibble(.) %>%
    set_names("wins") %>%
    mutate(difference = ifelse(is.na(wins - lag(wins, 1)), wins, wins - lag(wins, 1))) %>%
    mutate(record = difference / 20)
  
  records_over_time$record[4] <- records_over_time$difference[4]/22
  
  } else {
  
    records_over_time <- team_page %>%
      html_nodes("tr:nth-child(50) .center+ .right , tr:nth-child(37) .center+ .right , tr:nth-child(25) .center+ .right , tr:nth-child(12) .center+ .right") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      as_tibble(.) %>%
      set_names("wins") %>%
      mutate(difference = ifelse(is.na(wins - lag(wins, 1)), wins , wins - lag(wins, 1))) %>%
      mutate(record = difference / 12)
  }
  
  records_over_time <- records_over_time %>% 
    select(record) %>%
    slice(., 1:4) %>%
    bind_cols(quarter = c("q1_record", "q2_record", "q3_record", "q4_record"), record = .) %>%
    spread(., key = quarter, value = record) %>%
    bind_cols(tibble(year = year), .)
  
  records_over_time
}

calculate_player_points <- function(team_page) {
  
  #' Grabs the total points for every player in a NHL team.
  #'
  #' @param team_page A HTML page saved resulting from the function grab_page_and_games_specific_team 
  #'
  #' @return A vector containing the player points for every player in a NHL team.
  #' @export
  #'
  
  
player_points <- team_page$page %>%
    html_nodes("#skaters tfoot .right:nth-child(8)") %>%
    html_text(.) %>%
    as.numeric(.)

player_points

}

get_team_names <- function(year) {
  
  #' Grabs all of the team names for a particular NHL season.
  #'
  #' @param year The year of the NHL regular season to pull data from.
  #'
  #' @return A tibble that contains the year of data and the team names that played in that regular season.
  #' @export
  #'
  
  rem_dr$navigate(paste("https://www.hockey-reference.com/leagues/NHL_", year, ".html", sep = ""))

  main <- read_html(rem_dr$getPageSource()[[1]]) %>%
  html_nodes("#stats tbody .left") %>%
  html_text(.) %>%
  str_remove(.,"[*]") %>%
  str_remove(., "[.]") %>%
  tibble(year = rep(year, length(.)), team = .)
  
}

find_match <- function(team1, team2, stat, data, highest_seed) {
  
  #' Finds the two relevant teams playing each other in the raw dataset provided by get_data_nst
  #'   and calculates the difference in a statistic from the perspective of the higher seed.
  #'
  #' @param team1 character string; a team competing against team2 in a particular NHL series
  #' @param team2 character string; a team competing against team1 in a particular NHL series
  #' @param stat character string; a column name found in the raw data given by the argument data to compute the differencing
  #' @param data the raw dataset provided by get_data_nst
  #' @param highest_seed character string; gives the highest seed among team1 or team2. The highest seed is defined as the team that starts the series at home.
  #'
  #' @return
  #' A numeric value that gives the difference in a statistic, from the higher seeds perspective.
  #'
  #' @export
  #'
  tmp <- unlist(c(data[, names(data) %in% c(stat)][which(data$team == team1),], data[, names(data) %in% c(stat)][which(data$team == team2),]))
  tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)] 
  
}

process_data <- function(team1, team2, highest_seed, year_of_play, data, start_col = 3L) {
  
  #' Processes the dataset for team1 and team2 for a particular dataset. 
  #' Starts processing at column 3 of data by default.
  #'
  #' @param team1 character string; a team competing against team2 in a particular NHL series
  #' @param team2 character string; a team competing against team1 in a particular NHL series
  #' @param stat character string; a column name found in the raw data given by the argument data to compute the differencing
  #' @param data the raw dataset provided by get_data_nst
  #' @param highest_seed character string; gives the highest seed among team1 or team2. The highest seed is defined as the team that starts the series at home.
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

get_winner <- function(pages, year, highest_seed, team1, team2) {
  
  #' Gets the winner of a particular series between team1 and team2.
  #'
  #' @param pages A list containing all of matchup (head to head) pages from hockey-reference for all NHL playoff series.
  #' @param year A numeric vector of length one that gives the year in which the series between team1 and team2 is played.
  #' @param highest_seed A character vector of length one that gives the highest seed between team1 and team2. The highest seed is defined as 
  #'  the team who starts the series at home.
  #' @param team1 A character vector of length one representing one of the two teams playing in a playoff series.
  #' @param team2 A character vector of length one representing one of the two teams playing in a playoff series. 
  #'
  #' @return A character vector with the value "W", if the highest seed won, and "L", if the highest seed lost.
  #' @export
  #'
  
  all_playoff_teams <- pages[[year - 2005]] %>% 
              html_nodes("#all_playoffs td:nth-child(3)") %>%
              html_text(.) %>%
              str_remove(., "[.]") %>%
              .[str_detect(., "over")] %>%
              .[str_detect(., team1) & str_detect(., team2)] %>%
              str_split_fixed(., "over", n = 2)
  
  if (nrow(all_playoff_teams) == 0) {
    NA
  } else {
    
    all_playoff_teams <- all_playoff_teams %>% 
      as_tibble(.) %>%
      set_names(c("winner", "loser")) %>%
      mutate(winner = str_trim(winner),
             loser = str_trim(loser)) 
    
    ifelse(any(all_playoff_teams$winner == highest_seed), "W", "L")
  }
}

get_past_number_of_games <- function(year) {
  
  #' Calculates the past number of games played in the previous series during a particular year of the NHL Playoffs.
  #' Returns NA for the first round (since there are no prior games played in this case).
  #'
  #' @param year A numeric vector of length one that contains an integer representing the year of the NHL Playoffs to obtain data from.
  #'
  #' @return A tibble containing the year, the teams involved in a series, and the number of games played in the prior series.
  #' @export
  #'
  
  main_page <- read_html(paste("https://www.hockey-reference.com/playoffs/NHL_", year, ".html", sep = ""))
  
  all_playoff_teams <- main_page %>% 
    html_nodes("#all_playoffs td:nth-child(3)") %>%
    html_text(.) %>%
    str_remove(., "[.]") %>%
    .[str_detect(., "over")] %>%
    as_tibble(.) %>%
    rename(teams = value) %>%
    mutate(teams = str_trim(teams, side = "right"))
  
  game_series <- main_page %>%
    html_nodes("#all_playoffs td:nth-child(2)") %>%
    html_text(.) %>%
    .[str_detect(., "-")] %>%
    str_split_fixed(., "-", n = 2) %>%
    as_tibble(.) %>%
    rename(winning_game_count = V1, losing_game_count = V2) %>%
    mutate(winning_game_count = as.numeric(winning_game_count)) %>%
    mutate(losing_game_count = as.numeric(losing_game_count)) %>%
    filter(winning_game_count == 4) %>%
    transmute(total_games_played = rowSums(.)) %>%
    bind_cols(year = rep(year, nrow(.)), all_playoff_teams, .)
  
  game_series
  
}

process_data_number_games <- function(team1, team2, round, highest_seed, year, data) {
  
  #' Title
  #'
  #' @param team1 
  #' @param team2 
  #' @param round 
  #' @param highest_seed 
  #' @param year 
  #' @param data 
  #'
  #' @return
  #' @export
  #'
  #' @examples
  
  if (round == "quarter-finals") {
    0
  } else {
  
  team1_games <- data %>% 
    filter(year == year) %>%
    .[str_detect(.$teams, team1), ]
  
  team2_games <- data %>% 
    filter(year == year) %>% 
    .[str_detect(.$teams, team2), ]
  
  if (which(c(team1, team2) == highest_seed) == 1) {
    
    highest_seed_games <- team1_games
    lowest_seed_games <- team2_games
    
  } else {
    
    highest_seed_games <- team2_games
    lowest_seed_games <- team1_games
    
  }
  
    if (round == "semi-finals") {
      
      highest_seed_games$total_games_played[nrow(highest_seed_games)] - lowest_seed_games$total_games_played[nrow(lowest_seed_games)]
      
    } else if (round == "finals") {
      
      highest_seed_games$total_games_played[nrow(highest_seed_games) - 1] - lowest_seed_games$total_games_played[nrow(lowest_seed_games) - 1]
      
    } else {
      
      highest_seed_games$total_games_played[nrow(highest_seed_games) - 2] - lowest_seed_games$total_games_played[nrow(lowest_seed_games) - 2]
      
    }
  
  }
  
}


all_past_number_of_games <- map_df(2006:2019, get_past_number_of_games)

past_number_of_games_processed <- tibble(past_series_game_count = pmap_dbl(
  list(template$Year, template$Team1, template$Team2, template$Round, template$Highest.Seed),
  ~process_data_number_games(..2, ..3, ..4, ..5, ..1, data = all_past_number_of_games)))

all_data <- map_df(2006:2019, get_team_names)
all_team_pages <- pmap(list(all_data$team, all_data$year), ~grab_page_and_games_specific_team(..1, ..2, lookup_accronyms = lookup_accronyms))
all_winners <- map(2006:2019, function(year) read_html(paste("https://www.hockey-reference.com/playoffs/NHL_",year, ".html", sep = "")))
give_winners <- pmap_chr(list(template$Year, template$Team1, template$Team2, template$Highest.Seed), ~get_winner(pages = all_winners, ..1, ..4, ..2, ..3))

rm(all_winners)

final <- tibble(weighted_goalie_save_percentage = map_dbl(all_team_pages, calculate_goalie_stats, return_goalie_save_percentage = TRUE),
                weighted_GPS = map_dbl(all_team_pages, calculate_goalie_stats, return_goalie_save_percentage = FALSE),
                player_points = map_dbl(all_team_pages, calculate_player_points)) %>%
            bind_cols(all_data, .) %>%
            mutate(player_points = ifelse(year == 2013, player_points/48, player_points/82))

records_over_time <- map_df(all_team_pages, calculate_record_over_time) %>%
                  bind_cols(team = all_data$team, .) %>%
                  mutate(sd_record = pmap_dbl(list(.$q1_record, .$q2_record, .$q3_record, .$q4_record), ~sd(c(..1, ..2, ..3, ..4, na.rm = TRUE))))

all_H2H_pages <- pmap(list(template$Year, template$Team1, template$Team2, template$Round, template$Conference), ~grab_page_of_matchup(..1, ..2, ..3, ..4, ..5))
give_H2H <- tibble(H2H = pmap_dbl(list(template$Highest.Seed, all_H2H_pages), ~calculate_H2H(..2, ..1, process = TRUE)))

rm(all_data, all_H2H_pages, all_team_pages)
gc()

all_stats <- pmap_dfr(
  list(template$Team1, template$Team2, template$Highest.Seed, template$Year),
  ~process_data(..1, ..2, ..3, ..4, data = final)) %>%
bind_cols(
  tibble(result_factor = give_winners, past_series_game_count = past_number_of_games_processed$past_series_game_count),
  pmap_dfr(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, ..4, data = records_over_time)),
  .,
  give_H2H)  

write_csv(final, "data/raw/2006-2019_hockey-reference_other.csv")
write_csv(give_H2H, "data/raw/2006-2019_hockey-reference_h2h.csv")
write_csv(records_over_time, "data/raw/2006-2019_hockey-reference_records-over-time.csv")

rm(records_over_time, final, give_H2H)

write_csv(all_stats, "data/processed/2006-2019_hockey-reference_other.csv")
