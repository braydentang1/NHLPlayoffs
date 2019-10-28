library(tidyverse)
library(rvest)
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

#Create the rsDriver for Selenium

rem_dr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
rem_dr$open()

get_data_nhl_hitsandblocks <- function(year) {
  
  #' Pulls data from the NHL official page, namely, hits and blocks during the regular season.
  #'
  #' @param year an integer: the year of NHL Playoffs to pull data from.
  #'
  #' @return
  #' A tibble that contains stats on Hits and Blocks for all teams during a particular NHL regular season.
  #'
  #' @export
  #'
  
  rem_dr$navigate(paste(
    "http://www.nhl.com/stats/team?report=realtime&reportType=season&seasonFrom=", 
    year - 1, 
    year, 
    "&seasonTo=", 
    year - 1, 
    year, 
    "&gameType=2&filter=gamesPlayed,gte,1&sort=hits",
    sep = ""))
  
  main_page <- read_html(rem_dr$getPageSource()[[1]])
  
  team_name <- main_page %>%
    html_nodes(".rt-td:nth-child(2)") %>%
    html_text(.) %>%
    gsub("é", "e",.) %>%
    gsub("\\.", "",.) 
  
  hits <- main_page %>%
    html_nodes(".rt-td:nth-child(10)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  blocks <- main_page %>%
    html_nodes(".rt-td:nth-child(11)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  faceoff_win_percentage <- main_page %>%
    html_nodes(".rt-td:nth-child(18)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  give_aways <- main_page %>%
    html_nodes(".rt-td:nth-child(13)") %>%
    html_text(.) %>%
    as.numeric(.) 
  
  take_aways <- main_page %>%
    html_nodes(".rt-td:nth-child(14)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  if (year == 2013) {
    hits <- hits / 48
    blocks <- blocks / 48
    give_aways <- give_aways / 48
    take_aways <- take_aways / 48
  } else {
    hits <- hits / 82
    blocks <- blocks / 82
    give_aways <- give_aways / 82
    take_aways <- take_aways / 82
  }
  
  tibble(
    year = rep(year, length(team_name)), 
    team = team_name,
    blocks_at_ES = blocks, 
    hits_at_ES = hits,
    faceoff_win_percentage = faceoff_win_percentage, 
    give_aways = give_aways,
    take_aways = take_aways) %>%
  mutate(team = ifelse(team == "Anaheim Ducks" & year <= 2006, "Mighty Ducks of Anaheim", team))
  
}

get_data_nhl_leading_and_trailing <- function(year) {
  
  #' Pulls data from the NHL official page, namely, number of times a team is leading or
  #'  trailing during particular periods of any game during the regular season.
  #'
  #' @param year an integer: the year of NHL Playoffs to pull data from.
  #'
  #' @return
  #' A tibble that contains leading and trailing stats for a particular year of the NHL regular season.
  #'
  #' @export
  #'
  
  rem_dr$navigate(paste(
    "http://www.nhl.com/stats/team?report=leadingtrailing&reportType=season&seasonFrom=",
    year - 1, 
    year,
    "&seasonTo=",
    year - 1,
    year,
    "&gameType=2&filter=gamesPlayed,gte,1&sort=winsAfterLead1p", 
    sep = ""))
  
  main_page <- read_html(rem_dr$getPageSource()[[1]])
  
  main_page <- read_html(rem_dr$getPageSource()[[1]])
  
  team_name <- main_page %>%
    html_nodes(".rt-td:nth-child(2)") %>%
    html_text(.) %>%
    gsub("é", "e",.) %>%
    gsub("\\.", "",.) 
  
  win_percent_lead_1P <- main_page %>%
    html_nodes(".rt-td:nth-child(13)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  win_percent_lead_2P <- main_page %>%
    html_nodes(".rt-td:nth-child(17)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  win_percent_trail_1P <- main_page %>%
    html_nodes(".rt-td:nth-child(21)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  win_percent_trail_2P <- main_page %>%
    html_nodes(".rt-td:nth-child(25)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  ot_losses_lead_1P <- main_page %>%
    html_nodes(".rt-td:nth-child(12)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  ot_losses_lead_2P <- main_page %>%
    html_nodes(".rt-td:nth-child(16)") %>%
    html_text(.) %>%
    as.numeric(.)
  
  if (year == 2013) {
    ot_losses_lead_1P <- ot_losses_lead_1P / 48
    ot_losses_lead_2P <- ot_losses_lead_2P / 48
  } else {
    ot_losses_lead_1P <- ot_losses_lead_1P / 82
    ot_losses_lead_2P <- ot_losses_lead_2P / 82
  }
  
  tibble(year = rep(year, length(team_name)),
         team = team_name,
         win_percent_lead_1P = win_percent_lead_1P,
         win_percent_lead_2P = win_percent_lead_2P, 
         win_percent_trail_1P = win_percent_trail_1P, 
         win_percent_trail_2P = win_percent_trail_2P,
         ot_losses_lead_1P = ot_losses_lead_1P,
         ot_losses_lead_2P = ot_losses_lead_2P) %>%
  mutate(team = ifelse(team == "Anaheim Ducks" & year <= 2006, "Mighty Ducks of Anaheim", team))
  
}

find_match <- function(team1, team2, stat, data, highest_seed) {
  
  #' Finds the two relevant teams playing each other in the raw dataset provided by
  #' get_data_nhl_hitsandblocks or get_data_nhl_leading_and_trailing,
  #' and calculates the difference in a statistic from the perspective of the higher seed.
  #'
  #' @param team1 character string; a team competing against team2 in a particular NHL series
  #' @param team2 character string; a team competing against team1 in a particular NHL series
  #' @param stat character string; a column name found in the raw data given by the argument data to compute the differencing
  #' @param data the raw dataset provided by get_data_nhl_hitsandblocks or get_data_nhl_leading_and_trailing
  #' @param highest_seed character string; gives the highest seed among team1 or team2. 
  #' The highest seed is defined as the team that starts the series at home.
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
  
  #' Processes the dataset for team1 and team2 for a particular dataset. 
  #' Starts processing at column 3 of data by default.
  #'
  #' @param team1 character string; a team competing against team2 in a particular NHL series
  #' @param team2 character string; a team competing against team1 in a particular NHL series
  #' @param stat character string; a column name found in the raw data given by the argument data to compute the differencing
  #' @param data the raw dataset provided by get_data_nhl_hitsandblocks or get_data_nhl_leading_and_trailing
  #' @param highest_seed character string; gives the highest seed among team1 or team2.
  #'  The highest seed is defined as the team that starts the series at home.
  #' @param start_col a vector of length one that gives the starting column index to start processing from. 
  #' All columns from the given column index and onwards are processed. Default = 3L.
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

all_data <- map_df(2006:2019, get_data_nhl_hitsandblocks) %>% 
  left_join(., map_df(2006:2019, get_data_nhl_leading_and_trailing), by = c("year", "team")) %>%
  write_csv(., "data/raw/2006-2019_nhl-official_raw.csv")

final <- pmap_dfr(list(template$Year, template$Team1, template$Team2, template$Highest.Seed), ~process_data(..2, ..3, ..4, ..1, data = all_data))
write_csv(final, "data/processed/2006-2019_nhl-official.csv")