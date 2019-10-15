library(tidyverse)
library(lubridate)
library(rvest)

template <- read_csv("src/scraping/templates/template.csv") %>%
              mutate(Team1 = ifelse(Team1 == "St Louis Blues", "St. Louis Blues", Team1)) %>%
              mutate(Team2 = ifelse(Team2 == "St Louis Blues", "St. Louis Blues", Team2)) %>%
              mutate(Highest.Seed = ifelse(Highest.Seed == "St Louis Blues", "St. Louis Blues", Highest.Seed))

startdates <- read_csv("src/scraping/templates/time-related-features.csv") %>%
              mutate(Start = ymd(Start)) %>%
              mutate(End = ymd(End))
  

accronyms_pg <- read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")

accronyms <- accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)

fullnames <- accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_accronyms <- cbind(full_name = fullnames, accronym = accronyms) %>% as_tibble(.) %>% 
  bind_rows(., c(full_name = "Mighty Ducks of Anaheim", accronym = "MDA")) %>%
  bind_rows(., c(full_name = "St Louis Blues", accronym = "STL"))  

lookup_accronyms$accronym <- ifelse(lookup_accronyms$accronym == "VGK", "VEG", lookup_accronyms$accronym)                                                                       

rm(accronyms_pg, accronyms, fullnames)

#The function below uses the formula given by http://hockeyanalytics.com/2016/07/elo-ratings-for-the-nhl/ to actually calculate ELO ratings. This is quite complex and so refer 
#to the page for actual details.

calculate_M <- function(goal_difference_home, elo_home, elo_visitor) {
  
  M <- max(1,log(abs(goal_difference_home-0.0085*(elo_home - elo_visitor + 35)) + exp(1) -1))
  ifelse(is.na(M), stop(paste("List of Parameters:", goal_difference_home, elo_home, elo_visitor, sep = " ")), M)
}

calculate_EH <- function(elo_home, elo_visitor) {
  
  EH <- 1/(1 + 10^(-(elo_home - elo_visitor + 35) / 400))
  ifelse(is.na(EH), stop("NA"), EH)
}

get_data <- function(year_of_play, last_games = 0) {
  
  team_names <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_",year_of_play,"_standings.html", sep="")) %>%
                html_nodes("#standings td.left") %>%
                html_text(.)
  
  track_elo <- tibble(team = team_names) %>%
                bind_cols(., elo_rating = rep(1500, nrow(.)), year = rep(year_of_play, nrow(.)))
  
  rm(team_names)
  
  schedule_and_results <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_",year_of_play,"_games.html", sep="")) 
          
  visitor <- schedule_and_results %>%
                html_nodes("#games .left+ td.left") %>%
                html_text(.) 
  
  data <- schedule_and_results %>%
                html_nodes("#games td:nth-child(4)") %>%
                html_text(.) %>%
                bind_cols(visitor = visitor, home = .)
  
  rm(visitor)
  
  data <- schedule_and_results %>%
                html_nodes("#games .right:nth-child(3)") %>%
                html_text(.) %>%
                as.numeric(.) %>%
                bind_cols(data, goals_visitor = .)
  
  data <- schedule_and_results %>%
                html_nodes("#games .right:nth-child(5)") %>%
                html_text(.) %>%
                as.numeric(.) %>%
                bind_cols(data, goals_home = .) 
  
  data <- schedule_and_results %>%
                html_nodes("#games td.center") %>%
                html_text(.) %>%
                bind_cols(data, SO_or_OT_indicator = .) %>%
                mutate(outcome_home = ifelse(SO_or_OT_indicator == "SO", 0.5, 
                                      ifelse(goals_home > goals_visitor, 1,0))) %>%
                filter(.,!is.na(goals_visitor))
  
constant <- ifelse(last_games == 0, 1, nrow(data)-last_games)
  
  for (i in constant:nrow(data)) {
    
    visitor_elo <- track_elo$elo_rating[which(track_elo$team == data$visitor[i])]
    home_elo <- track_elo$elo_rating[which(track_elo$team == data$home[i])]
    goal_difference <- data$goals_home[i] - data$goals_visitor[i]
    
    elo_change <- 8 * 1 * calculate_M(
      goal_difference_home = goal_difference,
      elo_home = home_elo,
      elo_visitor = visitor_elo) * (data$outcome_home[i] - calculate_EH(elo_home = home_elo, elo_visitor = visitor_elo))

    if (is.na(elo_change)) {
      paste("Iteration:", i, sep = " ")
    }
    track_elo$elo_rating[which(track_elo$team == data$visitor[i])] <- visitor_elo + -elo_change
    track_elo$elo_rating[which(track_elo$team == data$home[i])] <- home_elo + elo_change
  }
  
  rm(i)
  
  track_elo
  
}

get_data_playoffs <- function(year_of_play, elo_end_of_regularseason, round_end_dates) {
  
  page <- read_html(paste("https://www.hockey-reference.com/leagues/NHL_",year_of_play,"_games.html", sep=""))
  
  dates <- page %>%
    html_nodes("#games_playoffs .left:nth-child(1)") %>%
    html_text(.) %>%
    .[-str_detect(., "Date")] %>%
    ymd(.)
  
  visitor <- page %>%
    html_nodes("#games_playoffs .left+ .left a") %>%
    html_text(.) 
  
  home <- page %>%
    html_nodes("#games_playoffs td~ .left a") %>%
    html_text(.) 
  
  visitor_score <- page %>%
    html_nodes("#games_playoffs .right:nth-child(3)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    .[!is.na(.)]
  
  home_score <- page %>%
    html_nodes("#games_playoffs .right~ .left+ .right") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    .[!is.na(.)]
  
  all_data <- tibble(
    date = dates[1:length(home_score)],
    visitor = visitor[1:length(home_score)],
    home = home[1:length(home_score)], 
    goals_visitor = visitor_score,
    goals_home = home_score) %>%
  mutate(outcome_home = ifelse(goals_home > goals_visitor, 1,0))
  
  end_dates <- round_end_dates %>% filter(Year == year_of_play)
  
  if (nrow(end_dates) == 1) {
    
  semis <- max(which(all_data$date <= end_dates$End[1]))
  finals <- NULL
  scf <- NULL
  
  } else if (nrow(end_dates) == 2) {
    
  semis <- max(which(all_data$date <= end_dates$End[1]))
  finals <- max(which(all_data$date <= end_dates$End[2]))
  scf <- NULL
  
  } else {
    
  semis <- max(which(all_data$date <= end_dates$End[1]))
  finals <- max(which(all_data$date <= end_dates$End[2]))
  scf <- max(which(all_data$date <= end_dates$End[3])) 
  
  }
  
  elo_ratings_playoffs <- vector("list", 3)
  track_elo <- elo_end_of_regularseason %>% filter(year == year_of_play)
  
  for (i in 1:nrow(all_data)) {
    
    visitor_elo <- track_elo$elo_rating[which(track_elo$team == all_data$visitor[i])]
    home_elo <- track_elo$elo_rating[which(track_elo$team == all_data$home[i])]
    goal_difference <- all_data$goals_home[i] - all_data$goals_visitor[i]
    
    elo_change <- 8 * 1.5 * calculate_M(goal_difference_home = goal_difference, elo_home = home_elo, elo_visitor = visitor_elo) * (all_data$outcome_home[i] - calculate_EH(elo_home = home_elo, elo_visitor = visitor_elo))
    track_elo$elo_rating[which(track_elo$team == all_data$visitor[i])] <- visitor_elo + -elo_change
    track_elo$elo_rating[which(track_elo$team == all_data$home[i])] <- home_elo + elo_change
    
    if (i == semis) {
      elo_ratings_playoffs[[1]] <- bind_cols(track_elo, up_to_start_of_round = rep("semis", nrow(track_elo)))
    } else if (i == finals && !is.null(finals)) {
      elo_ratings_playoffs[[2]] <- bind_cols(track_elo, up_to_start_of_round = rep("finals", nrow(track_elo)))
    } else if (i == scf && !is.null(scf)) {
      elo_ratings_playoffs[[3]] <- bind_cols(track_elo, up_to_start_of_round = rep("stanley-cup", nrow(track_elo)))
    }
  }
  
  abc <- bind_rows(elo_ratings_playoffs)
  
}

data_delay <- map_df(seq(2006, 2019, 1), get_data, last_games = 400)
data <- map_df(seq(2006, 2019, 1), get_data, last_games = 0)
data_playoffs <- map_df(seq(2008, 2019, 1), get_data_playoffs, elo_end_of_regularseason = data, round_end_dates = startdates)

write_csv(data_delay, "data/raw/2006-2019_elo-ratings_last400.csv")
write_csv(data, "data/raw/2006-2019_elo-ratings_all-games.csv")
write_csv(data, "data/raw/2006-2019_elo-ratings_playoffs.csv")

process_data <- function(team1, team2, highest_seed, data, year_of_play) {
  
  data <- data %>% filter(., year == year_of_play)
  
  team_elo <- c(data$elo_rating[which(data$team == team1)], data$elo_rating[which(data$team == team2)])

  as.numeric(team_elo[which(c(team1, team2) == highest_seed)] - team_elo[which(c(team1, team2) != highest_seed)]) 
  
}

process_data_playoffs <- function(team1, team2, highest_seed, data, year_of_play, round) {
  
  if (round == "quarter-finals") {
    NA
  } else if(round == "semi-finals") {
    
    data <- data %>% filter(., year == year_of_play, up_to_start_of_round == "semis")
    team_elo <- c(data$elo_rating[which(data$team == team1)], data$elo_rating[which(data$team == team2)])
    as.numeric(team_elo[which(c(team1, team2) == highest_seed)] - team_elo[which(c(team1, team2) != highest_seed)]) 
    
  } else if(round == "finals") {
    
    data <- data %>% filter(., year == year_of_play, up_to_start_of_round == "finals")
    team_elo <- c(data$elo_rating[which(data$team == team1)], data$elo_rating[which(data$team == team2)])
    as.numeric(team_elo[which(c(team1, team2) == highest_seed)] - team_elo[which(c(team1, team2) != highest_seed)]) 
    
  } else {
    
    data <- data %>% filter(., year == year_of_play, up_to_start_of_round == "stanley-cup")
    team_elo <- c(data$elo_rating[which(data$team == team1)], data$elo_rating[which(data$team == team2)])
    as.numeric(team_elo[which(c(team1, team2) == highest_seed)] - team_elo[which(c(team1, team2) != highest_seed)]) 
  
    }
}

final <- tibble(elo_rating = pmap_dbl(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, data = data, ..4))) %>%
  bind_cols(., tibble(elo_rating_q4 = pmap_dbl(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, data = data_delay, ..4)))) %>%
  bind_cols(., tibble(elo_rating_playoffs = pmap_dbl(list(template$Team1, template$Team2, template$Highest.Seed, template$Year, template$Round), ~process_data_playoffs(..1, ..2, ..3, data = data_playoffs, ..4, ..5)))) %>%
  mutate(elo_rating_playoffs = ifelse(is.na(elo_rating_playoffs), elo_rating, elo_rating_playoffs))

write_csv(final, "data/processed/2006-2019_elo-ratings.csv")