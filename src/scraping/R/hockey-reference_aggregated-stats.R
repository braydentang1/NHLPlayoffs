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

lookup_accronyms$accronym <- ifelse(
  lookup_accronyms$accronym == "VGK", "VEG", lookup_accronyms$accronym)                                                                       

rm(accronyms_pg, accronyms, full_names)

rem_dr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
rem_dr$open()

get_data <- function(year) {
  
  rem_dr$navigate(paste("https://www.hockey-reference.com/leagues/NHL_",year,".html", sep = ""))
  main <- read_html(rem_dr$getPageSource()[[1]])
  
  team_name <- main %>%
              html_nodes("#stats tbody .left") %>%
              html_text(.) %>%
              str_replace(.,"[*]", "") %>%
              str_replace(., "[.]", "") %>%
              tibble(team = .)
  
  average_age <- main %>%
              html_nodes("#stats tbody .left+ .right") %>%
              html_text(.) %>%
              as.numeric(.) %>%
              tibble(average_age = .)
  
  games_played <- main %>%
    html_nodes("#stats tbody .right:nth-child(4)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(games_played = .) 
  
  wins <- main %>%
    html_nodes("#stats tbody .right:nth-child(5)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(wins = .) %>%
    transmute(regular_season_winpercentage = wins/games_played$games_played)
  
  otl <- main %>%
    html_nodes("#stats tbody .right:nth-child(7)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(otl = .) %>%
    mutate(otl = case_when(year == 2013 ~ otl/48, year != 2013 ~ otl/82))
  
  points <- main %>%
    html_nodes("#stats tbody .right:nth-child(8)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(points = .) %>%
    mutate(points = case_when(year == 2013 ~ points/48, year != 2013 ~ points/82))
  
  points_percentage <- main %>%
    html_nodes("#stats tbody .right:nth-child(9)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(points_percentage_ofmax = .) 
  
  goals_for <- main %>%
    html_nodes("#stats tbody .right:nth-child(10)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(goals_for = .) %>%
    mutate(goals_for = case_when(year == 2013 ~ goals_for/48, year != 2013 ~ goals_for/82))
  
  goals_against <- main %>%
    html_nodes("#stats tbody .right:nth-child(11)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(goals_against = .) %>%
    mutate(goals_against = case_when(year == 2013 ~ goals_against/48, year != 2013 ~ goals_against/82))
  
  goal_differential <- tibble(goal_diff = goals_for$goals_for - goals_against$goals_against)
  
  srs <- main %>%
    html_nodes("#stats tbody .right:nth-child(14)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(srs = .)
  
  sos <- main %>%
    html_nodes("#stats tbody .right:nth-child(15)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(sos = .)
  
  penalty_mins_pg <- main %>%
    html_nodes("tbody .right:nth-child(27)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(penalty_mins_pg = .)
  
  powerplay_goals <- main %>%
    html_nodes("#stats tbody .right:nth-child(19)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(powerplay_goals = .) %>%
    mutate(powerplay_goals = case_when(year == 2013 ~ powerplay_goals/48, year != 2013 ~ powerplay_goals/82))
  
  powerplay_oppurtunities <- main %>%
    html_nodes("#stats tbody .right:nth-child(20)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(powerplay_oppurtunities = .) %>%
    mutate(powerplay_oppurtunities = case_when(year == 2013 ~ powerplay_oppurtunities/48, year != 2013 ~ powerplay_oppurtunities/82))
  
  powerplay_percentage <- main %>%
    html_nodes("#stats tbody .right:nth-child(21)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(powerplay_percentage = .)
  
  penaltykill_percentage <- main %>%
    html_nodes("tbody .right:nth-child(24)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(penaltykill_percentage = .)
  
  sog <- main %>%
    html_nodes("tbody .right:nth-child(29)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(sog =.) %>%
    mutate(sog = case_when(year == 2013 ~ sog/48, year != 2013 ~ sog/82))
  
  shot_percentage <- main %>%
    html_nodes("tbody .right:nth-child(30)") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(shot_percentage = .)
  
  bind_cols(tibble(
    year = as.numeric(rep(year, nrow(team_name)))),
    team_name, 
    average_age, 
    wins,
    otl, 
    points, 
    points_percentage, 
    goals_for, 
    goals_against, 
    goal_differential, 
    srs, 
    sos, 
    penalty_mins_pg, 
    powerplay_goals,
    powerplay_oppurtunities, 
    powerplay_percentage, 
    penaltykill_percentage, 
    sog, 
    shot_percentage)
}

find_match <- function(team1, team2, stat, data, highest_seed) {
  tmp <- unlist(c(data[, names(data) %in% c(stat)][which(data$team == team1),], data[, names(data) %in% c(stat)][which(data$team == team2),]))
  tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)] 
}
  
process_data = function(team1, team2, highest_seed, year_of_play, data) {
  
  data <- data %>% filter(., year == year_of_play)
  
  team_vec <- as_tibble(unlist(lapply(colnames(data)[3:ncol(data)], FUN = find_match, team1 = team1, team2 = team2, data = data, highest_seed = highest_seed))) %>%
                  rownames_to_column(.) %>%
                  mutate(rowname = colnames(data)[3:ncol(data)]) %>%
                  spread(rowname, value) 

}

all_data <- map_df(2006:2019, get_data)
write_csv(all_data, "data/raw/2006-2019_hockey-reference_aggregated_raw.csv")

teams <- template %>%
  transmute(series = paste(Team1, "vs.", Team2, sep = " "))

final <- bind_cols(teams, pmap_dfr(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, ..4, data = all_data)))

rm(teams)

write_csv(final, "data/processed/2006-2019_hockey-reference_aggregated.csv")