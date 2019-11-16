library(tidyverse)
library(rvest)

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
  bind_rows(., c(full_name = "Mighty Ducks of Anaheim", accronym = "MDA")) 

rm(accronyms_pg, accronyms, full_names)

get_data_ESPN <- function(year) {
  
  #' Grabs data from ESPN. Aggregates the data down so that one team = one row.
  #'
  #' @param year A numeric vector of length one that gives the year of a particular NHL regular season.
  #'
  #' @return A tibble containing the year of play and the teams aggregated WAR for all teams in the regular season.
  #' @export
  #'
  
  main_page <- read_html(paste("http://www.espn.com/nhl/stats/rpi/_/season/", year, sep=""))
  secondary_page <- read_html(paste("http://www.espn.com/nhl/statistics/team/_/stat/special-teams/sort/powerPlayPct/year/",year,"/seasontype/2/split/142", sep = ""))

  teams <- main_page %>%
    html_nodes("td:nth-child(2)") %>%
    html_text(.) %>%
    .[2:length(.)] %>%
    tibble(team = .) %>%
    mutate(team = ifelse(team == "Detroit", "Detroit Red Wings",team)) %>%
    mutate(team = ifelse(team == "Ottawa", "Ottawa Senators",team)) %>%
    mutate(team = ifelse(team == "Dallas", "Dallas Stars",team)) %>%
    mutate(team = ifelse(team == "Buffalo", "Buffalo Sabres",team)) %>%
    mutate(team = ifelse(team == "Carolina", "Carolina Hurricanes",team)) %>%
    mutate(team = ifelse(team == "Calgary", "Calgary Flames",team)) %>%
    mutate(team = ifelse(team == "Atlanta", "Atlanta Thrashers", team)) %>%
    mutate(team = ifelse(team == "Nashville", "Nashville Predators", team)) %>%
    mutate(team = ifelse(team == "Anaheim" & year <= 2006, "Mighty Ducks of Anaheim", team)) %>%
    mutate(team = ifelse(team == "Anaheim", "Anaheim Ducks",team)) %>%
    mutate(team = ifelse(team == "San Jose", "San Jose Sharks", team)) %>%
    mutate(team = ifelse(team == "Colorado", "Colorado Avalanche",team)) %>%
    mutate(team = ifelse(team == "Philadelphia", "Philadelphia Flyers",team)) %>%
    mutate(team = ifelse(team == "Montreal", "Montreal Canadiens", team)) %>%
    mutate(team = ifelse(team == "New Jersey", "New Jersey Devils",team)) %>%
    mutate(team = ifelse(team == "Edmonton", "Edmonton Oilers",team)) %>%
    mutate(team = ifelse(team == "NY Rangers", "New York Rangers",team)) %>%
    mutate(team = ifelse(team == "Vancouver", "Vancouver Canucks",team)) %>%
    mutate(team = ifelse(team == "Los Angeles", "Los Angeles Kings",team)) %>%
    mutate(team = ifelse(team == "Toronto", "Toronto Maple Leafs",team)) %>%
    mutate(team = ifelse(team == "Tampa Bay", "Tampa Bay Lightning",team)) %>%
    mutate(team = ifelse(team == "Winnipeg" & year <= 2011, "Atlanta Thrashers",team)) %>%
    mutate(team = ifelse(team == "Minnesota", "Minnesota Wild",team)) %>%
    mutate(team = ifelse(team == "Arizona" & year <= 2014, "Phoenix Coyotes",team)) %>%
    mutate(team = ifelse(team == "Florida", "Florida Panthers",team)) %>%
    mutate(team = ifelse(team == "Boston", "Boston Bruins",team)) %>%
    mutate(team = ifelse(team == "NY Islanders", "New York Islanders",team)) %>%
    mutate(team = ifelse(team == "Columbus", "Columbus Blue Jackets",team)) %>%
    mutate(team = ifelse(team == "Washington", "Washington Capitals",team)) %>%
    mutate(team = ifelse(team == "Chicago", "Chicago Blackhawks",team)) %>%
    mutate(team = ifelse(team == "Pittsburgh", "Pittsburgh Penguins",team)) %>%
    mutate(team = ifelse(team == "St. Louis", "St Louis Blues",team)) %>%
    mutate(team = ifelse(team == "Vegas", "Vegas Golden Knights", team)) %>%
    mutate(team = ifelse(team == "Arizona", "Arizona Coyotes", team)) %>%
    mutate(team = ifelse(team == "Winnipeg", "Winnipeg Jets", team))
  
  teams2 <- secondary_page %>%
    html_nodes("td:nth-child(2)") %>%
    html_text(.) %>%
    .[. != "TEAM"] %>%
    tibble(team = .) %>%
    mutate(team = ifelse(team == "Detroit", "Detroit Red Wings",team)) %>%
    mutate(team = ifelse(team == "Ottawa", "Ottawa Senators",team)) %>%
    mutate(team = ifelse(team == "Dallas", "Dallas Stars",team)) %>%
    mutate(team = ifelse(team == "Atlanta", "Atlanta Thrashers", team)) %>%
    mutate(team = ifelse(team == "Buffalo", "Buffalo Sabres",team)) %>%
    mutate(team = ifelse(team == "Carolina", "Carolina Hurricanes",team)) %>%
    mutate(team = ifelse(team == "Calgary", "Calgary Flames",team)) %>%
    mutate(team = ifelse(team == "Nashville", "Nashville Predators", team)) %>%
    mutate(team = ifelse(team == "Anaheim" & year <= 2006, "Mighty Ducks of Anaheim", team)) %>%
    mutate(team = ifelse(team == "Anaheim", "Anaheim Ducks",team)) %>%
    mutate(team = ifelse(team == "San Jose", "San Jose Sharks", team)) %>%
    mutate(team = ifelse(team == "Colorado", "Colorado Avalanche",team)) %>%
    mutate(team = ifelse(team == "Philadelphia", "Philadelphia Flyers",team)) %>%
    mutate(team = ifelse(team == "Montreal", "Montreal Canadiens", team)) %>%
    mutate(team = ifelse(team == "New Jersey", "New Jersey Devils",team)) %>%
    mutate(team = ifelse(team == "Edmonton", "Edmonton Oilers",team)) %>%
    mutate(team = ifelse(team == "NY Rangers", "New York Rangers",team)) %>%
    mutate(team = ifelse(team == "Vancouver", "Vancouver Canucks",team)) %>%
    mutate(team = ifelse(team == "Los Angeles", "Los Angeles Kings",team)) %>%
    mutate(team = ifelse(team == "Toronto", "Toronto Maple Leafs",team)) %>%
    mutate(team = ifelse(team == "Tampa Bay", "Tampa Bay Lightning",team)) %>%
    mutate(team = ifelse(team == "Winnipeg" & year <= 2011, "Atlanta Thrashers",team)) %>%
    mutate(team = ifelse(team == "Minnesota", "Minnesota Wild",team)) %>%
    mutate(team = ifelse(team == "Arizona" & year <= 2014, "Phoenix Coyotes",team)) %>%
    mutate(team = ifelse(team == "Florida", "Florida Panthers",team)) %>%
    mutate(team = ifelse(team == "Boston", "Boston Bruins",team)) %>%
    mutate(team = ifelse(team == "NY Islanders", "New York Islanders",team)) %>%
    mutate(team = ifelse(team == "Columbus", "Columbus Blue Jackets",team)) %>%
    mutate(team = ifelse(team == "Washington", "Washington Capitals",team)) %>%
    mutate(team = ifelse(team == "Chicago", "Chicago Blackhawks",team)) %>%
    mutate(team = ifelse(team == "Pittsburgh", "Pittsburgh Penguins",team)) %>%
    mutate(team = ifelse(team == "St. Louis", "St Louis Blues",team)) %>%
    mutate(team = ifelse(team == "Vegas", "Vegas Golden Knights", team)) %>%
    mutate(team = ifelse(team == "Arizona", "Arizona Coyotes", team)) %>%
    mutate(team = ifelse(team == "Winnipeg", "Winnipeg Jets", team))
    
  rpi <- main_page %>% 
    html_nodes(".sortcell") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(rpi = .) 
  
  penaltykill_post_allstar <- secondary_page %>% 
    html_nodes("td:nth-child(10)") %>%
    html_text(.) %>%
    .[!is.na(.)] %>%
    .[. != "PCT"] %>% 
    as.numeric(.) %>%
    bind_cols(teams2, penaltykill_post_allstar = .)

  bind_cols(tibble(year = rep(year, nrow(teams))), teams, rpi) %>% 
    left_join(., penaltykill_post_allstar, by = "team") %>%
    mutate(penaltykill_post_allstar = ifelse(penaltykill_post_allstar == 0, NA, penaltykill_post_allstar))
  
}

process_data <- function(team1, team2, highest_seed, data, year_of_play) {
  
  #' Processes the data resulting from a call to the function get_data_ESPN
  #'
  #' @param team1 A character vector of length one that is one of the two teams playing in a series, playing against team2.
  #' @param team2 A character vector of length one that is one of the two teams playing in a series, playing against team1.
  #' @param highest_seed A character vector of length one that is the highest seed between team1 and team2. The highest seed
  #'  is defined as the team that starts the series at home.
  #' @param data A tibble of data resulting from a call to the function get_data_ESPN 
  #' @param year_of_play The year of NHL playoffs that the series between team1 and team2 is played.
  #'
  #' @return A list containing the difference in statistics between the higher seed of team1 and team2 and the other team.
  #' @export
  #'
  
  data <- data %>% filter(., year == year_of_play)
  
  team_rpi <- c(data$rpi[which(data$team == team1)], data$rpi[which(data$team == team2)])
  team_pk <- c(data$penaltykill_post_allstar[which(data$team == team1)], data$penaltykill_post_allstar[which(data$team == team2)])
  
  list(rpi = as.numeric(team_rpi[which(c(team1, team2) == highest_seed)] - team_rpi[which(c(team1, team2) != highest_seed)]),
       penaltykill_post_allstar = as.numeric(team_pk[which(c(team1, team2) == highest_seed)] - team_pk[which(c(team1, team2) != highest_seed)]))
}

all_years <- map_df(seq(2006, 2019,1), get_data_ESPN)
all_years$penaltykill_post_allstar <- ifelse(all_years$penaltykill_post_allstar == 0, NA, all_years$penaltykill_post_allstar)

write_csv(all_years, "data/raw/2006-2019_espn_raw.csv")

final <- pmap_dfr(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, data = all_years, ..4))

write_csv(final, "data/processed/2006-2019_espn_stats.csv")