library(tidyverse)
library(rvest)
library(RSelenium)
library(testthat)

rem_dr <- remoteDriver(remoteServerAddr = "localhost", port = 4445L, browserName = "chrome")
rem_dr$open()

rem_dr2 <- remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "chrome")
rem_dr2$open()

get_data <- function(year) {

  #' Gets the historical odds from OddsPortal for every game during the playoffs of a particular year.
  #'
  #' @param year an integer; the desired year of playoff data to pull odds from
  #'
  #' @return
  #' A tibble that provides the odds for every game during the NHL playoffs.
  #'
  
rem_dr$navigate(paste("https://www.oddsportal.com/hockey/usa/nhl-", year - 1, "-", year, "/results/#/", sep = ""))  
Sys.sleep(10)
main <- read_html(rem_dr$getPageSource()[[1]])

#I tried navigating on the page, but it appears that it doesn't work (it keeps parsing only the first page, not the second)
rem_dr2$navigate(paste("https://www.oddsportal.com/hockey/usa/nhl-", year-1, "-", year, "/results/#/page/2/", sep = ""))  
Sys.sleep(10)
main2 <- read_html(rem_dr2$getPageSource()[[1]])

teams <- main %>% 
          html_nodes(".table-participant") %>%
          html_text(.) %>%
          tibble(teams = .) 

teams2 <- main2 %>%
          html_nodes(".table-participant") %>%
          html_text(.) %>%
          tibble(teams = .)

teams_fin <- bind_rows(teams, teams2)

playoff_indicator <- main2 %>%
          html_nodes(".ico-event-info") %>%
          html_text(.) 

playoff_indicator <- c(playoff_indicator, rep(NA, nrow(teams2) - length(playoff_indicator))) 
playoff_indicator <- c(rep(1, nrow(teams)),playoff_indicator)

rm(teams, teams2)

playoff_indicator <- ifelse(!is.na(playoff_indicator), 1,0) %>% tibble(playoff_indicator = .)

odds <- main %>%
          html_nodes(".table-score+ .odds-nowrp :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(odds_highest_seed = .)

odds2 <- main2 %>%
          html_nodes(".table-score+ .odds-nowrp :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(odds_highest_seed =.)

odds_highest_fin <- bind_rows(odds, odds2)

odds <- main %>% 
          html_nodes(".odds-nowrp:nth-child(6) :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(odds_lowest_seed = .)

odds2 <- main2 %>%
          html_nodes(".odds-nowrp:nth-child(6) :nth-child(1)") %>%
          html_text(.) %>%
          as.numeric(.) %>%
          tibble(odds_lowest_seed = .)

odds_lowest_fin <- bind_rows(odds, odds2)

rm(odds, odds2)

combined <- bind_cols(
  tibble(year = rep(year, nrow(playoff_indicator))),
  teams_fin,
  playoff_indicator, 
  odds_highest_fin, 
  odds_lowest_fin) %>%
filter(playoff_indicator != 0)

combined %>% select(-playoff_indicator)
          
}

process_data <- function(year_of_play, team1, team2, data) {
  
  #' Processes the raw dataset resulting from a call to the function get_data
  #'
  #' @param year an integer: playoff year of the particular series between team1 and team2 
  #' @param team1 a character string of a team in a playoff series that is playing against team2
  #' @param team2 a character string of a team in a playoff series that is playing against team1
  #' @param data the raw dataset resulting from the function get_data
  #'
  #' @return
  #' A tibble that provides the odds for every game during the NHL playoffs.
  #'
  #' @export
  #'
  
  data <- data %>% filter(year == year_of_play)
  
  string1 <- paste(team1, "-", team2, sep = " ")
  string2 <- paste(team2, "-", team1, sep = " ")
  
  first_game <- data[max(which(grepl(paste(c(string1, string2), collapse = "|"), data$teams))), ]
  
  tibble(vegas_odds = first_game$odds_highest_seed - first_game$odds_lowest_seed)
  
}

# Kind of a hacky way to do this
# Set the round argument to what is needed. Page 1 = quarters, page 2 = semis, page 3 = finals,
# page 4= stanley cup.

get_data_current <- function(year, round) {
  
  #' Grabs data from the current year. OddsPortal has a separate page for NHL playoff games that are upcoming. This function requires
  #' that a saved copy of the HTML page is saved in some directory.
  #'
  #' @param year an integer: current playoff year
  #' @param round a character string: one of "quarter-finals", "semi-finals", "finals" or "stanley-cup-final"
  #' 
  #' @return
  #' A tibble that provides the odds for the current year of the NHL playoffs, based off the saved HTML pages.
  #'
  #' @export
  #'
  
  
  if (round == "quarter-finals") {
  
  page <- read_html(paste("data/external/oddsportal_current-odds/", year_of_play, "_pg1.html", sep = ""))
  
  } else if (round == "semi-finals") {
    
    page <- read_html(paste("data/external/oddsportal_current-odds/", year_of_play, "_pg2.html", sep = ""))
    
  } else if (round == "finals") {
    
    page <- read_html(paste("data/external/oddsportal_current-odds/", year_of_play, "_pg3.html", sep = ""))
  
  } else {
    
    page <- read_html(paste("data/external/oddsportal_current-odds/", year_of_play, "_pg4.html", sep = ""))
    
  }
  
  teams <- page %>% 
    html_nodes(".table-participant a:nth-child(3)") %>%
    html_text(.) %>%
    tibble(Teams = .) 
  
  odds <- page %>%
    html_nodes(".table-participant+ .odds-nowrp a") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(Odds.HighestSeed =.)
  
  odds2 <- page %>%
    html_nodes(".odds-nowrp~ .odds-nowrp+ .odds-nowrp:nth-child(5) a") %>%
    html_text(.) %>%
    as.numeric(.) %>%
    tibble(Odds.LowestSeed =.)
  
  bind_cols(tibble(year = rep(year, nrow(teams))), teams, odds, odds2)
}


template <- read_csv("src/scraping/templates/template.csv") %>%
  replace(., . == "St Louis Blues", "St. Louis Blues") %>%
  replace(., . == "Mighty Ducks of Anaheim", "Anaheim Ducks") %>%
  replace(., . == "Phoenix Coyotes", "Arizona Coyotes") %>%
  replace(., . == "Atlanta Thrashers", "Winnipeg Jets")

#Note: the function call below sends an error because on OddsPortal the actual odds are missing! But, these values are not important as we only 
#take the first game odds

all_data <- map_df(2006:2019, get_data) %>%
  write_csv(., "data/raw/2006-2019_oddsportal_raw.csv")

test_that("Odds don't match historical.", {
  expect_equivalent(readRDS("tests/test_data/odds.rds"), final)
})

final <- pmap_dfr(
  list(template$Year, template$Team1, template$Team2), 
  ~process_data(..1, ..2, ..3, data = all_data)) %>%
write_csv(., "data/processed/2006-2019_oddsportal.csv")


