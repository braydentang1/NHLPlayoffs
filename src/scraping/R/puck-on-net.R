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

get_data_pon = function(year) {
  
  #' Pulls data from Puck on Net, namely, stats from the last 20 games of the regular season.
  #'
  #' @param year an integer: the year of NHL Playoffs to pull data from.
  #'
  #' @return
  #' A tibble that contains stats from the last 20 games played.
  #'
  #' @export
  #'
    year2 <- year - 1
    
    main_page <- read_html(paste("http://www.puckon.net/fenwick.php?s=", year2, "-09-01&e=", year, "-06-30&f=0&ld=1&l=82&p=0", sep=""))
    last20page_main <- read_html(paste("https://www.puckon.net/?s=", year2, "-08-01&e=", year, "-06-30&f=0&ld=-1&l=20&p=0", sep = ""))
    last20page_goals <- read_html(paste("https://www.puckon.net/goals.php?f=0&s=", year2, "-08-01&e=", year, "-06-30&l=-20&p=0", sep=""))
    last20page_misses <- read_html(paste("https://www.puckon.net/misses.php?f=0&s=", year2, "-08-01&e=", year, "-06-30&l=-20&p=0", sep=""))
    last20page_blocks <- read_html(paste("https://www.puckon.net/blocks.php?f=0&s=", year2, "-08-01&e=", year, "-06-30&l=-20&p=0", sep=""))
    last20page_hits <- read_html(paste("https://www.puckon.net/hits.php?f=0&s=", year2, "-08-01&e=", year, "-06-30&l=-20&p=0", sep=""))
    
    teams <- main_page %>%
      html_nodes("td:nth-child(1)") %>%
      html_text(.) %>%
      gsub("\\.", "",.) %>%
      as_tibble(.) %>%
      set_names(., "accronym") %>%
      mutate(accronym = ifelse(accronym == "LA", "LAK", accronym)) %>%
      mutate(accronym = ifelse(accronym == "NJ", "NJD", accronym)) %>%
      mutate(accronym = ifelse(accronym == "SJ", "SJS", accronym)) %>%
      mutate(accronym = ifelse(accronym =="TB", "TBL", accronym)) 

    fenwick <- main_page %>% 
      html_nodes("td:nth-child(6)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(fenwick = .) 

    fenwick_last20 <- last20page_main %>%
      html_nodes("td:nth-child(10)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(fenwick_last20 = .)
    
    corsi_last20 <- last20page_main %>%
      html_nodes("td:nth-child(6)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(corsi_last20 = .)
    
    sog_last20 <- last20page_main %>%
      html_nodes("td:nth-child(14)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(sog_last20 = .)
    
    rm(last20page_main)
    
    goals_percentage_last20 <- last20page_goals %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(goals_percentage_last20 = .)
    
    rm(last20page_goals)
    
    misses_percentage_last20 <- last20page_misses %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(misses_percentage_last20 = .)
  
    rm(last20page_misses)
    
    blocks_percentage_last20 <- last20page_blocks %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(blocks_percentage_last20 = .)
    
    rm(last20page_blocks)
    
    hits_percentage_last20 <- last20page_hits %>%
      html_nodes("td:nth-child(4)") %>%
      html_text(.) %>%
      as.numeric(.) %>%
      tibble(hits_percentage_last20 = .)
    
    rm(last20page_hits)
    
    left_join(teams, lookup_accronyms, by = "accronym") %>%
      bind_cols(., tibble(year = rep(year, nrow(teams))), 
                          teams, 
                          fenwick,
                          fenwick_last20, 
                          corsi_last20, 
                          sog_last20, 
                          goals_percentage_last20, 
                          misses_percentage_last20, 
                          blocks_percentage_last20, 
                          hits_percentage_last20) %>%
      mutate(full_name = ifelse(full_name == "St. Louis Blues", "St Louis Blues", full_name)) %>%
      mutate(full_name = ifelse(full_name == "Anaheim Ducks" & year <= 2006, "Mighty Ducks of Anaheim", full_name)) %>%
      rename(team = full_name)
 
}

find_match <- function(team1, team2, stat, data, highest_seed) {
  
  #' Finds the two relevant teams playing each other in the raw dataset provided by get_data_pon,
  #'  and calculates the difference in a statistic from the perspective of the higher seed.
  #'
  #' @param team1 character string; a team competing against team2 in a particular NHL series
  #' @param team2 a team competing against team1 in a particular NHL series
  #' @param stat character string; a column name found in the raw data given by the argument data to compute the differencing
  #' @param data the raw dataset provided by get_data_pon
  #' @param highest_seed character string; gives the highest seed among team1 or team2.
  #'  The highest seed is defined as the team that starts the series at home.
  #'
  #' @return
  #' A numeric value that gives the difference in a statistic, from the higher seeds perspective.
  #'
  #' @export
  #'
  
  tmp <- unlist(c(data[, names(data) %in% c(stat)][which(data$team == team1), ], data[, names(data) %in% c(stat)][which(data$team == team2), ]))
  tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)] 
}

process_data <- function(team1, team2, highest_seed, data, year_of_play, start_col = 5L) {
  
  #' Processes the raw data from the function get_data_pon to be the differences in stats
  #'  between two teams from the highest seeds perspective.
  #'  Starts processing at column 5 by default.
  #'
  #' @param team1 a particular team in a NHL playoff series, playing against team2
  #' @param team2 a particular team in a NHL playoff series, playing against team1
  #' @param highest_seed the highest seed between team1 and team2
  #' @param the raw data resulting from the function get_data_pon
  #' @param year_of_play the year of the NHL playoffs for the series played between team1 and team2
  #' @param start_col a vector of length one that gives the starting column index to start processing from.
  #'  All columns from the given column index and onwards are processed. Default = 5L.
  #'
  #' @return
  #'  A list of the processed stats given in the raw dataset.
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

all_years <- map_df(2006:2019, get_data_pon) %>%
  write_csv(., "data/raw/2006-2019_puck-on-net_last20_raw.csv")

final_data <- pmap_dfr(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, data = all_years, ..4)) %>%
  write_csv(., "data/processed/2006-2019_puck-on-net_last20.csv")