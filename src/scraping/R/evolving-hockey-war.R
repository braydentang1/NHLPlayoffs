library(tidyverse)
library(rvest)

template <- read_csv("src/scraping/templates/template.csv") %>%
  mutate_all(funs(str_replace(., "Mighty Ducks of Anaheim", "Anaheim Ducks"))) %>%
  mutate_all(funs(str_replace(., "Phoenix Coyotes", "Arizona Coyotes")))

accronyms_pg <- read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")

accronyms <- accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)

full_names <- accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_accronyms <- cbind(full_name = full_names, team = accronyms) %>%
  as_tibble(.) %>% 
  bind_rows(., c(full_name = "Mighty Ducks of Anaheim", team = "MDA")) 

rm(accronyms_pg, accronyms, full_names)

get_data <- function(year) {
  
  data <- read_csv(paste("data/external/evolving-hockey_WAR/", year , ".csv", sep = "")) 
  
  data[data == "S.J"] = "SJS"
  data[data == "L.A"] = "LAK"
  data[data == "T.B"] = "TBL"
  data[data == "N.J"] = "NJD"
  
  toi_byteam <- data %>% 
            rename(team = Team) %>%
            mutate(team = as.factor(team)) %>%
            group_by(team) %>%
            summarize_at(funs(sum(., na.rm = TRUE)), .vars = "TOI_all")
  
  data <- data %>%
            rename(team = Team) %>%
            mutate(team = as.factor(team)) %>%
            left_join(., toi_byteam, by = "team") %>%
            group_by(team) %>%
            summarize_at(funs(mean(., na.rm = TRUE), median(., na.rm = TRUE), max(., na.rm = TRUE), sd(., na.rm = TRUE)), .vars = c("WAR", "GAR")) %>%
            mutate(team = as.character(team))
  
  bind_cols(year = rep(year, nrow(data)), data)
          
}

find_match <- function(team1, team2, stat, data, highest_seed) {
  
  data <- data %>%
    left_join(., lookup_accronyms, by = "team") %>%
    mutate(full_name = ifelse(full_name == "St. Louis Blues", "St Louis Blues", full_name)) 
  
  tmp <- unlist(c(data[, names(data) %in% c(stat)][which(data$full_name == team1),], data[, names(data) %in% c(stat)][which(data$full_name == team2),]))
  tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)] 
  
}

process_data <- function(team1, team2, highest_seed, data, year_of_play) {
  
  data <- data %>% 
    filter(., year == year_of_play)
  
  team_vec <- as_tibble(unlist(lapply(colnames(data)[3:ncol(data)], FUN = find_match, team1 = team1, team2 = team2, data = data, highest_seed = highest_seed))) %>%
    rownames_to_column(.) %>%
    mutate(rowname = colnames(data)[3:ncol(data)]) %>%
    spread(rowname, value) 
  
  team_vec
  
}

allcombined <- map_df(2008:2019, get_data) %>%
  write_csv(., "data/raw/2008-2019_evolving-hockey_WAR.csv")

final <- pmap_dfr(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, data = allcombined, ..4)) %>%
  write_csv(., "data/processed/2008-2019_evolving-hockey_WAR.csv")