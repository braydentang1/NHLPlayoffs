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

lookup_accronyms <- cbind(full_names = full_names, team = accronyms) %>%
  as_tibble(.) %>% 
  bind_rows(., c(full_names = "Mighty Ducks of Anaheim", team = "MDA")) 

rm(accronyms_pg, accronyms, full_names)


get_data = function(year) {
  
  data <- read_csv(paste("data/external/corsica_game-score/", year, ".csv", sep=""), na = "--") %>%
          .[,2:ncol(.)] %>%
    rename(team = Team) %>%
    mutate(traded_player = ifelse(grepl("/", team) == TRUE, 1,0)) %>%
    mutate(team = gsub(" ", "", team, fixed = TRUE)) %>%
    mutate_if(is.numeric, funs(ifelse(is.infinite(.), 0,.))) %>%
    mutate_if(is.numeric, funs(ifelse(is.na(.), 0,.))) %>%
    mutate_if(is.numeric, funs(ifelse(is.nan(.),0,.))) %>%
    select(., -Position, -Season)
  
  all_traded_players <- filter(data, traded_player == 1)
  
  all_traded_players_team1 <- all_traded_players %>% mutate(team = sub("/.*", "", all_traded_players$team))
  all_traded_players_team2 <- all_traded_players %>% mutate(team = sub(".*/", "", all_traded_players$team))
  all_traded_players_team3 <- all_traded_players %>% mutate(team = sub(".*/ *(.*?) */.*", "\\1", all_traded_players$team))
  all_traded_players_team3 <- all_traded_players_team3[-which(grepl("/", all_traded_players_team3$team) == TRUE), ]
  
  combined <- bind_rows(all_traded_players_team1, all_traded_players_team2, all_traded_players_team3)
  rm(list = ls(pattern = "all_traded_players"))
  
  data <- bind_rows(data %>% filter(traded_player != 1), combined)
  rm(combined)
  
  data$team <- as.factor(data$team)
  
  #removed "GS", "CF.", "Rel.CF." since they are already included in Full Data
  data <- data %>%
              group_by(team) %>%
              mutate(cf = CF/TOI) %>%
              mutate(p1 = P1/TOI) %>%
              summarise_at(funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE), max(., na.rm=TRUE)), .vars = c("iCF/60", "P1", "P1/60", "GS", "GS/60", "CF", "Rel CF%",
                                                                                                                  "ixGF/60", "ZSR", "CF% QoT", "CF% QoC", "TOI% QoT", "TOI% QoC")) %>%
              mutate(team = as.character(team))
  
  bind_cols(data, tibble(year = rep(year, nrow(data))))
}

all_combined <- map_df(2008:2019, get_data) %>%
                mutate(team = ifelse(team == "L.A", "LAK", team)) %>%
                mutate(team = ifelse(team == "N.J", "NJD", team)) %>%
                mutate(team = ifelse(team == "S.J", "SJS", team)) %>%
                mutate(team = ifelse(team == "T.B", "TBL", team)) 

write_csv(all_combined, "data/raw/2008-2019_corsica_game-score_raw.csv")

find_match <- function(team1, team2, stat, data, highest_seed) {
  
  data <- data %>%
                left_join(., lookup_accronyms, by = "team") %>%
                mutate(full_names = ifelse(full_names == "St. Louis Blues", "St Louis Blues", full_names)) 
  
  tmp <- unlist(c(data[, names(data) %in% c(stat)][which(data$full_names == team1),], data[, names(data) %in% c(stat)][which(data$full_names == team2),]))
  tmp[which(c(team1, team2) == highest_seed)] - tmp[which(c(team1, team2) != highest_seed)] 
}

process_data <- function(team1, team2, highest_seed, data, year_of_play) {
  
  data <- data %>% 
          filter(., year == year_of_play)

  team_vec <- as_tibble(unlist(lapply(colnames(data)[2:(ncol(data)-1)], FUN = find_match, team1 = team1, team2 = team2, data = data, highest_seed = highest_seed))) %>%
    rownames_to_column(.) %>%
    mutate(rowname = colnames(data)[2:(ncol(data)-1)]) %>%
    spread(rowname, value) 
  
  team_vec
  
}

final <- pmap_dfr(list(template$Team1, template$Team2, template$Highest.Seed, template$Year), ~process_data(..1, ..2, ..3, data = all_combined, ..4)) %>%
         select_if(~sum(!is.na(.)) > 0) 
  
write_csv(final, "data/processed/2008-2019_corsica_game-score.csv")