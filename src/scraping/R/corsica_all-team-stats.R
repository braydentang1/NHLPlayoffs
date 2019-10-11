library(tidyverse)
library(rvest)

template <- read_csv("src/scraping/templates/template.csv") %>%
  mutate_all(funs(str_replace(., "Mighty Ducks of Anaheim", "Anaheim Ducks"))) %>%
  mutate_all(funs(str_replace(., "Phoenix Coyotes", "Arizona Coyotes")))

accronyms_pg <- read_html("https://en.wikipedia.org/wiki/Template:NHL_team_abbreviations")
accronyms = accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 1,3)

full_names <- accronyms_pg %>% 
  html_nodes(".column-width li") %>%
  html_text(.) %>%
  substr(., 7, 1000000L)

lookup_accronyms <- cbind(full_name = full_names, team = accronyms) %>%
  as_tibble(.) %>% 
  bind_rows(., c(full_name = "Mighty Ducks of Anaheim", team = "MDA")) %>%
  mutate(full_name = ifelse(full_name == "St. Louis Blues", "St Louis Blues", full_name))

rm(accronyms_pg, accronyms, full_names)

get_data <- function(year) {
  
  data <- read_csv(paste("data/external/corsica_all-team-stats/", year,".csv", sep = "")) %>%
          mutate_if(is.character, funs(str_replace(., "L.A", "LAK"))) %>%
          mutate_if(is.character, funs(str_replace(., "N.J", "NJD"))) %>%
          mutate_if(is.character, funs(str_replace(., "S.J", "SJS"))) %>%
          mutate_if(is.character, funs(str_replace(., "T.B", "TBL"))) %>%
          mutate_if(is.character, funs(str_replace(., "MON", "MTL"))) %>%
          mutate_if(is.character, funs(str_replace(., "WAS", "WSH"))) %>%
          mutate_if(is.character, funs(str_replace(., "CAL", "CGY")))
  
  bind_cols(tibble(year = rep(year, nrow(data))), data)
  
}

process_data <- function(team1, team2, highest_seed, year_of_play, data){
  
  data <- data %>% 
    filter(., year == year_of_play)
  
  games <- ifelse(year_of_play == 2013, 48, 82)
  
  team_acc <- c(lookup_accronyms$team[which(lookup_accronyms$full_name == team1)], lookup_accronyms$team[which(lookup_accronyms$full_name == team2)])
  highestseed_acc <- team_acc[which(team_acc == lookup_accronyms$team[which(lookup_accronyms$full_name == highest_seed)])]
  
  bind_cols(
  cf_team = data$"CF%"[which(data$Team == team_acc[which(team_acc == highestseed_acc)])] - data$"CF%"[which(data$Team == team_acc[which(team_acc != highestseed_acc)])],
  cf_per60team = data$"CF/60"[which(data$Team == team_acc[which(team_acc == highestseed_acc)])] - data$"CF/60"[which(data$Team == team_acc[which(team_acc != highestseed_acc)])],
  ca_per60team = data$"CA/60"[which(data$Team == team_acc[which(team_acc == highestseed_acc)])] - data$"CA/60"[which(data$Team == team_acc[which(team_acc != highestseed_acc)])],
  xgf_60 = data$"xGF/60"[which(data$Team == team_acc[which(team_acc == highestseed_acc)])] - data$"xGF/60"[which(data$Team == team_acc[which(team_acc != highestseed_acc)])],
  xga_60 = data$"xGA/60"[which(data$Team == team_acc[which(team_acc == highestseed_acc)])] - data$"xGA/60"[which(data$Team == team_acc[which(team_acc != highestseed_acc)])],
  pdo = data$PDO[which(data$Team == team_acc[which(team_acc == highestseed_acc)])] - data$PDO[which(data$Team == team_acc[which(team_acc != highestseed_acc)])],
  penalties_taken = data$PENT[which(data$Team == team_acc[which(team_acc == highestseed_acc)])]/games - data$PENT[which(data$Team == team_acc[which(team_acc != highestseed_acc)])]/games,
  penalties_drawn = data$PEND[which(data$Team == team_acc[which(team_acc == highestseed_acc)])]/games - data$PEND[which(data$Team == team_acc[which(team_acc != highestseed_acc)])]/games)
  
}

allData <- map_df(2008:2019, get_data)
write_csv(allData, "data/raw/2008-2019_corsica-all-team-stats_raw.csv")

final = bind_rows(tibble(cf_team = rep(NA,30), cf_per60team = rep(NA, 30), ca_per60team = rep(NA, 30)),
                  pmap_dfr(
                    list(template$Team1, template$Team2, template$Highest.Seed, template$Year),
                    ~process_data(..1, ..2, ..3, ..4, data = allData)))

write_csv(final, "data/processed/2008-2019_corsica-all-team-stats_processed.csv")
