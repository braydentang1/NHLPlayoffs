setwd("C:/Users/Brayden/Documents/NHLModel/Game Score/Game Score Cleaned")

require(dplyr)

years = seq(2008,2018, 1)

for (i in 1:length(years)){

data = read.csv(paste("C:/Users/Brayden/Documents/NHLModel/Game Score/", years[i], ".csv", sep=""), na.strings = "--", stringsAsFactors = TRUE)
data = data[, 2:ncol(data)]

data = data %>% mutate(TradedPlayer = ifelse(grepl("/", Team) == TRUE, 1,0)) %>%
                mutate(Team = as.factor(gsub(" ", "", Team, fixed = TRUE))) %>%
                mutate_if(is.numeric, funs(ifelse(is.infinite(.), 0,.))) %>%
                mutate_if(is.numeric, funs(ifelse(is.na(.), 0,.))) %>%
                mutate_if(is.numeric, funs(ifelse(is.nan(.),0,.)))

data = data[, !names(data) %in% c("Player", "Position", "Season")]

AllTradedPlayers = data[data$TradedPlayer == 1,]

AllTradedPlayers_Team1 = AllTradedPlayers %>% mutate(Team = sub("/.*", "", AllTradedPlayers$Team))
AllTradedPlayers_Team2 = AllTradedPlayers %>% mutate(Team = sub(".*/", "", AllTradedPlayers$Team))
AllTradedPlayers_Team3 = AllTradedPlayers %>% mutate(Team = sub(".*/ *(.*?) */.*", "\\1", AllTradedPlayers$Team))
AllTradedPlayers_Team3 = AllTradedPlayers_Team3[-which(grepl("/", AllTradedPlayers_Team3$Team) == TRUE), ]

rm(AllTradedPlayers)

combined = rbind(AllTradedPlayers_Team1, AllTradedPlayers_Team2, AllTradedPlayers_Team3)
rm(list = ls(pattern = "AllTradedPlayers"))

data = rbind(data[-which(data$TradedPlayer == 1),], combined)
rm(combined)

data$Team = as.factor(data$Team)

#removed "GS", "CF.", "Rel.CF." since they are already included in Full Data
data = data %>% group_by(Team) %>%
                summarise_at(funs(mean(., na.rm = TRUE), sd(., na.rm=TRUE), min(.,na.rm=TRUE), max(., na.rm=TRUE)), .vars = c("iCF.60",
                                                                                                                              "ixGF.60", "ZSR", "CF..QoT", "CF..QoC"))

colnames(data)[2:ncol(data)] = paste(colnames(data)[2:ncol(data)], sep="_")

write.csv(data, paste("GameScore", years[i], "_cleaned.csv", sep=""), row.names = FALSE)

}