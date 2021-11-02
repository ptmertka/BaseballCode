library("writexl")
library("ggplot2")
library("dplyr")
library("ggalt")
library(reshape2)

NCAAData <- read.csv("C:/Users/peter/Downloads/Baseball/NCAA_TM.csv")

InSeasonGames <- NCAAData %>%
  filter(GameType == "In-Season" & (HomeTeamConf == "Big 10") | (AwayTeamConf == "Big 10"))

byYear <-InSeasonGames %>%
  group_by(Stadium, Season )%>%
  summarise(Runs = sum(RunsScored),
            numGames = length(unique(GameID)))
  

Runs <- InSeasonGames %>%
  group_by(Stadium) %>%
  summarise(Runs = sum(RunsScored),
            numGames = length(unique(GameID)))


Runs$Runs[Runs$Runs == 170] <- 475
Runs$Stadium[Runs$Runs == 475] <- "BartKaufman"
Runs$numGames[Runs$numGames == 15] <- 15+26

Runs <- Runs[-c(3), ]

Big10Only <- Runs %>%
  filter(numGames > 8) %>%
  mutate(RPGH = round(Runs/numGames,4),
         TotalRunsAway = sum(Runs),
         TotalGamesAway = sum(numGames))

for(i in 1:nrow(Big10Only)){
  Big10Only$TotalRunsAway[i] <- Big10Only$TotalRunsAway[i] - Big10Only$Runs[i]
  Big10Only$TotalGamesAway[i] <- Big10Only$TotalGamesAway[i] - Big10Only$numGames[i]
}

Big10Only <- Big10Only %>%
  mutate(RPGA = round(TotalRunsAway/TotalGamesAway,4),
         rawPF = round((RPGH*7)/((6*RPGA)+RPGH),4),
         iPF = round((rawPF+1)/2,4))


yearsOfData <-c(2,5,2,1,2,2,2)

weights<- c(.6,.7,.8,.9,.9,.9)

Big10Only <- cbind(Big10Only, yearsOfData)



Big10Only <- Big10Only %>% mutate(finalPF = 1- ((1-iPF) * weights[yearsOfData]) )


HomeRuns  <- InSeasonGames %>%
  group_by(Stadium) %>%
  summarise(HomeRuns = sum(PlayResult == "HomeRun"),
            numGames = length(unique(GameID)))


HomeRuns$HomeRuns[HomeRuns$HomeRuns == 23] <- 78
HomeRuns$Stadium[HomeRuns$HomeRuns == 78] <- "BartKaufman"
HomeRuns$numGames[HomeRuns$numGames == 15] <- 15+26

HomeRuns <- HomeRuns[-c(3), ]

Big10HROnly <- HomeRuns %>%
  filter(numGames > 8) %>%
  mutate(HRPGH = round(HomeRuns/numGames,4),
         TotalHomeRunsAway = sum(HomeRuns),
         TotalGamesAway = sum(numGames))

for(i in 1:nrow(Big10HROnly)){
  Big10HROnly$TotalHomeRunsAway[i] <- Big10HROnly$TotalHomeRunsAway[i] - Big10HROnly$HomeRuns[i]
  Big10HROnly$TotalGamesAway[i] <- Big10HROnly$TotalGamesAway[i] - Big10HROnly$numGames[i]
}


Big10HROnly <- Big10HROnly %>%
  mutate(HRPGA = round(TotalHomeRunsAway/TotalGamesAway,4),
         rawPF = round((HRPGH*7)/((6*HRPGA)+HRPGH),4),
         iPF = round((rawPF+1)/2,4))

Big10HROnly <- cbind(Big10HROnly, yearsOfData)

Big10HROnly <- Big10HROnly %>% mutate(finalPF = 1- ((1-iPF) * weights[yearsOfData]) )

Big10HROnly <- Big10HROnly %>% 
  mutate(adjustedHRPF = finalPF * 100)


Big10Only <- Big10Only %>% 
  mutate(adjustedPF = finalPF * 100)


SinglesDoublesTriples <- InSeasonGames %>%
  group_by(Stadium) %>%
  summarise(numSingles = sum(PlayResult == "Single"),
            numDoubles = sum(PlayResult == "Double"),
            numTriples = sum(PlayResult == "Triple"),
            numGames = length(unique(GameID)))

SinglesDoublesTriples$numSingles[SinglesDoublesTriples$numSingles == 293] <- (293+161)
SinglesDoublesTriples$numDoubles[SinglesDoublesTriples$numDoubles == 95] <- (95+60)
SinglesDoublesTriples$numTriples[SinglesDoublesTriples$numTriples == 9] <- (9+7)
SinglesDoublesTriples$Stadium[SinglesDoublesTriples$numSingles == 293] <- "BartKaufman"
SinglesDoublesTriples$numGames[SinglesDoublesTriples$numGames == 15] <- 15+26


SinglesDoublesTriples <- SinglesDoublesTriples[-c(2), ]

Big10SDT <- SinglesDoublesTriples %>%
  filter(numGames > 8) %>%
  mutate(SPGH = round(numSingles/numGames,4),
         DPGH = round(numDoubles/numGames,4),
         TPGH = round(numTriples/numGames,4),
         TotalSinglesAway = sum(numSingles),
         TotalDoublesAway = sum(numDoubles),
         TotalTriplesAway = sum(numTriples),
         TotalGamesAway = sum(numGames))

for(i in 1:nrow(Big10SDT)){
  Big10SDT$TotalGamesAway[i] <- Big10SDT$TotalGamesAway[i] - Big10SDT$numGames[i]
  Big10SDT$TotalSinglesAway[i] <- Big10SDT$TotalSinglesAway[i] - Big10SDT$numSingles[i]
  Big10SDT$TotalDoublesAway[i] <- Big10SDT$TotalDoublesAway[i] - Big10SDT$numDoubles[i]
  Big10SDT$TotalTriplesAway[i] <- Big10SDT$TotalTriplesAway[i] - Big10SDT$numTriples[i]
 }

Big10SDT <- Big10SDT %>%
  mutate(SPGA = round(TotalSinglesAway/TotalGamesAway,4),
         DPGA = round(TotalDoublesAway/TotalGamesAway,4),
         TPGA = round(TotalTriplesAway/TotalGamesAway,4),
         rawPFS = round((SPGH*7)/((6*SPGA)+SPGH),4),
         iPFS = round((rawPFS+1)/2,4),
         rawPFD = round((DPGH*7)/((6*DPGA)+DPGH),4),
         iPFD = round((rawPFD+1)/2,4),
         rawPFT = round((TPGH*7)/((6*TPGA)+TPGH),4),
         iPFT = round((rawPFT+1)/2,4))



Big10SDT <- Big10SDT %>% mutate(finalPFS = 1- ((1-iPFS) * weights[yearsOfData]),
                                finalPFD =  1- ((1-iPFD) * weights[yearsOfData]),
                                finalPFT = 1- ((1-iPFT) * weights[yearsOfData]))


partA <- merge(Big10Only, Big10HROnly, by = "Stadium", all = TRUE)

finalTable <- merge(partA, Big10SDT, by = "Stadium", all = TRUE)

finalTable <- finalTable[, c(1,11,22,44,45,46)]

write_xlsx(finalTable, "C:/Users/peter/Downloads/Baseball/parkFactors.xlsx")



