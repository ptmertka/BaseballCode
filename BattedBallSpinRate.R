library("writexl")
library("ggplot2")
library("dplyr")
library("ggalt")
library(reshape2)

NCAAData <- read.csv("C:/Users/peter/Downloads/Baseball/NCAA_TM.csv")

ContactData <- NCAAData %>% filter(HitType != "Undefined")

PitchTypeListTM <- c("Fastball", "Sinker", "Cutter", "Curveball", "Slider", "ChangeUp")
PitchTypeListTMColors <- c("Fastball"="red", "Cutter"="blue", "Sinker"="greenyellow" , "Curveball"="orange", "Slider"="deepskyblue", "ChangeUp"="palegreen4")

ContactData$TaggedPitchType <- factor(ContactData$TaggedPitchType, levels = PitchTypeListTM )

HitsWithSpinRate <- ContactData %>% filter(!is.na(HitSpinRate))

HitsWithSpinRate <- HitsWithSpinRate %>% filter(!is.na(SpinRate))

HitsWithSpinRate <- HitsWithSpinRate %>% filter(!is.na(Distance))


HitsWithSpinRate <- HitsWithSpinRate %>% mutate(SpinRateDifference = HitSpinRate - SpinRate)


GroundBalls <- HitsWithSpinRate %>% filter(HitType=="GroundBall")

FlyBalls <- HitsWithSpinRate %>% filter(HitType == "FlyBall")

LineDrives <- HitsWithSpinRate %>% filter(HitType == "LineDrive")

Popups <- HitsWithSpinRate %>% filter(HitType == "Popup")

Singles <- HitsWithSpinRate %>% filter(PlayResult == "Single")

Doubles  <- HitsWithSpinRate %>% filter(PlayResult == "Double")

Triples  <- HitsWithSpinRate %>% filter(PlayResult == "Triple")

HomeRuns <- HitsWithSpinRate %>% filter(PlayResult == "HomeRun" & Distance >= 330) 

Outs  <- HitsWithSpinRate %>% filter(PlayResult == "Out" | PlayResult == "Sacrifice")

ggplot(GroundBalls, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "yellow") +
  ggtitle("Histogram of Batted Ball Spin Rate for Ground Balls, bin = 350 RPM")

ggplot(FlyBalls, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "green") +
  ggtitle("Histogram of Batted Ball Spin Rate for Fly Balls, bin = 350 RPM")

ggplot(LineDrives, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "red") +
  ggtitle("Histogram of Batted Ball Spin Rate for Line Drives, bin = 350 RPM")

ggplot(Popups, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "blue") +
  ggtitle("Histogram of Batted Ball Spin Rate for Popups, bin = 350 RPM")

GroundBallErrors <- GroundBalls %>% filter(PlayResult == "Error") 

#ggplot(GroundBallErrors, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "white") +
 # ggtitle("Histogram of Batted Ball Spin Rate for Ground Ball Errors, bin = 350 RPM")

ggplot(Singles, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "yellow") +
  ggtitle("Histogram of Batted Ball Spin Rate for Singles, bin = 350 RPM")

ggplot(Doubles, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "green") +
  ggtitle("Histogram of Batted Ball Spin Rate for Doubles, bin = 350 RPM")

ggplot(Triples, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "red") +
  ggtitle("Histogram of Batted Ball Spin Rate for Triples, bin = 350 RPM")

ggplot(HomeRuns, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "blue") +
  ggtitle("Histogram of Batted Ball Spin Rate for HomeRuns, bin = 350 RPM")

#These plots show good info, but how to refine?

ggplot(HomeRuns, aes(x=HitSpinRate, y = Distance, color = TaggedPitchType)) +geom_point() +
  ggtitle("Home Run Distance versus HitSpinRate")

ggplot(HomeRuns, aes(x=HitSpinRate, y = Angle, color = TaggedPitchType)) +geom_point() +
  ggtitle("Home Run Launch Angle versus HitSpinRate")

ggplot(HomeRuns, aes(x=HitSpinRate, y = ExitSpeed, color = TaggedPitchType)) +geom_point() +
  ggtitle("Home Run Exit Velo versus HitSpinRate")

ggplot(HomeRuns, aes(x=SpinRateDifference, y = Distance, color = TaggedPitchType)) +geom_point() +
 ggtitle("Home Run Distance versus Difference in Spin Rate")

ggplot(HomeRuns, aes(x=SpinRateDifference, y = Angle, color = TaggedPitchType)) +geom_point() +
  ggtitle("Home Run Launch Angle versus Difference in Spin Rate")

ggplot(HomeRuns, aes(x=SpinRateDifference, y = ExitSpeed, color = TaggedPitchType)) +geom_point() +
  ggtitle("Home Run Exit Velo versus Difference in Spin Rate")

AverageSpinRateByContactType <- HitsWithSpinRate %>%
  group_by(QualityOfContact) %>%
  summarise(AverageSpinRate = mean(SpinRate),
            AverageHitSpinRate = mean(HitSpinRate),
            AverageSpinRateDifference = mean(SpinRateDifference))

write_xlsx(AverageSpinRateByContactType, "C:/Users/peter/Downloads/Baseball/AverageSpinRateByContactType.xlsx")


InfieldSingles <- Singles %>%
  filter(Angle > -5 & Angle < 5) %>%
  filter(Distance > 0 & Distance < 130)

ggplot(InfieldSingles, aes(x= HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "yellow") +
  ggtitle("Histogram of Batted Ball Spin Rate for Infield Singles, bin = 350 RPM")

ggplot(InfieldSingles, aes(x= SpinRateDifference)) + geom_histogram(binwidth = 350, color = "black", fill = "yellow") +
  ggtitle("Histogram of Batted Ball Spin Rate Difference for Infield Singles, bin = 350 RPM")

ggplot(Singles, aes(x = SpinRate, SpinRateDifference, color = TaggedPitchType)) + geom_point() +
  ggtitle("Spin Rate versus added Spin Rate on Singles")

ggplot(GroundBalls, aes(x = SpinRate, SpinRateDifference, color = TaggedPitchType)) + geom_point() +
  ggtitle("Spin Rate versus added Spin Rate on GroundBalls")

Barrels <- HitsWithSpinRate %>%
  filter(QualityOfContact == "Barrel")

ggplot(Barrels, aes(x=SpinRateDifference)) + geom_histogram(binwidth = 350, color = "black", fill = "yellow") +
  ggtitle("Histogram of Spin Rate Difference on Barrels")

ggplot(Barrels, aes(x=SpinRate)) + geom_histogram(binwidth = 200, color = "black", fill = "yellow") +
  ggtitle("Histogram of Spin Rate on Barrels")

ggplot(Barrels, aes(x=HitSpinRate)) + geom_histogram(binwidth = 350, color = "black", fill = "yellow") +
  ggtitle("Histogram of Hit Spin Rate on Barrels")

Fastballs <- HitsWithSpinRate %>%
  filter(TaggedPitchType == "Fastball", 
         SpinAxis >= 165 & SpinAxis <= 240)

FastballHitsInRange <- Fastballs %>%
  filter(SpinRateDifference >= 0 & SpinRateDifference <= 500)

FastballHitsOutOfRange <- Fastballs %>%
  filter(SpinRateDifference <= 0 | SpinRateDifference >= 500)

InRangeSummary <- FastballHitsInRange %>%
  summarise(numSingles = sum(PlayResult == "Single"),
            numDoubles = sum(PlayResult == "Double"),
            numTriples = sum(PlayResult == "Triple"),
            numHomeRuns = sum(PlayResult == "HomeRun"),
            numOuts = sum(PlayResult == "Out"),
            avgExitVelo = mean(ExitSpeed),
            avgDistance = mean(Distance),
            avgLaunchAngle = mean(Angle),
            avgIncomingSpinRate = mean(SpinRate),
            numHits = n()) %>%
  mutate(SinglePercent = round((numSingles/numHits)* 100, 2),
           DoublePercent = round((numDoubles/numHits) * 100, 2),
           TriplePercent = round((numTriples/numHits) * 100, 2),
           HomeRunPercent = round((numHomeRuns/numHits) * 100, 2),
           OutPercent = round((numOuts/numHits) * 100, 2))

write_xlsx(InRangeSummary, "C:/Users/peter/Downloads/Baseball/InRangeSummary.xlsx")

OutRangeSummary <- FastballHitsOutOfRange %>%
  summarise(numSingles = sum(PlayResult == "Single"),
            numDoubles = sum(PlayResult == "Double"),
            numTriples = sum(PlayResult == "Triple"),
            numHomeRuns = sum(PlayResult == "HomeRun"),
            numOuts = sum(PlayResult == "Out"),
            avgExitVelo = mean(ExitSpeed),
            avgDistance = mean(Distance),
            avgLaunchAngle = mean(Angle),
            avgIncomingSpinRate = mean(SpinRate),
            numHits = n()) %>%
  mutate(SinglePercent = round((numSingles/numHits)* 100, 2),
           DoublePercent = round((numDoubles/numHits) * 100, 2),
           TriplePercent = round((numTriples/numHits) * 100, 2),
           HomeRunPercent = round((numHomeRuns/numHits) * 100, 2),
           OutPercent = round((numOuts/numHits) * 100, 2))

write_xlsx(OutRangeSummary, "C:/Users/peter/Downloads/Baseball/OutRangeSummary.xlsx")


CurveBalls <- HitsWithSpinRate %>%
  filter(TaggedPitchType == "Curveball", 
         SpinAxis >= 300 | SpinAxis <= 60)

CurveBallHitsInRange <- CurveBalls %>%
  filter(SpinRateDifference >= 0 & SpinRateDifference <= 500)

CurveBallHitsOutOfRange <- CurveBalls %>%
  filter(SpinRateDifference <= 0 | SpinRateDifference >= 500)

InRangeSummaryCB <- CurveBallHitsInRange %>%
  summarise(numSingles = sum(PlayResult == "Single"),
            numDoubles = sum(PlayResult == "Double"),
            numTriples = sum(PlayResult == "Triple"),
            numHomeRuns = sum(PlayResult == "HomeRun"),
            numOuts = sum(PlayResult == "Out"),
            avgExitVelo = mean(ExitSpeed),
            avgDistance = mean(Distance),
            avgLaunchAngle = mean(Angle),
            avgIncomingSpinRate = mean(SpinRate),
            numHits = n()) %>%
  mutate(SinglePercent = round((numSingles/numHits)* 100, 2),
         DoublePercent = round((numDoubles/numHits) * 100, 2),
         TriplePercent = round((numTriples/numHits) * 100, 2),
         HomeRunPercent = round((numHomeRuns/numHits) * 100, 2),
         OutPercent = round((numOuts/numHits) * 100, 2))

write_xlsx(InRangeSummaryCB, "C:/Users/peter/Downloads/Baseball/InRangeSummaryCB.xlsx")  

OutRangeSummaryCB <- CurveBallHitsOutOfRange %>%
  summarise(numSingles = sum(PlayResult == "Single"),
            numDoubles = sum(PlayResult == "Double"),
            numTriples = sum(PlayResult == "Triple"),
            numHomeRuns = sum(PlayResult == "HomeRun"),
            numOuts = sum(PlayResult == "Out"),
            avgExitVelo = mean(ExitSpeed),
            avgDistance = mean(Distance),
            avgLaunchAngle = mean(Angle),
            avgIncomingSpinRate = mean(SpinRate),
            numHits = n()) %>%
  mutate(SinglePercent = round((numSingles/numHits)* 100, 2),
         DoublePercent = round((numDoubles/numHits) * 100, 2),
         TriplePercent = round((numTriples/numHits) * 100, 2),
         HomeRunPercent = round((numHomeRuns/numHits) * 100, 2),
         OutPercent = round((numOuts/numHits) * 100, 2))

write_xlsx(OutRangeSummaryCB, "C:/Users/peter/Downloads/Baseball/OutRangeSummaryCB.xlsx")

JustHits <- HitsWithSpinRate %>%
  filter(PlayResult %in% c("Single", "Double", "Triple", "HomeRun", "Out"))

ggplot(JustHits, aes(x=PlayResult, y = HitSpinRate, color = PlayResult)) +
  geom_violin(trim = TRUE) +
  ggtitle("Violin plot of Hit Type vs Hit Spin Rate")
















