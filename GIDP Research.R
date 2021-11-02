library("writexl")
library("ggplot2")
library("dplyr")
library("ggalt")
library(reshape2)

NCAAData <- read.csv("C:/Users/peter/Downloads/Baseball/NCAA_TM.csv")


IowaPitchersOnly <- NCAAData %>% 
  filter(PitcherTeam == "IOW_HAW") %>%
  filter(Season == "2021")

AllPitchers <- NCAAData %>%
  filter(Season == "2021") %>%
  group_by(Pitcher) %>%
  summarize(BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall")) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2)) %>%
  filter(BattedBalls >= 20)

summary(AllPitchers$GBPerc)

PitcherSummary <- IowaPitchersOnly %>%
  group_by(Pitcher) %>%
  summarize(BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall")) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2)) %>%
  filter(BattedBalls >= 30)

PitcherVector <- PitcherSummary[["Pitcher"]]

MainPitchers <- IowaPitchersOnly %>%
  filter(Pitcher %in% PitcherVector)


InGeneralStats <- MainPitchers %>%
  group_by(Pitcher, TaggedPitchType) %>% 
  summarise(NumPitches = n(), 
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
  filter(NumPitches >= 20) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2),
         Avg = round((NumHits/numABs), 3),
         BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3))
  

write_xlsx(InGeneralStats, "C:/Users/peter/Downloads/Baseball/InGeneralStats.xlsx")

MovementAndZones <- MainPitchers %>% 
  group_by(Pitcher, TaggedPitchType, BatterSide) %>%
  summarise(NumPitches = n(),
            PitchesUp = sum(ZoneNo %in% c(1,2,3)),
            PitchesMiddle = sum(ZoneNo %in% c(4,5,6)),
            PitchesDown = sum(ZoneNo %in% c(7,8,9)),
            PitchesUpAndOut = sum(ZoneNo %in% c(11,12)),
            PitchesDownAndOut = sum(ZoneNo %in% c(13,14)),
            AvgHorzBreak = mean(HorzBreak),
            AvgIndVertBreak = mean(InducedVertBreak),
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
  filter(NumPitches >= 15) %>%
  mutate(UpPerc = round((PitchesUp/NumPitches)*100,3),
         MidPerc = round((PitchesMiddle/NumPitches)*100,3),
         DownPerc = round((PitchesDown/NumPitches)*100,3),
         UpAndOutPerc = round((PitchesUpAndOut/NumPitches)*100,3),
         DownAndOutPerc = round((PitchesDownAndOut/NumPitches)*100,3),
         GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2),
         Avg = round((NumHits/numABs), 3),
         BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3))


ggplot(MovementAndZones, aes(x=DownAndOutPerc, y= GBPerc, color = TaggedPitchType)) + geom_point() +
  geom_smooth(method = "lm", se = FALSE)

write_xlsx(MovementAndZones, "C:/Users/peter/Downloads/Baseball/MovementsAndZones.xlsx")

EffectivenessOnSides <- MainPitchers %>%
  group_by(Pitcher, BatterSide) %>%
  summarise(NumPitches = n(), 
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
  filter(NumPitches >= 20) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2),
         Avg = round((NumHits/numABs), 3),
         BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3))

write_xlsx(EffectivenessOnSides, "C:/Users/peter/Downloads/Baseball/EffectivenessOnSides.xlsx")


Fastballs <- MainPitchers %>%
  group_by(Pitcher) %>%
  filter(TaggedPitchType == "Fastball") %>%
  summarise(NumPitches = n(),
            Velo = mean(RelSpeed),
            SpinRate = mean(SpinRate),
            SpinAxis = mean(SpinAxis),
            VertBreak = mean(InducedVertBreak),
            HorzBreak =mean(HorzBreak),
            PlateX = mean(PlateLocSide),
            PlateY = mean(PlateLocHeight),
            CalledStrikes = sum(PitchCall == "StrikeCalled"),
            Whiffs = sum(PitchCall == "StrikeSwinging"),
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
    mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
           FBPerc = round((FlyBalls/BattedBalls)*100, 2),
           Avg = round((NumHits/numABs), 3),
           BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3),
           CSW = round((CalledStrikes + Whiffs)/ NumPitches, 3))
  
write_xlsx(Fastballs, "C:/Users/peter/Downloads/Baseball/fastballs.xlsx")

  
Curveballs <- MainPitchers %>%
  group_by(Pitcher) %>%
  filter(TaggedPitchType == "Curveball") %>%
  summarise(NumPitches = n(),
            Velo = mean(RelSpeed),
            SpinRate = mean(SpinRate),
            SpinAxis = mean(SpinAxis),
            VertBreak = mean(InducedVertBreak),
            HorzBreak =mean(HorzBreak),
            PlateX = mean(PlateLocSide),
            PlateY = mean(PlateLocHeight),
            CalledStrikes = sum(PitchCall == "StrikeCalled"),
            Whiffs = sum(PitchCall == "StrikeSwinging"),
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2),
         Avg = round((NumHits/numABs), 3),
         BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3),
         CSW = round((CalledStrikes + Whiffs)/ NumPitches, 3))

write_xlsx(Curveballs, "C:/Users/peter/Downloads/Baseball/curves.xlsx")

Sliders <- MainPitchers %>%
  group_by(Pitcher) %>%
  filter(TaggedPitchType == "Slider") %>%
  summarise(NumPitches = n(),
            Velo = mean(RelSpeed),
            SpinRate = mean(SpinRate, na.rm = TRUE),
            SpinAxis = mean(SpinAxis),
            VertBreak = mean(InducedVertBreak),
            HorzBreak =mean(HorzBreak),
            PlateX = mean(PlateLocSide),
            PlateY = mean(PlateLocHeight),
            CalledStrikes = sum(PitchCall == "StrikeCalled"),
            Whiffs = sum(PitchCall == "StrikeSwinging"),
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2),
         Avg = round((NumHits/numABs), 3),
         BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3),
         CSW = round((CalledStrikes + Whiffs)/ NumPitches, 3))

write_xlsx(Sliders, "C:/Users/peter/Downloads/Baseball/sliders.xlsx")


ChangeUp <- MainPitchers %>%
  group_by(Pitcher) %>%
  filter(TaggedPitchType == "ChangeUp") %>%
  summarise(NumPitches = n(),
            Velo = mean(RelSpeed),
            SpinRate = mean(SpinRate),
            SpinAxis = mean(SpinAxis),
            VertBreak = mean(InducedVertBreak),
            HorzBreak =mean(HorzBreak),
            PlateX = mean(PlateLocSide),
            PlateY = mean(PlateLocHeight),
            CalledStrikes = sum(PitchCall == "StrikeCalled"),
            Whiffs = sum(PitchCall == "StrikeSwinging"),
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2),
         Avg = round((NumHits/numABs), 3),
         BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3),
         CSW = round((CalledStrikes + Whiffs)/ NumPitches, 3)) %>%
  filter(NumPitches >= 30)

write_xlsx(ChangeUp, "C:/Users/peter/Downloads/Baseball/changes.xlsx")


DoublePlays <- NCAAData %>%
  group_by(Season, PitcherTeam) %>%
  summarise(GroundBalls = sum(HitType == "GroundBall"),
            DoublePlays = sum(HitType == "GroundBall" & OutsOnPlay == 2),
            Games = n_distinct(GameID)) %>%
  mutate(DPperGB = round((DoublePlays/GroundBalls),3),
         DPperGame = round(DoublePlays/Games,3)) %>%
  filter(Games >7, PitcherTeam=="IOW_HAW")

write_xlsx(DoublePlays, "C:/Users/peter/Downloads/Baseball/DoublePlays.xlsx")


JustGBPercents <- MainPitchers %>%
  group_by(Pitcher) %>% 
  summarise(NumPitches = n(), 
            BattedBalls = sum(HitType != "Undefined"),
            GroundBalls = sum(HitType == "GroundBall"),
            FlyBalls = sum(HitType == "FlyBall"),
            NumHits = sum(!PlayResult %in% c("Out", "Sacrifice", "Error", "Undefined")),
            numABs = sum((!PlayResult %in% c("Sacrifice", "Undefined")) | (KorBB == "Strikeout") ),
            numHR = sum(PlayResult == "HomeRun"),
            NumSF = sum((PlayResult == "Sacrifice") & (HitType == "FlyBall")),
            NumKs = sum(KorBB == "Strikeout")) %>%
  filter(NumPitches >= 20) %>%
  mutate(GBPerc = round((GroundBalls/BattedBalls)* 100, 2),
         FBPerc = round((FlyBalls/BattedBalls)*100, 2),
         Avg = round((NumHits/numABs), 3),
         BABIP = round(((NumHits-numHR)/(numABs - NumKs - numHR + NumSF)),3),
         GBperFB = round((GroundBalls/FlyBalls),3))

write_xlsx(JustGBPercents, "C:/Users/peter/Downloads/Baseball/JustGBPercents.xlsx")