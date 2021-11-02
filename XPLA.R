library(rsample)   # data splitting 
library(ggplot2)   # plotting
library(earth)     # fit MARS models
library(caret)     # automating the tuning process
library(vip)       # variable importance
library(pdp)  
library(tidyr)
library(caTools)
library(e1071)


NCAAData <- read.csv("C:/Users/peter/Downloads/Baseball/NCAA_TM_CLEAN.csv")

NCAAData$TaggedPitchType <- as.factor(NCAAData$TaggedPitchType)

NCAAData$TaggedPitchType[NCAAData$TaggedPitchType== "Changeup"] <- "ChangeUp"





NCAAData <- NCAAData %>%
  drop_na(12, c(29:40), 44,45,46) %>%
  filter(TaggedPitchType != "Undefined"  | TaggedPitchType != "Other")%>%
  filter(!grepl('Pitcher', Pitcher))

PitchTypeListTM <- c("Fastball", "Sinker", "Cutter", "Curveball", "Slider", "ChangeUp")
PitchTypeListTMColors <- c("Fastball"="red", "Cutter"="blue", "Sinker"="greenyellow", "Curveball"="orange", "Slider"="deepskyblue", "ChangeUp"="palegreen4")

NCAAData$TaggedPitchType <- factor(NCAAData$TaggedPitchType, levels = PitchTypeListTM )

LeftHandedPitches <- NCAAData %>%
  filter(PitcherThrows=="Left")

RightHandedPitches <- NCAAData %>%
  filter(PitcherThrows == "Right")

sampleLeft = sample.split(LeftHandedPitches$TaggedPitchType, SplitRatio = .70)
trainLeft = subset(LeftHandedPitches, sampleLeft == TRUE)
testLeft  = subset(LeftHandedPitches, sampleLeft == FALSE)

trainLeft <- trainLeft[c(12,20, (29:40), 44,45,46)]
trainLeft <- trainLeft[c((2:7), (9:17))]



mars1 <- earth(TaggedPitchType ~ VertRelAngle +
                 HorzRelAngle + HorzApprAngle + VertApprAngle + 
                 SpinAxis + HorzBreak + 
                 InducedVertBreak + RelSpeed + VertBreak +
                 Extension + RelSide + RelHeight + SpinRate, data=trainLeft,
               degree =2)

hyper_grid <- expand.grid(
  degree = 1:3,
  nprune = seq(2,100, length.out = 10) %>% floor())

# for reproducibiity
set.seed(123)

# cross validated model
tuned_mars <- train(
  x = subset(trainLeft, select = -TaggedPitchType),
  y = trainLeft$TaggedPitchType,
  method = "earth",
  metric = "Accuracy",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# best model
tuned_mars$bestTune
##    nprune degree
## 14     34      2

# plot results
ggplot(tuned_mars)
