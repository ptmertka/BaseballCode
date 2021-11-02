library(caret)
library(randomForest)
require(caTools)
library(dplyr)
library(tidyr)
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
dim(trainLeft)
dim(testLeft)

sampleRight = sample.split(RightHandedPitches$TaggedPitchType, SplitRatio = .70)
trainRight = subset(RightHandedPitches, sampleRight == TRUE)
testRight  = subset(RightHandedPitches, sampleRight == FALSE)
dim(trainRight)
dim(testRight)

trainLeft <- droplevels(trainLeft)
rfLeft <- randomForest(data=trainLeft, TaggedPitchType ~  VertRelAngle +
                         HorzRelAngle + HorzApprAngle + VertApprAngle + 
                         SpinAxis + HorzBreak + 
                         InducedVertBreak + RelSpeed + VertBreak +
                         Extension + RelSide + RelHeight + SpinRate + BatterSide, ntree= 600, mtry = 4, importance = TRUE)

predLeft = predict(rfLeft, newdata=testLeft[-20])

cmLeft = table(testLeft[,20], predLeft)

trainRight <- droplevels(trainRight)
rfRight <- randomForest(data=trainRight, TaggedPitchType ~  VertRelAngle +  
                         HorzRelAngle + HorzApprAngle + VertApprAngle + 
                          SpinAxis + HorzBreak + 
                         InducedVertBreak + RelSpeed + VertBreak +
                         Extension + RelSide + RelHeight + SpinRate +BatterSide, importance= TRUE, ntree= 600, mtry = 4)

predRight = predict(rfRight, newdata=testRight[-20])

cmRight = table(testRight[,20], predRight)

testLeft$PredictValues <- predict(rfLeft, newdata=testLeft[-20])
testRight$PredictValues <- predict(rfRight, newdata = testRight[-20])


ggplot(testLeft, aes(x=HorzBreak, y = InducedVertBreak, color = PredictValues)) +
  geom_point() +
  scale_color_manual(values = PitchTypeListTMColors) +
  ggtitle("Prediction Movement Plot")


ggplot(testRight, aes(x=HorzBreak, y = InducedVertBreak, color = PredictValues)) +
  geom_point() +
  scale_color_manual(values = PitchTypeListTMColors) +
  
  ggtitle("Prediction Movement Plot")

ggplot(trainLeft, aes(x=HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
  geom_point() +
  scale_color_manual(values = PitchTypeListTMColors) +
  ggtitle("Prediction Movement Plot")


ggplot(trainRight, aes(x=HorzBreak, y = InducedVertBreak, color = TaggedPitchType)) +
  geom_point() +
  scale_color_manual(values = PitchTypeListTMColors) +
  
  ggtitle("Prediction Movement Plot")

letMeSe <- testLeft[testLeft$PredictValues != testLeft$TaggedPitchType, ]

letMeSe <- letMeSe[, c(6, 20,29:40, 44,45,46,112)]


IowaLefties <- LeftHandedPitches %>%
  filter(HomeTeam == "IOW_HAW")

IowaRighties <- RightHandedPitches %>%
  filter(HomeTeam == "IOW_HAW")

sampleLeftIowa = sample.split(IowaLefties$TaggedPitchType, SplitRatio = .70)
trainLeftIowa = subset(IowaLefties, sampleLeftIowa == TRUE)
testLeftIowa  = subset(IowaLefties, sampleLeftIowa == FALSE)

sampleRightIowa = sample.split(IowaRighties$TaggedPitchType, SplitRatio = .70)
trainRightIowa = subset(IowaRighties, sampleRightIowa == TRUE)
testRightIowa  = subset(IowaRighties, sampleRightIowa == FALSE)

trainLeftIowa <- droplevels(trainLeftIowa)
rfLeftIowa <- randomForest(data=trainLeftIowa, TaggedPitchType ~  VertRelAngle +
                         HorzRelAngle + HorzApprAngle + VertApprAngle + 
                         SpinAxis + HorzBreak + 
                         InducedVertBreak + RelSpeed + VertBreak +
                         Extension + RelSide + RelHeight + SpinRate + BatterSide, ntree= 600, mtry = 4, importance = TRUE)

predLeftIowa = predict(rfLeftIowa, newdata=testLeftIowa[-20])

cmLeftIowa = table(testLeftIowa[,20], predLeftIowa)

trainRightIowa <- droplevels(trainRightIowa)
rfRightIowa <- randomForest(data=trainRightIowa, TaggedPitchType ~  VertRelAngle +  
                          HorzRelAngle + HorzApprAngle + VertApprAngle + 
                          SpinAxis + HorzBreak + 
                          InducedVertBreak + RelSpeed + VertBreak +
                          Extension + RelSide + RelHeight + SpinRate +BatterSide, importance= TRUE, ntree= 600, mtry = 4)

predRightIowa = predict(rfRightIowa, newdata=testRightIowa[-20])

cmRightIowa = table(testRightIowa[,20], predRightIowa)

testLeftIowa$PredictValues <- predict(rfLeftIowa, newdata=testLeftIowa[-20])
testRightIowa$PredictValues <- predict(rfRightIowa, newdata = testRightIowa[-20])

letMeSeIowa <- testLeftIowa[testLeftIowa$PredictValues != testLeftIowa$TaggedPitchType, ]

letMeSeIowa <- letMeSeIowa[, c(6, 20,29:40, 44,45,46,112)]


##Look at whether or not you can tag paired data together
