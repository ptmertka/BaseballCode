library(caret)
require(caTools)
library(dplyr)
library(tidyr)
library(e1071)
library(pscl)
library(car)
library(InformationValue)
library(MASS)


NCAAData <- read.csv("C:/Users/peter/Downloads/Baseball/NCAA_TM_CLEAN (1).csv")


NCAAData <- NCAAData %>%
  drop_na(c(8,12,20,29:46, 93, 96:100))



sampleSwing = sample.split(NCAAData$Swing, SplitRatio = .70)
trainSwing = subset(NCAAData, sampleSwing == TRUE)
testSwing = subset(NCAAData, sampleSwing == FALSE)




modelSwing <- glm(Swing~Extension + InZone + 
                    ZoneNo + 
                    ZoneTime + ZoneSpeed +
                SpinRate +SpinAxis + RelSpeed  + 
               HorzBreak +  InducedVertBreak + VertApprAngle + HorzApprAngle + 
               VertRelAngle + HorzRelAngle + PlateLocSide + Strikes
               + PitchofPA , family="binomial", data=trainSwing)

summary(modelSwing)

plot(modelSwing, which = 4, id.n = 5)





pR2(modelSwing)["McFadden"]
varImp(modelSwing)


vif(modelSwing)

testSwing$modelPrediction <- predict(modelSwing, testSwing, type = "response")

predicted <- predict(modelSwing, testSwing, type="response")


optimal <- optimalCutoff(testSwing$Swing, predicted )[1]
optimal

confusionMatrix(testSwing$Swing, predicted)


sensitivity(testSwing$Swing, predicted)

#calculate specificity
specificity(testSwing$Swing, predicted)

#calculate total misclassification error rate
misClassError(testSwing$Swing, predicted, threshold=optimal)

plotROC(testSwing$Swing, predicted)


swings <- NCAAData %>%
  filter(Swing == "TRUE")

swings <- mutate(swings, Whiff = ifelse(PitchCall=="StrikeSwinging", 1, 0))

sampleWhiff = sample.split(swings$Whiff, SplitRatio = .70)
trainWhiff = subset(swings, sampleWhiff == TRUE)
testWhiff= subset(swings, sampleWhiff == FALSE)


whiffModel <- glm(Whiff~Extension + InZone + 
                    ZoneNo + 
                    ZoneTime + ZoneSpeed +
                    SpinRate +SpinAxis + RelSpeed  + 
                    HorzBreak +  InducedVertBreak + VertApprAngle + HorzApprAngle + 
                    VertRelAngle + HorzRelAngle + PlateLocSide + Strikes
                  + PitchofPA 
                    , family="binomial", data=trainWhiff)
summary(whiffModel)

pR2(whiffModel)["McFadden"]
varImp(whiffModel)


vif(whiffModel)

testWhiff$modelPrediction <- predict(whiffModel, testWhiff, type = "response")

predicted <- predict(whiffModel, testWhiff, type="response")


optimal <- optimalCutoff(testWhiff$Whiff, predicted )[1]
optimal

confusionMatrix(testWhiff$Whiff, predicted)


sensitivity(testWhiff$Whiff, predicted)

#calculate specificity
specificity(testWhiff$Whiff, predicted)

#calculate total misclassification error rate
misClassError(testWhiff$Whiff, predicted, threshold=optimal)

plotROC(testWhiff$Whiff, predicted)


FastballSwings <- NCAAData %>%
  filter(PitchGroup == "Fastball")

BreakingballSwings <- NCAAData %>%
  filter(PitchGroup == "Breaking Ball")

OffSpeedSwings <- NCAAData %>%
  filter(PitchGroup == "Offspeed")

FastballWhiffs <- swings %>%
  filter(PitchGroup == "Fastball")

BreakingBallWhiffs <- swings %>%
  filter(PitchGroup == "Breaking Ball")

OffSpeedWhiffs <- swings %>%
  filter(PitchGroup == "Offspeed")


sampleFBSwing = sample.split(FastballSwings$Swing, SplitRatio = .70)
trainFBSwing = subset(FastballSwings, sampleFBSwing == TRUE)
testFBSwing = subset(FastballSwings, sampleFBSwing == FALSE)

modelFBSwing <- glm(Swing~ Extension + InZone + 
                      ZoneNo + 
                      ZoneTime + ZoneSpeed +
                      RelSpeed  + 
                      InducedVertBreak + VertApprAngle +  
                      VertRelAngle + PlateLocSide + Strikes
                      , family="binomial", data=trainFBSwing)

pR2(modelFBSwing)["McFadden"]
varImp(modelFBSwing)


vif(modelFBSwing)

testFBSwing$modelPrediction <- predict(modelFBSwing, testFBSwing, type = "response")

predicted <- predict(modelFBSwing, testFBSwing, type="response")


optimal <- optimalCutoff(testFBSwing$Swing, predicted )[1]
optimal

confusionMatrix(testFBSwing$Swing, predicted)


sensitivity(testFBSwing$Swing, predicted)

#calculate specificity
specificity(testFBSwing$Swing, predicted)

#calculate total misclassification error rate
misClassError(testFBSwing$Swing, predicted, threshold=optimal)

plotROC(testFBSwing$Swing, predicted)


sampleOSSwing = sample.split(OffSpeedSwings$Swing, SplitRatio = .70)
trainOSSwing = subset(OffSpeedSwings, sampleOSSwing == TRUE)
testOSSwing = subset(OffSpeedSwings, sampleOSSwing == FALSE)

modelOSSwing <- glm(Swing~Extension + InZone + 
                      ZoneNo + 
                      ZoneTime + ZoneSpeed +
                      SpinRate +SpinAxis + RelSpeed  + 
                      HorzBreak +  InducedVertBreak + VertApprAngle + HorzApprAngle + 
                      VertRelAngle + HorzRelAngle + PlateLocSide  + PlateLocHeight+ Strikes
                    + PitchofPA
                    , family="binomial", data=trainOSSwing)

pR2(modelOSSwing)["McFadden"]
varImp(modelOSSwing)


vif(modelOSSwing)

testOSSwing$modelPrediction <- predict(modelOSSwing, testOSSwing, type = "response")

predicted <- predict(modelOSSwing, testOSSwing, type="response")


optimal <- optimalCutoff(testOSSwing$Swing, predicted )[1]
optimal

confusionMatrix(testOSSwing$Swing, predicted)


sensitivity(testOSSwing$Swing, predicted)

#calculate specificity
specificity(testOSSwing$Swing, predicted)

#calculate total misclassification error rate
misClassError(testOSSwing$Swing, predicted, threshold=optimal)

plotROC(testOSSwing$Swing, predicted)














