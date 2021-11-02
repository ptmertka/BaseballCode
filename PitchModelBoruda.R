install.packages("Rcpp")

library(Rcpp)
library(Boruta)
library(dplyr)
library(tidyr)


NCAAData <- read.csv("C:/Users/peter/Downloads/Baseball/NCAA_TM_CLEAN.csv")

NCAAData$TaggedPitchType <- as.factor(NCAAData$TaggedPitchType)

NCAAData <- NCAAData %>%
  drop_na(c(29:40), 44,45,46)



Lefties <- NCAAData %>%
  filter(PitcherThrows == "Left")
  
  
randomSet <- sample_n(Lefties, 150) 


Boruta_Point = Boruta(TaggedPitchType~RelSpeed+VertRelAngle+HorzRelAngle+SpinRate+SpinAxis+
                        Tilt+RelHeight+RelSide+Extension+VertBreak+InducedVertBreak+HorzBreak+ZoneSpeed+VertApprAngle+
                        HorzApprAngle+ZoneTime, data = randomSet)

plot(Boruta_Point, las=2, xlab="")