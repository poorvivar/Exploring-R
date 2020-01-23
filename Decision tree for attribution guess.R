#import libary
library(dplyr)


#reading the file
EssayData <- read.csv("fedPapers85.csv")

View(EssayData)

EssayData =  EssayData[-c(2)]

EssayData$author = dplyr::recode(EssayData$author, Hamilton=1, Jay=2, HM=3, Madison=4, dispt = 5)

EssayDataKnown = EssayData[12:85,]


#Splitting Training

library(caTools)
set.seed(1)
split = sample.split(EssayDataKnown$author, SplitRatio = 0.75)
trainiofKnown = subset(EssayDataKnown, split == TRUE)
testofknown = subset(EssayDataKnown, split == FALSE)

View(testofknown)
unknown = EssayData[1:11,]
View(unknown)

#feature scaling

trainiofKnown[-1] = scale(trainiofKnown[-1])
testofknown[-1] = scale(testofknown[-1])

library(rpart)

classifier = rpart(formula = author ~.,
                   data = trainiofKnown)
y_pred = predict(classifier, newdata = unknown[-1])
y_pred

plot(classifier)

