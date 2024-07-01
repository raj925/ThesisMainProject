trainingDataStrats <- infoSeekingMatrixTA
testingDataStrats <- infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantType=="p",]

testingDataStrats$initialDiagnoses <- studentCaseDf$initialDifferentials
testingDataStrats$confidenceChange <- studentCaseDf$confidenceChange
testingDataStrats$differentialChange <- studentCaseDf$differentialChange
testingDataStrats$likelihoodAcc <- studentCaseDf$likelihoodOfCorrectDiagnosis

trainingDataStrats <- trainingDataStrats[trainingDataStrats$Strat!="NONE",]

trainingDataStrats$HDLabel <- ifelse(trainingDataStrats$Strat=="HD",1,0)
trainingDataStrats$PRLabel <- ifelse(trainingDataStrats$Strat=="PR",1,0)
trainingDataStrats$HDLabel <- as.factor(trainingDataStrats$HDLabel)
trainingDataStrats$PRLabel <- as.factor(trainingDataStrats$PRLabel)

trainingDataStrats$CaseLabel <- as.factor(trainingDataStrats$Strat)

colnames(trainingDataStrats)[1:29] <- c("T1", "T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                      "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                      "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22",
                                      "T23", "T24", "T25", "T26", "T27", "T28", "T29")

colnames(testingDataStrats)[1:29] <- c("T1", "T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                        "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                        "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22",
                                        "T23", "T24", "T25", "T26", "T27", "T28", "T29")


thresh<-seq(0,1,0.001)
#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV", number=90, savePredictions = TRUE)

set.seed(1001)

#######################################

library(nnet)

trainingDataStrats$stratLabel <- as.factor(trainingDataStrats$Strat)

model <- train(stratLabel ~ Condition + T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 +
                    T11 + T12 + T13 + T14 + T15 + T16 + T17 +  T18 + T19 + T20 +
                    T21 + T22 + T23 + T24 + T25 + T26 + T27 + T28 + T29, method = "multinom", data = trainingDataStrats, trControl = ctrl)

# Make predictions
predictions <- predict(model, newdata = testingDataStrats)

testingDataStrats$classifiedStrat <- predictions


predictionsMatrix <- model$pred

# Count the number of times each class label is predicted
classCounts <- table(predictions)

# Compute the proportion of classifiers that predicted each class label
classProportions <- prop.table(classCounts)


############################
testingDataStratsExp <- infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantType=="e",]

colnames(testingDataStratsExp)[1:29] <- c("T1", "T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                       "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                       "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22",
                                       "T23", "T24", "T25", "T26", "T27", "T28", "T29")

testingDataStratsExp$initialDiagnoses <- expertCaseDf$initialDifferentials
testingDataStratsExp$confidenceChange <- expertCaseDf$confidenceChange
testingDataStratsExp$differentialChange <- expertCaseDf$differentialChange
testingDataStratsExp$infoProp <- expertCaseDf$caseInformationProportion
testingDataStratsExp$LikelihoodAcc <- expertCaseDf$likelihoodOfCorrectDiagnosis
testingDataStratsExp$Correct <- expertCaseDf$correct

# Make predictions
predictions <- predict(model, newdata = testingDataStratsExp)

testingDataStratsExp$classifiedStrat <- predictions

classifiedStratBreakdownExp  <- testingDataStratsExp %>%
  group_by(Condition,classifiedStrat) %>%
  dplyr::mutate(N = n()) %>%
  dplyr::summarise(n = mean(N),
                   Accuracy = mean(LikelihoodAcc),
                   InfoAmount = mean(infoProp),
                   InitialDiagnoses = mean(initialDiagnoses),
                   confidenceChange = mean(confidenceChange))

######################################

testingDataStrats$value <- temp$infoValue
testingDataStrats$infoAmount <- rowSums(testingDataStrats[,c(1:29)])/29

testingPptBreakdown <- testingDataStrats %>%
  group_by(classifiedStrat) %>%
  dplyr::mutate(N = n()) %>%
  dplyr::summarise(n = mean(N),
                   Accuracy = mean(likelihoodAcc),
                   InfoAmount = mean(infoAmount),
                   InfoValue = mean(value),
                   confidenceChange = mean(confidenceChange),
                   InitialDiagnoses = mean(initialDiagnoses),
                   differentialChange = mean(differentialChange))


model <- lm(likelihoodAcc ~ classifiedStrat*initialDiagnoses,data=testingDataStrats) 
summary(model)

library(interactions) 
intplot <- interact_plot(model, pred = initialDiagnoses, modx = classifiedStrat) +
  labs(y="Accuracy", x = "Initial Diagnoses", colour = "Reasoning Strategy") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        legend.title=element_text(size=16),
        legend.text=element_text(size=16))
print(intplot)


# Mixed effect models

# HD
modelData <- testingDataStrats[testingDataStrats$classifiedStrat=="HD",]
model <- lmerTest::lmer(confidenceChange ~ initialDiagnoses + infoAmount + (1 | Condition) + (1 | ID), data=modelData)
summary(model)

# PR - initial diagnoses

modelData <- testingDataStrats[testingDataStrats$classifiedStrat=="PR",]
model <- lmerTest::lmer(confidenceChange ~ initialDiagnoses + infoAmount + (1 | Condition) + (1 | ID), data=modelData)
summary(model)

# SI - information seeking

modelData <- testingDataStrats[testingDataStrats$classifiedStrat=="SI",]
model <- lmerTest::lmer(confidenceChange ~ initialDiagnoses + infoAmount + (1 | Condition) + (1 | ID), data=modelData)
summary(model)

########################################

classifiedStratBreakdown  <- testingDataStrats %>%
  group_by(Condition,classifiedStrat) %>%
  dplyr::mutate(N = n()) %>%
  dplyr::summarise(n = mean(N),
                   Accuracy = mean(LikelihoodAcc),
                   Value = mean(value),
                   InfoAmount = mean(infoAmount),
                   InitialDiagnoses = mean(initialDiagnoses),
                   confidenceChange = mean(confidenceChange))

prDists <- c()
hdDists <- c()

for (i in 1:nrow(classifiedStratBreakdown))
{
  strat <- classifiedStratBreakdown$classifiedStrat[i]
  stratTrials <- rownames(testingDataStrats[testingDataStrats$classifiedStrat==strat,])
  
  rows <- rownames(distances) %in% stratTrials
  cols <- colnames(distances) %in% stratTrials
  rc <- rows&cols
  
  stratDistances <- distances[rc, rc]
  
  if (strat == "PR")
  {
    prDists <- stratDistances
  }
  if (strat == "HD")
  {
    hdDists <- stratDistances
  }
  classifiedStratBreakdown$infoVariance[i] <- mean(as.matrix(stratDistances))
}

t.test(prDists,hdDists)

########################################

strats <- c("HD","PR","SI")
stratConsistency <- testingDataStrats %>%
  group_by(ID) %>%
  dplyr::summarise(n = max(c(sum(classifiedStrat=="HD"),sum(classifiedStrat=="PR"),sum(classifiedStrat=="SI"))),
                   MostUsedStrat = strats[which.max(c(sum(classifiedStrat=="HD"),sum(classifiedStrat=="PR"),sum(classifiedStrat=="SI")))],
                  MeanCorrect = mean(Correct),
                   Accuracy = mean(likelihoodAcc),
                  InitialDiagnoses = mean(initialDiagnoses),
                   Value = mean(value))

stratConsistency$consistentStrat <- ifelse(stratConsistency$n<4,"Inconsistent",
                                           ifelse(stratConsistency$MostUsedStrat=="HD","HD",
                                                  ifelse(stratConsistency$MostUsedStrat=="PR","PR",
                                                         ifelse(stratConsistency$MostUsedStrat=="SI","SI","Inconsistent"))))


stratConsistency$Variance <- infoSeekingDf$MDSDistanceVariance
stratConsistency$InfoAmount <- infoSeekingDf$InformationSeekingProportion

HDUseAcc <- stratConsistency %>%
  group_by(n) %>%
  dplyr::mutate(N = n()) %>%
  dplyr::summarise(num = mean(N),
                  MeanCorrect = mean(MeanCorrect),
                  Accuracy = mean(Accuracy),
                   Value = mean(Value),
                  Variance = mean(Variance),
                  InfoAmount = mean(InfoAmount))


model <- lm(Accuracy ~ n, data=stratConsistency)
summary(model)

data <- data.frame(
  var=c(rep("Accuracy" , nrow(HDUseAcc)) , rep("Value" , nrow(HDUseAcc)) ),
  val=c(HDUseAcc$Accuracy,HDUseAcc$Value),
  HDCases=c(HDUseAcc$n,HDUseAcc$n)
)

p <- ggplot(data) +
  geom_bar( aes(fill=var, x=HDCases, y=val), stat="identity", alpha=0.5,position="dodge") +
  theme_classic()

plot(p)

