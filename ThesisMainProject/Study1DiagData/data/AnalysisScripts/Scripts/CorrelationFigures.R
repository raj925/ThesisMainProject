### Correlation between initial differentials and later information proportion

cor <- cor.test(studentAggData$meanInitialDiffs,studentAggData$proportionOfInfo,method="pearson")

diffInfo <- ggplot(data = studentAggData, aes(x=meanInitialDiffs, y=proportionOfInfo)) +
  geom_point() +
  geom_smooth(method=lm , color=differentialColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Initial Diffs against % of Information Sought: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffInfo + 
        ggtitle(title)
      + labs(y="Proportion of Possible Information Requested", x = "Number of Initial Differentials")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))


### Correlation between initial differentials and overall confidence change

cor <- cor.test(studentAggData$meanInitialDiffs,studentAggData$meanConfidenceOverallChange,method="pearson")

diffCon <- ggplot(data = studentAggData, aes(x=meanInitialDiffs, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Initial Diffs against Overall Confidence Change: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Final Confidence - Initial Confidence", x = "Number of Initial Differentials")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))


### Correlation between difficulty and confidence means

cor <- cor.test(studentAggData$meanDifficulty,studentAggData$meanFinalConfidence,method="pearson")

diffInfo <- ggplot(data = studentAggData, aes(x=meanDifficulty, y=meanFinalConfidence)) +
  geom_point() +
  geom_smooth(method=lm , color=difficultyColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Perceived Difficulty Against Final Confidence Ratings: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffInfo + 
        ggtitle(title)
      + labs(y="Final Confidence", x = "Perceived Difficulty Rating")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))


### Correlation between information proportion and confidence changes

cor <- cor.test(studentAggData$proportionOfInfo,studentAggData$meanConfidenceOverallChange,method="pearson")

diffCon <- ggplot(data = studentAggData, aes(x=proportionOfInfo, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Info Seeking % against Change in Confidence: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Final Confidence - Initial Confidence", x = "Proportion of Available Information Sought")
      + theme_classic()
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))


### Correlation between calibration and info seeking

cor <- cor.test(studentAggData$proportionOfInfo,studentAggData$finalBrierScore,method="pearson")

resInfo <- ggplot(data = studentAggData, aes(x=proportionOfInfo, y=finalBrierScore)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Info Seeking Proportion against Calibration: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(resInfo + 
        ggtitle(title)
      + labs(y="Calibration of Confidence (1 - Brier Score)", x = "Proportion of Info")
      +theme(axis.text=element_text(size=18),
             axis.title=element_text(size=16),
             plot.title=element_text(size=16,face="bold")
      ))

### Correlation between calibration and accuracy

cor <- cor.test(studentAggData$meanFinalAccuracy,studentAggData$finalCalibration,method="pearson")

resInfo <- ggplot(data = studentAggData, aes(x=meanFinalAccuracy, y=finalCalibration)) +
  geom_point() +
  geom_smooth(method=lm , color=resolutionColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Accuracy against Calibration: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(resInfo + 
        ggtitle(title)
      + labs(y="Calibration of Confidence", x = "Accuracy (Likelihood Score)")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))



### Correlation between correct diagnosis likelihood and information seeking

cor <- cor.test(studentAggData$meanFinalAccuracy,studentAggData$proportionOfInfo,method="pearson")

diffCon <- ggplot(data = studentAggData, aes(x=proportionOfInfo, y=meanFinalAccuracy)) +
  geom_point() +
  geom_smooth(method=lm , color=likelihoodColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Likelihood of Correct Diagnosis / Info Seeking: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Accuracy", x = "Proportion of Available Information Sought")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))


### Correlation between correct diagnosis likelihood and confidence

cor <- cor.test(studentAggData$meanFinalAccuracy,studentAggData$meanFinalConfidence,method="pearson")

diffCon <- ggplot(data = studentAggData, aes(x=meanFinalConfidence, y=meanFinalAccuracy)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Accuracy / Final Confidence: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Accuracy (Likelihood Score)", x = "Mean Final Confidence")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))

######################################

cor <- cor.test(studentAggData$averageLikelihoodOfCorrectDiagnosisFinal/10,studentAggData$highestLikelihoodCorrectValueFinal,method="pearson")

diffCon <- ggplot(data = studentAggData, aes(x=highestLikelihoodCorrectValueFinal, y=averageLikelihoodOfCorrectDiagnosisFinal/10)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Accuracy Measure Correlations: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Average Likelihood of Correct Diagnosis", x = "Highest Likelihood Correct Value")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))

######################################

# Interactive version
p <- studentAggData %>%
  
  mutate(meanFinalConfidence=round(meanFinalConfidence,1)) %>%
  mutate(meanFinalAccuracy=round(meanFinalAccuracy,1)) %>%
  mutate(proportionOfInfo=round(proportionOfInfo,1)) %>%
  mutate(resolutionPositive=as.factor(resolutionPositive)) %>%
  
  # Reorder countries to having big bubbles on top
  arrange(desc(proportionOfInfo)) %>%
  mutate(participantID = factor(participantID, participantID)) %>%
  
  # Classic ggplot
  ggplot( aes(x=meanFinalConfidence, y=meanFinalAccuracy, size = proportionOfInfo, fill = resolutionPositive, color = resolutionPositive)) +
  geom_point(alpha=0.7, shape=21, color="black") +
  scale_size(range = c(2, 19), name="InfoProportion") +
  scale_color_viridis(discrete=TRUE) +
  theme_classic() +
  labs(y="Average Likelihood of Correct Diagnosis", x = "Mean Final Confidence") +
  theme(axis.text=element_text(size=16),
         axis.title=element_text(size=16))

print(p)

######################################
