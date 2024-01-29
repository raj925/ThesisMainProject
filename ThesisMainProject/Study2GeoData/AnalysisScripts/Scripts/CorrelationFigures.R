### Correlation between initial differentials and later information proportion

cor <- cor.test(aggData$meanInitialDiffs,aggData$meanFinalConfidence,method="pearson")

diffInfo <- ggplot(data = aggData, aes(x=meanInitialDiffs, y=laterPropOfInfo)) +
  geom_point() +
  geom_smooth(method=lm , color=differentialColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Initial Differentials against Later Proportion of Information: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffInfo + 
        ggtitle(title)
      + labs(y="Proportion of Possible Information Requested", x = "Number of Initial Differentials"))


### Correlation between initial differentials and overall confidence change

cor <- cor.test(aggData$meanInitialDiffs,aggData$meanConfidenceOverallChange,method="pearson")

diffCon <- ggplot(data = aggData, aes(x=meanInitialDiffs, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Initial Differentials against Overall Confidence Change: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Final Confidence - Initial Confidence", x = "Number of Initial Differentials"))


### Correlation between difficulty and confidence means

cor <- cor.test(aggData$meanDifficulty,aggData$meanFinalConfidence,method="pearson")

diffInfo <- ggplot(data = aggData, aes(x=meanDifficulty, y=meanFinalConfidence)) +
  geom_point() +
  geom_smooth(method=lm , color=difficultyColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Perceived Difficulty Against Final Confidence Ratings: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffInfo + 
        ggtitle(title)
      + labs(y="Final Confidence", x = "Perceived Difficulty Rating"))


### Correlation between information proportion and confidence changes

cor <- cor.test(aggData$proportionOfInfo,aggData$meanConfidenceOverallChange,method="pearson")

diffCon <- ggplot(data = aggData, aes(x=proportionOfInfo, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Information Seeking Proportion against Change in Confidence: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Confidence Change (Final Confidence - Initial Confidence)", x = "Proportion of Available Information Sought")
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=16,face="bold")
      ))


### Correlation between resolution and info seeking

cor <- cor.test(aggData$proportionOfInfo,aggData$finalResolution,method="pearson")

resInfo <- ggplot(data = aggData, aes(x=proportionOfInfo, y=finalResolution)) +
  geom_point() +
  geom_smooth(method=lm , color=resolutionColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Information Seeking Proportion against Resolution: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(resInfo + 
        ggtitle(title)
      + labs(y="Difference in Confidence when Correct and Incorrect", x = "Proportion of Info"))



### Correlation between correct diagnosis likelihood and information seeking

cor <- cor.test(aggData$averageLikelihoodOfCorrectCountry,aggData$laterPropOfInfo,method="pearson")

diffCon <- ggplot(data = aggData, aes(x=laterPropOfInfo, y=averageLikelihoodOfCorrectCountry)) +
  geom_point() +
  geom_smooth(method=lm , color=likelihoodColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Likelihood for Correct Diagnosis against Information Seeking: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Likelihood of Correct Diagnosis", x = "Information Seeking Proportion"))


### Correlation between final confidence and accuracy

cor <- cor.test(aggData$meanFinalConfidence,aggData$averageLikelihoodOfCorrectCountryFinal,method="pearson")

diffCon <- ggplot(data = aggData, aes(x=meanFinalConfidence, y=averageLikelihoodOfCorrectCountryFinal)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Final Confidence against Likelihood Score (Accuracy): ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Likelihood of Correct Country", x = "Final Confidence"))



### Correlation between geographic score and accuracy

cor <- cor.test(aggData$geoScore,aggData$averageLikelihoodOfCorrectCountryFinal,method="pearson")

diffCon <- ggplot(data = aggData, aes(x=geoScore, y=averageLikelihoodOfCorrectCountryFinal)) +
  geom_point() +
  geom_smooth(method=lm , color=likelihoodColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Geographic Knowledge against Likelihood Score (Accuracy): ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Likelihood of Correct Country", x = "Geographic Knowledge"))

######################################
# Look at information seeking distance variance against proportion

distanceVars <- c()
infoProps <- c()
likCors <- c()
resolutions <- c()
geoKnowledges <- c()
rationalism <- c()
for (n in 1:nrow(aggData))
{
  ppt <- paste('p', n, sep="")
  compareColumns <- distances[grep(ppt, rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  distanceVars[n] <- (sd(compareColumns))^2
  infoProps[n] <- aggData$proportionOfInfo[n]
  likCors[n] <- aggData$averageLikelihoodOfCorrectCountry[n]
  resolutions[n] <- aggData$finalResolution[n]
  geoKnowledges[n] <- aggData$geoScore[n]
  rationalism[n] <- aggData$relativeRationalism[n]
}
infoSeekingDf <- data.frame(distanceVars,infoProps,likCors,resolutions,geoKnowledges,rationalism)
colnames(infoSeekingDf) <- c("MDSDistanceVariance", "InformationSeekingProportion","LikelihoodOfCorrectCountry","Resolution","GeographicKnowledge", "RelativeRationalism")

cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$InformationSeekingProportion,method="pearson")

msdCorr <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=InformationSeekingProportion)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("MSD Distance Variance Against Information Seeking Proportion: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(msdCorr + 
        ggtitle(title)
      + labs(y="Proportion of Possible Information Requested", x = "Variance in Participant's MSD Distances"))
#############


cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$LikelihoodOfCorrectCountry,method="pearson")

msdLik <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=LikelihoodOfCorrectCountry)) +
  geom_point() +
  geom_smooth(method=lm , color=likelihoodColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("MSD Distance Variance Against Likelihood Of Correct Diagnosis: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(msdLik + 
        ggtitle(title)
      + labs(y="Likelihood Of Correct Diagnosis", x = "Variance in Participant's MSD Distances"))

###############

cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$Resolution,method="pearson")

resVar <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=Resolution)) +
  geom_point() +
  geom_smooth(method=lm , color=resolutionColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("MSD Distance Variance Against Resolution: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(resVar + 
        ggtitle(title)
      + labs(y="Confidence when Correct - Confidence when Incorrect", x = "Variance in Participant's MSD Distances"))

#############


cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$RelativeRationalism,method="pearson")

resVar <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=RelativeRationalism)) +
  geom_point() +
  geom_smooth(method=lm , color=resolutionColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("MSD Distance Variance Against Relative Rationalism: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(resVar + 
        ggtitle(title)
      + labs(y="Rationalism Scale Score Relative to Intuition Score", x = "Variance in Participant's MSD Distances"))

###################

summary(lm(meanConfidenceChangeStage2 ~ middlePropOfInfo, data = aggData))
summary(lm(meanConfidenceChangeStage3 ~ finalPropOfInfo, data = aggData))


confchange2 <- ggplot(data = aggData, aes(x=middlePropOfInfo, y=meanConfidenceChangeStage2)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(confchange2 + 
        ggtitle("Change in Confidence in Stage 2 against Information Seeking")
      + labs(y="Difference in Confidence Between Stages 1 and 2", x = "Information Sought in Stage 2"))


confchange3 <- ggplot(data = aggData, aes(x=finalPropOfInfo, y=meanConfidenceChangeStage3)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(confchange3 + 
        ggtitle("Change in Confidence in Stage 3 against Information Seeking")
      + labs(y="Difference in Confidence Between Stages 2 and 3", x = "Information Sought in Stage 3"))


nPpts <- nrow(aggData)

xb <- c(rep("First Confidence Change",nPpts),rep("Second Confidence Change",nPpts))
yb <- c(aggData$meanConfidenceChangeStage2, aggData$meanConfidenceChangeStage3)
dataF <- data.frame("Stage" = xb, "Mean"= yb)

dataF$Stage <- as.factor(dataF$Stage)
diffs <- ggplot(dataF, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=confidenceColour, alpha=0.8, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.03), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(diffs +
        ggtitle("Confidence Change by Stages") +
        labs(x = "Stage", y = "Change in Confidence") +
        theme_classic()) 

t.test(aggData$meanConfidenceChangeStage2, aggData$meanConfidenceChangeStage3, paired = TRUE)
