data_summary <- function(x) {
  m <- median(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

# Differentials by stage

nPpts <- nrow(aggData)

xb <- c(rep("Geography",nPpts),rep("History & Politics",nPpts), rep("People & Culture",nPpts))
yb <- c(aggData$meanInitialDiffs, aggData$meanMiddleDiffs, aggData$meanFinalDiffs)
dataF <- data.frame("Stage" = xb, "Mean"= yb)
dataF$ID <- rep(c(1:nPpts),3)

dataF$Stage <- as.factor(dataF$Stage)
diffs <- ggplot(dataF, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=differentialColour, alpha=0.8, trim=FALSE) + 
  geom_point(shape=16, position=position_jitter(width = 0.04, seed = 12), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(diffs +
        ggtitle("Average Differentials by Information Seeking Stage") +
        labs(x = "Stage", y = "Average Differentials") +
        theme_classic()) 

res.aov2 <- anova_test(
  data = dataF, dv = Mean, wid = ID, within = Stage
)
get_anova_table(res.aov2)


# Information proportion by stage

yb <- c(aggData$initialPropOfInfo, aggData$middlePropOfInfo, aggData$finalPropOfInfo)
dataF <- data.frame("Stage" = xb, "Mean"= yb)
dataF$ID <- rep(c(1:nPpts),3)
#inf <- ggplot(dataF) +
#  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.7)

dataF$Stage <- as.factor(dataF$Stage)
inf <- ggplot(dataF, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=infoSeekingColour, alpha=0.8, trim=FALSE) + 
  geom_point(shape=16, position=position_jitter(width = 0.04, seed = 12), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")



print(inf +
        ggtitle("Information Gathering by Information Seeking Stage") +
        labs(x = "Stage", y = "Proportion of Available Information Sought") +
        theme_classic()) 

res.aov2 <- anova_test(
  data = dataF, dv = Mean, wid = ID, within = Stage
)
get_anova_table(res.aov2)

# Confidence by stage

yb <- c(aggData$meanInitialConfidence, aggData$meanMiddleConfidence, aggData$meanFinalConfidence)
dataF <- data.frame("Stage" = xb, "Mean"= yb)
inf <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=confidenceColour, alpha=0.7)

dataF$Stage <- as.factor(dataF$Stage)
inf <- ggplot(dataF, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=confidenceColour, alpha=0.8, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.04), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Confidence by Information Seeking Stage") +
        labs(x = "Stage", y = "Mean Confidence") +
        theme_classic()) 

dataF$ID <- rep(c(1:nPpts),3)
res.aov2 <- anova_test(
  data = dataF, dv = Mean, wid = ID, within = Stage
)
get_anova_table(res.aov2)

# Accuracy by stage

yb <- c(aggData$meanInitialAccuracy, aggData$meanMiddleAccuracy, aggData$meanFinalAccuracy)
dataF <- data.frame("Stage" = xb, "Mean"= yb)
#inf <- ggplot(dataF) +
#  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=accuracyColour, alpha=0.7)

dataF$Stage <- as.factor(dataF$Stage)
inf <- ggplot(dataF, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=accuracyColour, alpha=0.8, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.04), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Accuracy by Information Seeking Stage") +
        labs(x = "Stage", y = "Mean Accuracy") +
        theme_classic()) 

dataF$ID <- rep(c(1:nPpts),3)
res.aov2 <- anova_test(
  data = dataF, dv = Mean, wid = ID, within = Stage
)
get_anova_table(res.aov2)

# Likelihood of correct country by stage (include zeros)

yb <- c(aggData$averageLikelihoodOfCorrectCountryInitial, aggData$averageLikelihoodOfCorrectCountryMiddle, aggData$averageLikelihoodOfCorrectCountryFinal)
dataF <- data.frame("Stage" = xb, "Mean"= yb)
dataF$ID <- rep(c(1:nPpts),3)
#inf <- ggplot(dataF) +
#  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=likelihoodColour, alpha=0.7)

dataF$Stage <- as.factor(dataF$Stage)
inf <- ggplot(dataF, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=likelihoodColour, alpha=0.8, trim=FALSE) + 
  geom_point(shape=16, position=position_jitter(width = 0.04, seed = 12), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Likelihood of Correct Country by Information Seeking Stage") +
        labs(x = "Stage", y = "Likelihood Rating") +
        theme_classic()) 

res.aov2 <- anova_test(
  data = dataF, dv = Mean, wid = ID, within = Stage
)
get_anova_table(res.aov2)


# Likelihood of incorrect countries by stage (include zeros)


yb <- c(aggData$incorrectLikelihoodInitial, aggData$incorrectLikelihoodMiddle, aggData$incorrectLikelihoodFinal)
dataF <- data.frame("Stage" = xb, "Mean"= yb)

dataF$Stage <- as.factor(dataF$Stage)
inf <- ggplot(dataF, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=likelihoodColour, alpha=0.8, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.04), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Likelihood of Incorrect Countries by Information Seeking Stage") +
        labs(x = "Stage", y = "Likelihood Rating") +
        theme_classic()) 

dataF$ID <- rep(c(1:nPpts),3)
res.aov2 <- anova_test(
  data = dataF, dv = Mean, wid = ID, within = Stage
)
get_anova_table(res.aov2)

######

nPpts <- nrow(aggData)
rootN <- sqrt(nPpts)

xb <- c("Geo","Pol/His", "Peo/Cul")
yb <- c(mean(aggData$meanInitialConfidence)/100, mean(aggData$meanMiddleConfidence)/100, mean(aggData$meanFinalConfidence)/100)
zb <- c(mean(aggData$meanInitialAccuracy), mean(aggData$meanMiddleAccuracy), mean(aggData$meanFinalAccuracy))

val <- c(yb,zb)
typ <- c(rep("Confidence",3),rep("Accuracy",3))

secon <- c(sd(aggData$meanInitialConfidence/100)/rootN, sd(aggData$meanMiddleConfidence/100)/rootN, sd(aggData$meanFinalConfidence/100)/rootN)
selik <- c(sd(aggData$meanInitialAccuracy)/rootN, sd(aggData$meanMiddleAccuracy)/rootN, sd(aggData$meanFinalAccuracy)/rootN)

ses <- c(secon,selik)

dataV <- data.frame("Stage" = xb, "Value"= val, "Type"= typ, "se" = ses)

p <- ggplot(dataV, aes(x = Stage, y = Value, group = Type, color = Type )) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin=Value-se, ymax=Value+se), width=.2, position=position_dodge(0.05)) +
  labs(title="   ",x="Stage",y="% Value") +
  theme_classic() +
  scale_color_manual(values = c(accuracyColour,confidenceColour)) +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18),
        plot.title=element_text(size=20,face="bold"),
        legend.text = element_text(size = 18),
        line = element_blank())

print(p)

xb <- c(rep("G",nPpts),rep("PH",nPpts), rep("PC",nPpts))
yb <- c(aggData$meanInitialConfidence/100, aggData$meanMiddleConfidence/100, aggData$meanFinalConfidence/100)
zb <- c(aggData$meanInitialAccuracy, aggData$meanMiddleAccuracy, aggData$meanFinalAccuracy)

allDataDf <- data.frame("Stage" = xb, "Confidence"= yb, "Accuracy"= zb)

print(t.test(allDataDf[allDataDf$Stage=="PC",]$Confidence,allDataDf[allDataDf$Stage=="PC",]$Accuracy))




