
# Differentials by stage

nPpts <- nrow(studentAggData)
#diffs <- ggplot(dataF) +
#geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=differentialColour, alpha=0.7)


xb <- c(rep("Patient History",nPpts),rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(studentAggData$meanInitialDiffs, studentAggData$meanMiddleDiffs, studentAggData$meanFinalDiffs)
dataV <- data.frame("Stage" = xb, "Mean"= yb)
dataV$Stage <- as.factor(dataV$Stage)
diffs <- ggplot(dataV, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=differentialColour, alpha=0.8, trim=FALSE) + 
  # geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(diffs +
        ggtitle("Average Differentials by Information Seeking Stage") +
        labs(x = "Stage", y = "Average Differentials") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
               axis.title=element_text(size=18),
               plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )
      ) 

dataV$Stage <- as.numeric(dataV$Stage)
model <- lm(Mean ~ Stage, data=dataV)
print(summary(model))

dataV$ID <- rep(c(1:nPpts),3)
res.aov2 <- anova_test(
   data = dataV, dv = Mean, wid = ID, within = Stage
 )
 get_anova_table(res.aov2)

 differentialSummary <- dataV %>%
   group_by(Stage) %>%
   dplyr::mutate(N = n()) %>%
   dplyr::summarise(m_N = mean(N),         
                    m_val = mean(Mean),
                    sd_val = sd(Mean),
                    min_val = min(Mean),
                    max_val = max(Mean))
 
 differentialSummary
 

# Information proportion by stage

nPpts <- nrow(studentAggData)
#inf <- ggplot(dataF) +
#  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.7)


xb <- c(rep("Patient History",nPpts),rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(studentAggData$initialPropOfInfo, studentAggData$middlePropOfInfo, studentAggData$finalPropOfInfo)
dataV <- data.frame("Stage" = xb, "Mean"= yb)
dataV$Stage <- as.factor(dataV$Stage)
inf <- ggplot(dataV, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=infoSeekingColour, alpha=0.8, trim=FALSE) + 
  geom_dotplot(binaxis='y', stackdir='center', dotsize=0.5,colour="white") +
  stat_summary(fun.data=data_summary, colour="blue")

print(inf +
        ggtitle("Information Gathering by Information Seeking Stage") +
        labs(x = "Stage", y = "Proportion of Available Information Sought") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )) 


dataV$Stage <- as.numeric(dataV$Stage)
#model <- lm(Mean ~ Stage, data=dataV)
#print(summary(model))
 
 dataV$ID <- rep(c(1:nPpts),3)
 res.aov2 <- anova_test(
   data = dataV, dv = Mean, wid = ID, within = Stage
 )
 get_anova_table(res.aov2)
 
 infoSeekingSummary <- dataV %>%
   group_by(Stage) %>%
   dplyr::mutate(N = n()) %>%
   dplyr::summarise(m_N = mean(N),         
                    m_val = mean(Mean),
                    sd_val = sd(Mean),
                    min_val = min(Mean),
                    max_val = max(Mean))
 
 infoSeekingSummary

# Confidence by stage

nPpts <- nrow(studentAggData)
#inf <- ggplot(dataF) +
#  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=confidenceColour, alpha=0.7)

xb <- c(rep("Patient History",nPpts),rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(studentAggData$meanInitialConfidence/100, studentAggData$meanMiddleConfidence/100, studentAggData$meanFinalConfidence/100)
dataV <- data.frame("Stage" = xb, "Mean"= yb)
dataV$Stage <- as.factor(dataV$Stage)
inf <- ggplot(dataV, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=confidenceColour, alpha=0.8, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.05), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Confidence by Information Seeking Stage") +
        labs(x = "Stage", y = "Mean Confidence") +
        theme_classic() +
        ylim(c(0,1)) +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )) 


dataV$Stage <- as.numeric(dataV$Stage)
#model <- lm(Mean ~ Stage, data=dataV)
#print(summary(model))

dataV$ID <- rep(c(1:nPpts),3)
res.aov2 <- anova_test(
   data = dataV, dv = Mean, wid = ID, within = Stage
 )
 get_anova_table(res.aov2)
 
 confidenceSummary <- dataV %>%
   group_by(Stage) %>%
   dplyr::mutate(N = n()) %>%
   dplyr::summarise(m_N = mean(N),         
                    m_val = mean(Mean),
                    sd_val = sd(Mean),
                    min_val = min(Mean),
                    max_val = max(Mean))
 
 confidenceSummary
 

# Accuracy by stage

nPpts <- nrow(aggData)

xb <- c(rep("Patient History",nPpts),rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(aggData$meanInitialAccuracy, aggData$meanMiddleAccuracy, aggData$meanFinalAccuracy)
dataV <- data.frame("Stage" = xb, "Mean"= yb)
dataV$Stage <- as.factor(dataV$Stage)
inf <- ggplot(dataV, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=accuracyColour, alpha=0.8, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.05), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Accuracy by Information Seeking Stage") +
        labs(x = "Stage", y = "Mean Accuracy") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )) 

dataV$Stage <- as.numeric(dataV$Stage)
model <- lm(Mean ~ Stage, data=dataV)
print(summary(model))

# Differential Accuracy by stage

nPpts <- nrow(studentAggData)

xb <- c(rep("Patient History",nPpts),rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(studentAggData$meanInitialCorrect, studentAggData$meanMiddleCorrect, studentAggData$meanFinalCorrect)
dataV <- data.frame("Stage" = xb, "Mean"= yb)
dataV$Stage <- as.factor(dataV$Stage)
inf <- ggplot(dataV, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=accuracyColour, alpha=0.8, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.05), colour="white") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Differential Accuracy by Information Seeking Stage") +
        labs(x = "Stage", y = "Mean Accuracy") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )) 

dataV$Stage <- as.numeric(dataV$Stage)
model <- lm(Mean ~ Stage, data=dataV)
print(summary(model))

dataV$ID <- rep(c(1:nPpts),3)
res.aov2 <- anova_test(
  data = dataV, dv = Mean, wid = ID, within = Stage
)
get_anova_table(res.aov2)

differentialAccuracySummary <- dataV %>%
  group_by(Stage) %>%
  dplyr::mutate(N = n()) %>%
  dplyr::summarise(m_N = mean(N),         
                   m_val = mean(Mean),
                   sd_val = sd(Mean),
                   min_val = min(Mean),
                   max_val = max(Mean))

differentialAccuracySummary

# Calibration by stage

nPpts <- nrow(studentAggData)

xb <- c(rep("Patient History",nPpts),rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(studentAggData$initialBrierScore, studentAggData$middleBrierScore, studentAggData$finalBrierScore)
dataV <- data.frame("Stage" = xb, "Mean"= yb)
dataV$Stage <- as.factor(dataV$Stage)
inf <- ggplot(dataV, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=resolutionColour, alpha=1, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.1), colour="black") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Calibration by Information Seeking Stage") +
        labs(x = "Stage", y = "Confidence Brier Score") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )) 

dataV$Stage <- as.numeric(dataV$Stage)
model <- lm(Mean ~ Stage, data=dataV)
print(summary(model))


###########################
# Meyer Graph

nPpts <- nrow(studentAggData)
rootN <- sqrt(nPpts)

xb <- c("Patient History","Physical Exams", "Testing")
yb <- c(mean(studentAggData$meanInitialConfidence), mean(studentAggData$meanMiddleConfidence), mean(studentAggData$meanFinalConfidence))
zb <- c(mean(studentAggData$meanInitialAccuracy), mean(studentAggData$meanMiddleAccuracy), mean(studentAggData$meanFinalAccuracy))

val <- c(yb,zb)
typ <- c(rep("Confidence",3),rep("Accuracy",3))

secon <- c(sd(studentAggData$meanInitialConfidence)/rootN, sd(studentAggData$meanMiddleConfidence)/rootN, sd(studentAggData$meanFinalConfidence)/rootN)
seacc <- c(sd(studentAggData$meanInitialAccuracy)/rootN, sd(studentAggData$meanMiddleAccuracy)/rootN, sd(studentAggData$meanFinalAccuracy)/rootN)

ses <- c(secon, seacc)

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
                             legend.text = element_text(size = 18))

print(p)

######################

nPpts <- nrow(studentAggData)

xb <- c(rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(studentAggData$stage2DiffsAdded, studentAggData$stage3DiffsAdded)
dataV <- data.frame("Stage" = xb, "Mean"= yb)
dataV$Stage <- as.factor(dataV$Stage)
inf <- ggplot(dataV, aes(x=Stage, y=Mean)) +
  geom_violin(colour="black", fill=differentialColour, alpha=1, trim=FALSE) + 
  geom_jitter(shape=16, position=position_jitter(0.1), colour="black") +
  stat_summary(fun.data=data_summary, colour="red")

print(inf +
        ggtitle("Differentials Added in Later Stages") +
        labs(x = "Stage", y = "Differentials Added/Removed") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )) 

######################
# Differentials for high and low accuracy participants

nPpts <- nrow(aggData)

xb <- c(rep("Patient History",nPpts),rep("Physical Examination",nPpts), rep("Testing",nPpts))
yb <- c(aggData$meanInitialDiffs, aggData$meanMiddleDiffs, aggData$meanFinalDiffs)
accs <- c(aggData$meanFinalAccuracy, aggData$meanFinalAccuracy, aggData$meanFinalAccuracy)
medSplit <- median(aggData$meanFinalAccuracy)
dataV <- data.frame("Stage" = xb, "Mean"= yb, "Accuracy" = accs)
dataV$Stage <- as.factor(dataV$Stage)
dataV$AccuracyGroup <- ifelse(dataV$Accuracy<medSplit,"Low","High")
dataV$AccuracyGroup <- as.factor(dataV$AccuracyGroup)

diffs <- ggplot(dataV, aes(x=Stage, y=Mean, fill=AccuracyGroup)) +
  geom_violin(position=position_dodge(1),colour="black", alpha=0.8, trim=FALSE) + 
  stat_summary(position=position_dodge(1),fun.data=data_summary, colour="red")

print(diffs +
        ggtitle("Average Differentials by Information Stage, By Accuracy ") +
        labs(x = "Stage", y = "Average Differentials") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=18),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )
) 

######################
trialsWhenCorrect <- studentCaseDf$earliestCorrectStage
np <- length(trialsWhenCorrect)
xaxis  <- c("Incorrect", "Incorrect after S1", "Incorrect after S2", "Correct from S1", "Correct from S2", "Correct from S3")
yaxis <- c(sum(trialsWhenCorrect==0),
           sum(trialsWhenCorrect==-2),
           sum(trialsWhenCorrect==-1),
           sum(trialsWhenCorrect==1),
           sum(trialsWhenCorrect==2),
           sum(trialsWhenCorrect==3))
bardf <- data.frame(xaxis,yaxis)

trialCor <- ggplot(bardf,aes(xaxis,yaxis)) +
geom_bar( colour="black", stat="identity", fill="grey", alpha=0.7) +
  geom_text(aes(label = signif(yaxis)), nudge_y = 5)
print(trialCor +
        ggtitle("Breakdown of Incorrect and Correct Trials") +
        labs(x = "Type", y = "Number of Trials") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=18),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        )
) 

