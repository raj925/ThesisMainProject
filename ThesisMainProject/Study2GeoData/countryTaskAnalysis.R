requiredPackages <- c("psych", "ltm", "stringr", "ggplot2", "rjson", "reticulate", "ggpubr")
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(requiredPackages, require, character.only = TRUE)

wd <- dirname(rstudioapi::getSourceEditorContext()$path)
source_python(paste(wd, '/', 'processFile.py', sep=""))
ids <- list.dirs(wd,recursive = FALSE) 
df <- data.frame(matrix(ncol = 0, nrow = 108))
count <- 0

infoStages <- c("Geography", "Economy & Politics", "People & Culture")

participantIDS <- c()
  
for (id in ids)
{
  participantID <- str_split(id, "/", simplify = TRUE)
  participantID <- participantID[length(participantID)]
  participantIDS <- c(participantIDS, participantID)
  files <- list.files(paste(wd, "/", participantID, sep="")) 
  file <- files[1]
  filePath <- paste(wd, "/", participantID, "/", file, sep="")
  processFile(filePath)
  # Give the input file name to the function.
  myData <- fromJSON(file=filePath)
  trials <- myData$processedData$trials
  
  infoSet <- myData$rawData$scenarioObject[[1]]
  totalInfo <- length(infoSet$`Geography`) + length(infoSet$`Economy & Politics`) + length(infoSet$`People & Culture`)
  testSet1 <- names(infoSet$`Geography`)
  testSet2 <- names(infoSet$`Economy & Politics`)
  testSet3 <- names(infoSet$`People & Culture`)
  
  for (x in 1:length(trials))
  {
    row <- x + count
    trialSelect <- trials[x]
    trialSelect <- trialSelect[[1]]
    df$participantID[row] <- participantID
    df$trialNum[row] <- trialSelect$trial
    df$stage[row] <- trialSelect$subtrial
    df$stageName[row] <- infoStages[trialSelect$subtrial]
    df$trueCountry[row] <- toupper(trialSelect$trueCondition)
    df$requestedInfo[row] <- trialSelect$numOfRequestedTests
    
    currentTests <- c()
    possibleTests <- 0
    if (trialSelect$subtrial == 1)
    {
      currentTests <- testSet1
      possibleTests <- length(testSet1)
    }
    if (trialSelect$subtrial == 2)
    {
      currentTests <- testSet2
      possibleTests <- length(testSet2)
    }
    if (trialSelect$subtrial == 3)
    {
      currentTests <- testSet3
      possibleTests <- length(testSet3)
    }
    
    df$uniqueTests[row] <- length(unique(trialSelect$requestedInfoText)) # this only captures unique across the subtrial NOT the trial
    
    df$pastTests[row] <- length(setdiff(trialSelect$requestedInfoText, currentTests))
    df$currentTests[row] <- df$uniqueTests[row] - df$pastTests[row]
    df$possibleTests[row] <- possibleTests
    df$proportionOfInfo[row] <- df$currentTests[row]/possibleTests
    
    df$numOfDifferentials[row] <- length(trialSelect$differentials)
    df$confidence[row] <- trialSelect$confidence
    df$correctCountry[row] <- toupper(trialSelect$trueCountry) %in% trialSelect$differentials
    df$perceivedDifficulty[row] <- myData$rawData$difficulties[trialSelect$trial]
    df$highestLikelihood[row] <- max(trialSelect$likelihoods)
    df$competingDifferentials[row] <- sum(trialSelect$likelihoods>5, na.rm = TRUE)
    df$hasCompetingDifferentials[row] <- sum(trialSelect$likelihoods>5, na.rm = TRUE) > 1
  }
  count <- count + length(trials)
}

aggData <- data.frame(matrix(ncol = 0, nrow = length(ids)))

for (n in 1:length(participantIDS))
{
  id <- participantIDS[n]
  aggData$participantID[n] <- id
  pptTrials <- df[df$participantID==id,]
  aggData$totalMeanConfidence[n] <- mean(pptTrials$confidence)
  aggData$meanInitialConfidence[n] <- mean(pptTrials[pptTrials$stage==1,]$confidence)
  aggData$meanMiddleConfidence[n] <- mean(pptTrials[pptTrials$stage==2,]$confidence)
  aggData$meanFinalConfidence[n] <- mean(pptTrials[pptTrials$stage==3,]$confidence)
  aggData$meanDifficulty[n] <- mean(pptTrials$perceivedDifficulty)
  aggData$meanInitialDiffs[n] <- mean(pptTrials[pptTrials$stage==1,]$numOfDifferentials)
  aggData$meanMiddleDiffs[n] <- mean(pptTrials[pptTrials$stage==2,]$numOfDifferentials)
  aggData$meanFinalDiffs[n] <- mean(pptTrials[pptTrials$stage==3,]$numOfDifferentials)
  aggData$proportionOfInfo[n] <- sum(pptTrials$currentTests)/(totalInfo*max(pptTrials$trialNum))
  aggData$initialPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==1,]$proportionOfInfo)
  aggData$middlePropOfInfo[n] <- mean(pptTrials[pptTrials$stage==2,]$proportionOfInfo)
  aggData$finalPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==3,]$proportionOfInfo)
  aggData$meanConfidenceWhenCompeting[n] <- mean(pptTrials[pptTrials$hasCompetingDifferentials==TRUE,]$confidence,na.rm=TRUE)
  aggData$meanConfidenceWhenNotCompeting[n] <- mean(pptTrials[pptTrials$hasCompetingDifferentials==FALSE,]$confidence,na.rm=TRUE)
  
  files <- list.files(paste(wd, "/", id, sep="")) 
  fileName <- files[1]
  fileName <- paste(wd, "/", id, "/", fileName, sep="")
  processFile(fileName)
  myData <- fromJSON(file=fileName)
  demoQs <- myData$rawData$demoQuestionnaire
  aggData$age[n] <- demoQs[1]
  aggData$gender[n] <- demoQs[2]
  
  rawdata <- myData$rawData
  
  decisionRationalScore <- 0
  decisionIntuitionScore <- 0
  
  extraversionScore <- 0
  agreeableScore <- 0
  conscientiousScore <- 0
  neuroticScore <- 0 
  opennessScore <- 0
  
  if ("decisionAnswers" %in% names(rawdata)) 
  {  
    
    # Decision Scale:
    # 1 = Strongly Disagree
    # 2 = Disagree
    # 3 = Neutral
    # 4 = Agree
    # 5 = Strongly Agree
    
    
    decisionAnswers <- rawdata$decisionAnswers
    decisionRationalItems <- c(decisionAnswers$RationalInfo, decisionAnswers$RationalEval,
                               decisionAnswers$RationalCont, decisionAnswers$RationalDecision,
                               decisionAnswers$RationalWeigh)
    
    decisionScale <- c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")
    
    decisionIntuitiveItems <- c(decisionAnswers$IntuitiveGut, decisionAnswers$IntuitiveHunch,
                                decisionAnswers$IntuitiveDecision, decisionAnswers$IntuitiveImpress,
                                decisionAnswers$IntuitiveWeigh)
    
    for (x in 1:length(decisionRationalItems))
    {
      item <- decisionRationalItems[x]
      decisionRationalScore <- decisionRationalScore + match(item,decisionScale)
    }
    
    for (x in 1:length(decisionIntuitiveItems))
    {
      item <- decisionIntuitiveItems[x]
      decisionIntuitionScore <- decisionIntuitionScore + match(item,decisionScale)
    }
  }
  
  if ("bigFiveAnswers" %in% names(rawdata)) 
  {  
    
    # Big Five Scale:
    # 1 = Disagree Strongly
    # 2 = Disagree a little
    # 3 = Neither agree nor disagree
    # 4 = Agree a little
    # 5 = Agree Strongly
    
    bigFiveScale <- c("Disagree Strongly", "Disagree a little", "Neither agree nor disagree", "Agree a little", "Agree Strongly")
    
    bigFiveAnswers <- rawdata$bigFiveAnswers
    
    extraversionItems <- c(bigFiveAnswers$Talk, bigFiveAnswers$Reserved, bigFiveAnswers$Energy, bigFiveAnswers$Enthusiastic, bigFiveAnswers$Quiet, bigFiveAnswers$Assertive, bigFiveAnswers$Shy, bigFiveAnswers$Sociable)
    agreeableItems <- c(bigFiveAnswers$Fault, bigFiveAnswers$Help, bigFiveAnswers$Quarrel, bigFiveAnswers$Forgive, bigFiveAnswers$Trusting, bigFiveAnswers$Cold, bigFiveAnswers$Considerate, bigFiveAnswers$Rude, bigFiveAnswers$Cooperate)
    conscientiousItems <- c(bigFiveAnswers$Thorough, bigFiveAnswers$Careless, bigFiveAnswers$Reliable, bigFiveAnswers$Disorganise, bigFiveAnswers$Lazy, bigFiveAnswers$Persevere, bigFiveAnswers$Efficient, bigFiveAnswers$Plan, bigFiveAnswers$Distract)
    neuroticItems <- c(bigFiveAnswers$Blue, bigFiveAnswers$Relax, bigFiveAnswers$Tense, bigFiveAnswers$Worry, bigFiveAnswers$Stable, bigFiveAnswers$Moody, bigFiveAnswers$Calm, bigFiveAnswers$Nervous)
    openItems <- c(bigFiveAnswers$Orig, bigFiveAnswers$Curious, bigFiveAnswers$Ingenious, bigFiveAnswers$Active, bigFiveAnswers$Inventive, bigFiveAnswers$Art, bigFiveAnswers$Routine, bigFiveAnswers$Ideas, bigFiveAnswers$Artistic, bigFiveAnswers$Lit)
    
    extraversionReverses <- c(2,5,7)
    agreeableReverses <- c(1,3,6,8)
    conscientiousReverses <- c(2,4,5,9)
    neuroticReverses <- c(2,5,7)
    openReverses <- c(7,9)
    
    for (x in 1:length(extraversionItems))
    {
      item <- extraversionItems[x]
      if (x %in% extraversionReverses)
      {
        extraversionScore <- extraversionScore + (6-match(item,bigFiveScale))
      }
      else
      {
        extraversionScore <- extraversionScore + match(item,bigFiveScale)
      }
    }
    
    for (x in 1:length(agreeableItems))
    {
      item <- agreeableItems[x]
      if (x %in% agreeableReverses)
      {
        agreeableScore <- agreeableScore + (6-match(item,bigFiveScale))
      }
      else
      {
        agreeableScore <- agreeableScore + match(item,bigFiveScale)
      }
    }
    
    for (x in 1:length(conscientiousItems))
    {
      item <- conscientiousItems[x]
      if (x %in% conscientiousReverses)
      {
        conscientiousScore <- conscientiousScore + (6-match(item,bigFiveScale))
      }
      else
      {
        conscientiousScore <- conscientiousScore + match(item,bigFiveScale)
      }
    }
    
    for (x in 1:length(neuroticItems))
    {
      item <- neuroticItems[x]
      if (x %in% neuroticReverses)
      {
        neuroticScore <- neuroticScore + (6-match(item,bigFiveScale))
      }
      else
      {
        neuroticScore <- neuroticScore + match(item,bigFiveScale)
      }
    }
    
    for (x in 1:length(openItems))
    {
      item <- openItems[x]
      if (x %in% openReverses)
      {
        opennessScore <- opennessScore + (6-match(item,bigFiveScale))
      }
      else
      {
        opennessScore <- opennessScore + match(item,bigFiveScale)
      }
    }
    
    extraversionScore <- extraversionScore/length(extraversionItems)
    agreeableScore <- agreeableScore/length(agreeableItems)
    conscientiousScore <- conscientiousScore/length(conscientiousItems)
    neuroticScore <- neuroticScore/length(neuroticItems)
    opennessScore <- opennessScore/length(openItems)
    
  }
  aggData$decisionRationalScore[n] <- decisionRationalScore
  aggData$decisionIntuitionScore[n] <- decisionIntuitionScore
  aggData$relativeRationalism[n] <- decisionRationalScore - decisionIntuitionScore
  
  aggData$extraversionScore[n] <- extraversionScore
  aggData$agreeableScore[n] <- agreeableScore
  aggData$conscientiousScore[n] <- conscientiousScore
  aggData$neuroticScore[n] <- neuroticScore
  aggData$opennessScore[n] <- opennessScore

}

###### Figures #######

# par(mfrow=c(2,1))
count <- 0
diffs1 <- c()
diffs2 <- c()
for (id in participantIDS)
{
  ppt <- aggData[aggData$participantID==id,]
  xb <- infoStages
  yb <- c(ppt$meanInitialDiffs,
          ppt$meanMiddleDiffs,
          ppt$meanFinalDiffs)
  dataF <- data.frame("Stage" = xb, "Mean"= yb)
  if (count == 0)
  {
    diffs1 <- ggplot(dataF) +
      geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=differentialColour, alpha=1) +
      ggtitle(paste("Differentials by Stage for ID ", id, sep="")) +
      labs(x = "Stage", y = "Average Differentials") +
      theme_classic()
    count <- count + 1
  }
  else
  {
    diffs2 <- ggplot(dataF) +
      geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=differentialColour, alpha=1) +
      ggtitle(paste("Differentials by Stage for ID ", id, sep="")) +
      labs(x = "Stage", y = "Average Differentials") +
      theme_classic()
  }
}
ggarrange(diffs1, diffs2, ncol = 2, nrow = 1)


#### Variable Colour Coding ####

confidenceColour <- "#03c200"
difficultyColour <- "#bf00c2"
infoSeekingColour <- "#ca0600"
differentialColour <- "skyblue"

### Distribution of information proportion

infoProp <- aggData$proportionOfInfo
h <- hist(infoProp,ylim=c(0,20))
text(h$mids,h$counts,labels=h$counts, adj=c(0, 1))

### Average initial vs final differentials
nPpts <- nrow(aggData)
se <- c(sd(aggData$meanInitialDiffs)/sqrt(nPpts),
        sd(aggData$meanFinalDiffs)/sqrt(nPpts))
xb <- c("First Stage","Last Stage")
yb <- c(mean(aggData$meanInitialDiffs),
        mean(aggData$meanFinalDiffs))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=differentialColour, alpha=1) +
  geom_errorbar( aes(x=Stage, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4)

print(diffs +
        ggtitle("Average Differentials by Information Seeking Stage") +
        labs(x = "Stage", y = "Average Differentials") +
        theme_classic()) 

t.test(aggData$meanInitialDiffs, aggData$meanFinalDiffs)

### Correlation between initial differentials and information proportion

diffInfo <- ggplot(data = aggData, aes(x=meanInitialDiffs, y=proportionOfInfo)) +
  geom_point() +
  geom_smooth(method=lm , color="skyblue", fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffInfo + 
        ggtitle("Mean Initial Differentials against Proportion of Information")
      + labs(y="Proportion of Possible Information Requested", x = "Number of Initial Differentials"))

cor.test(aggData$meanInitialDiffs,aggData$proportionOfInfo,method="pearson")

### Correlation between difficulty and confidence means

diffInfo <- ggplot(data = aggData, aes(x=meanDifficulty, y=meanFinalConfidence)) +
  geom_point() +
  geom_smooth(method=lm , color="#03c200", fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffInfo + 
        ggtitle("Perceived Difficulty Against Final Confidence Ratings")
      + labs(y="Final Confidence", x = "Perceived Difficulty Rating"))

cor.test(aggData$meanDifficulty,aggData$meanFinalConfidence,method="pearson")

### Mixed model of confidence

mixedConModel = lmer(confidence ~ numOfDifferentials*stage + perceivedDifficulty + proportionOfInfo + pastTests + highestLikelihood +
                       (1 | trueCondition) + (1 | participantID), data = df)
summary(mixedConModel)

### Perceived difficulty and confidence by case

cases <- unique(df$trueCondition)
caseDifficulty <- c()
caseConfidence <- c()
for (y in 1:length(cases))
{
  condition <- cases[y]
  trials <- df[df$trueCondition==condition,]
  caseDifficulty <- c(caseDifficulty, mean(trials$perceivedDifficulty,na.rm=TRUE))
  caseConfidence <- c(caseConfidence, mean(trials[trials$stage==3,]$confidence))
}

caseData <- data.frame(
  case = c(1,2,3,4,5,6),
  difficulty = caseDifficulty,
  confidence = caseConfidence
)

ggplot(caseData, aes(x=case, y=difficulty)) +
  geom_line( aes(y=difficulty*10), color=difficultyColour, size=2) +
  geom_line( aes(y=confidence), color=confidenceColour, size=2) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean Final Confidence",
    
    # Add a second axis and specify its features
    sec.axis = dup_axis(~.*0.1, name="Perceived Difficulty", breaks=seq(0,10,1))
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = confidenceColour, size=13),
    axis.title.y.right = element_text(color = difficultyColour, size=13)
  ) +
  ggtitle("Difficulty and Confidence Ratings by Case")



### Info Requests (past and current) and Confidence at each stage line graph

data <- data.frame(
  stage = c(1,2,3),
  info = c(mean(df[df$stage==1,]$proportionOfInfo)*100, mean(df[df$stage==2,]$proportionOfInfo)*100, mean(df[df$stage==3,]$proportionOfInfo)*100),
  confidence = c(mean(df[df$stage==1,]$confidence), mean(df[df$stage==2,]$confidence), mean(df[df$stage==3,]$confidence))
)



# Most basic line chart
ggplot(data, aes(x=stage, y=info)) +
  geom_line( aes(y=info), color=infoSeekingColour, size=2) +
  geom_line( aes(y=confidence), color=confidenceColour, size=2) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Proportion of Information Seeking (%)",
    
    # Add a second axis and specify its features
    sec.axis = dup_axis(name="Confidence")
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = infoSeekingColour, size=13),
    axis.title.y.right = element_text(color = confidenceColour, size=13)
  ) +
  ggtitle("Information Seeking and Confidence Across Each Stage")
  
### Confidence with vs without competing diagnoses

nPpts <- nrow(aggData)
se <- c(sd(aggData$meanConfidenceWhenCompeting)/sqrt(nPpts),
        sd(aggData$meanConfidenceWhenNotCompeting)/sqrt(nPpts))
xb <- c("With Competing Diagnosis","Without Competing Diagnosis")
yb <- c(mean(aggData$meanConfidenceWhenCompeting),
        mean(aggData$meanConfidenceWhenNotCompeting))
dataF <- data.frame("Condition" = xb, "Mean"= yb)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Condition, y=Mean), colour="black", stat="identity", fill=confidenceColour, alpha=1) +
  geom_errorbar( aes(x=Condition, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4)

print(diffs +
        ggtitle("Effect of Competing Diagnoses on Confidence") +
        labs(x = "Condition", y = "Average Confidence") +
        theme_classic()) 

t.test(aggData$meanConfidenceWhenCompeting, aggData$meanConfidenceWhenNotCompeting)

### Highest severity as it effects confidence

nPpts <- nrow(aggData)
se <- c(sd(aggData$infoWhenSevere)/sqrt(nPpts),
        sd(aggData$infoWhenNotSevere)/sqrt(nPpts))
xb <- c("Severe Differential","No Severe Differential")
yb <- c(mean(aggData$infoWhenSevere),
        mean(aggData$infoWhenNotSevere))
dataF <- data.frame("Condition" = xb, "Mean"= yb)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Condition, y=Mean), colour="black", stat="identity", fill=infoSeekingColour, alpha=1) +
  geom_errorbar( aes(x=Condition, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4)

print(diffs +
        ggtitle("Effect of Competing Diagnoses on Confidence") +
        labs(x = "Condition", y = "Average Confidence") +
        theme_classic()) 

t.test(aggData$infoWhenSevere, aggData$infoWhenNotSevere)


### Relationship between severity and likelihoods
### Information in severity should use proportion
### Look in differentials provided - are they close?
### 
severities1 <- c()
severities2 <- c()
likelihood1 <- c()
likelihood2 <- c()
count <- 0
for (id in ids)
{
  participantID <- str_split(id, "/", simplify = TRUE)
  participantID <- participantID[length(participantID)]
  files <- list.files(paste(wd, "/", participantID, sep="")) 
  file <- files[1]
  filePath <- paste(wd, "/", participantID, "/", file, sep="")
  processFile(filePath)
  # Give the input file name to the function.
  myData <- fromJSON(file=filePath)
  trials <- myData$processedData$trials
  
  for (x in 1:length(trials))
  {
    trialSelect <- trials[x]
    trialSelect <- trialSelect[[1]]
    if (count == 0)
    {
      severities1 <- c(severities1, trialSelect$severities+1)
      likelihood1 <- c(likelihood1, trialSelect$likelihoods)
    }
    else
    {
      severities2 <- c(severities2, trialSelect$severities+1)
      likelihood2 <- c(likelihood2, trialSelect$likelihoods)
    }
  }
  count <- count + 1
}

title <- paste("Level of Concern Labels for ID ", participantIDS[1], sep="")
sev1 <- barplot(table(severities1),
        main=title,
        xlab="Concern Label",
        ylab="Count",
        border="black",
        col="black",
        density=10
)
title <- paste("Level of Concern Labels for ID ", participantIDS[2], sep="")
sev2 <- barplot(table(severities2),
                main=title,
                xlab="Concern Label",
                ylab="Count",
                border="black",
                col="black",
                density=10
)
#######################################
count <- 0
diffs1 <- c()
diffs2 <- c()
for (id in participantIDS)
{
  ppt <- aggData[aggData$participantID==id,]
  xb <- c("Emergency","Non-Emergency")
  yb <- c(ppt$confidenceWhenSevere,
          ppt$confidenceWhenNotSevere)
  dataF <- data.frame("Concern" = xb, "Mean"= yb)
  if (count == 0)
  {
    diffs1 <- ggplot(dataF) +
      geom_bar( aes(x=Concern, y=Mean), colour="black", stat="identity", fill=infoSeekingColour, alpha=1) +
      ggtitle(paste("Confidence for ID ", id, sep="")) +
      labs(x = "Highest Level of Concern", y = "Mean Final Confidence") +
      theme_classic()
    count <- count + 1
  }
  else
  {
    diffs2 <- ggplot(dataF) +
      geom_bar( aes(x=Concern, y=Mean), colour="black", stat="identity", fill=infoSeekingColour, alpha=1) +
      ggtitle(paste("Confidence for ID ", id, sep="")) +
      labs(x = "Highest Level of Concern", y = "Mean Final Confidence") +
      theme_classic()
  }
}
ggarrange(diffs1, diffs2, ncol = 2, nrow = 1)

df1 <- data.frame(severities1, likelihood1)
df2 <- data.frame(severities2, likelihood2)

sevLik <- ggplot(data = df2, aes(x=severities2, y=likelihood2)) +
  geom_jitter(width=0.1, height=0) +
  theme_classic()
title <- paste("Levels of Concern against Likelihood Ratings for ID ", participantIDS[2], sep="")
print(sevLik + 
        ggtitle(title) +
      labs(y="Likelihood Ratings", x = "Levels of Concern"))

##############################

count <- 0
accDf <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
for (participantID in participantIDS)
{
  files <- list.files(paste(wd, "/", participantID, sep="")) 
  file <- files[1]
  filePath <- paste(wd, "/", participantID, "/", file, sep="")
  processFile(filePath)
  # Give the input file name to the function.
  myData <- fromJSON(file=filePath)
  trials <- myData$processedData$trials
  
  for (x in 1:length(trials))
  {
    row <- x + count
    trialSelect <- trials[x]
    trialSelect <- trialSelect[[1]]
    accDf$participantID[row] <- participantID
    accDf$trialNum[row] <- trialSelect$trial
    accDf$stage[row] <- trialSelect$subtrial
    accDf$stageName[row] <- infoStages[trialSelect$subtrial]
    accDf$trueCountry[row] <- toupper(trialSelect$trueCountry)
    accDf$infoRequests[row] <- trialSelect$numOfRequestedInfo
    if (trialSelect$numOfRequestedInfo > 0)
    {
      accDf$information[row] <- toString(trialSelect$requestedInfoText)
    }
    else
    {
      accDf$information[row] <- NA
    }
    accDf$differentials[row] <- toString(trialSelect$differentials) 
  }
  count <- count + length(trials)
}
path <- paste(wd, '/differentialsForAccuracy.csv', sep="")
write.csv(accDf, path)
