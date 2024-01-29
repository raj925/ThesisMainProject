requiredPackages <- c("psych", "ltm", "stringr", "ggplot2", "rjson", "reticulate", "ggpubr", "lme4", "lmerTest", "pracma", "lattice", "MASS", "apcluster", "blme", "smacof","cluster","factoextra", "pwr", "magrittr", "tidyr")
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(requiredPackages, require, character.only = TRUE)

wd <- dirname(rstudioapi::getSourceEditorContext()$path)
source_python(paste(wd, '/', 'processFile.py', sep=""))
ids <- list.dirs(wd,recursive = FALSE) 
ids <- ids[!(grepl("AnalysisScripts", ids, fixed = TRUE))]
df <- data.frame(matrix(ncol = 0, nrow = 432))
count <- 0

infoStages <- c("Patient History", "Physical Exmination", "Testing")

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
  totalInfo <- length(infoSet$`Patient History`) + length(infoSet$`Physical Examination`) + length(infoSet$Testing)
  testSet1 <- names(infoSet$`Patient History`)
  testSet2 <- names(infoSet$`Physical Examination`)
  testSet3 <- names(infoSet$Testing)
  concernLabels <- c("Low", "Medium", "High", "Emergency")
  
  for (x in 1:length(trials))
  {
    row <- x + count
    trialSelect <- trials[x]
    trialSelect <- trialSelect[[1]]
    df$participantID[row] <- participantID
    df$trialNum[row] <- trialSelect$trial
    df$stage[row] <- trialSelect$subtrial
    df$stageName[row] <- infoStages[trialSelect$subtrial]
    df$trueCondition[row] <- toupper(trialSelect$trueCondition)
    df$requestedTests[row] <- trialSelect$numOfRequestedTests
    
    currentTests <- c()
    possibleTests <- 0
    if (trialSelect$subtrial == 1)
    {
      currentTests <- testSet1
      possibleTests <- length(testSet1)
      df$likelihoodChange[row] <- 0
    }
    if (trialSelect$subtrial == 2)
    {
      currentTests <- testSet2
      possibleTests <- length(testSet2)
      previousTrialSelect <- trials[x-1]
      previousTrialSelect <- previousTrialSelect[[1]]
    }
    if (trialSelect$subtrial == 3)
    {
      currentTests <- testSet3
      possibleTests <- length(testSet3)
    }
    
    df$uniqueTests[row] <- length(unique(trialSelect$requestedTestsText)) # this only captures unique across the subtrial NOT the trial
    
    df$pastTests[row] <- length(setdiff(trialSelect$requestedTestsText, currentTests))
    df$currentTests[row] <- df$uniqueTests[row] - df$pastTests[row]
    df$possibleTest[row] <- possibleTests
    df$proportionOfInfo[row] <- df$currentTests[row]/possibleTests
    df$testNames[row] <- toString(unique(trialSelect$requestedTestsText))
    
    df$numOfDifferentials[row] <- length(trialSelect$severities) 
    df$confidence[row] <- trialSelect$confidence
    df$correctDiagnosis[row] <- toupper(trialSelect$trueCondition) %in% trialSelect$diagnoses
    df$perceivedDifficulty[row] <- myData$rawData$difficulties[trialSelect$trial]
    df$highestSeverity[row] <- max(trialSelect$severities)
    df$hasHighSeverity[row] <- max(trialSelect$severities) > 2
    df$highestLikelihood[row] <- max(trialSelect$likelihoods)
    df$likelihoods[row] <- toString(trialSelect$likelihoods)
    df$competingDifferentials[row] <- sum(trialSelect$likelihoods>4, na.rm = TRUE)
    df$hasCompetingDifferentials[row] <- sum(trialSelect$likelihoods>4, na.rm = TRUE) > 2
    df$treatmentPlan[row] <- trialSelect$treatmentPlan
    df$infoSeekingTime[row] <- (trialSelect$totalInfoSeekingTime)/60000
    
    df$remoteDifferentials[row] <- sum(trialSelect$likelihoods<2, na.rm = TRUE)
    df$nonRemoteDifferentials[row] <- sum(trialSelect$likelihoods>1, na.rm = TRUE)

  }
  count <- count + length(trials)
}

#########
# Add correctness labels 

markedAccFile <- paste(wd, "/differentialsForAccuracy.csv", sep="")
accValues <- read.csv(markedAccFile, header=TRUE, sep=",")
for (x in 1:nrow(accValues))
{
  df$correct[x] <- accValues$correct[x]
  if (df$numOfDifferentials[x] > 0)
  {
    if (df$correct[x] == 0)
    {
      df$differentialAccuracy[x] <- -1/df$numOfDifferentials[x]
      df$likelihoodOfCorrectDiagnosis[x] <- 0
    }
    if (df$correct[x] == 1)
    {
      df$differentialAccuracy[x] <- 1/df$numOfDifferentials[x]
      liks<-strsplit(df$likelihoods[x], ", ")
      liks <- liks[[1]]
      df$likelihoodOfCorrectDiagnosis[x] <- liks[accValues[x,]$correctDiagnosisIdx]
    }
  }
  else
  {
    df$differentialAccuracy[x] <- 0
  }
}


aggData <- data.frame(matrix(ncol = 0, nrow = length(ids)))

for (n in 1:length(participantIDS))
{
  id <- participantIDS[n]
  aggData$participantID[n] <- id
  pptTrials <- df[df$participantID==id,]
  aggData$accuracy[n] <- mean(pptTrials$correct)
  aggData$meanInitialAccuracy[n] <- mean(pptTrials[pptTrials$stage==1,]$correct)
  aggData$meanMiddleAccuracy[n] <- mean(pptTrials[pptTrials$stage==2,]$correct)
  aggData$meanFinalAccuracy[n] <- mean(pptTrials[pptTrials$stage==3,]$correct)
  aggData$totalMeanConfidence[n] <- mean(pptTrials$confidence)
  aggData$meanInitialConfidence[n] <- mean(pptTrials[pptTrials$stage==1,]$confidence)
  aggData$meanMiddleConfidence[n] <- mean(pptTrials[pptTrials$stage==2,]$confidence)
  aggData$meanFinalConfidence[n] <- mean(pptTrials[pptTrials$stage==3,]$confidence)
  aggData$meanDifficulty[n] <- mean(pptTrials$perceivedDifficulty, na.rm=TRUE)
  aggData$meanInitialDiffs[n] <- mean(pptTrials[pptTrials$stage==1,]$numOfDifferentials)
  aggData$meanMiddleDiffs[n] <- mean(pptTrials[pptTrials$stage==2,]$numOfDifferentials)
  aggData$meanFinalDiffs[n] <- mean(pptTrials[pptTrials$stage==3,]$numOfDifferentials)
  aggData$proportionOfInfo[n] <- sum(pptTrials$currentTests)/(totalInfo*max(pptTrials$trialNum))
  aggData$initialPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==1,]$proportionOfInfo)
  aggData$middlePropOfInfo[n] <- mean(pptTrials[pptTrials$stage==2,]$proportionOfInfo)
  aggData$finalPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==3,]$proportionOfInfo)
  aggData$laterPropOfInfo[n] <- (sum(pptTrials[pptTrials$stage==2,]$currentTests) + sum(pptTrials[pptTrials$stage==3,]$currentTests))/(sum(pptTrials[pptTrials$stage==2,]$possibleTest) + sum(pptTrials[pptTrials$stage==3,]$possibleTest))
  aggData$meanConfidenceWhenCompeting[n] <- mean(pptTrials[pptTrials$hasCompetingDifferentials==TRUE,]$confidence,na.rm=TRUE)
  aggData$meanConfidenceWhenNotCompeting[n] <- mean(pptTrials[pptTrials$hasCompetingDifferentials==FALSE,]$confidence,na.rm=TRUE)
  aggData$confidenceWhenSevere[n] <- mean(pptTrials[pptTrials$hasHighSeverity==TRUE&pptTrials$stage==3,]$confidence,na.rm=TRUE)
  aggData$confidenceWhenNotSevere[n] <- mean(pptTrials[pptTrials$hasHighSeverity==FALSE&pptTrials$stage==3,]$confidence,na.rm=TRUE)
  
  aggData$meanHighestInitialLikelihood[n] <- mean(pptTrials[pptTrials$stage==1,]$highestLikelihood)
  aggData$meanHighestFinalLikelihood[n] <- mean(pptTrials[pptTrials$stage==3,]$highestLikelihood)
  aggData$meanLikelihoodGain[n] <- mean(pptTrials[pptTrials$stage==3,]$highestLikelihood) - mean(pptTrials[pptTrials$stage==1,]$highestLikelihood)
  
  aggData$meanConfidenceChangeStage2[n] <-  aggData$meanMiddleConfidence[n] - aggData$meanInitialConfidence[n]
  aggData$meanConfidenceChangeStage3[n] <-  aggData$meanFinalConfidence[n] - aggData$meanMiddleConfidence[n]
  aggData$meanConfidenceOverallChange[n] <-  aggData$meanFinalConfidence[n] - aggData$meanInitialConfidence[n]
  
  
  aggData$meanConfidenceWhenCorrect[n] <- mean(pptTrials[pptTrials$stage==1&pptTrials$correct==1,]$confidence)
  aggData$meanConfidenceWhenIncorrect[n] <- mean(pptTrials[pptTrials$stage==1&pptTrials$correct==0,]$confidence)
  aggData$initialResolution[n] <- aggData$meanConfidenceWhenCorrect[n] - aggData$meanConfidenceWhenIncorrect[n]
  
  aggData$meanConfidenceWhenCorrect[n] <- mean(pptTrials[pptTrials$stage==2&pptTrials$correct==1,]$confidence)
  aggData$meanConfidenceWhenIncorrect[n] <- mean(pptTrials[pptTrials$stage==2&pptTrials$correct==0,]$confidence)
  aggData$middleResolution[n] <- aggData$meanConfidenceWhenCorrect[n] - aggData$meanConfidenceWhenIncorrect[n]
  
  aggData$meanConfidenceWhenCorrect[n] <- mean(pptTrials[pptTrials$stage==3&pptTrials$correct==1,]$confidence)
  aggData$meanConfidenceWhenIncorrect[n] <- mean(pptTrials[pptTrials$stage==3&pptTrials$correct==0,]$confidence)
  aggData$finalResolution[n] <- aggData$meanConfidenceWhenCorrect[n] - aggData$meanConfidenceWhenIncorrect[n]
  
  aggData$meanRemoteDifferentials[n] <- mean(pptTrials$remoteDifferentials)
  aggData$meanDifferentialAccuracy[n] <- mean(pptTrials$differentialAccuracy)
  
  aggData$meanLikelihoodOfCorrectDiagnosis[n] <- mean(as.integer(pptTrials$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  aggData$meanLikelihoodOfCorrectDiagnosisInitial[n] <- mean(as.integer(pptTrials[pptTrials$stage==1,]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  aggData$meanLikelihoodOfCorrectDiagnosisMiddle[n] <- mean(as.integer(pptTrials[pptTrials$stage==2,]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  aggData$meanLikelihoodOfCorrectDiagnosisFinal[n] <- mean(as.integer(pptTrials[pptTrials$stage==3,]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  
  aggData$meanLikelihoodOfCorrectDiagnosisPresent[n] <- mean(as.integer(pptTrials[pptTrials$likelihoodOfCorrectDiagnosis!="0",]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  aggData$meanLikelihoodOfCorrectDiagnosisInitialPresent[n] <- mean(as.integer(pptTrials[pptTrials$stage==1&pptTrials$likelihoodOfCorrectDiagnosis!="0",]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  aggData$meanLikelihoodOfCorrectDiagnosisMiddlePresent[n] <- mean(as.integer(pptTrials[pptTrials$stage==2&pptTrials$likelihoodOfCorrectDiagnosis!="0",]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  aggData$meanLikelihoodOfCorrectDiagnosisFinalPresent[n] <- mean(as.integer(pptTrials[pptTrials$stage==3&pptTrials$likelihoodOfCorrectDiagnosis!="0",]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)
  
  aggData$correctDiagnosisLikelihoodGain[n] <- aggData$meanLikelihoodOfCorrectDiagnosisFinal[n] - aggData$meanLikelihoodOfCorrectDiagnosisInitial[n]
  aggData$correctDiagnosisLikelihoodGainPresent[n] <- aggData$meanLikelihoodOfCorrectDiagnosisFinalPresent[n] - aggData$meanLikelihoodOfCorrectDiagnosisInitialPresent[n]
  
  files <- list.files(paste(wd, "/", id, sep="")) 
  fileName <- files[1]
  fileName <- paste(wd, "/", id, "/", fileName, sep="")
  processFile(fileName)
  myData <- fromJSON(file=fileName)
  demoQs <- myData$rawData$demoQuestionnaire
  aggData$age[n] <- demoQs[1]
  aggData$gender[n] <- demoQs[2]
  aggData$medExp[n] <- demoQs[3]
  
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

#### Get just student data

experiencedIDs <- c("qj4vcw", "sz5k4r")

studentDf <- df[df$participantID != experiencedIDs[1] & df$participantID != experiencedIDs[2],]
studentAggData <- aggData[aggData$participantID != experiencedIDs[1] & aggData$participantID != experiencedIDs[2],]

expertDf <- df[df$participantID == experiencedIDs[1] | df$participantID == experiencedIDs[2],]
expertAggData <- aggData[aggData$participantID == experiencedIDs[1] | aggData$participantID == experiencedIDs[2],]



###### Figures #######

#### Variable Colour Coding ####

confidenceColour <- "#03c200"
difficultyColour <- "#bf00c2"
infoSeekingColour <- "#ca0600"
differentialColour <- "skyblue"
likelihoodColour <- "orange"
accuracyColour <- "black"
resolutionColour <- "yellow"


### Distribution of information proportion

infoProp <- df$proportionOfInfo
h <- hist(infoProp,breaks=5)
text(h$mids,h$counts,labels=h$counts, adj=c(0, 1))

### Distribution of final information proportion

finalInfoProp <- aggData$finalPropOfInfo
h <- hist(finalInfoProp,breaks=5)
text(h$mids,h$counts,labels=h$counts, adj=c(0, 1))

### Distribution of confidence

confidence <- df$confidence
h <- hist(confidence,breaks=5)
text(h$mids,h$counts,labels=h$counts, adj=c(0, 1))

# Differentials by stage

nPpts <- nrow(aggData)
xb <- c("Patient History","Physical Examination", "Testing")
yb <- c(mean(aggData$meanInitialDiffs),
        mean(aggData$meanMiddleDiffs),
        mean(aggData$meanFinalDiffs))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=differentialColour, alpha=1)

print(diffs +
        ggtitle("Average Differentials by Information Seeking Stage") +
        labs(x = "Stage", y = "Average Differentials") +
        theme_classic()) 


# Information proportion by stage

nPpts <- nrow(aggData)
xb <- c("Patient History","Physical Examination", "Testing")
yb <- c(mean(aggData$initialPropOfInfo),
        mean(aggData$middlePropOfInfo),
        mean(aggData$finalPropOfInfo))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
inf <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.7)

print(inf +
        ggtitle("Information Gathering by Information Seeking Stage") +
        labs(x = "Stage", y = "Proportion of Available Information Sought") +
        theme_classic()) 

# Confidence by stage

nPpts <- nrow(aggData)
xb <- c("Patient History","Physical Examination", "Testing")
yb <- c(mean(aggData$meanInitialConfidence),
        mean(aggData$meanMiddleConfidence),
        mean(aggData$meanFinalConfidence))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
inf <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=confidenceColour, alpha=0.7)

print(inf +
        ggtitle("Confidence by Information Seeking Stage") +
        labs(x = "Stage", y = "Mean Confidence") +
        theme_classic()) 

# Accuracy by stage

nPpts <- nrow(aggData)
xb <- c("Patient History","Physical Examination", "Testing")
yb <- c(mean(aggData$meanInitialAccuracy),
        mean(aggData$meanMiddleAccuracy),
        mean(aggData$meanFinalAccuracy))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
inf <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=accuracyColour, alpha=0.7)

print(inf +
        ggtitle("Accuracy by Information Seeking Stage") +
        labs(x = "Stage", y = "Mean Accuracy") +
        theme_classic()) 

# Resolution by stage

nPpts <- nrow(studentAggData)
xb <- c("Patient History","Physical Examination", "Testing")
yb <- c(mean(studentAggData$initialResolution,na.rm=TRUE),
        mean(studentAggData$middleResolution),
        mean(studentAggData$finalResolution))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
inf <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=resolutionColour, alpha=0.7)

print(inf +
        ggtitle("Resolution by Information Seeking Stage") +
        labs(x = "Stage", y = "Difference in Confidence when Correct and Incorrect") +
        theme_classic()) 

# Likelihood of correct diagnosis by stage (include zeros)

nPpts <- nrow(studentAggData)
xb <- c("Patient History","Physical Examination", "Testing")
yb <- c(mean(studentAggData$meanLikelihoodOfCorrectDiagnosisInitial,na.rm=TRUE),
        mean(studentAggData$meanLikelihoodOfCorrectDiagnosisMiddle,na.rm=TRUE ),
        mean(studentAggData$meanLikelihoodOfCorrectDiagnosisFinal, na.rm=TRUE))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
inf <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=likelihoodColour, alpha=0.7)

print(inf +
        ggtitle("Likelihood of Correct Diagnosis by Information Seeking Stage") +
        labs(x = "Stage", y = "Likelihood Rating") +
        theme_classic()) 

# Likelihood of correct diagnosis by stage (exclude zeros)

nPpts <- nrow(studentAggData)
xb <- c("Patient History","Physical Examination", "Testing")
yb <- c(mean(studentAggData$meanLikelihoodOfCorrectDiagnosisInitialPresent,na.rm=TRUE),
        mean(studentAggData$meanLikelihoodOfCorrectDiagnosisMiddlePresent,na.rm=TRUE ),
        mean(studentAggData$meanLikelihoodOfCorrectDiagnosisFinalPresent, na.rm=TRUE))
dataF <- data.frame("Stage" = xb, "Mean"= yb)
inf <- ggplot(dataF) +
  geom_bar( aes(x=Stage, y=Mean), colour="black", stat="identity", fill=likelihoodColour, alpha=0.7)

print(inf +
        ggtitle("Likelihood of Correct Diagnosis by Information Seeking Stage") +
        labs(x = "Stage", y = "Likelihood Rating") +
        theme_classic()) 

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

t.test(aggData$meanInitialDiffs, aggData$meanFinalDiffs, paired=TRUE)

### Correlation between initial differentials and information proportion

diffInfo <- ggplot(data = studentAggData, aes(x=meanInitialDiffs, y=laterPropOfInfo)) +
  geom_point() +
  geom_smooth(method=lm , color="skyblue", fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffInfo + 
        ggtitle("Mean Initial Differentials against Later Proportion of Information")
      + labs(y="Proportion of Possible Information Requested", x = "Number of Initial Differentials"))


### Correlation between initial differentials and final confidence

diffCon <- ggplot(data = aggData, aes(x=meanInitialDiffs, y=meanFinalConfidence)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Mean Initial Differentials against Final Confidence")
      + labs(y="Final Confidence", x = "Number of Initial Differentials"))

cor.test(aggData$meanInitialDiffs,aggData$meanFinalConfidence,method="pearson")

### Correlation between final confidence and accuracy

diffCon <- ggplot(data = studentAggData, aes(x=confidenceChange, y=meanFinalAccuracy)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Mean Final Accuracy against Confidence Change")
      + labs(y="Final Accuracy", x = "Final Confidence - Initial Confidence"))

cor.test(studentAggData$meanFinalAccuracy,studentAggData$confidenceChange,method="pearson")

### Correlation between resolution and accuracy

diffCon <- ggplot(data = studentAggData, aes(x=resolution, y=meanFinalAccuracy)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Resolution against Confidence Change")
      + labs(y="Final Accuracy", x = "Resolution"))

cor.test(studentAggData$meanFinalAccuracy,studentAggData$resolution,method="pearson")


### Correlation between initial differentials and overall confidence change

diffCon <- ggplot(data = studentAggData, aes(x=meanInitialDiffs, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Mean Initial Differentials against Overall Confidence Change")
      + labs(y="Final Confidence - Initial Confidence", x = "Number of Initial Differentials"))

cor.test(studentAggData$meanInitialDiffs,studentAggData$meanConfidenceOverallChange,method="pearson")

### Correlation between correct diagnosis likelihood gain and overall confidence change

diffCon <- ggplot(data = studentAggData, aes(x=correctDiagnosisLikelihoodGain, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Likelihood Gain for Correct Diagnosis against Overall Confidence Change")
      + labs(y="Final Confidence - Initial Confidence", x = "Likelihood Gain"))

cor.test(studentAggData$correctDiagnosisLikelihoodGain,studentAggData$meanConfidenceOverallChange,method="pearson")

### Correlation between correct diagnosis likelihood and information seeking

diffCon <- ggplot(data = studentAggData, aes(x=meanLikelihoodOfCorrectDiagnosisFinalPresent, y=laterPropOfInfo)) +
  geom_point() +
  geom_smooth(method=lm , color=likelihoodColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Likelihood for Correct Diagnosis against Information Seeking")
      + labs(y="Information Seeking", x = "Likelihood Gain"))

cor.test(studentAggData$meanLikelihoodOfCorrectDiagnosisFinalPresent,studentAggData$laterPropOfInfo,method="pearson")


### Correlation between difficulty and confidence means

diffInfo <- ggplot(data = aggData, aes(x=meanDifficulty, y=meanFinalConfidence)) +
  geom_point() +
  geom_smooth(method=lm , color="#03c200", fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffInfo + 
        ggtitle("Perceived Difficulty Against Final Confidence Ratings")
      + labs(y="Final Confidence", x = "Perceived Difficulty Rating"))

cor.test(aggData$meanDifficulty,aggData$meanFinalConfidence,method="pearson")

### Correlation between difficulty and confidence means (all cases)

diffInfo <- ggplot(data = df[df$stage==3,], aes(x=perceivedDifficulty, y=confidence)) +
  geom_point() +
  geom_smooth(method=lm , color="#03c200", fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffInfo + 
        ggtitle("Perceived Difficulty Against Final Confidence Ratings")
      + labs(y="Final Confidence", x = "Perceived Difficulty Rating"))

cor.test(df[df$stage==3,]$perceivedDifficulty,df[df$stage==3,]$confidence,method="pearson")

### Correlation between highest likelihood and final confidence means (all cases)

likCon <- ggplot(data = df[df$stage==3,], aes(x=highestLikelihood, y=confidence)) +
  geom_point() +
  geom_smooth(method=lm , color=likelihoodColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(likCon + 
        ggtitle("Highest Likelihood Against Final Confidence Ratings")
      + labs(y="Final Confidence", x = "Highest Likelihood Across All Differentials"))

cor.test(df[df$stage==3,]$highestLikelihood,df[df$stage==3,]$confidence,method="pearson")

### Correlation between likelihood gain and confidence gain (aggregate)

likCon <- ggplot(data = studentAggData, aes(x=meanLikelihoodGain, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=likelihoodColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(likCon + 
        ggtitle("Likelihood Gain against Confidence Change")
      + labs(y="Difference between Final and Initial Confidence", x = "Difference Between Initial and Final Highest Likelihood Ratings"))

cor.test(studentAggData$meanLikelihoodGain,studentAggData$meanConfidenceOverallChange,method="pearson")

### Mixed model of confidence

mixedConModel = lmer(normConfidence ~ numOfDifferentials*stage + perceivedDifficulty + proportionOfInfo + highestLikelihood + hasHighSeverity + infoSeekingTime +
                       (1 | trueCondition) + (1 | participantID), data = df, na.action="na.exclude")
summary(mixedConModel)

ranef(mixedConModel)

# Box cox test
bc <- boxcox(confidence+1 ~ numOfDifferentials*stage + perceivedDifficulty + proportionOfInfo + highestLikelihood + hasHighSeverity + infoSeekingTime, data = df)

# Assumption testing  
# Linearity
modelLin<-plot(resid(mixedConModel),normConfidence)

# Homogeneity of variance
df$res<- residuals(mixedConModel) #extracts the residuals 
df$res <-abs(df$res) #absolute value of the residuals
df$res <- df$res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(res ~ participantID, data=df) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

# Normal distribution of residuals
qqmath(mixedConModel, id=0.05)

### Perceived difficulty, accuracy and confidence by case

cases <- unique(studentDf$trueCondition)
caseDifficulty <- c()
caseConfidence <- c()
caseAccuracy <- c()
caseInfoSeeking <- c()
for (y in 1:length(cases))
{
  condition <- cases[y]
  trials <- studentDf[studentDf$trueCondition==condition,]
  caseDifficulty <- c(caseDifficulty, mean(trials$perceivedDifficulty,na.rm=TRUE))
  caseLikelihood <- c(caseLikelihood, mean(as.integer(trials[trials$stage==3&trials$likelihoodOfCorrectDiagnosis!="0",]$likelihoodOfCorrectDiagnosis),na.rm=TRUE)) 
  caseConfidence <- c(caseConfidence, mean(trials[trials$stage==3,]$confidence))
  caseAccuracy <- c(caseAccuracy, mean(trials$correct))
  caseInfoSeeking <- c(caseInfoSeeking, mean(trials$proportionOfInfo))
}

caseData <- data.frame(
  case = c(1,2,3,4,5,6),
  difficulty = caseDifficulty,
  likelihood = caseLikelihood,
  confidence = caseConfidence,
  accuracy = caseAccuracy,
  infoSeeking = caseInfoSeeking
)

ggplot(caseData, aes(x=case, y=difficulty)) +
  geom_line( aes(y=difficulty*10), color=difficultyColour, size=2) +
  geom_line( aes(y=likelihood*10), color=likelihoodColour, size=2) +
  geom_line( aes(y=confidence), color=confidenceColour, size=2) +
  geom_line( aes(y=accuracy*100), color=accuracyColour, size=2) +
  geom_line( aes(y=infoSeeking*100), color=infoSeekingColour, size=2) +
  scale_color_manual(values = colors) +
  labs(color = "Legend") +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean Final Confidence / Accuracy / Info Seeking",
    
    # Add a second axis and specify its features
    sec.axis = dup_axis(~.*0.1, name="Perceived Difficulty / Likelihood of Correct Diagnosis", breaks=seq(0,10,1))
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = confidenceColour, size=13),
    axis.title.y.right = element_text(color = difficultyColour, size=13)
  ) +
  ggtitle("Difficulty, Confidence Ratings and Accuracy by Case")


### Differentials and later info seeking 

cases <- unique(studentDf$trueCondition)
caseInitialDifferentials <- c()
caseInfoSeeking <- c()
for (y in 1:length(cases))
{
  condition <- cases[y]
  trials <- studentDf[studentDf$trueCondition==condition,]
  caseInitialDifferentials <- c(caseInitialDifferentials, mean(trials[trials$stage==1,]$numOfDifferentials))
  caseInfoSeeking <- c(caseInfoSeeking, mean(trials[trials$stage==2|trials$stage==3,]$proportionOfInfo))
}

caseData <- data.frame(
  case = c(1,2,3,4,5,6),
  differentials = caseInitialDifferentials,
  infoSeeking = caseInfoSeeking
)

ggplot(caseData, aes(x=case, y=difficulty)) +
  geom_line( aes(y=differentials*10), color=differentialColour, size=2) +
  geom_line( aes(y=infoSeeking*100), color=infoSeekingColour, size=2) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Later Information Seeking",
    
    # Add a second axis and specify its features
    sec.axis = dup_axis(~.*0.1, name="Number of Initial Differentials", breaks=seq(0,10,1))
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = infoSeekingColour, size=13),
    axis.title.y.right = element_text(color = differentialColour, size=13)
  ) +
  ggtitle("Differentials and Information Seeking by Case")


### Accuracy, info seeking likelihood and confidence by case

cases <- unique(studentDf$trueCondition)
caseLikelihood <- c()
caseConfidence <- c()
caseAccuracy <- c()
caseInfoSeeking <- c()
for (y in 1:length(cases))
{
  condition <- cases[y]
  trials <- studentDf[studentDf$trueCondition==condition,]
  caseLikelihood <- c(caseLikelihood, mean(as.integer(trials[trials$stage==3&trials$likelihoodOfCorrectDiagnosis!="0",]$likelihoodOfCorrectDiagnosis),na.rm=TRUE))
  caseConfidence <- c(caseConfidence, mean(trials[trials$stage==3,]$confidence))
  caseAccuracy <- c(caseAccuracy, mean(trials$correct))
  caseInfoSeeking <- c(caseInfoSeeking, mean(trials$proportionOfInfo))
}

caseData <- data.frame(
  case = c(1,2,3,4,5,6),
  likelihood = caseLikelihood,
  confidence = caseConfidence,
  accuracy = caseAccuracy,
  infoSeeking = caseInfoSeeking
)

ggplot(caseData, aes(x=case, y=likelihood)) +
  geom_line( aes(y=likelihood*10), color=resolutionColour, size=2) +
  geom_line( aes(y=confidence), color=confidenceColour, size=2) +
  geom_line( aes(y=accuracy*100), color=accuracyColour, size=2) +
  geom_line( aes(y=infoSeeking*100), color=infoSeekingColour, size=2) +
  
  scale_y_continuous(
    
    # Features of the first axis
    name = "Mean Final Confidence / Accuracy / Info Seeking",
    
    # Add a second axis and specify its features
    sec.axis = dup_axis(~.*0.1, name="Likelihood Rating of Correct Diagnosis", breaks=seq(0,10,1))
  ) + 
  
  theme_classic() +
  
  theme(
    axis.title.y = element_text(color = confidenceColour, size=13),
    axis.title.y.right = element_text(color = resolutionColour, size=13)
  ) +
  ggtitle("Likelihood, Confidence Ratings and Accuracy by Case")




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
se <- c(sd(aggData$meanConfidenceWhenCompeting, na.rm=TRUE)/sqrt(nPpts),
        sd(aggData$meanConfidenceWhenNotCompeting, na.rm=TRUE)/sqrt(nPpts))
xb <- c("With Competing Diagnosis","Without Competing Diagnosis")
yb <- c(mean(aggData$meanConfidenceWhenCompeting, na.rm=TRUE),
        mean(aggData$meanConfidenceWhenNotCompeting, na.rm=TRUE))
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
se <- c(sd(studentAggData$confidenceWhenSevere)/sqrt(nPpts),
        sd(studentAggData$confidenceWhenNotSevere)/sqrt(nPpts))
xb <- c("Severe Differential","No Severe Differential")
yb <- c(mean(studentAggData$confidenceWhenSevere),
        mean(studentAggData$confidenceWhenNotSevere))
dataF <- data.frame("Condition" = xb, "Mean"= yb)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Condition, y=Mean), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.7) +
  geom_errorbar( aes(x=Condition, y=Mean, ymin=Mean-se, ymax=Mean+se), colour="orange", alpha=0.9, size=1.1, width = 0.4)

print(diffs +
        ggtitle("Effect of Severe Diagnoses on Confidence") +
        labs(x = "Condition", y = "Average Confidence") +
        theme_classic()) 

t.test(studentAggData$confidenceWhenSevere, studentAggData$confidenceWhenNotSevere)


### Relationship between severity and likelihoods
### Information in severity should use proportion
### Look in differentials provided - are they close?
### 
severities <- c()
likelihood <- c()
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
    severities <- c(severities, trialSelect$severities+1)
    likelihood <- c(likelihood, trialSelect$likelihoods)
  }
  count <- count + 1
}

title <- paste("Level of Concern Labels", sep="")
sev2 <- barplot(table(severities),
                main=title,
                xlab="Concern Label",
                ylab="Count",
                border="black",
                col="black",
                density=10
)
title <- paste("Likelihood Ratings", sep="")
lik <- barplot(table(likelihood),
                main=title,
                xlab="Likelihood",
                ylab="Count",
                border="black",
                col="black",
                density=10
)
#######################################

count <- 0
accDf <- data.frame(matrix(ncol = 0, nrow = nrow(df)))
for (participantID in participantIDS)
{
  files <- list.files(paste(wd, "/JSONs/", participantID, sep="")) 
  file <- files[1]
  filePath <- paste(wd, "/JSONs/", participantID, "/", file, sep="")
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
    accDf$trueCondition[row] <- toupper(trialSelect$trueCondition)
    
    if (length(trialSelect$diagnoses) > 0)
    {
      accDf$differentials[row] <- toString(trialSelect$diagnoses) 
    }
    else
    {
      accDf$differentials[row] <- toString(trialSelect$differentials) 
    }
    
    accDf$highestLikelihoodValue[row] <- max(trialSelect$likelihoods)
    differentials <-  accDf$differentials[row]
    differentials <- str_split(differentials,", ")
    differentials <- differentials[[1]]
    accDf$highestLikelihoodDifferential[row] <- differentials[which.max(trialSelect$likelihoods)]
  }
  count <- count + length(trials)
}

###################
# Case by case initial differentials against Info prop

nCase <- length(ids)*6
caseDf <- data.frame(matrix(ncol = 0, nrow = nCase))
#Generate tests and paired values
allTests <- c(testSet1, testSet2, testSet3)
testIndexes <- c(1:length(allTests))
for (y in 1:nCase)
{
  caseDf$id[y] <- df$participantID[(3*y)-2]
  caseDf$condition[y] <- df$trueCondition[(3*y)-2]
  caseDf$initialDifferentials[y] <- df$numOfDifferentials[(3*y)-2]
  caseDf$initialCorrect[y] <- df$correct[(3*y)-2]
  caseDf$caseInformationProportion[y] <- (df$uniqueTests[(3*y)-2] + df$uniqueTests[(3*y)-1] + df$uniqueTests[(3*y)])/(df$possibleTest[(3*y)-2] + df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  caseDf$laterInfoProp[y] <- (df$uniqueTests[(3*y)-1] + df$uniqueTests[(3*y)])/(df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  caseDf$finalConfidence[y] <-df$confidence[(3*y)]
  caseDf$confidenceChange[y] <-df$confidence[(3*y)] - df$confidence[(3*y)-2]
  caseDf$subjectiveDifficulty[y] <- df$perceivedDifficulty[(3*y)]
  caseDf$caseInformationReqs[y] <- df$uniqueTests[(3*y)-2] + df$uniqueTests[(3*y)-1] + df$uniqueTests[(3*y)]
  caseDf$likelihoodChange[y] <- df$highestLikelihood[(3*y)] - df$highestLikelihood[(3*y)-2]
  caseDf$differentialChange[y] <- df$numOfDifferentials[(3*y)] - df$numOfDifferentials[(3*y)-2]
  caseDf$confidenceArray[y] <- toString(c(df$confidence[(3*y)-2],df$confidence[(3*y)-1],df$confidence[(3*y)]))
  caseDf$highestFinalLikelihood[y] <- df$highestLikelihood[(3*y)]
  caseDf$correct[y] <- df$correct[(3*y)]
  caseDf$likelihoodOfCorrectDiagnosis[y] <- df$likelihoodOfCorrectDiagnosis[(3*y)]
  testArray <- replicate(length(allTests), 0)
  allReqTests <- paste(df$testNames[(3*y)-2],df$testNames[(3*y)-1],df$testNames[(3*y)], sep=", ")
  allReqTests <- strsplit(allReqTests, ", ")
  allReqTests <- unique(allReqTests[[1]])
  for (i in 1:length(allTests))
  {
    test <- allTests[i]
    if (test %in% allReqTests)
    {
      testArray[i] <- 1
    }
  }
  caseDf$reqTestArray[y] <- toString(testArray)
}

studentCaseDf <- caseDf[caseDf$id != experiencedIDs[1] & caseDf$id != experiencedIDs[2],]
expertCaseDf <- caseDf[caseDf$id == experiencedIDs[1] | caseDf$id == experiencedIDs[2],]
# Add similarity measures to aggdata based on all case matrices
# To measure the similarity in the angle of direction of the vectors:
#Step 1 is to normalise each vector, ie divide it by its length eg (x,y)/sqrt(x²+y²)
#Step 2 is to arrange the normalised vectors into a matrix of size L×N
#Step 3 is to compute the transpose. Your transpose matrix should be N×L
#Step 4 is to multiple the matrices such that you get an N×N matrix.
#The closer an entry is to 1 the more 'similar' the angles of thetwo vectors. 
# You can ignore half the matrix for speed as the matrix will be symmertrical and the main diagonal will be always 1.
# Similairity computed by taking the upper half of your computed matrix (ie above the main diagonal) and then finding the mean of the entries.
# COSINE SIMILARITY MATRIX
normVec <- function(x) {x / sqrt(sum(x^2))} # compute normalised vector
tranposeMult <- function(x) {x %*% t(x)}  # multiply matrix by its tranpose
upperTriMean <- function(x) {mean(x[upper.tri(x)])} # compute mean of matrix upper triangle 

studentIDs <- setdiff(participantIDS, experiencedIDs)
studentsPpts <- length(studentIDs)

for (n in 1:studentsPpts)
{
  pid <-  studentIDs[n]
  cases <- studentCaseDf[studentCaseDf$id==pid,]
  confidenceMat <- c()
  #confidenceNonMat <- c()
  infoSeekingMat <- c()
  for (m in 1:nrow(cases))
  {
    conVector <- cases$confidenceArray[m]
    conVector <- strsplit(conVector, ", ")
    conVector <- conVector[[1]]
    conVector <- as.integer(conVector)
    normConVector <- normVec(conVector)
    confidenceMat <- rbind(normConVector, confidenceMat)
    #confidenceNonMat <- rbind(conVector, confidenceNonMat)
    
    infVector <- cases$reqTestArray[m]
    infVector <- strsplit(infVector, ", ")
    infVector <- infVector[[1]]
    infVector <- as.integer(infVector)
    normInfVector <- normVec(infVector)
    infoSeekingMat <- rbind(normInfVector, infoSeekingMat)
  }
  confidenceSimMat <- tranposeMult(confidenceMat)
  infoSeekingSimMat <- tranposeMult(infoSeekingMat)
  studentAggData$confidenceMatCosSimMeasure[n] <- upperTriMean(confidenceSimMat)
  studentAggData$infoSeekingMatCosSimMeasure[n] <- upperTriMean(infoSeekingSimMat)
  studentAggData$confidenceMatVarSimMeasure[n] <- var(colMeans(confidenceMat))
  studentAggData$infoSeekingMatVarSimMeasure[n] <- var(colMeans(infoSeekingMat)) 

}

infoSeekingFullMatrix <- data.frame()
confidenceMatrix <- data.frame()
conditionsShort <- c("GBS", "AD", "TTP", "UC", "MTB", "TA")
conditionsLong <- c("GUILLAIN-BARRE SYNDROME", "AORTIC DISSECTION", 
                    "THROMBOTIC THROMBOCYTOPENIC PURPURA", "ULCERATIVE COLITIS", 
                    "MILIARY TB", "TEMPORAL ARTERITIS")
#easyCaseGroup <- c("GBS", "UC", "TTP") # below 50% accuracy
#hardCaseGroup <- c("MTB", "TA", "AD") # over 50% accuracy

easyCaseGroup <- c("GBS", "UC") 
medCaseGroup <- c("TTP", "AD") 
hardCaseGroup <- c("MTB", "TA") 
for (n in 1:nrow(studentCaseDf))
{
  row <- studentCaseDf$reqTestArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  infoSeekingFullMatrix <- rbind(row, infoSeekingFullMatrix)
  row <- studentCaseDf$confidenceArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  confidenceMatrix <- rbind(row, confidenceMatrix)
}

for (n in 1:nrow(expertCaseDf))
{
  row <- expertCaseDf$reqTestArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  infoSeekingFullMatrix <- rbind(row, infoSeekingFullMatrix)
  row <- expertCaseDf$confidenceArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  confidenceMatrix <- rbind(row, confidenceMatrix)
}

ids <- unique(studentCaseDf$id)
for (n in 1:nrow(studentCaseDf))
{
  currentCondition <- studentCaseDf$condition[n]
  idx <- match(currentCondition,conditionsLong)
  #rowName <- paste("p", match(caseDf$id[n],ids), conditionsShort[idx], sep="")
  rowName <- paste("p", match(studentCaseDf$id[n],studentIDs), "-", round(studentAggData[studentAggData$participantID==studentCaseDf$id[n],]$meanFinalAccuracy,digits=1), "-", conditionsShort[idx], sep="")
  infoSeekingFullMatrix$id[n] <- studentCaseDf$id[n]
  infoSeekingFullMatrix$condition[n] <- conditionsShort[idx]
  if (conditionsShort[idx] %in% easyCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 1
  }
  if (conditionsShort[idx] %in% medCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 2
  }
  if (conditionsShort[idx] %in% hardCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 3
  }
  infoSeekingFullMatrix$correct[n] <- studentCaseDf$correct[n]
  infoSeekingFullMatrix$pptAccuracy[n] <- round(studentAggData[studentAggData$participantID==studentCaseDf$id[n],]$meanFinalAccuracy, digits = 1)
  infoSeekingFullMatrix$initialCorrect[n] <- studentCaseDf$initialCorrect[n]
  infoSeekingFullMatrix$participantType[n] <- "p"
  rownames(infoSeekingFullMatrix)[n] <- rowName
  rownames(confidenceMatrix)[n] <- rowName
}
offset <- nrow(studentCaseDf)
for (m in 1:nrow(expertCaseDf))
{
  currentCondition <- expertCaseDf$condition[m]
  idx <- match(currentCondition,conditionsLong)
  rowName <- paste("e", ceil(m/6), "-", round(expertAggData[expertAggData$participantID==expertCaseDf$id[m],]$meanFinalAccuracy,digits=1), "-", conditionsShort[idx], sep="")
  infoSeekingFullMatrix$id[m+offset] <- expertCaseDf$id[m]
  infoSeekingFullMatrix$condition[m+offset] <- conditionsShort[idx]
  if (conditionsShort[idx] %in% easyCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[m+offset] <- 1
  }
  if (conditionsShort[idx] %in% medCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[m+offset] <- 2
  }
  if (conditionsShort[idx] %in% hardCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[m+offset] <- 3
  }
  infoSeekingFullMatrix$correct[m+offset] <- expertCaseDf$correct[m]
  infoSeekingFullMatrix$pptAccuracy[m+offset] <- round(expertAggData[expertAggData$participantID==expertCaseDf$id[m],]$meanFinalAccuracy, digits = 1)
  infoSeekingFullMatrix$initialCorrect[m+offset] <- expertCaseDf$initialCorrect[m]
  infoSeekingFullMatrix$participantType[m+offset] <- "e"
  rownames(infoSeekingFullMatrix)[m+offset] <- rowName
  rownames(confidenceMatrix)[m+offset] <- rowName
}

colnames(infoSeekingFullMatrix) <- c(1:29)
colnames(infoSeekingFullMatrix)[30] <- "ID"
colnames(infoSeekingFullMatrix)[31] <- "Condition"
colnames(infoSeekingFullMatrix)[32] <- "CaseDifficultyGroup"
colnames(infoSeekingFullMatrix)[33] <- "Correct"
colnames(infoSeekingFullMatrix)[34] <- "ParticipantAccuracy"
colnames(infoSeekingFullMatrix)[35] <- "InitialCorrect"
colnames(infoSeekingFullMatrix)[36] <- "ParticipantType"

# Classical MDS
# N rows (objects) x p columns (variables)
# each row identified by a unique row name

d <- dist(infoSeekingFullMatrix) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution
x <- fit$points[,1]
y <- fit$points[,2]
ID <- infoSeekingFullMatrix$ID
plot(x, y, xlab="Coordinate 1", ylab="Coordinate 2",
     main="Metric MDS", type="n",pch = 19)
text(x, y, labels = row.names(infoSeekingFullMatrix), cex=.7)

numOfClusters <- 4
clusterType <- "pam"

# Clustering by PAM rather than k means

mds <- infoSeekingFullMatrix[,1:29] %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()

distances <- infoSeekingFullMatrix[,1:29] %>% dist() %>% as.matrix()

##########

# Distance heat plot by case

cases <- c("UC", "GBS", "TA", "AD", "TTP", "MTB")
distanceMatrix <- data.frame(matrix(ncol = 6, nrow = 6))
for (x in 1:length(cases))
{
  case <- cases[x]
  for (y in 1:length(cases))
  {
    if (y == x)
    {
      distanceMatrix[x,y] <- 0
      colnames(distanceMatrix)[y] <- paste(y, ". ", case)
    }
    else
    {
      comparisonCase <- cases[y]
      compareColumns <- distances[grep(comparisonCase, rownames(distances)), ]
      compareColumns <- compareColumns[,grep(case, colnames(compareColumns)) ]
      meanDist <- mean(compareColumns)
      distanceMatrix[x,y] <- meanDist-3
      colnames(distanceMatrix)[y] <- paste(y, ". ", comparisonCase)
    }
  }
  rownames(distanceMatrix)[x] <- paste(x, ". ", case)
}

dt2 <- distanceMatrix %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000")

##########

# Look at variance in distances 

# By case

cases <- c("UC", "GBS", "TA", "AD", "TTP", "MTB")
caseVars <- c()
for (x in 1:length(cases))
{
  compareColumns <- distances[grep(cases[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(cases[x], colnames(compareColumns)) ]
  caseVars[x] <- (sd(compareColumns))^2
}

dataF <- data.frame("Case" = cases, "Variance"= caseVars)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Case, y=Variance), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(diffs +
        ggtitle("Variance in Information Seeking by Case") +
        labs(x = "Case", y = "Variance in MDS Distances") +
        theme_classic()) 

# By accuracy

accs <- c("0.3", "0.5", "0.7", "0.8")
accVars <- c()
accInfoProps <- c()
for (x in 1:length(accs))
{
  compareColumns <- distances[grep(accs[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(accs[x], colnames(compareColumns)) ]
  accVars[x] <- (sd(compareColumns))^2
  accInfoProps[x] <- mean(aggData[accs[x]==round(aggData$meanFinalAccuracy,1),]$proportionOfInfo)
}

dataF <- data.frame("Accuracy" = accs, "Variance"= accVars)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=Variance), colour="black", stat="identity", fill=accuracyColour, alpha=0.8)

print(diffs +
        ggtitle("Variance in Information Seeking by Participant Accuracy") +
        labs(x = "Participant Accuracy", y = "Variance in MDS Distances") +
        theme_classic()) 


dataF <- data.frame("Accuracy" = accs, "InformationSeeking"= accInfoProps)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=InformationSeeking), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(diffs +
        ggtitle("Total Information Seeking by Participant Accuracy") +
        labs(x = "Participant Accuracy", y = "Total Information Seeking") +
        theme_classic()) 

# By participant type

ppts <- c("p", "e")
pptsVars <- c()
for (x in 1:length(ppts))
{
  compareColumns <- distances[grep(ppts[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppts[x], colnames(compareColumns)) ]
  pptsVars[x] <- (sd(compareColumns))^2
}

dataF <- data.frame("Participant" = ppts, "Variance"= pptsVars)
diffs <- ggplot(dataF) +
  geom_bar( aes(x=Participant, y=Variance), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(diffs +
        ggtitle("Variance in Information Seeking by Participant Type") +
        labs(x = "Participant/Expert", y = "Variance in MDS Distances") +
        theme_classic()) 


##########

# Distance heat plot by participant accuracy

accs <- c("0.3", "0.5", "0.7", "0.8")
distanceMatrix <- data.frame(matrix(ncol = 4, nrow = 4))
for (x in 1:length(accs))
{
  acc <- accs[x]
  for (y in 1:length(accs))
  {
    if (y == x)
    {
      distanceMatrix[x,y] <- 0
      colnames(distanceMatrix)[y] <- paste(y, ". ", acc)
    }
    else
    {
      comparisonAcc <- accs[y]
      compareColumns <- distances[grep(comparisonAcc, rownames(distances)), ]
      compareColumns <- compareColumns[,grep(acc, colnames(compareColumns)) ]
      meanDist <- mean(compareColumns)-3
      distanceMatrix[x,y] <- meanDist
      colnames(distanceMatrix)[y] <- paste(y, ". ", comparisonAcc)
    }
  }
  rownames(distanceMatrix)[x] <- paste(x, ". ", acc)
}

dt2 <- distanceMatrix %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000")

##########

# Distance heat plot by participant accuracy AND case

accs <- c("0.3", "0.5", "0.7", "0.8")
cases <- c("UC", "GBS", "TA", "AD", "TTP", "MTB")
accCases <- c()
for (n in 1:length(accs))
{
  for (m in 1:length(cases))
  {
    accCases[m+(length(cases)*(n-1))] <- paste(accs[n], "-", cases[m], sep="")
  }
}
distanceMatrix <- data.frame(matrix(ncol = 24, nrow = 24))
for (x in 1:length(accCases))
{
  acc <- accCases[x]
  for (y in 1:length(accCases))
  {
    if (y == x)
    {
      distanceMatrix[x,y] <- 0
      colnames(distanceMatrix)[y] <- acc
    }
    else
    {
      comparisonAcc <- accCases[y]
      currentAcc <- str_split(acc,"-")[[1]][1]
      nextAcc <- str_split(comparisonAcc,"-")[[1]][1]
      currentCase <- str_split(acc,"-")[[1]][2]
      nextCase <- str_split(comparisonAcc,"-")[[1]][2]
      compareColumns <- distances[grep(currentAcc, rownames(distances)), ]
      compareColumns <- compareColumns[grep(currentCase, rownames(compareColumns)), ]
      compareColumns <- compareColumns[,grep(nextAcc, colnames(compareColumns)) ]
      compareColumns <- compareColumns[,grep(nextCase, colnames(compareColumns)) ]
      meanDist <- mean(compareColumns)^2
      distanceMatrix[x,y] <- meanDist
      colnames(distanceMatrix)[y] <- comparisonAcc
    }
  }
  rownames(distanceMatrix)[x] <- acc
}

dt2 <- distanceMatrix %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000")


##########

# Distance heat plot by participant expertise and ability

cases <- c("UC", "GBS", "TA", "AD", "TTP", "MTB")
ppts <- c("p", "e")
casePpts <- c()
for (n in 1:length(cases))
{
  for (m in 1:length(ppts))
  {
    casePpts[m+(length(ppts)*(n-1))] <- paste(ppts[m], "-", cases[n], sep="")
  }
}
distanceMatrix <- data.frame(matrix(ncol = 12, nrow = 12))
for (x in 1:length(casePpts))
{
  case <- casePpts[x]
  for (y in 1:length(casePpts))
  {
    if (y == x)
    {
      distanceMatrix[x,y] <- 0
      colnames(distanceMatrix)[y] <- case
    }
    else
    {
      comparisonCase <- casePpts[y]
      currentPpt <- str_split(case,"-")[[1]][1]
      nextPpt <- str_split(comparisonCase,"-")[[1]][1]
      currentCase <- str_split(case,"-")[[1]][2]
      nextCase <- str_split(comparisonCase,"-")[[1]][2]
      compareColumns <- distances[grep(currentCase, rownames(distances)), ]
      compareColumns <- distances[grep(currentPpt, rownames(distances)), ]
      compareColumns <- compareColumns[,grep(nextCase, colnames(compareColumns)) ]
      compareColumns <- compareColumns[,grep(nextPpt, colnames(compareColumns)) ]
      meanDist <- mean(compareColumns)^2
      distanceMatrix[x,y] <- meanDist
      colnames(distanceMatrix)[y] <- comparisonCase
    }
  }
  rownames(distanceMatrix)[x] <- case
}

dt2 <- distanceMatrix %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000")




##########

infoSeekingFullMatrix$v1 <- mds$V1
infoSeekingFullMatrix$v2 <- mds$V2

meanCoordinatesCaseGBSv1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="GBS",]$v1)
meanCoordinatesCaseGBSv2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="GBS",]$v2)

meanCoordinatesCaseADv1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="AD",]$v1)
meanCoordinatesCaseADv2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="AD",]$v2)

meanCoordinatesCaseUCv1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="UC",]$v1)
meanCoordinatesCaseUCv2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="UC",]$v2)

meanCoordinatesCaseTTPv1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="TTP",]$v1)
meanCoordinatesCaseTTPv2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="TTP",]$v2)

meanCoordinatesCaseMTBv1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="MTB",]$v1)
meanCoordinatesCaseMTBv2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="MTB",]$v2)

meanCoordinatesCaseTAv1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="TA",]$v1)
meanCoordinatesCaseTAv2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$Condition=="TA",]$v2)

xCoordinates <- c(meanCoordinatesCaseGBSv1, meanCoordinatesCaseADv1, meanCoordinatesCaseUCv1,
                 meanCoordinatesCaseTTPv1, meanCoordinatesCaseMTBv1, meanCoordinatesCaseTAv1)
yCoordinates <- c(meanCoordinatesCaseGBSv2, meanCoordinatesCaseADv2, meanCoordinatesCaseUCv2,
                  meanCoordinatesCaseTTPv2, meanCoordinatesCaseMTBv2, meanCoordinatesCaseTAv2)
label <- c("easy", "hard", "easy", "easy", "hard", "hard")
plotDf <- data.frame(xCoordinates, yCoordinates,label)
labels <- c("GBS", "AD", "UC", "TTP", "MTB", "TA")
ggplot(plotDf, aes(x=xCoordinates, y=yCoordinates)) +
  geom_point(aes(colour=factor(label), size=0.02)) + # Show dots
  geom_text(
    label=labels, 
    nudge_x = 0.01, nudge_y = 0.01, 
    check_overlap = T
  )

##############


meanCoordinatesAccGroup1v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.3,]$v1)
meanCoordinatesAccGroup1v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.3,]$v2)

meanCoordinatesAccGroup2v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.5,]$v1)
meanCoordinatesAccGroup2v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.5,]$v2)

meanCoordinatesAccGroup3v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.7,]$v1)
meanCoordinatesAccGroup3v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.7,]$v2)

meanCoordinatesAccGroup4v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.8,]$v1)
meanCoordinatesAccGroup4v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantAccuracy==0.8,]$v2)

xCoordinates <- c(meanCoordinatesAccGroup1v1, meanCoordinatesAccGroup2v1, meanCoordinatesAccGroup3v1,
                  meanCoordinatesAccGroup4v1)
yCoordinates <- c(meanCoordinatesAccGroup1v2, meanCoordinatesAccGroup2v2, meanCoordinatesAccGroup3v2,
                  meanCoordinatesAccGroup4v2)
plotDf <- data.frame(xCoordinates, yCoordinates)
labels <- c("AccGroup1", "AccGroup2", "AccGroup3", "AccGroup4")
ggplot(plotDf, aes(x=xCoordinates, y=yCoordinates)) +
  geom_point() + # Show dots
  geom_text(
    label=labels, 
    nudge_x = 0.01, nudge_y = 0.01, 
    check_overlap = T
  )

##############

if (clusterType == "pam")
{
  # Clustering by PAM rather than k means
  pamResult <-pam(mds, k = numOfClusters)
  mds <- mds %>%
    mutate(groups = pamResult$clustering)
  infoSeekingFullMatrix$cluster <- pamResult$clustering
  mds$groups <- as.factor(mds$groups)
}
if (clusterType == "kmeans")
{
  # K-means clustering
  clust <- kmeans(mds, numOfClusters, iter.max=1000,nstart = 10)$cluster %>%
    as.factor()
  mds <- mds %>%
    mutate(groups = clust)
  infoSeekingFullMatrix$cluster <- clust
}
# Plot and color by groups
ggscatter(mds, x = "V1", y = "V2", 
          label = rownames(infoSeekingFullMatrix),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE)

## Look at clustering and whether it corresponds with ppt accuracy
# if (numOfClusters == 2)
# {
#   clusterLabels <- c()
#   accGroupLabels <- c()
#   values <- c()
#   for (i in 1:numOfClusters)
#   {
#     group <- infoSeekingFullMatrix[infoSeekingFullMatrix$cluster==i,]
#     size <- nrow(group)
#     diffGroup1 <- nrow(group[group$CaseDifficultyGroup==1,])/size
#     diffGroup2 <- nrow(group[group$CaseDifficultyGroup==2,])/size
#     
#     clusterLab <- paste("Cluster", i, sep="")
#     clusterLabels <- c(clusterLabels, rep(clusterLab , 2) )
#     accGroupLabels <- c(accGroupLabels, c("easyDiffGroup", "hardDiffGroup"))
#     values <- c(values, c(diffGroup1, diffGroup2))
#   }
#   clusterAccDf <- data.frame(clusterLabels, accGroupLabels, values)
#   
#   ggplot(clusterAccDf, aes(fill=accGroupLabels, y=values, x=clusterLabels)) + 
#     geom_bar(position="stack", stat="identity")
# }
if (numOfClusters == 2)
{
  clusterLabels <- c()
  accGroupLabels <- c()
  values <- c()
  for (i in 1:numOfClusters)
  {
    group <- infoSeekingFullMatrix[infoSeekingFullMatrix$cluster==i,]
    size <- nrow(group)
    initialAccGroup1 <- nrow(group[group$InitialCorrect==0,])/size
    initialAccGroup2 <- nrow(group[group$InitialCorrect==1,])/size

    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 2) )
    accGroupLabels <- c(accGroupLabels, c("initialDiffCorrectGroup", "initialDiffIncorrectGroup"))
    values <- c(values, c(initialAccGroup1, initialAccGroup2))
  }
  clusterAccDf <- data.frame(clusterLabels, accGroupLabels, values)

  ggplot(clusterAccDf, aes(fill=accGroupLabels, y=values, x=clusterLabels)) +
    geom_bar(position="stack", stat="identity")
}
## Look at clustering and whether it corresponds with ppt accuracy
if (numOfClusters == 3)
{
  clusterLabels <- c()
  accGroupLabels <- c()
  values <- c()
  for (i in 1:numOfClusters)
  {
    group <- infoSeekingFullMatrix[infoSeekingFullMatrix$cluster==i,]
    size <- nrow(group)
    diffGroup1 <- nrow(group[group$CaseDifficultyGroup==1,])/size
    diffGroup2 <- nrow(group[group$CaseDifficultyGroup==2,])/size
    diffGroup3 <- nrow(group[group$CaseDifficultyGroup==3,])/size
    
    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 3) )
    accGroupLabels <- c(accGroupLabels, c("1. easyDiffGroup", "2. medDiffGroup", "3. hardDiffGroup"))
    values <- c(values, c(diffGroup1, diffGroup2, diffGroup3))
  }
  clusterAccDf <- data.frame(clusterLabels, accGroupLabels, values)
  
  ggplot(clusterAccDf, aes(fill=accGroupLabels, y=values, x=clusterLabels)) + 
    geom_bar(position="stack", stat="identity")
}
## Look at clustering and whether it corresponds with ppt accuracy
if (numOfClusters == 4)
{
  clusterLabels <- c()
  accGroupLabels <- c()
  values <- c()
  for (i in 1:numOfClusters)
  {
    group <- infoSeekingFullMatrix[infoSeekingFullMatrix$cluster==i,]
    size <- nrow(group)
    accGroup1 <- nrow(group[group$ParticipantAccuracy==0.3,])/size
    accGroup2 <- nrow(group[group$ParticipantAccuracy==0.5,])/size
    accGroup3 <- nrow(group[group$ParticipantAccuracy==0.7,])/size
    accGroup4 <- nrow(group[group$ParticipantAccuracy==0.8,])/size
    
    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 4) )
    accGroupLabels <- c(accGroupLabels, c("accGroup0.3", "accGroup0.5", "accGroup0.7", "accGroup0.8"))
    values <- c(values, c(accGroup1, accGroup2, accGroup3, accGroup4))
  }
  clusterAccDf <- data.frame(clusterLabels, accGroupLabels, values)
  
  ggplot(clusterAccDf, aes(fill=accGroupLabels, y=values, x=clusterLabels)) + 
    geom_bar(position="stack", stat="identity")
}
## Look at clustering and whether it corresponds with case
if (numOfClusters == 6)
{
  clusterLabels <- c()
  caseGroupLabels <- c()
  values <- c()
  for (i in 1:numOfClusters)
  {
    group <- infoSeekingFullMatrix[infoSeekingFullMatrix$cluster==i,]
    size <- nrow(group)
    caseGroup1 <- nrow(group[group$Condition=="GBS",])/size
    caseGroup2 <- nrow(group[group$Condition=="AD",])/size
    caseGroup3 <- nrow(group[group$Condition=="UC",])/size
    caseGroup4 <- nrow(group[group$Condition=="TTP",])/size
    caseGroup5 <- nrow(group[group$Condition=="MTB",])/size
    caseGroup6 <- nrow(group[group$Condition=="TA",])/size
    
    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 6) )
    caseGroupLabels <- c(caseGroupLabels, c("caseGroupGBS", "caseGroupAD", "caseGroupUC", "caseGroupTTP", "caseGroupMTB", "caseGroupTA"))
    values <- c(values, c(caseGroup1, caseGroup2, caseGroup3, caseGroup4, caseGroup5, caseGroup6))
  }
  clusterAccDf <- data.frame(clusterLabels, caseGroupLabels, values)
  
  ggplot(clusterAccDf, aes(fill=caseGroupLabels, y=values, x=clusterLabels)) + 
    geom_bar(position="stack", stat="identity")
}

### MDS a different way

mdsInfo = mds(delta = infoSeekingFullMatrix[1:29],ndim = 2, type = "ratio" )
plot(mdsInfo)

### Correlation between initial differentials and information proportion (all cases)

diffInfo <- ggplot(data = caseDf, aes(x=initialDifferentials, y=laterInfoProp)) +
  geom_point() +
  geom_smooth(method=lm , color="skyblue", fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffInfo + 
        ggtitle("Mean Initial Differentials against Proportion of Information")
      + labs(y="Proportion of Possible Information Requested", x = "Number of Initial Differentials"))

cor.test(caseDf$initialDifferentials,caseDf$laterInfoProp,method="pearson")


### Correlation between initial differentials and final confidence (all cases)

diffCon <- ggplot(data = caseDf, aes(x=initialDifferentials, y=finalConfidence)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Mean Initial Differentials against Final Confidence")
      + labs(y="Final Confidence", x = "Number of Initial Differentials"))

cor.test(caseDf$initialDifferentials,caseDf$finalConfidence,method="pearson")

### Correlation between initial differentials and confidence change (all cases)

diffCon <- ggplot(data = caseDf, aes(x=initialDifferentials, y=confidenceChange)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Mean Initial Differentials against Change in Confidence")
      + labs(y="Confidence Change", x = "Number of Initial Differentials"))

cor.test(caseDf$initialDifferentials,caseDf$confidenceChange,method="pearson")

### Correlation between information proportion and confidence change (all cases)

diffCon <- ggplot(data = studentCaseDf, aes(x=laterInfoProp, y=confidenceChange)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Proportion against Change in Confidence")
      + labs(y="Confidence Change (Final Confidence - Initial Confidence)", x = "Proportion of Available Information Sought (During 2nd and 3rd Information Stages)"))

cor.test(studentCaseDf$laterInfoProp,studentCaseDf$confidenceChange,method="pearson")


### Correlation between information proportion and confidence change (aggregate)

diffCon <- ggplot(data = studentAggData, aes(x=laterPropOfInfo, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Proportion against Change in Confidence")
      + labs(y="Confidence Change (Final Confidence - Initial Confidence)", x = "Proportion of Available Information Sought (During 2nd and 3rd Information Stages)"))

cor.test(studentAggData$laterPropOfInfo,studentAggData$meanConfidenceOverallChange,method="pearson")


### Mixed model of confidence (case level)

mixedCaseConModel = lmer(confidenceChange ~ initialDifferentials + subjectiveDifficulty + laterInfoProp + likelihoodChange +
                           (1 | condition) + (1 | id), data = studentCaseDf, na.action="na.exclude")
summary(mixedCaseConModel)

ranef(mixedCaseConModel)

conRegressionModel <- lm(confidenceChange ~  initialDifferentials + subjectiveDifficulty + laterInfoProp + likelihoodChange, data = studentCaseDf)
summary(conRegressionModel)
# Assumption testing  
# Linearity
confidenceChange <- studentCaseDf$confidenceChange
modelLin<-plot(residuals(mixedCaseConModel),confidenceChange)

# Homogeneity of variance
studentCaseDf$res<- residuals(mixedCaseConModel) #extracts the residuals 
studentCaseDf$res <-abs(studentCaseDf$res) #absolute value of the residuals
studentCaseDf$res <- studentCaseDf$res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(res ~ id, data=studentCaseDf) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

# Normal distribution of residuals
qqmath(mixedCaseConModel, id=0.05, na.rm=TRUE)

### Correlation between information proportion similarity and confidence similarity

diffCon <- ggplot(data = studentAggData, aes(x=confidenceMatVarSimMeasure, y=infoSeekingMatCosSimMeasure)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Similarity against Confidence Similarity")
      + labs(y="Information Seeking Dissimilarity (Cosine Similarity Matrix)", x = "Confidence Dissimilarity (Variance of Mean Vector)"))

cor.test(aggData$confidenceMatVarSimMeasure,aggData$infoSeekingMatCosSimMeasure,method="pearson")

### Correlation between information proportion similarity and confidence change

diffCon <- ggplot(data = studentAggData, aes(x=infoSeekingMatCosSimMeasure, y=meanConfidenceOverallChange)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Similarity against Mean Change in Confidence")
      + labs(y="Mean Change in Confidence", x = "Information Seeking Similarity (Cosine Similarity Matrix)"))

cor.test(studentAggData$meanConfidenceOverallChange,studentAggData$infoSeekingMatCosSimMeasure,method="pearson")

### Correlation between information proportion similarity and final differentials

diffCon <- ggplot(data = aggData, aes(x=infoSeekingMatCosSimMeasure, y=meanFinalDiffs)) +
  geom_point() +
  geom_smooth(method=lm , color=differentialColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Similarity against Number of Final Differentials")
      + labs(y="Mean Number of Final Differentials", x = "Information Seeking Dissimilarity (Cosine Similarity Matrix)"))

cor.test(aggData$infoSeekingMatCosSimMeasure,aggData$meanFinalDiffs,method="pearson")

### Correlation between information proportion similarity and mean difficulty rating

diffCon <- ggplot(data = aggData, aes(x=infoSeekingMatCosSimMeasure, y=meanDifficulty)) +
  geom_point() +
  geom_smooth(method=lm , color=difficultyColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Similarity against Mean Difficulty Rating")
      + labs(y="Mean Difficulty Rating", x = "Information Seeking Similarity (Cosine Similarity Matrix)"))

cor.test(aggData$infoSeekingMatCosSimMeasure,aggData$meanDifficulty,method="pearson")

### Correlation between information proportion similarity and rational decision making style

diffCon <- ggplot(data = studentAggData, aes(x=laterPropOfInfo, y=relativeRationalism)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Similarity against Decision Making Style (Rational)")
      + labs(y="Difference between Rational and Intuition Decision Style Scores", x = "Information Seeking Similarity (Cosine Similarity Matrix)"))

cor.test(studentAggData$laterPropOfInfo,studentAggData$relativeRationalism,method="pearson")

### Correlation between information proportion similarity and accuracy

diffCon <- ggplot(data = aggData, aes(x=infoSeekingMatCosSimMeasure, y=meanFinalAccuracy)) +
  geom_point() +
  geom_smooth(method=lm , color=accuracyColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking Similarity against Accuracy")
      + labs(y="Accuracy", x = "Information Seeking Similarity (Cosine Similarity Matrix)"))

cor.test(aggData$infoSeekingMatCosSimMeasure,aggData$meanFinalAccuracy,method="pearson")

### Correlation between resolution and info seeking

diffCon <- ggplot(data = studentAggData, aes(x=proportionOfInfo, y=finalResolution)) +
  geom_point() +
  geom_smooth(method=lm , color=resolutionColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Information Seeking against Resolution")
      + labs(y="Difference in Confidence when Correct and Incorrect", x = "Proportion of Info"))

cor.test(studentAggData$proportionOfInfo,studentAggData$finalResolution,method="pearson")

### Correlation between resolution and differential accuracy

diffCon <- ggplot(data = studentAggData, aes(x=meanDifferentialAccuracy, y=proportionOfInfo)) +
  geom_point() +
  geom_smooth(method=lm , color=resolutionColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

print(diffCon + 
        ggtitle("Differential Accuracy against Resolution")
      + labs(y="Difference in Confidence when Correct and Incorrect", x = "Accuracy Over Number of Differentials"))

cor.test(studentAggData$meanDifferentialAccuracy,studentAggData$proportionOfInfo,method="pearson")


###################
m1 <- blmer(confidenceChange ~ initialDifferentials + subjectiveDifficulty + laterInfoProp + likelihoodChange + (1 | condition) + (1|id), caseDf,
            cov.prior = NULL,
            fixef.prior = normal)

summary(m1)

mixedConModel = lmer(confidenceChange ~ laterInfoProp + (1 | condition) + (1 | id), data = studentCaseDf, na.action="na.exclude")
summary(mixedConModel)

correctCases <- studentCaseDf[studentCaseDf$correct==1,]
mixedLikModel = lmer(as.integer(likelihoodOfCorrectDiagnosis) ~ laterInfoProp + (1 | condition) + (1 | id), data = correctCases, na.action="na.exclude")
summary(mixedLikModel)

mixedLikConModel = lmer(confidenceChange ~ likelihoodChange + (1 | condition) + (1 | id), data = correctCases, na.action="na.exclude")
summary(mixedLikConModel)
