
participantIDS <- c()

################################################
# Main df for stage wise data from JSON files

count <- 0
for (id in ids)
{
  participantID <- str_split(id, "/", simplify = TRUE)
  participantID <- participantID[length(participantID)]
  participantIDS <- c(participantIDS, participantID)
  files <- list.files(paste(dataFolder, "/", participantID, sep="")) 
  file <- files[1]
  filePath <- paste(dataFolder, "/", participantID, "/", file, sep="")
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
    df$trueCondition[row] <- str_replace((toupper(trialSelect$trueCondition)),"-","")
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
    
    df$confidence[row] <- trialSelect$confidence

    df$perceivedDifficulty[row] <- myData$rawData$difficulties[trialSelect$trial]

    df$treatmentPlan[row] <- trialSelect$treatmentPlan
    df$infoSeekingTime[row] <- (trialSelect$totalInfoSeekingTime)/60000
    
  }
  count <- count + length(trials)
}

###############################################
################################################
# Aggregate df for participant wise data

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
  
  aggData$readyToTreatTrials[n] <- length(unique(pptTrials[pptTrials$treatmentPlan!="Not Provided",]$trialNum))
  
  aggData$meanDifficulty[n] <- mean(pptTrials$perceivedDifficulty, na.rm=TRUE)
  aggData$proportionOfInfo[n] <- sum(pptTrials$currentTests)/(totalInfo*max(pptTrials$trialNum))
  aggData$initialPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==1,]$proportionOfInfo)
  aggData$middlePropOfInfo[n] <- mean(pptTrials[pptTrials$stage==2,]$proportionOfInfo)
  aggData$finalPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==3,]$proportionOfInfo)
  aggData$laterPropOfInfo[n] <- (sum(pptTrials[pptTrials$stage==2,]$currentTests) + sum(pptTrials[pptTrials$stage==3,]$currentTests))/(sum(pptTrials[pptTrials$stage==2,]$possibleTest) + sum(pptTrials[pptTrials$stage==3,]$possibleTest))
  
  aggData$propOfInfoEasy[n] <-  sum(pptTrials[pptTrials$trueCondition %in% easyCaseGroupLong,]$currentTests)/(totalInfo*((max(pptTrials$trialNum))/2))
  aggData$propOfInfoHard[n] <-  sum(pptTrials[pptTrials$trueCondition %in% hardCaseGroupLong,]$currentTests)/(totalInfo*((max(pptTrials$trialNum))/2))
  
  aggData$meanConfidenceChangeStage2[n] <-  aggData$meanMiddleConfidence[n] - aggData$meanInitialConfidence[n]
  aggData$meanConfidenceChangeStage3[n] <-  aggData$meanFinalConfidence[n] - aggData$meanMiddleConfidence[n]
  aggData$meanConfidenceOverallChange[n] <-  aggData$meanFinalConfidence[n] - aggData$meanInitialConfidence[n]
  
  files <- list.files(paste(dataFolder, "/", id, sep="")) 
  fileName <- files[1]
  fileName <- paste(dataFolder, "/", id, "/", fileName, sep="")
  processFile(fileName)
  myData <- fromJSON(file=fileName)
  demoQs <- myData$rawData$demoQuestionnaire
  aggData$age[n] <- as.integer(demoQs[1])
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
  
  if ("debrief" %in% names(myData$processedData))
  {
    aggData$diagnosticApproach[n] <- myData$processedData$debrief$DiagnosticApproach
  }
  else
  {
    aggData$diagnosticApproach[n] <- ""
  }
}


################################################
## Generate df for case-wise data

nCase <- length(ids)*6
caseDf <- data.frame(matrix(ncol = 0, nrow = nCase))
#Generate tests and paired values
allTests <- c(testSet1, testSet2, testSet3)
testIndexes <- c(1:length(allTests))
for (y in 1:nCase)
{
  caseDf$id[y] <- df$participantID[(3*y)-2]
  caseDf$condition[y] <- df$trueCondition[(3*y)-2]
  caseIdx <- match(caseDf$condition[y],conditionsLong)
  caseDf$caseCode[y] <- conditionsShort[caseIdx]

  caseDf$caseInformationProportion[y] <- (df$currentTests[(3*y)-2] + df$currentTests[(3*y)-1] + df$currentTests[(3*y)])/(df$possibleTest[(3*y)-2] + df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  caseDf$laterInfoProp[y] <- (df$currentTests[(3*y)-1] + df$currentTests[(3*y)])/(df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  caseDf$finalConfidence[y] <-df$confidence[(3*y)]
  caseDf$confidenceChange[y] <-df$confidence[(3*y)] - df$confidence[(3*y)-2]

  caseDf$subjectiveDifficulty[y] <- df$perceivedDifficulty[(3*y)]
  caseDf$caseInformationReqs[y] <- df$currentTests[(3*y)-2] + df$currentTests[(3*y)-1] + df$currentTests[(3*y)]

  caseDf$confidenceArray[y] <- toString(c(df$confidence[(3*y)-2],df$confidence[(3*y)-1],df$confidence[(3*y)]))

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


################################################
## Generate matrix for information seeking data analysis

infoSeekingFullMatrix <- data.frame()
confidenceMatrix <- data.frame()

for (n in 1:nrow(caseDf))
{
  row <- caseDf$reqTestArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  infoSeekingFullMatrix <- rbind(infoSeekingFullMatrix, row)
  row <- caseDf$confidenceArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  confidenceMatrix <- rbind(confidenceMatrix, row)
}

ids <- unique(caseDf$id)
for (n in 1:nrow(caseDf))
{
  currentCondition <- caseDf$condition[n]
  idx <- match(currentCondition,conditionsLong)
  infoSeekingFullMatrix$id[n] <- caseDf$id[n]
  infoSeekingFullMatrix$condition[n] <- conditionsShort[idx]
  if (conditionsShort[idx] %in% easyCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 1
  }
  if (conditionsShort[idx] %in% hardCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 2
  }

  
  confidenceQuantiles <- quantile(aggData$meanFinalConfidence)
  confidenceVal <- aggData[aggData$participantID==caseDf$id[n],]$meanFinalConfidence 

  
  infoSeekingFullMatrix$confidenceScore[n] <- confidenceVal
  if (confidenceVal > confidenceQuantiles[4])
  {
    infoSeekingFullMatrix$confidenceGroup[n] <- 4
  } else if (confidenceVal > confidenceQuantiles[3])
  {
    infoSeekingFullMatrix$confidenceGroup[n] <- 3
  } else if (confidenceVal > confidenceQuantiles[2])
  {
    infoSeekingFullMatrix$confidenceGroup[n] <- 2
  } else
  {
    infoSeekingFullMatrix$confidenceGroup[n] <- 1
  }
  
  rowName <- paste("p", match(caseDf$id[n],participantIDS), conditionsShort[idx], sep="")
  rownames(infoSeekingFullMatrix)[n] <- rowName
  rownames(confidenceMatrix)[n] <- rowName
}

colnames(infoSeekingFullMatrix) <- c(1:29)
colnames(infoSeekingFullMatrix)[30] <- "ID"
colnames(infoSeekingFullMatrix)[31] <- "Condition"
colnames(infoSeekingFullMatrix)[32] <- "CaseDifficultyGroup"
colnames(infoSeekingFullMatrix)[33] <- "ConfidenceScore"
colnames(infoSeekingFullMatrix)[34] <- "ConfidenceGroup"
