
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
    
    df$numOfDifferentials[row] <- length(trialSelect$severities) 
    df$confidence[row] <- (trialSelect$confidence)/100
    if (is.null(trialSelect$diagnoses))
    {
      df$correctDiagnosis[row] <- toupper(trialSelect$trueCondition) %in% trialSelect$differentials
    }
    else
    {
      df$correctDiagnosis[row] <- toupper(trialSelect$trueCondition) %in% trialSelect$diagnoses
    }
    df$perceivedDifficulty[row] <- myData$rawData$difficulties[trialSelect$trial]
    df$highestSeverity[row] <- max(trialSelect$severities)
    df$hasHighSeverity[row] <- max(trialSelect$severities) > 2
    df$highestLikelihood[row] <- max(trialSelect$likelihoods)
    df$likelihoods[row] <- toString(trialSelect$likelihoods)
    df$severities[row] <- toString(trialSelect$severities)
    df$sevOfHighestLikelihood[row] <- trialSelect$severities[which.max(trialSelect$likelihoods)]
    df$competingDifferentials[row] <- sum(trialSelect$likelihoods>4, na.rm = TRUE)
    df$hasCompetingDifferentials[row] <- sum(trialSelect$likelihoods>4, na.rm = TRUE) > 2
    df$treatmentPlan[row] <- trialSelect$treatmentPlan
    df$infoSeekingTime[row] <- (trialSelect$totalInfoSeekingTime)/60000
    
    df$remoteDifferentials[row] <- sum(trialSelect$likelihoods<2, na.rm = TRUE)
    df$nonRemoteDifferentials[row] <- sum(trialSelect$likelihoods>1, na.rm = TRUE)
    
  }
  count <- count + length(trials)
}

# Get student ids
studentIDs <- setdiff(participantIDS, experiencedIDs)
studentsPpts <- length(studentIDs)

###############################################

#########
# Add correctness labels from external file

markedAccFile <- paste(dataFolder, "/../DifferentialsForAccuracy.csv", sep="")
accValues <- read.csv(markedAccFile, header=TRUE, sep=",")
for (x in 1:nrow(accValues))
{
  df$correct[x] <- as.integer(accValues$correct[x])
  df$brierConfidence[x] <- ((df$confidence[x]/100) - df$correct[x])^2
  liks<-strsplit(df$likelihoods[x], ", ")
  liks <- liks[[1]]
  sevs<-strsplit(df$severities[x], ", ")
  sevs <- sevs[[1]]
  if (df$numOfDifferentials[x] > 0)
  {
    if (df$correct[x] == 0)
    {
      df$differentialAccuracy[x] <- -1/df$numOfDifferentials[x]
      df$correctDiagnosisIdx[x] <- 0
      df$likelihoodOfCorrectDiagnosis[x] <- 0
      df$sevOfCorrectDiagnosis[x] <- 0
    }
    if (df$correct[x] == 1)
    {
      df$differentialAccuracy[x] <- 1/df$numOfDifferentials[x]
      df$correctDiagnosisIdx[x] <- as.integer(accValues$correct[x])
      df$likelihoodOfCorrectDiagnosis[x] <- as.integer(liks[accValues[x,]$correctDiagnosisIdx])
      df$sevOfCorrectDiagnosis[x] <- as.integer(sevs[accValues[x,]$correctDiagnosisIdx])+1
    }
    df$highestLikelihoodCorrect[x] <- accValues$highestLikelihoodCorrect[x]
    if (df$highestLikelihoodCorrect[x] == 0)
    {
      df$highestLikelihoodCorrectValue[x] <- 0
    }
    else
    {
      df$highestLikelihoodCorrectValue[x] <- accValues$highestLikelihoodValue[x]
    }
  }
  else
  {
    df$differentialAccuracy[x] <- 0
  }
}

################################################
# Aggregate df for participant wise data

aggData <- data.frame(matrix(ncol = 0, nrow = length(ids)))

for (n in 1:length(participantIDS))
{
  id <- participantIDS[n]
  aggData$participantID[n] <- id
  pptTrials <- df[df$participantID==id,]
  
  aggData$correct[n] <- mean(pptTrials$correct)
  aggData$meanInitialCorrect[n] <- mean(pptTrials[pptTrials$stage==1,]$correct)
  aggData$meanMiddleCorrect[n] <- mean(pptTrials[pptTrials$stage==2,]$correct)
  aggData$meanFinalCorrect[n] <- mean(pptTrials[pptTrials$stage==3,]$correct)
  
  aggData$meanTotalDiffs[n] <- mean(pptTrials$numOfDifferentials)
  aggData$meanInitialDiffs[n] <- mean(pptTrials[pptTrials$stage==1,]$numOfDifferentials)
  aggData$meanMiddleDiffs[n] <- mean(pptTrials[pptTrials$stage==2,]$numOfDifferentials)
  aggData$meanFinalDiffs[n] <- mean(pptTrials[pptTrials$stage==3,]$numOfDifferentials)
  aggData$averageDiffChange[n] <- aggData$meanFinalDiffs[n] - aggData$meanInitialDiffs[n]
  aggData$stage2DiffsAdded[n] <- aggData$meanMiddleDiffs[n] - aggData$meanInitialDiffs[n]
  aggData$stage3DiffsAdded[n] <- aggData$meanFinalDiffs[n] - aggData$meanMiddleDiffs[n]
  
  aggData$totalMeanConfidence[n] <- mean(pptTrials$confidence)
  aggData$meanInitialConfidence[n] <- mean(pptTrials[pptTrials$stage==1,]$confidence)
  aggData$meanMiddleConfidence[n] <- mean(pptTrials[pptTrials$stage==2,]$confidence)
  aggData$meanFinalConfidence[n] <- mean(pptTrials[pptTrials$stage==3,]$confidence)
  
  aggData$readyToTreatTrials[n] <- length(unique(pptTrials[pptTrials$treatmentPlan!="Not Provided",]$trialNum))
  
  brierVector <- pptTrials[pptTrials$stage==1,]$brierConfidence
  aggData$initialBrierScore[n] <- 1-(sum(brierVector)*(1/length(brierVector)))
  brierVector <- pptTrials[pptTrials$stage==2,]$brierConfidence
  aggData$middleBrierScore[n] <- 1-(sum(brierVector)*(1/length(brierVector)))
  brierVector <- pptTrials[pptTrials$stage==3,]$brierConfidence
  aggData$finalBrierScore[n] <- 1-(sum(brierVector)*(1/length(brierVector)))
  
  aggData$meanDifficulty[n] <- mean(pptTrials$perceivedDifficulty, na.rm=TRUE)
  aggData$proportionOfInfo[n] <- sum(pptTrials$currentTests)/(totalInfo*max(pptTrials$trialNum))
  aggData$initialPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==1,]$proportionOfInfo)
  aggData$middlePropOfInfo[n] <- mean(pptTrials[pptTrials$stage==2,]$proportionOfInfo)
  aggData$finalPropOfInfo[n] <- mean(pptTrials[pptTrials$stage==3,]$proportionOfInfo)
  aggData$laterPropOfInfo[n] <- (sum(pptTrials[pptTrials$stage==2,]$currentTests) + sum(pptTrials[pptTrials$stage==3,]$currentTests))/(sum(pptTrials[pptTrials$stage==2,]$possibleTest) + sum(pptTrials[pptTrials$stage==3,]$possibleTest))
  
  aggData$propOfInfoEasy[n] <-  sum(pptTrials[pptTrials$trueCondition %in% easyCaseGroupLong,]$currentTests)/(totalInfo*((max(pptTrials$trialNum))/2))
  #aggData$propOfInfoMed[n] <-  sum(pptTrials[pptTrials$trueCondition %in% medCaseGroupLong,]$currentTests)/(totalInfo*((max(pptTrials$trialNum))/3))
  aggData$propOfInfoHard[n] <-  sum(pptTrials[pptTrials$trueCondition %in% hardCaseGroupLong,]$currentTests)/(totalInfo*((max(pptTrials$trialNum))/2))
  
  aggData$likelihoodCorrectEasy[n] <-  mean(pptTrials[pptTrials$trueCondition %in% easyCaseGroupLong,]$highestLikelihoodCorrectValue)
  aggData$likelihoodCorrectHard[n] <-  mean(pptTrials[pptTrials$trueCondition %in% hardCaseGroupLong,]$highestLikelihoodCorrectValue)
  
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
  
  aggData$resolutionPositive[n] <- ifelse(aggData$finalResolution[n] > 0,1,0)
  
  aggData$meanRemoteDifferentials[n] <- mean(pptTrials$remoteDifferentials)
  aggData$meanDifferentialAccuracy[n] <- mean(pptTrials$differentialAccuracy)
  
  aggData$averageLikelihoodOfCorrectDiagnosis[n] <- mean(as.integer(pptTrials$likelihoodOfCorrectDiagnosis))
  aggData$averageLikelihoodOfCorrectDiagnosisInitial[n] <- mean(as.integer(pptTrials[pptTrials$stage==1,]$likelihoodOfCorrectDiagnosis))
  aggData$averageLikelihoodOfCorrectDiagnosisMiddle[n] <- mean(as.integer(pptTrials[pptTrials$stage==2,]$likelihoodOfCorrectDiagnosis))
  aggData$averageLikelihoodOfCorrectDiagnosisFinal[n] <- mean(as.integer(pptTrials[pptTrials$stage==3,]$likelihoodOfCorrectDiagnosis))
  
  aggData$highestLikelihoodCorrect[n] <- mean(as.integer(pptTrials$highestLikelihoodCorrect))
  aggData$highestLikelihoodCorrectInitial[n] <- mean(as.integer(pptTrials[pptTrials$stage==1,]$highestLikelihoodCorrect))
  aggData$highestLikelihoodCorrectMiddle[n] <- mean(as.integer(pptTrials[pptTrials$stage==2,]$highestLikelihoodCorrect))
  aggData$highestLikelihoodCorrectFinal[n] <- mean(as.integer(pptTrials[pptTrials$stage==3,]$highestLikelihoodCorrect))
  
  aggData$highestLikelihoodCorrectValue[n] <- mean(as.integer(pptTrials$highestLikelihoodCorrectValue))
  aggData$highestLikelihoodCorrectValueInitial[n] <- mean(as.integer(pptTrials[pptTrials$stage==1,]$highestLikelihoodCorrectValue))
  aggData$highestLikelihoodCorrectValueMiddle[n] <- mean(as.integer(pptTrials[pptTrials$stage==2,]$highestLikelihoodCorrectValue))
  aggData$highestLikelihoodCorrectValueFinal[n] <- mean(as.integer(pptTrials[pptTrials$stage==3,]$highestLikelihoodCorrectValue))
  
  aggData$correctDiagnosisLikelihoodGain[n] <- aggData$meanLikelihoodOfCorrectDiagnosisFinal[n] - aggData$meanLikelihoodOfCorrectDiagnosisInitial[n]
  aggData$correctDiagnosisLikelihoodGainPresent[n] <- aggData$meanLikelihoodOfCorrectDiagnosisFinalPresent[n] - aggData$meanLikelihoodOfCorrectDiagnosisInitialPresent[n]
  
  if (accuracyMeasure == "CorrectLikelihood")
  {
    aggData$accuracy[n] <- aggData$averageLikelihoodOfCorrectDiagnosis[n]/10
    aggData$meanInitialAccuracy[n] <- aggData$averageLikelihoodOfCorrectDiagnosisInitial[n]/10
    aggData$meanMiddleAccuracy[n] <- aggData$averageLikelihoodOfCorrectDiagnosisMiddle[n]/10
    aggData$meanFinalAccuracy[n] <- aggData$averageLikelihoodOfCorrectDiagnosisFinal[n]/10
    
  } else if (accuracyMeasure == "HighestLikelihood")
  {
    aggData$accuracy[n] <- aggData$highestLikelihoodCorrectValue[n]/10
    aggData$meanInitialAccuracy[n] <- aggData$highestLikelihoodCorrectValueInitial[n]/10
    aggData$meanMiddleAccuracy[n] <- aggData$highestLikelihoodCorrectValueMiddle[n]/10
    aggData$meanFinalAccuracy[n] <- aggData$highestLikelihoodCorrectValueFinal[n]/10
  } else 
  {
    aggData$accuracy[n] <- aggData$correct[n]
    aggData$meanInitialAccuracy[n] <- aggData$meanInitialCorrect[n]
    aggData$meanMiddleAccuracy[n] <- aggData$meanMiddleCorrect[n]
    aggData$meanFinalAccuracy[n] <- aggData$meanFinalCorrect[n]
  }
  
  aggData$finalCalibration[n] <- aggData$meanFinalConfidence[n] - aggData$meanFinalAccuracy[n]
  
  stage1Diffs <- pptTrials[pptTrials$stage==1,]$numOfDifferentials
  stage3Diffs <- pptTrials[pptTrials$stage==3,]$numOfDifferentials
  aggData$numOfTimesDiffsReduced[n] <- sum(stage3Diffs < stage1Diffs)
  
  
  stage1HighestLikelihood <- pptTrials[pptTrials$stage==1,]$highestLikelihood
  stage3HighestLikelihood <- pptTrials[pptTrials$stage==3,]$highestLikelihood
  aggData$numOfTimesLikelihoodsIncreased[n] <- sum(stage3HighestLikelihood > stage1HighestLikelihood)
  
  currentLiks <- str_split(pptTrials$likelihoods,",")
  
  incorrectMeanLikelihoods <- c()
  for (x in 1:18)
  {
    incorrectMeanLikelihoods[x] <- ifelse(pptTrials$correct[x]==0,0,mean(as.integer(currentLiks[[x]])[-c(pptTrials$correctDiagnosisIdx[x])]))
  }
  
  incorrectMeanLikelihoods[is.nan(incorrectMeanLikelihoods)] <- 0
  
  initialIncorrectLikelihoods <- incorrectMeanLikelihoods[c(1,4,7,10,13,16)]
  middleIncorrectLikelihoods <- incorrectMeanLikelihoods[c(2,5,8,11,14,17)]
  finalIncorrectLikelihoods <- incorrectMeanLikelihoods[c(3,6,9,12,15,18)]
  
  aggData$initialIncorrectLik[n] <- mean(initialIncorrectLikelihoods)
  aggData$middleIncorrectLik[n] <- mean(middleIncorrectLikelihoods)
  aggData$finalIncorrectLik[n] <- mean(finalIncorrectLikelihoods)
  
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
# Seperate into student and expert datasets


studentDf <- df[!(df$participantID %in% experiencedIDs),]
studentAggData <- aggData[!(aggData$participantID %in% experiencedIDs),]

expertDf <- df[df$participantID %in% experiencedIDs,]
expertAggData <- aggData[aggData$participantID %in% experiencedIDs,]


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
  caseDf$initialDifferentials[y] <- df$numOfDifferentials[(3*y)-2]
  caseDf$finalDifferentials[y] <- df$numOfDifferentials[(3*y)]
  caseDf$initialCorrect[y] <- df$correct[(3*y)-2]
  caseDf$caseInformationProportion[y] <- (df$currentTests[(3*y)-2] + df$currentTests[(3*y)-1] + df$currentTests[(3*y)])/(df$possibleTest[(3*y)-2] + df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  caseDf$laterInfoProp[y] <- (df$currentTests[(3*y)-1] + df$currentTests[(3*y)])/(df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  caseDf$finalConfidence[y] <-df$confidence[(3*y)]
  caseDf$confidenceChange[y] <-df$confidence[(3*y)] - df$confidence[(3*y)-2]
  caseDf$brierConfidence[y] <- df$brierConfidence[(3*y)]
  caseDf$subjectiveDifficulty[y] <- df$perceivedDifficulty[(3*y)]
  caseDf$caseInformationReqs[y] <- df$currentTests[(3*y)-2] + df$currentTests[(3*y)-1] + df$currentTests[(3*y)]
  caseDf$likelihoodChange[y] <- df$highestLikelihood[(3*y)] - df$highestLikelihood[(3*y)-2]
  caseDf$differentialChange[y] <- df$numOfDifferentials[(3*y)] - df$numOfDifferentials[(3*y)-2]
  caseDf$confidenceArray[y] <- toString(c(df$confidence[(3*y)-2],df$confidence[(3*y)-1],df$confidence[(3*y)]))
  caseDf$highestFinalLikelihood[y] <- df$highestLikelihood[(3*y)]
  correct <- df$correct[(3*y)]
  caseDf$correct[y] <- correct
  if (correct == 1)
  {
    if (df$correct[(3*y)-2] == 1)
    {
      caseDf$earliestCorrectStage[y] <- 1
    } else if (df$correct[(3*y)-1] == 1) {
      caseDf$earliestCorrectStage[y] <- 2
    } else {
      caseDf$earliestCorrectStage[y] <- 3
    }
  }
  else
  {
    if (df$correct[(3*y)-1] == 1)
    {
      caseDf$earliestCorrectStage[y] <- -1
    } else if (df$correct[(3*y)-2] == 1)
    {
      caseDf$earliestCorrectStage[y] <- -2
    }
    else {
      caseDf$earliestCorrectStage[y] <- 0
    }
  }
  caseDf$likelihoodOfCorrectDiagnosis[y] <- df$likelihoodOfCorrectDiagnosis[(3*y)]
  caseDf$likelihoodOfCorrectDiagnosisSigned[y] <- ifelse(correct==1,df$likelihoodOfCorrectDiagnosis[(3*y)],df$highestLikelihood[(3*y)]*-1)
  caseDf$sevOfCorrectDiagnosis[y] <- df$sevOfCorrectDiagnosis[(3*y)]
  caseDf$differentialsDropped[y] <- (df$numOfDifferentials[(3*y)-2] > df$numOfDifferentials[(3*y)-1]) ||(df$numOfDifferentials[(3*y)-1] > df$numOfDifferentials[(3*y)])
  caseDf$likelihoodsUpdated[y] <- !((df$likelihoods[(3*y)-2] == df$likelihoods[(3*y)-1]) || (df$likelihoods[(3*y)-1] == df$likelihoods[(3*y)]))
  caseDf$sevOfHighestLikelihoodInitial[y] <- df$sevOfHighestLikelihood[(3*y)-2]
  caseDf$sevOfHighestLikelihoodFinal[y] <- df$sevOfHighestLikelihood[(3*y)]
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
## Seperate case df into student and expert datasets

studentCaseDf <- caseDf[!(caseDf$id %in% experiencedIDs),]
expertCaseDf <- caseDf[caseDf$id %in% experiencedIDs,]

################################################
# Run info seeking exclusions

if (runExclusions)
{
  idsRemove <- c()
  
  for (x in 1:nrow(studentCaseDf))
  {
    arr <- as.numeric(str_split(studentCaseDf$reqTestArray,", ")[[x]])
    infoSought <- sum(arr==1)
    if (infoSought < 2)
    {
      idsRemove <- c(idsRemove, studentCaseDf$id[x])
    }
  }
  
  idsRemove <- unique(idsRemove)
  
  studentAggData <- studentAggData[!studentAggData$participantID %in% idsRemove,]
  studentCaseDf <- studentCaseDf[!studentCaseDf$id %in% idsRemove,]
}

################################################
## Generate matrix for information seeking data analysis

infoSeekingFullMatrix <- data.frame()
confidenceMatrix <- data.frame()

for (n in 1:nrow(studentCaseDf))
{
  row <- studentCaseDf$reqTestArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  infoSeekingFullMatrix <- rbind(infoSeekingFullMatrix, row)
  row <- studentCaseDf$confidenceArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  confidenceMatrix <- rbind(confidenceMatrix, row)
}

for (n in 1:nrow(expertCaseDf))
{
  row <- expertCaseDf$reqTestArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  infoSeekingFullMatrix <- rbind(infoSeekingFullMatrix, row)
  row <- expertCaseDf$confidenceArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  confidenceMatrix <- rbind(confidenceMatrix, row)
}

ids <- unique(studentCaseDf$id)
for (n in 1:nrow(studentCaseDf))
{
  currentCondition <- studentCaseDf$condition[n]
  idx <- match(currentCondition,conditionsLong)
  infoSeekingFullMatrix$id[n] <- studentCaseDf$id[n]
  infoSeekingFullMatrix$condition[n] <- conditionsShort[idx]
  if (conditionsShort[idx] %in% easyCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 1
  }
  if (conditionsShort[idx] %in% hardCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 2
  }
  infoSeekingFullMatrix$correct[n] <- studentCaseDf$correct[n]
  infoSeekingFullMatrix$pptAccuracy[n] <- round(studentAggData[studentAggData$participantID==studentCaseDf$id[n],]$meanFinalAccuracy, digits = 1)
  infoSeekingFullMatrix$initialCorrect[n] <- studentCaseDf$initialCorrect[n]
  infoSeekingFullMatrix$participantType[n] <- "p"
  
  accuracyQuantiles <- quantile(studentAggData$meanFinalAccuracy)
  accuracyVal <- studentAggData[studentAggData$participantID==studentCaseDf$id[n],]$meanFinalAccuracy 
  
  confidenceQuantiles <- quantile(studentAggData$meanFinalConfidence)
  confidenceVal <- studentAggData[studentAggData$participantID==studentCaseDf$id[n],]$meanFinalConfidence - studentAggData[studentAggData$participantID==studentCaseDf$id[n],]$meanInitialConfidence
  
  infoSeekingFullMatrix$accuracyScore[n] <- accuracyVal
  if (accuracyVal > accuracyQuantiles[4])
  {
    infoSeekingFullMatrix$accuracyGroup[n] <- 4
  } else if (accuracyVal > accuracyQuantiles[3])
  {
    infoSeekingFullMatrix$accuracyGroup[n] <- 3
  } else if (accuracyVal > accuracyQuantiles[2])
  {
    infoSeekingFullMatrix$accuracyGroup[n] <- 2
  } else
  {
    infoSeekingFullMatrix$accuracyGroup[n] <- 1
  }
  
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
  
  infoSeekingFullMatrix$sevOfHighestLikelihood[n] <- studentCaseDf$sevOfHighestLikelihoodFinal[n]
  medBrier <- median(aggData$finalBrierScore)
  infoSeekingFullMatrix$ResolutionGroup[n] <- ifelse(studentAggData[studentAggData$participantID==studentCaseDf$id[n],]$finalBrierScore>medBrier,1,0)
  infoSeekingFullMatrix$likAccuracy[n] <- studentCaseDf$likelihoodOfCorrectDiagnosis[n]
  rowName <- paste("p", match(studentCaseDf$id[n],studentIDs), "-accGroup", infoSeekingFullMatrix$accuracyGroup[n] , "-", conditionsShort[idx], "-sev", studentCaseDf$sevOfHighestLikelihoodInitial[n], "-resGroup", infoSeekingFullMatrix$ResolutionGroup[n], sep="")
  rownames(infoSeekingFullMatrix)[n] <- rowName
  rownames(confidenceMatrix)[n] <- rowName
}
offset <- nrow(studentCaseDf)
for (m in 1:nrow(expertCaseDf))
{
  currentCondition <- expertCaseDf$condition[m]
  idx <- match(currentCondition,conditionsLong)
  infoSeekingFullMatrix$id[m+offset] <- expertCaseDf$id[m]
  infoSeekingFullMatrix$condition[m+offset] <- conditionsShort[idx]
  if (conditionsShort[idx] %in% easyCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[m+offset] <- 1
  }
  if (conditionsShort[idx] %in% hardCaseGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[m+offset] <- 2
  }
  infoSeekingFullMatrix$correct[m+offset] <- expertCaseDf$correct[m]
  infoSeekingFullMatrix$pptAccuracy[m+offset] <- round(expertAggData[expertAggData$participantID==expertCaseDf$id[m],]$meanFinalAccuracy, digits = 1)
  infoSeekingFullMatrix$initialCorrect[m+offset] <- expertCaseDf$initialCorrect[m]
  infoSeekingFullMatrix$participantType[m+offset] <- "e"
  
  accuracyQuantiles <- quantile(expertAggData$meanFinalAccuracy)
  accuracyVal <- expertAggData[expertAggData$participantID==expertCaseDf$id[m],]$meanFinalAccuracy 
  
  confidenceQuantiles <- quantile(expertAggData$meanFinalConfidence)
  confidenceVal <- expertAggData[expertAggData$participantID==expertCaseDf$id[m],]$meanFinalConfidence - expertAggData[expertAggData$participantID==expertCaseDf$id[m],]$meanInitialConfidence 
  
  infoSeekingFullMatrix$accuracyScore[m+offset] <- accuracyVal
  if (accuracyVal > accuracyQuantiles[4])
  {
    infoSeekingFullMatrix$accuracyGroup[m+offset] <- 4
  } else if (accuracyVal > accuracyQuantiles[3])
  {
    infoSeekingFullMatrix$accuracyGroup[m+offset] <- 3
  } else if (accuracyVal > accuracyQuantiles[2])
  {
    infoSeekingFullMatrix$accuracyGroup[m+offset] <- 2
  } else
  {
    infoSeekingFullMatrix$accuracyGroup[m+offset] <- 1
  }
  
  infoSeekingFullMatrix$confidenceScore[m+offset] <- confidenceVal
  if (confidenceVal > confidenceQuantiles[4])
  {
    infoSeekingFullMatrix$confidenceGroup[m+offset] <- 4
  } else if (confidenceVal > confidenceQuantiles[3])
  {
    infoSeekingFullMatrix$confidenceGroup[m+offset] <- 3
  } else if (confidenceVal > confidenceQuantiles[2])
  {
    infoSeekingFullMatrix$confidenceGroup[m+offset] <- 2
  } else
  {
    infoSeekingFullMatrix$confidenceGroup[m+offset] <- 1
  }
  
  infoSeekingFullMatrix$sevOfHighestLikelihood[m+offset] <- expertCaseDf$sevOfHighestLikelihoodInitial[m]
  medBrier <- median(aggData$finalBrierScore)
  infoSeekingFullMatrix$ResolutionGroup[m+offset] <- ifelse(expertAggData[expertAggData$participantID==expertCaseDf$id[n],]$finalBrierScore>medBrier,1,0)
  infoSeekingFullMatrix$likAccuracy[m+offset] <- expertCaseDf$likelihoodOfCorrectDiagnosis[m+offset]
  rowName <- paste("e", ceil(m/6), "-accGroup", infoSeekingFullMatrix$accuracyGroup[m+offset] , "-confGroup", infoSeekingFullMatrix$confidenceGroup[m+offset] , "-", conditionsShort[idx], "-sev", expertCaseDf$sevOfHighestLikelihoodInitial[m], "-exp", sep="")
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
colnames(infoSeekingFullMatrix)[37] <- "AccuracyScore"
colnames(infoSeekingFullMatrix)[38] <- "AccuracyGroup"
colnames(infoSeekingFullMatrix)[39] <- "ConfidenceScore"
colnames(infoSeekingFullMatrix)[40] <- "ConfidenceGroup"
colnames(infoSeekingFullMatrix)[41] <- "sevOfHighestLikelihood"
colnames(infoSeekingFullMatrix)[42] <- "ResolutionGroup"
colnames(infoSeekingFullMatrix)[43] <- "LikelihoodAcc"
