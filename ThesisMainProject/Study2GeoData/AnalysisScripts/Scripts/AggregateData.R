
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
    df$trueCountry[row] <- toupper(trialSelect$trueCountry)
    df$requestedTests[row] <- trialSelect$numOfRequestedInfo
    
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
    
    df$uniqueTests[row] <- length(unique(trialSelect$requestedInfoText)) # this only captures unique across the subtrial NOT the trial
    
    df$pastTests[row] <- length(setdiff(trialSelect$requestedInfoText, currentTests))
    df$currentTests[row] <- df$uniqueTests[row] - df$pastTests[row]
    df$possibleTest[row] <- possibleTests
    df$proportionOfInfo[row] <- df$currentTests[row]/possibleTests
    df$testNames[row] <- toString(unique(trialSelect$requestedInfoText))
  
    df$confidence[row] <- trialSelect$confidence
    df$correctCountry[row] <- toupper(trialSelect$trueCountry) %in% trialSelect$differentials
    df$perceivedDifficulty[row] <- myData$rawData$difficulties[trialSelect$trial]
    if (length(trialSelect$differentials) > 0)
    {
      df$highestLikelihood[row] <- max(trialSelect$likelihoods)
      df$likelihoods[row] <- toString(trialSelect$likelihoods)
      df$competingDifferentials[row] <- sum(trialSelect$likelihoods>4, na.rm = TRUE)
      df$hasCompetingDifferentials[row] <- sum(trialSelect$likelihoods>4, na.rm = TRUE) > 2
      df$numOfDifferentials[row] <- length(trialSelect$likelihoods) 
    } else
    {
      df$highestLikelihood[row] <- 0
      df$likelihoods[row] <- "0"
      df$competingDifferentials[row] <- 0
      df$hasCompetingDifferentials[row] <- FALSE
      df$numOfDifferentials[row] <- 0
    }
    df$infoSeekingTime[row] <- (trialSelect$totalInfoSeekingTime)/60000
    
  }
  count <- count + length(trials)
}

###############################################

#########
# Add correctness labels from external file

markedAccFile <- "./countriesForCoding.csv"
accValues <- read.csv(markedAccFile, header=TRUE, sep=",")
for (x in 1:nrow(accValues))
{
  df$correct[x] <- accValues$correct[x]
  liks<-strsplit(df$likelihoods[x], ", ")
  liks <- liks[[1]]
  if (df$numOfDifferentials[x] > 0)
  {
    if (df$correct[x] == 0)
    {
      df$differentialAccuracy[x] <- -1/df$numOfDifferentials[x]
      df$likelihoodOfCorrectCountry[x] <- 0
      if (length(liks) > 1)
      {
        df$incorrectLikelihood[x] <- mean(as.integer(liks))
      }
      else
      {
        df$incorrectLikelihood[x] <- 0
      }
    }
    if (df$correct[x] == 1)
    {
      df$differentialAccuracy[x] <- 1/df$numOfDifferentials[x]
      corIdx <- accValues[x,]$correctCountryIdx
      df$likelihoodOfCorrectCountry[x] <- liks[corIdx]
      if (length(liks) > 1)
      {
        df$incorrectLikelihood[x] <- mean(as.integer(liks[-c(corIdx)]))
      }
      else
      {
        df$incorrectLikelihood[x] <- 0
      }
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
surveysFile <- "./allSurveys.csv"
surveys <- read.csv(surveysFile, header=TRUE, sep=",")
answersFile <- "./GeographyAnswersShort.csv"
geoAnswers <- read.csv(answersFile, header=TRUE, sep=",")

for (n in 1:length(participantIDS))
{
  id <- participantIDS[n]
  aggData$participantID[n] <- id
  pptTrials <- df[df$participantID==id,]
  aggData$accuracy[n] <- mean(pptTrials$correct)
  aggData$meanInitialCorrect[n] <- mean(pptTrials[pptTrials$stage==1,]$correct)
  aggData$meanMiddleCorrect[n] <- mean(pptTrials[pptTrials$stage==2,]$correct)
  aggData$meanFinalCorrect[n] <- mean(pptTrials[pptTrials$stage==3,]$correct)
  
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

  aggData$meanHighestInitialLikelihood[n] <- mean(pptTrials[pptTrials$stage==1,]$highestLikelihood)
  aggData$meanHighestFinalLikelihood[n] <- mean(pptTrials[pptTrials$stage==3,]$highestLikelihood)
  aggData$meanLikelihoodGain[n] <- mean(pptTrials[pptTrials$stage==3,]$highestLikelihood) - mean(pptTrials[pptTrials$stage==1,]$highestLikelihood)
  
  aggData$meanConfidenceChangeStage2[n] <-  aggData$meanMiddleConfidence[n] - aggData$meanInitialConfidence[n]
  aggData$meanConfidenceChangeStage3[n] <-  aggData$meanFinalConfidence[n] - aggData$meanMiddleConfidence[n]
  aggData$meanConfidenceOverallChange[n] <-  aggData$meanFinalConfidence[n] - aggData$meanInitialConfidence[n]
  
  
  aggData$meanConfidenceWhenCorrect[n] <- mean(pptTrials[pptTrials$stage==1&pptTrials$correct==1,]$confidence)
  if (nrow(pptTrials[pptTrials$stage==1&pptTrials$correct==0,]) > 0)
  {
    aggData$meanConfidenceWhenIncorrect[n] <- mean(pptTrials[pptTrials$stage==1&pptTrials$correct==0,]$confidence)
  } else {
    aggData$meanConfidenceWhenIncorrect[n] <- 0 
  }
  aggData$initialResolution[n] <- aggData$meanConfidenceWhenCorrect[n] - aggData$meanConfidenceWhenIncorrect[n]
  
  aggData$meanConfidenceWhenCorrect[n] <- mean(pptTrials[pptTrials$stage==2&pptTrials$correct==1,]$confidence)
  if (nrow(pptTrials[pptTrials$stage==2&pptTrials$correct==0,]) > 0)
  {
    aggData$meanConfidenceWhenIncorrect[n] <- mean(pptTrials[pptTrials$stage==2&pptTrials$correct==0,]$confidence)
  } else {
    aggData$meanConfidenceWhenIncorrect[n] <- 0 
  }
  aggData$middleResolution[n] <- aggData$meanConfidenceWhenCorrect[n] - aggData$meanConfidenceWhenIncorrect[n]
  
  aggData$meanConfidenceWhenCorrect[n] <- mean(pptTrials[pptTrials$stage==3&pptTrials$correct==1,]$confidence)
  if (nrow(pptTrials[pptTrials$stage==3&pptTrials$correct==0,]) > 0)
  {
    aggData$meanConfidenceWhenIncorrect[n] <- mean(pptTrials[pptTrials$stage==3&pptTrials$correct==0,]$confidence)
  } else {
    aggData$meanConfidenceWhenIncorrect[n] <- 0 
  }
  aggData$finalResolution[n] <- aggData$meanConfidenceWhenCorrect[n] - aggData$meanConfidenceWhenIncorrect[n]

  aggData$meanDifferentialAccuracy[n] <- mean(pptTrials$differentialAccuracy)
  
  aggData$averageLikelihoodOfCorrectCountry[n] <- mean(as.integer(pptTrials$likelihoodOfCorrectCountry))
  aggData$averageLikelihoodOfCorrectCountryInitial[n] <- mean(as.integer(pptTrials[pptTrials$stage==1,]$likelihoodOfCorrectCountry))
  aggData$averageLikelihoodOfCorrectCountryMiddle[n] <- mean(as.integer(pptTrials[pptTrials$stage==2,]$likelihoodOfCorrectCountry))
  aggData$averageLikelihoodOfCorrectCountryFinal[n] <- mean(as.integer(pptTrials[pptTrials$stage==3,]$likelihoodOfCorrectCountry))
  
  aggData$incorrectLikelihood[n] <- mean(as.integer(pptTrials$incorrectLikelihood))
  aggData$incorrectLikelihoodInitial[n] <- mean(as.integer(pptTrials[pptTrials$stage==1,]$incorrectLikelihood))
  aggData$incorrectLikelihoodMiddle[n] <- mean(as.integer(pptTrials[pptTrials$stage==2,]$incorrectLikelihood))
  aggData$incorrectLikelihoodFinal[n] <- mean(as.integer(pptTrials[pptTrials$stage==3,]$incorrectLikelihood))
  
  aggData$highestLikelihoodCorrectValue[n] <- mean(as.integer(pptTrials$highestLikelihoodCorrectValue))
  aggData$highestLikelihoodCorrectValueInitial[n] <- mean(as.integer(pptTrials[pptTrials$stage==1,]$highestLikelihoodCorrectValue))
  aggData$highestLikelihoodCorrectValueMiddle[n] <- mean(as.integer(pptTrials[pptTrials$stage==2,]$highestLikelihoodCorrectValue))
  aggData$highestLikelihoodCorrectValueFinal[n] <- mean(as.integer(pptTrials[pptTrials$stage==3,]$highestLikelihoodCorrectValue))
    
  aggData$correctDiagnosisLikelihoodGain[n] <- aggData$meanLikelihoodOfCorrectDiagnosisFinal[n] - aggData$meanLikelihoodOfCorrectDiagnosisInitial[n]
  aggData$correctDiagnosisLikelihoodGainPresent[n] <- aggData$meanLikelihoodOfCorrectDiagnosisFinalPresent[n] - aggData$meanLikelihoodOfCorrectDiagnosisInitialPresent[n]
  
  if (accuracyMeasure == "CorrectLikelihood")
  {
    aggData$accuracy[n] <- aggData$averageLikelihoodOfCorrectCountry[n]/10
    aggData$meanInitialAccuracy[n] <- aggData$averageLikelihoodOfCorrectCountryInitial[n]/10 
    aggData$meanMiddleAccuracy[n] <- aggData$averageLikelihoodOfCorrectCountryMiddle[n]/10
    aggData$meanFinalAccuracy[n] <- aggData$averageLikelihoodOfCorrectCountryFinal[n]/10
    
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
  
  aggData$geoScore[n] <- as.integer(surveys[surveys$ParticipantID==id,]$Score)
  
  decisionRationalScore <- 0
  decisionIntuitionScore <- 0
  
  extraversionScore <- 0
  agreeableScore <- 0
  conscientiousScore <- 0
  neuroticScore <- 0 
  opennessScore <- 0
  
  surveys[,grep("REV", colnames(surveys)) ] <- 6-surveys[,grep("REV", colnames(surveys)) ]
  
  extraversionCols <- rowSums(surveys[,grep("BFE", colnames(surveys)) ])
  agreeableCols <- rowSums(surveys[,grep("BFA", colnames(surveys)) ])
  conscientiousCols <- rowSums(surveys[,grep("BFC", colnames(surveys)) ])
  neuroticCols <- rowSums(surveys[,grep("BFN", colnames(surveys)) ])
  openessCols <- rowSums(surveys[,grep("BFO", colnames(surveys)) ])
  decisionRationalCols <- rowSums(surveys[,grep("DMR", colnames(surveys)) ])
  decisionIntuitionCols <- rowSums(surveys[,grep("DMI", colnames(surveys)) ])
  
  aggData$decisionRationalScore[n] <- decisionRationalCols[n]
  aggData$decisionIntuitionScore[n] <-decisionIntuitionCols[n]
  aggData$relativeRationalism[n] <- aggData$decisionRationalScore[n] - aggData$decisionIntuitionScore[n]
  
  aggData$extraversionScore[n] <- extraversionCols[n]
  aggData$agreeableScore[n] <- agreeableCols[n]
  aggData$conscientiousScore[n] <- conscientiousCols[n]
  aggData$neuroticScore[n] <- neuroticCols[n]
  aggData$opennessScore[n] <- openessCols[n]
}

################################################
## Generate df for case-wise data

nCountry <- length(ids)*6
countryDf <- data.frame(matrix(ncol = 0, nrow = nCountry))
#Generate tests and paired values
allTests <- c(testSet1, testSet2, testSet3)
testIndexes <- c(1:length(allTests))
for (y in 1:nCountry)
{
  countryDf$id[y] <- df$participantID[(3*y)-2]
  countryDf$country[y] <- df$trueCountry[(3*y)-2]
  countryIdx <- match(countryDf$country[y],countriesLong)
  countryDf$countryCode[y] <- countriesShort[countryIdx]
  countryDf$initialDifferentials[y] <- df$numOfDifferentials[(3*y)-2]
  countryDf$initialCorrect[y] <- df$correct[(3*y)-2]
  countryDf$caseInformationProportion[y] <- (df$uniqueTests[(3*y)-2] + df$uniqueTests[(3*y)-1] + df$uniqueTests[(3*y)])/(df$possibleTest[(3*y)-2] + df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  countryDf$laterInfoProp[y] <- (df$uniqueTests[(3*y)-1] + df$uniqueTests[(3*y)])/(df$possibleTest[(3*y)-1] + df$possibleTest[(3*y)])
  countryDf$finalConfidence[y] <-df$confidence[(3*y)]
  countryDf$confidenceChange[y] <-df$confidence[(3*y)] - df$confidence[(3*y)-2]
  countryDf$subjectiveDifficulty[y] <- df$perceivedDifficulty[(3*y)]
  countryDf$caseInformationReqs[y] <- df$uniqueTests[(3*y)-2] + df$uniqueTests[(3*y)-1] + df$uniqueTests[(3*y)]
  countryDf$likelihoodChange[y] <- df$highestLikelihood[(3*y)] - df$highestLikelihood[(3*y)-2]
  countryDf$differentialChange[y] <- df$numOfDifferentials[(3*y)] - df$numOfDifferentials[(3*y)-2]
  countryDf$confidenceArray[y] <- toString(c(df$confidence[(3*y)-2],df$confidence[(3*y)-1],df$confidence[(3*y)]))
  countryDf$highestFinalLikelihood[y] <- df$highestLikelihood[(3*y)]
  countryDf$correct[y] <- df$correct[(3*y)]
  countryDf$likelihoodOfCorrectCountry[y] <- df$likelihoodOfCorrectCountry[(3*y)]
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
  countryDf$reqTestArray[y] <- toString(testArray)
}

################################################
## Generate matrix for information seeking data analysis

infoSeekingFullMatrix <- data.frame()
confidenceMatrix <- data.frame()

for (n in 1:nrow(countryDf))
{
  row <- countryDf$reqTestArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  infoSeekingFullMatrix <- rbind(infoSeekingFullMatrix, row)
  row <- countryDf$confidenceArray[n]
  row <- strsplit(row, ", ")
  row <- row[[1]]
  row <- as.integer(row)
  confidenceMatrix <- rbind(confidenceMatrix, row)
}

ids <- unique(countryDf$id)
geoKnowledgeArr <- c()
count <- 1
for (n in 1:nrow(countryDf))
{
  currentCondition <- countryDf$country[n]
  idx <- match(currentCondition,countriesLong)
  infoSeekingFullMatrix$id[n] <- countryDf$id[n]
  infoSeekingFullMatrix$country[n] <- countriesShort[idx]
  if (countriesShort[idx] %in% easyCountryGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 1
  }
  if (countriesShort[idx] %in% hardCountryGroup)
  {
    infoSeekingFullMatrix$diffCaseGroup[n] <- 2
  }
  infoSeekingFullMatrix$correct[n] <- countryDf$correct[n]
  infoSeekingFullMatrix$pptAccuracy[n] <- round(aggData[aggData$participantID==countryDf$id[n],]$meanFinalAccuracy, digits = 1)
  infoSeekingFullMatrix$initialCorrect[n] <- countryDf$initialCorrect[n]
  if (is.nan(aggData[aggData$participantID==countryDf$id[n],]$finalResolution)) 
  {
    infoSeekingFullMatrix$resolutionGroup[n] <- 0
  } else 
  {
    if (aggData[aggData$participantID==countryDf$id[n],]$finalResolution < 0)
    {
      infoSeekingFullMatrix$resolutionGroup[n] <- 1
    } else 
    {
      infoSeekingFullMatrix$resolutionGroup[n] <- 2
    }
  }
  likelihoodQuantiles <- quantile(aggData$averageLikelihoodOfCorrectCountryInitial)
  likelihoodCorrectVal <- aggData[aggData$participantID==countryDf$id[n],]$averageLikelihoodOfCorrectCountryInitial
  if (likelihoodCorrectVal > likelihoodQuantiles[4])
  {
    infoSeekingFullMatrix$likelihoodCorrectGroup[n] <- 4
  } else if (likelihoodCorrectVal > likelihoodQuantiles[3])
  {
    infoSeekingFullMatrix$likelihoodCorrectGroup[n] <- 3
  } else if (likelihoodCorrectVal > likelihoodQuantiles[2])
  {
    infoSeekingFullMatrix$likelihoodCorrectGroup[n] <- 2
  } else
  {
    infoSeekingFullMatrix$likelihoodCorrectGroup[n] <- 1
  }
  geoKnowledgeQuantiles <- quantile(aggData$geoScore)
  geoKnowledgeVal <- aggData[aggData$participantID==countryDf$id[n],]$geoScore
  count <- count - 1
  if (geoKnowledgeVal > geoKnowledgeQuantiles[4])
  {
    infoSeekingFullMatrix$geoKnowledgeGroup[n] <- 4
    if (count == 0)
    {
      geoKnowledgeArr <- c(geoKnowledgeArr, paste("geoKnowledgeGroup", 4, sep=""))
      count <- 6
    }
  } else if (geoKnowledgeVal > geoKnowledgeQuantiles[3])
  {
    infoSeekingFullMatrix$geoKnowledgeGroup[n] <- 3
    if (count == 0)
    {
      geoKnowledgeArr <- c(geoKnowledgeArr, paste("geoKnowledgeGroup",3, sep=""))
      count <- 6
    }
  } else if (geoKnowledgeVal > geoKnowledgeQuantiles[2])
  {
    infoSeekingFullMatrix$geoKnowledgeGroup[n] <- 2
    if (count == 0)
    {
      geoKnowledgeArr <- c(geoKnowledgeArr, paste("geoKnowledgeGroup",2, sep=""))
      count <- 6
    }
  } else
  {
    infoSeekingFullMatrix$geoKnowledgeGroup[n] <- 1
    if (count == 0)
    {
      geoKnowledgeArr <- c(geoKnowledgeArr, paste("geoKnowledgeGroup",1, sep=""))
      count <- 6
    }
  }
  infoSeekingFullMatrix$geoKnowledgeScore[n] <- aggData[aggData$participantID==countryDf$id[n],]$geoScore
  
  accuracyQuantiles <- quantile(aggData$meanFinalAccuracy)
  accuracyVal <- aggData[aggData$participantID==countryDf$id[n],]$meanFinalAccuracy 
  
  confidenceQuantiles <- quantile(aggData$meanFinalConfidence)
  confidenceVal <- aggData[aggData$participantID==countryDf$id[n],]$meanFinalConfidence 
  
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
  
  rowName <- paste("p", match(countryDf$id[n],participantIDS), "-geoKnowledgeGroup", infoSeekingFullMatrix$geoKnowledgeGroup[n] , "-accGroup", infoSeekingFullMatrix$accuracyGroup[n] , "-confGroup", infoSeekingFullMatrix$confidenceGroup[n] , "-", countriesShort[idx], sep="")
  rownames(infoSeekingFullMatrix)[n] <- rowName
  rownames(confidenceMatrix)[n] <- rowName
}

aggData$geoKnowledgeGroup <- geoKnowledgeArr

colnames(infoSeekingFullMatrix) <- c(1:29)
colnames(infoSeekingFullMatrix)[30] <- "ID"
colnames(infoSeekingFullMatrix)[31] <- "Country"
colnames(infoSeekingFullMatrix)[32] <- "CaseDifficultyGroup"
colnames(infoSeekingFullMatrix)[33] <- "Correct"
colnames(infoSeekingFullMatrix)[34] <- "ParticipantAccuracy"
colnames(infoSeekingFullMatrix)[35] <- "InitialCorrect"
colnames(infoSeekingFullMatrix)[36] <- "ResolutionGroup"
colnames(infoSeekingFullMatrix)[37] <- "LikelihoodCorrectGroup"
colnames(infoSeekingFullMatrix)[38] <- "GeoKnowledgeGroup"
colnames(infoSeekingFullMatrix)[39] <- "GeoKnowledgeScore"
colnames(infoSeekingFullMatrix)[40] <- "AccuracyGroup"
colnames(infoSeekingFullMatrix)[41] <- "AccuracyScore"
colnames(infoSeekingFullMatrix)[42] <- "ConfidenceGroup"
colnames(infoSeekingFullMatrix)[43] <- "ConfidenceScore"


infoSeekingCollMatrix <- data.frame()
for (p in 1:nrow(aggData))
{
  id <- aggData$participantID[p]
  pptTrials <- infoSeekingFullMatrix[infoSeekingFullMatrix$ID==id,]
  for (t in 1:29)
  {
    infoSeekingCollMatrix[p,t] <- sum(pptTrials[,t])
  }
}
colnames(infoSeekingCollMatrix) <- c(1:29)
infoSeekingCollMatrix$ID <- aggData$participantID
infoSeekingCollMatrix$ParticipantAccuracy <- aggData$meanFinalAccuracy
infoSeekingCollMatrix$LikelihoodCorrect <- aggData$averageLikelihoodOfCorrectCountryFinal
infoSeekingCollMatrix$GeoKnowledgeScore <- aggData$geoScore



