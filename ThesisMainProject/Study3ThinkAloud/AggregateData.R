
dataFolder <- "./JSONs"
dfTA <- data.frame(matrix(ncol = 0, nrow = 288))

participantIDS <- c()
ids <- list.dirs(dataFolder,recursive = FALSE) 
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
    dfTA$participantID[row] <- participantID
    dfTA$trialNum[row] <- trialSelect$trial
    dfTA$stage[row] <- trialSelect$subtrial
    dfTA$stageName[row] <- infoStages[trialSelect$subtrial]
    dfTA$trueCondition[row] <- str_replace((toupper(trialSelect$trueCondition)),"-","")
    dfTA$requestedTests[row] <- trialSelect$numOfRequestedTests
    
    currentTests <- c()
    possibleTests <- 0
    if (trialSelect$subtrial == 1)
    {
      currentTests <- testSet1
      possibleTests <- length(testSet1)
      dfTA$likelihoodChange[row] <- 0
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
    
    dfTA$uniqueTests[row] <- length(unique(trialSelect$requestedTestsText)) # this only captures unique across the subtrial NOT the trial
    
    dfTA$pastTests[row] <- length(setdiff(trialSelect$requestedTestsText, currentTests))
    dfTA$currentTests[row] <- dfTA$uniqueTests[row] - dfTA$pastTests[row]
    dfTA$possibleTest[row] <- possibleTests
    dfTA$proportionOfInfo[row] <- dfTA$currentTests[row]/possibleTests
    dfTA$testNames[row] <- toString(unique(trialSelect$requestedTestsText))
    
    dfTA$confidence[row] <- trialSelect$confidence

    dfTA$perceivedDifficulty[row] <- myData$rawData$difficulties[trialSelect$trial]

    dfTA$treatmentPlan[row] <- trialSelect$treatmentPlan
    dfTA$infoSeekingTime[row] <- (trialSelect$totalInfoSeekingTime)/60000
    
  }
  count <- count + length(trials)
}

################################################
## Generate df for case-wise data

nCase <- length(ids)*6
caseDfTA <- data.frame(matrix(ncol = 0, nrow = nCase))
#Generate tests and paired values
allTests <- c(testSet1, testSet2, testSet3)
testIndexes <- c(1:length(allTests))
for (y in 1:nCase)
{
  caseDfTA$id[y] <- dfTA$participantID[(3*y)-2]
  caseDfTA$condition[y] <- dfTA$trueCondition[(3*y)-2]
  caseIdx <- match(caseDfTA$condition[y],conditionsLong)
  caseDfTA$caseCode[y] <- conditionsShort[caseIdx]

  caseDfTA$caseInformationProportion[y] <- mean(c(dfTA$proportionOfInfo[(3*y)-2],dfTA$proportionOfInfo[(3*y)-1],dfTA$proportionOfInfo[(3*y)]))
  caseDfTA$laterInfoProp[y] <- (dfTA$currentTests[(3*y)-1] + dfTA$currentTests[(3*y)])/(dfTA$possibleTest[(3*y)-1] + dfTA$possibleTest[(3*y)])
  caseDfTA$finalConfidence[y] <-dfTA$confidence[(3*y)]
  caseDfTA$confidenceChange[y] <-dfTA$confidence[(3*y)] - dfTA$confidence[(3*y)-2]

  caseDfTA$subjectiveDifficulty[y] <- dfTA$perceivedDifficulty[(3*y)]
  caseDfTA$caseInformationReqs[y] <- dfTA$currentTests[(3*y)-2] + dfTA$currentTests[(3*y)-1] + dfTA$currentTests[(3*y)]

  caseDfTA$confidenceArray[y] <- toString(c(dfTA$confidence[(3*y)-2],dfTA$confidence[(3*y)-1],dfTA$confidence[(3*y)]))

  testArray <- replicate(length(allTests), 0)
  allReqTests <- paste(dfTA$testNames[(3*y)-2],dfTA$testNames[(3*y)-1],dfTA$testNames[(3*y)], sep=", ")
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
  caseDfTA$reqTestArray[y] <- toString(testArray)
}

