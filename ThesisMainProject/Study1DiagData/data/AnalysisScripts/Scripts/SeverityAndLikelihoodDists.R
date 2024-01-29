severities <- c()
likelihood <- c()
count <- 0
for (id in ids)
{
  participantID <- str_split(id, "/", simplify = TRUE)
  participantID <- participantID[length(participantID)]
  files <- list.files(paste(dataFolder, "/", participantID, sep="")) 
  file <- files[1]
  filePath <- paste(dataFolder, "/", participantID, "/", file, sep="")
  processFile(filePath)
  # Give the input file name to the function.
  myData <- fromJSON(file=filePath)
  trials <- myData$processedData$trials
  
  for (x in 1:length(trials))
  {
    trialSelect <- trials[x]
    trialSelect <- trialSelect[[1]]
    if (trialSelect$subtrial == 3)
    {
      severities <- c(severities, trialSelect$severities+1)
      likelihood <- c(likelihood, trialSelect$likelihoods)
    }
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

clusterLabels <- c()
caseGroupLabels <- c()
values <- c()
sizes <- c()

for (i in 1:6)
{
  cond <- conditionsLong[i]
  group <- caseDf[studentCaseDf$condition==cond&studentCaseDf$sevOfCorrectDiagnosis!=0,]
  size <- nrow(group)
  sevGroup1 <- nrow(group[group$sevOfCorrectDiagnosis==1,])/size
  sevGroup2 <- nrow(group[group$sevOfCorrectDiagnosis==2,])/size
  sevGroup3 <- nrow(group[group$sevOfCorrectDiagnosis==3,])/size
  sevGroup4 <- nrow(group[group$sevOfCorrectDiagnosis==4,])/size
  
  clusterLab <- conditionsShort[i]
  clusterLabels <- c(clusterLabels, rep(clusterLab , 4) )
  caseGroupLabels <- c(caseGroupLabels, c("1. Low", "2. Medium", "3. High", "4. Emergency"))
  values <- c(values, c(sevGroup1, sevGroup2, sevGroup3, sevGroup4))
  sizes[i] <- size
}
clusterAccDf <- data.frame(clusterLabels, caseGroupLabels, values)

print(ggplot(clusterAccDf, aes(fill=caseGroupLabels, y=values, x=clusterLabels)) + 
        geom_bar(position="stack", stat="identity") +
        scale_x_discrete(limits=conditionsShort) +
        ggtitle("Severity Usage for Each Condition (Correct Trials Only)") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title=element_text(size=14,face="bold"),
        line = element_blank()))

###################################################



