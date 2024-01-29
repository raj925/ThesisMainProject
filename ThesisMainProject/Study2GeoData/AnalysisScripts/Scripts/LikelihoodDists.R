
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
    likelihood <- c(likelihood, as.integer(trialSelect$likelihoods))
  }
  count <- count + 1
}
likelihood <- as.vector(likelihood)


title <- paste("Likelihood Ratings", sep="")
lik <- barplot(table(likelihood),
               main=title,
               xlab="Likelihood",
               ylab="Count",
               border="black",
               col="black",
               density=10
)