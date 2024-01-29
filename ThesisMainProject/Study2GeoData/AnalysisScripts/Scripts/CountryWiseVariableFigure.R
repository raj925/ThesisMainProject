### Perceived difficulty, accuracy and confidence by case

cases <- countriesLong
caseDifficulty <- c()
caseConfidence <- c()
caseAccuracy <- c()
caseInfoSeeking <- c()
caseLikelihood <- c()
for (y in 1:length(cases))
{
  country <- cases[y]
  trials <- df[df$trueCountry==country,]
  caseDifficulty <- c(caseDifficulty, mean(trials$perceivedDifficulty,na.rm=TRUE))
  caseLikelihood <- c(caseLikelihood, mean(as.integer(trials[trials$stage==3&trials$likelihoodOfCorrectCountry!="0",]$likelihoodOfCorrectCountry),na.rm=TRUE)) 
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

plot <- ggplot(caseData, aes(x=case, y=difficulty)) +
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
    sec.axis = dup_axis(~.*0.1, name="Perceived Difficulty / Likelihood of Correct Country", breaks=seq(0,10,1))
  ) + 
  
  theme_classic(base_size = 22) +
  
  theme(
    axis.title.y = element_text(color = confidenceColour, size=13),
    axis.title.y.right = element_text(color = difficultyColour, size=13)
  ) +
  ggtitle(paste("Dependent Variables by Country: ",toString(countriesShort), sep=""))


print(plot)
