requiredPackages <- c("psych", "ggplot2", "lavaan")
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(requiredPackages, require, character.only = TRUE)

wd <- dirname(rstudioapi::getSourceEditorContext()$path)
geoPilotPath <- paste(wd, '/GeographyQuizPilot.csv', sep="")# add path here
geoPilotData <- read.csv(geoPilotPath)

geoAnsPath <- paste(wd, '/GeographyAnswers.csv', sep="")# add path here
geoAnsData <- read.csv(geoAnsPath)

geoMarkedData <- data.frame(matrix(ncol = 41, nrow = nrow(geoPilotData)))
for (x in 1:40)
{
  col <- paste("Q", x, sep="")
  colnames(geoMarkedData)[x] <- col
  geoMarkedData[x] <- as.integer(as.logical(geoPilotData[,x+1] == geoAnsData[x,2]))
}
colnames(geoMarkedData)[41] <- "Total Score"
geoMarkedData[41] <- rowSums(geoMarkedData[1:40])

# Scree plot using PCA
results <- prcomp(geoMarkedData[1:40], scale = TRUE)
var_explained = results$sdev^2 / sum(results$sdev^2)
qplot(c(1:40), var_explained, size=I(5)) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.4) +
  xlim(0,15) + theme_classic()

# Maximum Likelihood Factor Analysis
# entering raw data and extracting 3 factors with rotation
mlfit <- factanal(geoMarkedData[1:40], 1, rotation="promax")
print(mlfit, digits=4)
loadings <- mlfit$loadings
geoMarkedData[42] <- loadings[1:40]
colnames(geoMarkedData)[42] <- "PCA Loading"

questionTotals <- colSums(geoMarkedData[1:40])


# Create data
plotData <- data.frame(
  name=c(1:40) ,  
  value=questionTotals/99
)

# Barplot
ggplot(plotData, aes(x=name, y=value)) + 
  geom_bar(stat = "identity") +
  geom_ribbon(aes(xmin=0, ymin = 0.25, ymax = 0.75), fill = "green", alpha = .2)

questionProps <- questionTotals/99
incQuestions <- sum(as.integer(questionProps>0.25&questionProps<0.75))

