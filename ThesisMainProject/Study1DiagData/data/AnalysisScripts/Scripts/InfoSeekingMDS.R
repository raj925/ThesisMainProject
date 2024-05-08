
mds <- infoSeekingFullMatrix[,1:29] %>%
  stats::dist(method="euclidean") %>%  # Default for dist function is Euclidean distance        
  cmdscale() %>% # 
  as_tibble()


# distances <- infoSeekingFullMatrix[,1:29] %>% stats::dist(method="binary") %>% as.matrix()

# Compute  similarity distance (appropriate for binary data)
infoSeekingNoEmpties <- infoSeekingFullMatrix[,1:29]
infoSeekingNoEmpties <- infoSeekingNoEmpties[rowSums(infoSeekingNoEmpties)>1,]

distances <- infoSeekingNoEmpties[,1:29] %>% proxy::dist(method = distanceMethod) %>% as.matrix()

infoSeekingFullMatrix$v1 <- mds$V1
infoSeekingFullMatrix$v2 <- mds$V2

hist(distances)


#######################################
# Use of regular PCA

pca_result <- principal(infoSeekingFullMatrix[,1:29], nfactors = 5, rotate = "promax")
pcs <- pca_result$scores
weights <- pca_result$loadings

topPCS <- weights

testCodes <- c("ILLHIST","PASTHIST","MEDS","ALLER","FAMHIST","SOCHIST",
               "PULSE", "BP", "RESP", "LUNG", "HEART", "EYES", "TEMP",
               "ABDEX", "REC", "NECK", "HEAD", "NEURO", "EXTR",
               "URINE", "ECG", "ABCT", "VBG", "ELEC", "CRP", "CLOT", "FBC",
               "BIOCH", "CHXR")

rownames(topPCS) <- c(1:29)
rownames(topPCS) <- testCodes[as.numeric(rownames(topPCS))] 

# Sort the columns alphabetically
colnames(topPCS) <- c("RC1", "RC2", "RC3", "RC4", "RC5")
topPCS <- topPCS[, order(colnames(topPCS))]

#######################################
# Use of logistic (binary) PCA instead

# Perform PCA on binary data
pca_result <- PCA(infoSeekingFullMatrix[,1:29], graph = FALSE)

# Scree plot shows elbow around 5
qplot(c(1:29), pca_result$eig[,2]) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  theme_classic()

#  we will fit the parameters assuming two-dimensional representation
# For logistic PCA, we want to first decide which m to use with cross validation. 
# We are assuming k = 5 and trying different ms from 1 to 10.
logpca_cv = cv.lpca(infoSeekingFullMatrix[,1:29], ks = 5, ms = 1:10)

# Seems like 4 is optimal m
# plot(logpca_cv)

logpca_model = logisticPCA(infoSeekingFullMatrix[,1:29], k = 5, m = which.min(logpca_cv))
clogpca_model = convexLogisticPCA(infoSeekingFullMatrix[,1:29], k = 5, m = which.min(logpca_cv))


# Determine the maximum absolute value in the loadings matrix
max_abs <- max(abs(c(min(topPCS), max(topPCS))))
# Normalize the data to the range [-1, 1]
normalized_topPCS <- topPCS / max_abs
# Define a custom color palette with clear differentiation between negative, zero, and positive values
n_colors <- 20
color_palette <- colorRampPalette(c("blue", "white", "red"))(n_colors)

apcluster::heatmap(normalized_topPCS,
                   main = "Variable Weights (Normalised) Clustered by PC",
                   xlab = "PCs", ylab = "Test",
                   col = color_palette, scale = "none", breaks = seq(-1, 1, length.out = n_colors + 1), Colv = NA)

# Define colors for legend
legend_colors <- c("blue", "white", "red")

# Add color key
legend("topleft", legend = c("Negative", "Zero", "Positive"),
       fill = legend_colors, title = "Color Key", cex = 0.8)

########

pcs <- pcs[, order(colnames(pcs))]
pcDF <- data.frame(pcs[,1],pcs[,2],pcs[,3],pcs[,4],pcs[,5])
colnames(pcDF) <- c("PC1","PC2","PC3","PC4","PC5")
pcDF$pid <- infoSeekingFullMatrix$ID
pcDF$condition <- infoSeekingFullMatrix$Condition
pcDF$correct <- infoSeekingFullMatrix$LikelihoodAcc
pcDF$AccuracyGroup <- as.integer(as.logical(infoSeekingFullMatrix$AccuracyGroup>2))
pcDF$AccuracyGroup <- as.factor(pcDF$AccuracyGroup)

mixedPCModel = lmer(correct ~ PC1 + PC2 + PC3 + PC4 + PC5 + condition + (1 | pid), data = pcDF)
summary(mixedPCModel)

#######################################
# Latent Profile Analysis on Information Seeking


infoSeekingMatrixByPptPCs <- pcDF[1:5]
infoSeekingMatrixByPptPCs <- apply(infoSeekingMatrixByPptPCs,2,
                                   function(x) cut(x, breaks = 5, labels=FALSE))

infoSeekingMatrixByPptPCs <- as.data.frame(infoSeekingMatrixByPptPCs)

require(poLCA)
f <- with(infoSeekingMatrixByPptPCs, cbind(PC1,PC2,PC3,PC4,PC5)~1)

minBIC <- 100000
for (n in 2:10)
{
  lp <- poLCA::poLCA(f, data = infoSeekingMatrixByPptPCs, nclass=n, maxiter=3000)
  if(lp$bic < minBIC) {
    minBIC <- lp$bic
    bestModel <- lp
  }
}
summary(bestModel)
latentProfile <- bestModel$predclass

#############
PCdistances <- pcDF[,1:5] %>% proxy::dist(method = "cosine") %>% as.matrix()
distanceVars <- c()
accuracies <- c()
meanPC1 <- c()
meanPC2 <- c()
meanPC3 <- c()
meanPC4 <- c()
meanPC5 <- c()
for (n in 1:nrow(studentAggData))
{
  ppt <- paste("p", n, "-a", sep="")
  
  compareColumns <- PCdistances[grep(ppt, rownames(PCdistances)), ]
  compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  distanceVars[n] <- mean(compareColumns)
  
  compareColumns <- pcDF[grep(ppt, rownames(pcDF)), ]
  meanPC1[n] <- mean(compareColumns$PC1)
  meanPC2[n] <- mean(compareColumns$PC2)
  meanPC3[n] <- mean(compareColumns$PC3)
  meanPC4[n] <- mean(compareColumns$PC4)
  meanPC5[n] <- mean(compareColumns$PC5)

  accuracies[n] <- studentAggData$meanFinalAccuracy[n]
}
infoSeekingDf <- data.frame(distanceVars,accuracies,meanPC1,meanPC2,meanPC3,meanPC4,meanPC5)
colnames(infoSeekingDf) <- c("DistanceVariance", "Accuracy","MeanPC1","MeanPC2","MeanPC3","MeanPC4", "MeanPC5")

######################################
colnames(confidenceMatrix) <- c("1","2","3")
confDistances <- confidenceMatrix %>% stats::dist(method="euclidean") %>% as.matrix()

######################################
vals <- c()
information <- c()
val1 <- c()
val2 <- c()
for (x in 1:nrow(distances))
{
  {
    for (y in 1:ncol(distances))
    {
      if (x > y)
      {
        val <- distances[x,y]
        vals <- c(vals, val)
        
        row <- rownames(distances)[x]
        col <- colnames(distances)[y]
        rowVector <- infoSeekingFullMatrix[row,][1:29]
        colVector <- infoSeekingFullMatrix[col,][1:29]
        
        val1 <- c(val1,(sum(rowVector)/29))
        val2 <- c(val2,(sum(colVector)/29))
        
        infoMean <- (sum(rowVector) + sum(colVector))/58
        information <- c(information, infoMean)
      }
    }
  }
}
distanceTable <- data.frame(vals,information,val1,val2)
colnames(distanceTable) <- c("Variance","InformationProportion","Info1","Info2")

distanceTable <- distanceTable[distanceTable$Info1>0&distanceTable$Info2>0,]
###
disInfo <- ggplot(data = distanceTable, aes(x=InformationProportion, y=Variance)) +
  geom_point() +
  theme_classic()

title <- paste("Info Seeking Variance Against Amount of Information - ", distanceMethod)
print(disInfo + 
        ggtitle(title))
###
# Create a scatter plot
ggplot(distanceTable, aes(x = Info1, y = Info2, color = Variance)) +
  geom_point() +
  scale_color_gradient(low = "blue", high = "red") +  # Adjust color scale as needed
  labs(
    title = "Distance by Information Seeking Amount",
    x = "Info1",
    y = "Info2",
    color = "DistanceValue"
  ) +
  theme_classic()



######################################
# Look at information seeking distance variance against proportion

distanceVars <- c()
confidenceVars <- c()
infoProps <- c()
accuracies <- c()
relRat <- c()
brier <- c()
confidences <- c()
for (n in 1:nrow(studentAggData))
{
  ppt <- paste("p", n, "-a", sep="")
  
  compareColumns <- distances[grep(ppt, rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  distanceVars[n] <- mean(compareColumns)
  
  infoProps[n] <- studentAggData$proportionOfInfo[n]
  accuracies[n] <- studentAggData$meanFinalAccuracy[n]
  relRat[n] <- studentAggData$relativeRationalism[n]
  brier[n] <- studentAggData$finalBrierScore[n]
  confidences[n] <- studentAggData$meanFinalConfidence[n] - studentAggData$meanInitialConfidence[n]
}
infoSeekingDf <- data.frame(distanceVars,infoProps,accuracies,relRat,brier,confidences)
colnames(infoSeekingDf) <- c("MDSDistanceVariance", "InformationSeekingProportion","Accuracy","RelativeRationalism","BrierScore","ConfidenceChange")

distanceVars <- c()
confidenceVars <- c()
infoProps <- c()
accuracies <- c()
relRat <- c()
brier <- c()
confidences <- c()
for (n in 1:nrow(expertAggData))
{
  ppt <- paste("p", n, "-a", sep="")
  
  compareColumns <- distances[grep(ppt, rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  distanceVars[n] <- (mean(compareColumns))
  
  infoProps[n] <- expertAggData$proportionOfInfo[n]
  accuracies[n] <- expertAggData$meanFinalAccuracy[n]
  relRat[n] <- expertAggData$relativeRationalism[n]
  brier[n] <- expertAggData$finalBrierScore[n]
  confidences[n] <- expertAggData$meanFinalConfidence[n] - expertAggData$meanInitialConfidence[n]
}
infoSeekingDfExp <- data.frame(distanceVars,infoProps,accuracies,relRat,brier,confidences)
colnames(infoSeekingDfExp) <- c("MDSDistanceVariance", "InformationSeekingProportion","Accuracy","RelativeRationalism","BrierScore","ConfidenceChange")


cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$Accuracy,method="pearson")

msdAcc <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=Accuracy)) +
  geom_point() +
  geom_smooth(method=lm , color=accuracyColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Info Seeking Variance Against Accuracy: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(msdAcc + 
        ggtitle(title) +
      labs(y="Accuracy", x = paste("Variance in Information Seeking Across Cases - ", distanceMethod)) +
        ylim(0,1) +
      theme(axis.text=element_text(size=16),
            axis.title=element_text(size=16),
            plot.title=element_text(size=18,face="bold")))

cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$ConfidenceChange,method="pearson")

msdConf <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=ConfidenceChange)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Info Seeking Variance Against Change in Confidence: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(msdConf + 
        ggtitle(title) +
        labs(y="Change in Confidence", x = "Variance in Information Seeking Across Cases") +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold")))



######################################
# Mean variance by participant

nPpts <- nrow(infoSeekingDf)/4

varVector <- infoSeekingDf$MDSDistanceVariance
infoSeekVector <- infoSeekingDf$InformationSeekingProportion
accVector <- c()
if (classifyVar == "confidence")
{
  accVector <- infoSeekingDf$Confidence
} else {
  accVector <- infoSeekingDf$Accuracy
}
quantileAcc <- quantile(accVector)
medAcc <- median(accVector)

accGroups <- ifelse(accVector<quantileAcc[2],1,
       ifelse(accVector<quantileAcc[3],2,
              ifelse(accVector<quantileAcc[4],3,4))
       )

accMedGroups <- ifelse(accVector<medAcc,1,2) 


varDfTemp <- data.frame(varVector,accGroups,accMedGroups,infoSeekVector)

varVector <- c(varDfTemp$varVector,infoSeekingDfExp$MDSDistanceVariance)
accGroups <- c(varDfTemp$accGroups,rep("e",nrow(infoSeekingDfExp)))
accMedGroups <- c(varDfTemp$accMedGroups,rep("e",nrow(infoSeekingDfExp)))
infoSeekVector <- c(varDfTemp$varVector,infoSeekingDfExp$InformationSeekingProportion)

varDfTemp <- data.frame(varVector,accGroups,accMedGroups,infoSeekVector)

varDfMeans <- c(mean(varDfTemp[varDfTemp$accGroups==1,]$varVector,na.rm=T),
                mean(varDfTemp[varDfTemp$accGroups==2,]$varVector,na.rm=T),
                mean(varDfTemp[varDfTemp$accGroups==3,]$varVector,na.rm=T),
                mean(varDfTemp[varDfTemp$accGroups==4,]$varVector,na.rm=T),
                mean(varDfTemp[varDfTemp$accGroups=="e",]$varVector,na.rm=T))
varDfSds <- c(sd(varDfTemp[varDfTemp$accGroups==1,]$varVector,na.rm=T)/sqrt(nPpts),
                sd(varDfTemp[varDfTemp$accGroups==2,]$varVector,na.rm=T)/sqrt(nPpts),
                sd(varDfTemp[varDfTemp$accGroups==3,]$varVector,na.rm=T)/sqrt(nPpts),
                sd(varDfTemp[varDfTemp$accGroups==4,]$varVector,na.rm=T)/sqrt(nPpts),
              sd(varDfTemp[varDfTemp$accGroups=="e",]$varVector,na.rm=T)/sqrt(nrow(varDfTemp[varDfTemp$accGroups=="e",])))
accGroupNum <- c(1,2,3,4,"e")
varDf <-data.frame(accGroupNum, varDfMeans,varDfSds)

varMedMeans <- c(mean(varDfTemp[varDfTemp$accMedGroups==1,]$varVector),
                mean(varDfTemp[varDfTemp$accMedGroups==2,]$varVector))
varMedSds <- c(sd(varDfTemp[varDfTemp$accMedGroups==1,]$varVector)/sqrt(nPpts),
              sd(varDfTemp[varDfTemp$accMedGroups==2,]$varVector)/sqrt(nPpts))
accMedGroupNum <- c(1,2)
varMedDf <-data.frame(accMedGroupNum, varMedMeans,varMedSds)

p <- ggplot(varDf) +
  geom_bar(aes(x = accGroupNum, y = varDfMeans),stat="identity",fill="black",alpha=0.7) +
  geom_errorbar(aes(x = accGroupNum,ymin=varDfMeans-varDfSds, ymax=varDfMeans+varDfSds), width=.2, position=position_dodge(0.05),color="orange") +
  labs(title="Mean of Individual Variance Across Cases",x=paste(classifyVar, " Group (Quantiles)",sep=""),y="Participant Variance Across Cases") +
  theme_classic() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18,face="bold"),
        line = element_blank())

print(p)

######

#accVarGroup1 <- subsample(varDfTemp[varDfTemp$accGroups==1,]$infoSeekVector,6,10)
#accVarGroup1$meanVar <- rowMeans(accVarGroup1,na.rm=T)


######


#res.aov2 <- anova_test(data = varDfTemp, dv = varVector, between = accGroups)
#antab <- get_anova_table(res.aov2)
#print(antab)

lmvar <- lm(varVector~accGroups, data = varDfTemp)
print(summary(lmvar))

######################################
# Bar chart that breaks down variance by case and accuracy

toMatchLow <- c("accGroup1", "accGroup2")
toMatchHigh <- c("accGroup3", "accGroup4")

compareColumnsLow <- distances[grep(paste(toMatchLow,collapse="|"), rownames(distances)), ]
compareColumnsLow <- compareColumnsLow[,grep(paste(toMatchLow,collapse="|"), colnames(compareColumnsLow)) ]
varsArr <- c()
num <- 1
for (cond in conditionsShort)
{
  compareColumns <- compareColumnsLow[grep(cond, rownames(compareColumnsLow)), ]
  compareColumns <- compareColumns[,grep(cond, colnames(compareColumns)) ]
  varsArr[num] <- (mean(compareColumns))
  num <- num + 1
}

compareColumnsHigh <- distances[grep(paste(toMatchHigh,collapse="|"), rownames(distances)), ]
compareColumnsHigh <- compareColumnsHigh[,grep(paste(toMatchHigh,collapse="|"), colnames(compareColumnsHigh)) ]
for (cond in conditionsShort)
{
  compareColumns <- compareColumnsHigh[grep(cond, rownames(compareColumnsHigh)), ]
  compareColumns <- compareColumns[,grep(cond, colnames(compareColumns)) ]
  varsArr[num] <- (mean(compareColumns))
  num <- num + 1
}
dataB <- data.frame(
  condition=rep(c("UC", "GBS", "TA", "TTP", "AD", "MTB") , 2),
  accGroup=c(rep("low", 6),rep("high", 6)),
  variance=varsArr
)

varsPlot <- ggplot(dataB,aes(x=condition, y=variance, fill=accGroup)) +
  geom_bar(stat="identity", position="dodge", alpha=0.8)

print(varsPlot +
        scale_x_discrete(limits=conditionsShort) +
        ggtitle("Variance in Information Seeking by Accuracy and Condition") +
        labs(x = "Condition (ordered by accuracy in descending order)", y = "Average Distance") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
               axis.title=element_text(size=16),
               plot.title=element_text(size=14,face="bold"),
              line = element_blank()
        )) 

print(t.test(variance ~ accGroup,data = dataB))
cohensD(variance ~ accGroup, data=dataB)

##########

# Look at variance in distances 

# By case

caseVars <- c()
for (x in 1:length(conditionsShort))
{
  compareColumns <- distances[grep(conditionsShort[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(conditionsShort[x], colnames(compareColumns)) ]
  caseVars[x] <- mean(compareColumns)
}

dataCaseVar <- data.frame("Case" = conditionsShort, "Variance"= caseVars)
diffs <- ggplot(dataCaseVar) +
  geom_bar( aes(x=Case, y=Variance), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(diffs +
        scale_x_discrete(limits=conditionsShort) +
        ggtitle("Variance in Information Seeking by Case") +
        labs(x = "Case (in decreasing accuracy order)", y = "Variance in MDS Distances") +
        theme_classic()) 

# By accuracy

accVars <- c()
accInfoProps <- c()
accDiffs <- c()
accVals <- c()
for (x in 1:length(accs))
{
  compareColumns <- distances[grep(accs[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(accs[x], colnames(compareColumns)) ]
  accVars[x] <- (mean(compareColumns))
  
  if (accs[x] == "e")
  {
    accInfoProps[x] <- mean(expertAggData$proportionOfInfo,na.rm=TRUE)
    accDiffs[x] <- mean(expertAggData$meanFinalDiffs,na.rm=TRUE)
    accVals[x] <- mean(expertAggData$meanFinalAccuracy,na.rm=TRUE)
  }
  
  else
  {

    accQuantiles <- quantile(studentAggData$meanFinalAccuracy)
    if (accs[x] == "accGroup4")
    {
      accInfoProps[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy > accQuantiles[4],]$proportionOfInfo,na.rm=TRUE)
      accDiffs[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy > accQuantiles[4],]$meanFinalDiffs,na.rm=TRUE)
      accVals[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy > accQuantiles[4],]$meanFinalAccuracy,na.rm=TRUE)
    } else if (accs[x] == "accGroup3")
    {
      accInfoProps[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[4] & studentAggData$meanFinalAccuracy > accQuantiles[3],]$proportionOfInfo,na.rm=TRUE)
      accDiffs[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[4] & studentAggData$meanFinalAccuracy > accQuantiles[3],]$meanFinalDiffs,na.rm=TRUE)
      accVals[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[4]& studentAggData$meanFinalAccuracy > accQuantiles[3],]$meanFinalAccuracy,na.rm=TRUE)
    } else if (accs[x] == "accGroup2")
    {
      accInfoProps[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[3] & studentAggData$meanFinalAccuracy > accQuantiles[2],]$proportionOfInfo,na.rm=TRUE)
      accDiffs[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[3] & studentAggData$meanFinalAccuracy > accQuantiles[2],]$meanFinalDiffs,na.rm=TRUE)
      accVals[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[3]& studentAggData$meanFinalAccuracy > accQuantiles[2],]$meanFinalAccuracy,na.rm=TRUE)
    } else
    {
      accInfoProps[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[2],]$proportionOfInfo,na.rm=TRUE)
      accDiffs[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[2],]$meanFinalDiffs,na.rm=TRUE)
      accVals[x] <- mean(studentAggData[studentAggData$meanFinalAccuracy < accQuantiles[2],]$meanFinalAccuracy,na.rm=TRUE)
    }
  }
}

dataF <- data.frame("Accuracy" = accs, "Variance"= accVars, "Differentials" = accDiffs,
                    "InformationSeeking"= accInfoProps, "AccuracyValue" = accVals)


diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=Variance), colour="black", stat="identity", fill=accuracyColour, alpha=0.8)

print(diffs +
        ggtitle("Variance in Information Seeking by Participant Accuracy") +
        labs(x = "Participant Accuracy", y = "Variance Across Participant Group") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=14,face="bold")
        )) 

diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=InformationSeeking), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(diffs +
        ggtitle("Total Information Seeking by Participant Accuracy") +
        labs(x = "Participant Accuracy", y = "Total Information Seeking") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold")
        )) 

diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=Differentials), colour="black", stat="identity", fill=differentialColour, alpha=0.8)

print(diffs +
        ggtitle("Average Differentials by Participant Accuracy") +
        labs(x = "Participant Accuracy", y = "Number of Final Differentials") +
        theme_classic() +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold")
        )) 


# By participant type

ppts <- c("p", "e")
pptsVars <- c()
for (x in 1:length(ppts))
{
  compareColumns <- distances[grep(ppts[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppts[x], colnames(compareColumns)) ]
  pptsVars[x] <- mean(compareColumns)
}

dataPE <- data.frame("Participant" = ppts, "Variance"= pptsVars)
diffs <- ggplot(dataPE) +
  geom_bar( aes(x=Participant, y=Variance), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(diffs +
        ggtitle("Variance in Information Seeking by Participant Type") +
        labs(x = "Participant/Expert", y = "Variance in MDS Distances") +
        theme_classic()) 


##########

# Distance heat plot by participant accuracy

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
print(ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000"))

##########

# # Distance heat plot by participant accuracy AND case
# 
# accCases <- c()
# for (n in 1:length(accs))
# {
#   for (m in 1:length(cases))
#   {
#     #accCases[m+(length(cases)*(n-1))] <- paste(m, "/", accs[n], "-", cases[m], sep="")
#     accCases[m+(length(cases)*(n-1))] <- paste(accs[n], "-", cases[m], sep="")
#   }
# }
# distanceMatrix <- data.frame(matrix(ncol = 30, nrow = 30))
# for (x in 1:length(accCases))
# {
#   acc <- accCases[x]
#   #accSplit <- str_split(acc,"/")[[1]][2]
#   for (y in 1:length(accCases))
#   {
#     if (y == x)
#     {
#       distanceMatrix[x,y] <- 0
#       colnames(distanceMatrix)[y] <- acc
#     }
#     else
#     {
#       comparisonAcc <- accCases[y]
#       #comparisonAccSplit <- str_split(comparisonAcc,"/")[[1]][2]
#       currentAcc <- str_split(acc,"-")[[1]][1]
#       nextAcc <- str_split(comparisonAcc,"-")[[1]][1]
#       currentCase <- str_split(acc,"-")[[1]][2]
#       nextCase <- str_split(comparisonAcc,"-")[[1]][2]
#       compareColumns <- distances[grep(currentAcc, rownames(distances)), ]
#       compareColumns <- compareColumns[grep(currentCase, rownames(compareColumns)), ]
#       compareColumns <- compareColumns[,grep(nextAcc, colnames(compareColumns)) ]
#       compareColumns <- compareColumns[,grep(nextCase, colnames(compareColumns)) ]
#       meanDist <- mean(compareColumns)^2
#       distanceMatrix[x,y] <- meanDist
#       colnames(distanceMatrix)[y] <- comparisonAcc
#     }
#   }
#   rownames(distanceMatrix)[x] <- acc
# }
# 
# dt2 <- distanceMatrix %>%
#   rownames_to_column() %>%
#   gather(colname, value, -rowname)
# print(ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
#   geom_tile() +
#   scale_fill_gradient2(low = "#075AFF",
#                        mid = "#FFFFCC",
#                        high = "#FF0000")) +
#   theme(
#     axis.text.x = element_text(size = 7))
# 

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
print(ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000"))


##########

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

xCoordinates <- c(meanCoordinatesCaseGBSv1, meanCoordinatesCaseUCv1, meanCoordinatesCaseADv1,
                  meanCoordinatesCaseTAv1, meanCoordinatesCaseTTPv1, meanCoordinatesCaseMTBv1)
yCoordinates <- c(meanCoordinatesCaseGBSv2, meanCoordinatesCaseUCv2, meanCoordinatesCaseADv2,
                  meanCoordinatesCaseTAv2, meanCoordinatesCaseTTPv2, meanCoordinatesCaseMTBv2)
label <- c("easy", "easy", "easy", "hard", "hard", "hard")
plotDf <- data.frame(xCoordinates, yCoordinates,label)
labels <- conditionsShort
print(ggplot(plotDf, aes(x=xCoordinates, y=yCoordinates)) +
  geom_point(aes(colour=factor(label), size=0.02)) + # Show dots
  geom_text(
    label=labels, 
    nudge_x = 0.01, nudge_y = 0.01, 
    check_overlap = T
  ))

##############


meanCoordinatesAccGroup1v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==1,]$v1)
meanCoordinatesAccGroup1v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==1,]$v2)

meanCoordinatesAccGroup2v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==2,]$v1)
meanCoordinatesAccGroup2v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==2,]$v2)

meanCoordinatesAccGroup3v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==3,]$v1)
meanCoordinatesAccGroup3v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==3,]$v2)

meanCoordinatesAccGroup4v1 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==4,]$v1)
meanCoordinatesAccGroup4v2 <- mean(infoSeekingFullMatrix[infoSeekingFullMatrix$AccuracyGroup==4,]$v2)

xCoordinates <- c(meanCoordinatesAccGroup1v1, meanCoordinatesAccGroup2v1, meanCoordinatesAccGroup3v1,
                  meanCoordinatesAccGroup4v1)
yCoordinates <- c(meanCoordinatesAccGroup1v2, meanCoordinatesAccGroup2v2, meanCoordinatesAccGroup3v2,
                  meanCoordinatesAccGroup4v2)
plotDf <- data.frame(xCoordinates, yCoordinates)
labels <- c("AccGroup1", "AccGroup2", "AccGroup3", "AccGroup4")
print(ggplot(plotDf, aes(x=xCoordinates, y=yCoordinates)) +
  geom_point() + # Show dots
  geom_text(
    label=labels, 
    nudge_x = 0.01, nudge_y = 0.01, 
    check_overlap = T
  ))

##############
# Determine optimal number of clusters using average
# silhouette method
print(fviz_nbclust(mds[,1:2],cluster::pam,"silhouette"))
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
  
  print(ggplot(clusterAccDf, aes(fill=accGroupLabels, y=values, x=clusterLabels)) +
    geom_bar(position="stack", stat="identity"))
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
  
  print(ggplot(clusterAccDf, aes(fill=accGroupLabels, y=values, x=clusterLabels)) + 
    geom_bar(position="stack", stat="identity"))
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
    
    accGroup1 <- nrow(group[group$AccuracyGroup==1,])/size
    accGroup2 <- nrow(group[group$AccuracyGroup==2,])/size
    accGroup3 <- nrow(group[group$AccuracyGroup==3,])/size
    accGroup4 <- nrow(group[group$AccuracyGroup==4,])/size
    
    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 4) )
    accGroupLabels <- c(accGroupLabels, c("accGroup1", "accGroup2", "accGroup3", "accGroup4"))
    values <- c(values, c(accGroup1, accGroup2, accGroup3, accGroup4))
  }
  clusterAccDf <- data.frame(clusterLabels, accGroupLabels, values)
  
  print(ggplot(clusterAccDf, aes(fill=accGroupLabels, y=values, x=clusterLabels)) + 
    geom_bar(position="stack", stat="identity"))
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
  
  print(ggplot(clusterAccDf, aes(fill=caseGroupLabels, y=values, x=clusterLabels)) + 
    geom_bar(position="stack", stat="identity"))
}


##############
