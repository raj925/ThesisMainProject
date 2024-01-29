mds <- infoSeekingFullMatrix[,1:29] %>%
  dist() %>%          
  cmdscale() %>%
  as_tibble()

distances <- infoSeekingFullMatrix[,1:29] %>% dist() %>% as.matrix()

infoSeekingFullMatrix$v1 <- mds$V1
infoSeekingFullMatrix$v2 <- mds$V2

cases <- countriesShort

######################################
# Look at information seeking distance variance against proportion

distanceVars <- c()
infoProps <- c()
accuracies <- c()
confidences <- c()
knowledge <- c()
for (n in 1:nrow(aggData))
{
  ppt <- paste("p", n, sep="")
  compareColumns <- distances[grep(ppt, rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  distanceVars[n] <- (sd(compareColumns))^2
  infoProps[n] <- aggData$proportionOfInfo[n]
  accuracies[n] <- aggData$meanFinalAccuracy[n]
  confidences[n] <- aggData$meanFinalConfidence[n]
  knowledge[n] <- aggData$geoScore[n]
}
infoSeekingDf <- data.frame(distanceVars,infoProps,accuracies,confidences,knowledge)
colnames(infoSeekingDf) <- c("MDSDistanceVariance", "InformationSeekingProportion","Accuracy","Confidence","GeoScore")

cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$InformationSeekingProportion,method="pearson")

msdCorr <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=InformationSeekingProportion)) +
  geom_point() +
  geom_smooth(method=lm , color=infoSeekingColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("MSD Dist. Var. Against Info Seeking Proportion: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(msdCorr + 
        ggtitle(title)
      + labs(y="Proportion of Possible Information Requested", x = "Variance in Participant's MSD Distances"))


cor <- cor.test(infoSeekingDf$MDSDistanceVariance,infoSeekingDf$Accuracy,method="pearson")

msdAcc <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=Accuracy)) +
  geom_point() +
  geom_smooth(method=lm , color=accuracyColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Info Seeking Variance Against Accuracy: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(msdAcc + 
        ggtitle(title) +
        labs(y="Accuracy", x = "Variance in Information Seeking Across Cases") +
        ylim(0,1) +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold")))

msdConf <- ggplot(data = infoSeekingDf, aes(x=MDSDistanceVariance, y=Confidence)) +
  geom_point() +
  geom_smooth(method=lm , color=confidenceColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Info Seeking Variance Against Confidence: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(msdAcc + 
        ggtitle(title) +
        labs(y="Confidence", x = "Variance in Information Seeking Across Cases") +
        ylim(0,1) +
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold")))



######################################
# Distance heat plot by case

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
print(ggplot(dt2, aes(x = colname, y = rowname, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000"))

##########

# Look at variance in distances 

# By country

caseVars <- c()
infoSeeks <- c()
for (x in 1:length(cases))
{
  compareColumns <- distances[grep(cases[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(cases[x], colnames(compareColumns)) ]
  caseVars[x] <- (sd(compareColumns))^2
  idx <- match(cases[x],cases)
  infoSeeks[x] <- mean(countryDf[countryDf$country==countriesLong[idx],]$caseInformationProportion,na.rm=TRUE)
}

dataCaseVar <- data.frame("Case" = cases, "Variance"= caseVars, "InfoSeeking" = infoSeeks)
diffs <- ggplot(dataCaseVar) +
  geom_bar( aes(x=Case, y=Variance), colour="black", stat="identity", fill=accuracyColour, alpha=0.8)

print(diffs +
        ggtitle("Variance in Information Seeking by Country") +
        labs(x = "Case", y = "Variance in MDS Distances") +
        theme_classic()) 

infoSeeking <- ggplot(dataCaseVar) +
  geom_bar( aes(x=Case, y=InfoSeeking), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(infoSeeking +
        ggtitle("Proportion of Information Seeking by Country") +
        labs(x = "Case", y = "Variance in MDS Distances") +
        theme_classic()) 

# By accuracy

accVars <- c()
accInfoProps <- c()
accDiffs <- c()
for (x in 1:length(accs))
{
  compareColumns <- distances[grep(accs[x], rownames(distances)), ]
  compareColumns <- compareColumns[,grep(accs[x], colnames(compareColumns)) ]
  accVars[x] <- (summary(boot(compareColumns, varBoot, R=500)))$bootMed
  accInfoProps[x] <- mean(aggData[accs[x]==aggData$geoKnowledgeGroup,]$proportionOfInfo,na.rm=TRUE)
  accDiffs[x] <- mean(aggData[accs[x]==aggData$geoKnowledgeGroup,]$meanFinalDiffs,na.rm=TRUE)
}

dataF <- data.frame("Accuracy" = accs, "Variance"= accVars, "Differentials" = accDiffs,
                    "InformationSeeking"= accInfoProps, "correctDiagnosisLikelihood" = accVars)


diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=Variance), colour="black", stat="identity", fill=accuracyColour, alpha=0.8)

print(diffs +
        ggtitle("Variance in Information Seeking by Participant Knowledge") +
        labs(x = "Participant Geographic Knowledge", y = "Variance in MDS Distances") +
        theme_classic()) 

diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=InformationSeeking), colour="black", stat="identity", fill=infoSeekingColour, alpha=0.8)

print(diffs +
        ggtitle("Total Information Seeking by Participant Accuracy") +
        labs(x = "Participant Accuracy", y = "Total Information Seeking") +
        theme_classic()) 

diffs <- ggplot(dataF) +
  geom_bar( aes(x=Accuracy, y=Differentials), colour="black", stat="identity", fill=differentialColour, alpha=0.8)

print(diffs +
        ggtitle("Average Differentials by Participant Accuracy") +
        labs(x = "Participant Accuracy", y = "Number of Final Differentials") +
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

######################################
# Mean variance by participant

nPpts <- nrow(infoSeekingDf)/4

varVector <- infoSeekingDf$MDSDistanceVariance
accVector <- c()
if (classifyVar == "confidence")
{
  accVector <- infoSeekingDf$Confidence
} else if (classifyVar == "accuracy") {
  accVector <- infoSeekingDf$Accuracy
} else {
  accVector <- infoSeekingDf$GeoScore
}
quantileAcc <- quantile(accVector)
medAcc <- median(accVector)

accGroups <- ifelse(accVector<quantileAcc[2],1,
                    ifelse(accVector<quantileAcc[3],2,
                           ifelse(accVector<quantileAcc[4],3,4))
)

accMedGroups <- ifelse(accVector<medAcc,1,2) 

varDfTemp <- data.frame(varVector,accGroups,accMedGroups)
varDfMeans <- c(mean(varDfTemp[varDfTemp$accGroups==1,]$varVector),
                mean(varDfTemp[varDfTemp$accGroups==2,]$varVector),
                mean(varDfTemp[varDfTemp$accGroups==3,]$varVector),
                mean(varDfTemp[varDfTemp$accGroups==4,]$varVector))
varDfSds <- c(sd(varDfTemp[varDfTemp$accGroups==1,]$varVector)/sqrt(nPpts),
              sd(varDfTemp[varDfTemp$accGroups==2,]$varVector)/sqrt(nPpts),
              sd(varDfTemp[varDfTemp$accGroups==3,]$varVector)/sqrt(nPpts),
              sd(varDfTemp[varDfTemp$accGroups==4,]$varVector)/sqrt(nPpts))
accGroupNum <- c(1,2,3,4)
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

res.aov2 <- anova_test(data = varDfTemp, dv = varVector, between = accGroups)
antab <- get_anova_table(res.aov2)
print(antab)

lmvar <- lm(varVector~accGroups, data = varDfTemp)
print(summary(lmvar))

p <- ggplot(varMedDf) +
  geom_bar(aes(x = accMedGroupNum, y = varMedMeans),stat="identity",fill="black",alpha=0.7) +
  geom_errorbar(aes(x = accMedGroupNum,ymin=varMedMeans-varMedSds, ymax=varMedMeans+varMedSds), width=.2, position=position_dodge(0.05),color="orange") +
  labs(title="Mean of Individual Variance Across Cases",x=paste(classifyVar," Group (Median Split)",sep=""),y="Participant Variance Across Cases") +
  theme_classic() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18,face="bold"),
        line = element_blank())

print(p)

print(t.test(varDfTemp[varDfTemp$accGroups==1,]$varVector,varDfTemp[varDfTemp$accGroups==4,]$varVector))

print(t.test(varDfTemp[varDfTemp$accMedGroups==1,]$varVector,varDfTemp[varDfTemp$accMedGroups==2,]$varVector))


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
print(ggscatter(mds, x = "V1", y = "V2", 
          label = rownames(infoSeekingFullMatrix),
          color = "groups",
          palette = "jco",
          size = 1, 
          ellipse = TRUE,
          ellipse.type = "convex",
          repel = TRUE))

if (numOfClusters == 2)
{
  clusterLabels <- c()
  accGroupLabels <- c()
  values <- c()
  for (i in 1:numOfClusters)
  {
    group <- infoSeekingFullMatrix[infoSeekingFullMatrix$cluster==i,]
    size <- nrow(group)
    #initialAccGroup1 <- nrow(group[group$InitialCorrect==0,])/size
    #initialAccGroup2 <- nrow(group[group$InitialCorrect==1,])/size
    resGroup1 <- nrow(group[group$ResolutionGroup==1,])/size
    resGroup2 <- nrow(group[group$ResolutionGroup==2,])/size
    
    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 2) )
    accGroupLabels <- c(accGroupLabels, c("NegativeResolutionGroup", "PositiveResolutionGroup"))
    values <- c(values, c(resGroup1, resGroup2))
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
    accGroup1 <- nrow(group[group$GeoKnowledgeGroup==1,])/size
    accGroup2 <- nrow(group[group$GeoKnowledgeGroup==2,])/size
    accGroup3 <- nrow(group[group$GeoKnowledgeGroup==3,])/size
    accGroup4 <- nrow(group[group$GeoKnowledgeGroup==4,])/size
    
    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 4) )
    accGroupLabels <- c(accGroupLabels, c("geoKnowledgeGroup1", "geoKnowledgeGroup2", "geoKnowledgeGroup3", "geoKnowledgeGroup4"))
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
    caseGroup1 <- nrow(group[group$Country=="MON",])/size
    caseGroup2 <- nrow(group[group$Country=="SWI",])/size
    caseGroup3 <- nrow(group[group$Country=="KOR",])/size
    caseGroup4 <- nrow(group[group$Country=="COL",])/size
    caseGroup5 <- nrow(group[group$Country=="GRE",])/size
    caseGroup6 <- nrow(group[group$Country=="BOT",])/size
    
    clusterLab <- paste("Cluster", i, sep="")
    clusterLabels <- c(clusterLabels, rep(clusterLab , 6) )
    caseGroupLabels <- c(caseGroupLabels, c("countryGroupMON", "countryGroupSWI", "countryGroupKOR", "countryGroupCOL", "countryGroupGRE", "countryGroupBOT"))
    values <- c(values, c(caseGroup1, caseGroup2, caseGroup3, caseGroup4, caseGroup5, caseGroup6))
  }
  clusterAccDf <- data.frame(clusterLabels, caseGroupLabels, values)
  
  print(ggplot(clusterAccDf, aes(fill=caseGroupLabels, y=values, x=clusterLabels)) + 
    geom_bar(position="stack", stat="identity"))
}


##############

#################################
infoseeking.pca <- prcomp(distances, center = TRUE,scale. = TRUE)

summary(infoseeking.pca)

infoseeking.country <- infoSeekingFullMatrix$Country
infoseeking.accGroup <- as.factor(infoSeekingFullMatrix$LikelihoodCorrectGroup)

ggbiplot(infoseeking.pca,ellipse=TRUE,choices=c(1,2), obs.scale = 1, var.scale = 1, var.axes=FALSE, groups=infoseeking.accGroup) +
  ggtitle("PCA of Info Seeking Matrix")+
  theme_minimal()+
  theme(legend.position = "bottom")

###################


# Look at variance in information seeking varies as a function
# of case difficulty and ability 

pptIds <- c()
pptAcc <- c()
distanceVarsEasyCases <- c()
#distanceVarsMedCases <- c()
distanceVarsHardCases <- c()
infoSeekingEasy <- c()
#infoSeekingMed <- c()
infoSeekingHard <- c()
confidenceHard <- c()
confidenceEasy <- c()
geoKnowledge <- c()

for (n in 1:nrow(aggData))
{
  ppt <- paste("p", n, sep="")
  pptIds[n] <- ppt
  #pptAcc[n] <- round(aggData$meanFinalAccuracy[n],1)
  pptAcc[n] <- round(aggData$averageLikelihoodOfCorrectCountryFinal[n],1)

  
  compareColumns <- distances[grep(ppt, rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  
  compareColumns <- compareColumns[grep(paste(easyCountryGroup, collapse="|"), rownames(compareColumns)), ]
  compareColumns <- compareColumns[,grep(paste(easyCountryGroup, collapse="|"), colnames(compareColumns)) ]
  distanceVarsEasyCases[n] <- (sd(compareColumns))^2
  
  
  #compareColumns <- distances[grep(ppt, rownames(distances)), ]
  #compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  
  #compareColumns <- compareColumns[grep(paste(medCountryGroup, collapse="|"), rownames(compareColumns)), ]
  #compareColumns <- compareColumns[,grep(paste(medCountryGroup, collapse="|"), colnames(compareColumns)) ]
  #distanceVarsMedCases[n] <- (sd(compareColumns))^2
  
  compareColumns <- distances[grep(ppt, rownames(distances)), ]
  compareColumns <- compareColumns[,grep(ppt, colnames(compareColumns)) ]
  
  compareColumns <- compareColumns[grep(paste(hardCountryGroup, collapse="|"), rownames(compareColumns)), ]
  compareColumns <- compareColumns[,grep(paste(hardCountryGroup, collapse="|"), colnames(compareColumns)) ]
  distanceVarsHardCases[n] <- (sd(compareColumns))^2
  
  geoKnowledge[n] <- aggData$geoScore[n]
  
  infoSeekingEasy[n] <- mean(countryDf[countryDf$id==aggData$participantID[n]&countryDf$countryCode %in% easyCountryGroup,]$caseInformationProportion)
  #infoSeekingMed[n] <- mean(countryDf[countryDf$id==aggData$participantID[n]&countryDf$countryCode %in% medCountryGroup,]$caseInformationProportion)
  infoSeekingHard[n] <- mean(countryDf[countryDf$id==aggData$participantID[n]&countryDf$countryCode %in% hardCountryGroup,]$caseInformationProportion)
  
  confidenceEasy[n] <- mean(abs(countryDf[countryDf$id==aggData$participantID[n]&countryDf$countryCode %in% easyCountryGroup,]$finalConfidence))
  confidenceHard[n] <- mean(abs(countryDf[countryDf$id==aggData$participantID[n]&countryDf$countryCode %in% hardCountryGroup,]$finalConfidence))
  
}
distVarByDiff <- data.frame(pptIds, pptAcc,distanceVarsEasyCases,distanceVarsHardCases,geoKnowledge,infoSeekingEasy,infoSeekingHard,confidenceEasy,confidenceHard)
colnames(distVarByDiff) <- c("ParticipantID","ParticipantAccuracy", "EasyDistanceVariance","HardDistanceVariance","GeographicKnowledge","EasyInfoSeeking", "HardInfoSeeking","EasyConfidenceChange","HardConfidenceChange")

medAcc <- median(aggData$geoScore)

easyLow <- distVarByDiff[distVarByDiff$GeographicKnowledge<=medAcc,]$EasyDistanceVariance
easyHigh <- distVarByDiff[distVarByDiff$GeographicKnowledge>medAcc,]$EasyDistanceVariance
#medLow <- distVarByDiff[distVarByDiff$GeographicKnowledge<=medAcc,]$MedDistanceVariance
#medHigh <- distVarByDiff[distVarByDiff$GeographicKnowledge>medAcc,]$MedDistanceVariance
hardLow <- distVarByDiff[distVarByDiff$GeographicKnowledge<=medAcc,]$HardDistanceVariance
hardHigh <- distVarByDiff[distVarByDiff$GeographicKnowledge>medAcc,]$HardDistanceVariance

anovaDf <- c(easyLow, easyHigh, hardLow, hardHigh)
anovaDf <- as.data.frame(anovaDf)
anovaDf <- cbind(anovaDf,c(rep("easy",nrow(aggData)),c(rep("hard",nrow(aggData)))))
anovaDf <- cbind(anovaDf,c(rep("low",length(easyLow)),rep("high",length(easyHigh)),rep("low",length(hardLow)),rep("high",length(hardHigh))))
anovaDf <- cbind(anovaDf,c(rep(distVarByDiff$ParticipantID,2)))
names(anovaDf)[1] <- "DistanceVariance"
names(anovaDf)[2] <- "CaseDifficulty"
names(anovaDf)[3] <- "GeographicKnowledge"
names(anovaDf)[4] <- "ID"


p <- ggboxplot(anovaDf, x = "CaseDifficulty", y = "DistanceVariance", color = "GeographicKnowledge",
               palette = c("#00AFBB", "#E7B800")) +
  scale_x_discrete(limit = c("easy","hard")) + ggtitle("MDS Distance Variance Across Geographic Knowledge and Difficulty")

plot(p)

res.aov2 <- anova_test(
  data = anovaDf, dv = DistanceVariance, wid = ID, between = GeographicKnowledge,
  within = CaseDifficulty
)
get_anova_table(res.aov2)


#####################


easyLow <- distVarByDiff[distVarByDiff$GeographicKnowledge<=medAcc,]$EasyInfoSeeking
easyHigh <- distVarByDiff[distVarByDiff$GeographicKnowledge>medAcc,]$EasyInfoSeeking
hardLow <- distVarByDiff[distVarByDiff$GeographicKnowledge<=medAcc,]$HardInfoSeeking
hardHigh <- distVarByDiff[distVarByDiff$GeographicKnowledge>medAcc,]$HardInfoSeeking

anovaDf <- c(easyLow, easyHigh, hardLow, hardHigh)
anovaDf <- as.data.frame(anovaDf)
anovaDf <- cbind(anovaDf,c(rep("easy",nrow(aggData)),c(rep("hard",nrow(aggData)))))
anovaDf <- cbind(anovaDf,c(rep("low",length(easyLow)),rep("high",length(easyHigh)),rep("low",length(hardLow)),rep("high",length(hardHigh))))
anovaDf <- cbind(anovaDf,c(rep(distVarByDiff$ParticipantID,2)))
names(anovaDf)[1] <- "InfoSeeking"
names(anovaDf)[2] <- "CaseDifficulty"
names(anovaDf)[3] <- "GeographicKnowledge"
names(anovaDf)[4] <- "ID"


p <- ggboxplot(anovaDf, x = "CaseDifficulty", y = "InfoSeeking", color = "GeographicKnowledge",
               palette = c("#00AFBB", "#E7B800")) +
  scale_x_discrete(limit = c("easy","hard")) + ggtitle("Information Seeking Proportion Across Geographic Knowledge and Difficulty")

plot(p)

res.aov2 <- anova_test(
  data = anovaDf, dv = InfoSeeking, wid = ID, between = GeographicKnowledge,
  within = CaseDifficulty
)
get_anova_table(res.aov2)



#####################


easyLow <- distVarByDiff[distVarByDiff$GeographicKnowledge<=medAcc,]$EasyConfidenceChange
easyHigh <- distVarByDiff[distVarByDiff$GeographicKnowledge>medAcc,]$EasyConfidenceChange
hardLow <- distVarByDiff[distVarByDiff$GeographicKnowledge<=medAcc,]$HardConfidenceChange
hardHigh <- distVarByDiff[distVarByDiff$GeographicKnowledge>medAcc,]$HardConfidenceChange

anovaDf <- c(easyLow, easyHigh, hardLow, hardHigh)
anovaDf <- as.data.frame(anovaDf)
anovaDf <- cbind(anovaDf,c(rep("easy",nrow(aggData)),c(rep("hard",nrow(aggData)))))
anovaDf <- cbind(anovaDf,c(rep("low",length(easyLow)),rep("high",length(easyHigh)),rep("low",length(hardLow)),rep("high",length(hardHigh))))
anovaDf <- cbind(anovaDf,c(rep(distVarByDiff$ParticipantID,2)))
names(anovaDf)[1] <- "ConfidenceChange"
names(anovaDf)[2] <- "CaseDifficulty"
names(anovaDf)[3] <- "GeographicKnowledge"
names(anovaDf)[4] <- "ID"


p <- ggboxplot(anovaDf, x = "CaseDifficulty", y = "ConfidenceChange", color = "GeographicKnowledge",
               palette = c("#00AFBB", "#E7B800")) +
  scale_x_discrete(limit = c("easy","hard")) + ggtitle("Absolute Confidence Change Across Geographic Knowledge and Difficulty")

plot(p)

res.aov2 <- anova_test(
  data = anovaDf, dv = ConfidenceChange, wid = ID, between = GeographicKnowledge,
  within = CaseDifficulty
)
get_anova_table(res.aov2)


######################################
# Bar chart that breaks down variance by case and accuracy

toMatchLow <- c("geoKnowledgeGroup1", "geoKnowledgeGroup2")
toMatchHigh <- c("geoKnowledgeGroup3", "geoKnowledgeGroup4")

compareColumnsLow <- distances[grep(paste(toMatchLow,collapse="|"), rownames(distances)), ]
compareColumnsLow <- compareColumnsLow[,grep(paste(toMatchLow,collapse="|"), colnames(compareColumnsLow)) ]
varsArr <- c()
num <- 1
for (cond in countriesShort)
{
  compareColumns <- compareColumnsLow[grep(cond, rownames(compareColumnsLow)), ]
  compareColumns <- compareColumns[,grep(cond, colnames(compareColumns)) ]
  varsArr[num] <- (sd(compareColumns))^2
  num <- num + 1
}

compareColumnsHigh <- distances[grep(paste(toMatchHigh,collapse="|"), rownames(distances)), ]
compareColumnsHigh <- compareColumnsHigh[,grep(paste(toMatchHigh,collapse="|"), colnames(compareColumnsHigh)) ]
for (cond in countriesShort)
{
  compareColumns <- compareColumnsHigh[grep(cond, rownames(compareColumnsHigh)), ]
  compareColumns <- compareColumns[,grep(cond, colnames(compareColumns)) ]
  varsArr[num] <- (sd(compareColumns))^2
  num <- num + 1
}
dataB <- data.frame(
  country=rep(c("MON", "SWI", "KOR", "COL", "GRE", "BOT") , 2),
  knowledgeGroup=c(rep("low", 6),rep("high", 6)),
  variance=varsArr
)

varsPlot <- ggplot(dataB,aes(x=country, y=variance, fill=knowledgeGroup)) +
  geom_bar(stat="identity", position="dodge", alpha=0.8)

print(varsPlot +
        scale_x_discrete(limits=countriesShort) +
        ggtitle("Variance in Information Seeking by Participant Knowledge and Country") +
        labs(x = "Country (ordered by accuracy in descending order)", y = "Variance in MDS Distances") +
        theme_classic()) 



################################
