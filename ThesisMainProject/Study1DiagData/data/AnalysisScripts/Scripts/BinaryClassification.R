requiredPackages <- c("rpart", "caret", "tidyverse", "data.table", "verification", "glmnet",
                      "GGally", "corrplot", "verification", "ROCR", "maptree",
                      "glmnet", "gridExtra", "randomForest", "mgcv", "nnet", "pROC", "pls",
                      "gbm", "e1071", "xgboost", "DT", "NeuralNetTools", "rpart.plot", "ROCR")
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(requiredPackages, require, character.only = TRUE)

#####################
### Binary classification of ability

# T1 (past patient history) is requested 490 out of 492 trials

# Set up data
set.seed(222)
if (classifyVar == "confidence")
{
  classifierData <- infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantType=="p",]
  classifierData <- classifierData[,c(2:29,40)]
  classifierData$ConfidenceGroup <- as.integer(as.logical(classifierData$ConfidenceGroup>2))
  classifierData$ConfidenceGroup <- as.factor(classifierData$ConfidenceGroup)
  colnames(classifierData)[1:29] <- c("T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                      "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                      "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                                      "T23", "T24", "T25", "T26", "T27", "T28", "T29","Group")
  contClassifierData <- cbind(classifierData, infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantType=="p",]$ConfidenceScore)
  colnames(contClassifierData)[30] <- "Score"
} else 
{
  classifierData <- infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantType=="p",]
  classifierData <- classifierData[,c(2:29,38)]
  classifierData$AccuracyGroup <- as.integer(as.logical(classifierData$AccuracyGroup>2))
  classifierData$AccuracyGroup <- as.factor(classifierData$AccuracyGroup)
  colnames(classifierData)[1:29] <- c("T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                      "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                      "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                                      "T23", "T24", "T25", "T26", "T27", "T28", "T29","Group")
  contClassifierData <- cbind(classifierData, infoSeekingFullMatrix[infoSeekingFullMatrix$ParticipantType=="p",]$AccuracyScore)
  colnames(contClassifierData)[30] <- "Score"
}

thresh<-seq(0,1,0.001)
#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV", number = 100, savePredictions = TRUE)


#####################################################

# LOOCV with predicting geo score as a continuous/ordinal variable


#fit a regression model and use LOOCV to evaluate performance
#model <- train(Score ~ T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 +
#                 T11 + T12 + T13 + T14 + T15 + T16 + T17 + T18 + T19 + T20 +
#                 T21 + T22 + T23 + T24 + T25 + T26 + T27 + T28 + T29, data = contClassifierData, method = "lm", trControl = ctrl)

# binda - Binary Discriminant Analysis
# logreg - logic regression (for binary predictors)
# Source: dude just trust me
# http://topepo.github.io/caret/train-models-by-tag.html#Binary_Predictors_Only

#print(model)
#model$results$RMSE

######################################
# Models using LOOCV

set.seed(1000)

# Shuffle rows in case there are order biases
classifierData <- classifierData[sample(1:nrow(classifierData)),]
modelglm<-train(Group ~ T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 +
                T11 + T12 + T13 + T14 + T15 + T16 + T17 +  T18 + T19 + T20 +
                T21 + T22 + T23 + T24 + T25 + T26 + T27 + T28 + T29, method = "glm", family = binomial(link=probit), data = classifierData, trControl = ctrl)
prediglm<-predict(modelglm,type = "prob")[2]

modelrpart<-train(Group ~ T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 +
                  T11 + T12 + T13 + T14 + T15 + T16 + T17 +  T18 + T19 + T20 +
                  T21 + T22 + T23 + T24 + T25 + T26 + T27 + T28 + T29, method = "rpart", data = classifierData, trControl = ctrl)
predirpart<-predict(modelrpart,type = "prob")[2]

thresh<-seq(0,1,0.001)
# Plot all test results on one ROC curve
rocPlot <- roc.plot(x=classifierData$Group=="1",pred=cbind(prediglm,predirpart),legend = T,
                    leg.text = c("GLM","ClassificationTree"),thresholds = thresh)$roc.vol



# Extract coefficients from the GLM model
coefficients <- coef(modelglm$finalModel)

# Sort coefficients by absolute values
sorted_coefficients <- sort(abs(coefficients), decreasing = TRUE)

# Display sorted coefficients
print(sorted_coefficients)

sorted_coefficients <- as.data.frame(sorted_coefficients)
sorted_coefficients$testName <- rownames(sorted_coefficients)
sorted_coefficients <- sorted_coefficients[-c(1), ]
colnames(sorted_coefficients) <- c("Coefficient","Test")
sorted_coefficients$Test <- as.numeric(str_replace(sorted_coefficients$Test,"T",""))

sorted_coefficients <- sorted_coefficients[sorted_coefficients$Coefficient>0.3,]

testCodes <- c("PRESHIST", "PASTMED", "MEDS", "ALLERG", "FAMHIST", "SOCHIST", "PULSE", "BP", "RESP", "LUNG", "HEART", "EYES", "TEMP",
               "ABDEX", "REC", "NECK", "HEAD", "NEURO", "EXTR",
               "URINE", "ECG", "ABCT", "VBG", "ELEC", "CRP", "CLOT", "FBC",
               "BIOCH", "CHXR")

sorted_coefficients$Test <- testCodes[c(sorted_coefficients$Test)]

ggplot(sorted_coefficients, aes(x = reorder(Test,-Coefficient), y = as.double(Coefficient))) +
  geom_bar(position="dodge", stat="identity") +
  labs(y="Regression Coefficient", x = "Test") +
  theme_classic()


#####################################################
# PCA Regression

contClassifierData <- subset(contClassifierData, select= -c(Group))

contClassifierData$Score <- as.numeric(contClassifierData$Score)
pcr_model <- pcr(Score~., data = contClassifierData, scale = TRUE, validation = "CV")

summary(pcr_model)

validationplot(pcr_model)

pcr_pred <- predict(pcr_model, contClassifierData, ncomp = 3)
mean((pcr_pred - contClassifierData$Score)^2)
# This is the average deviation between the predicted value and the observed value

results <- prcomp(contClassifierData[,1:28], scale = TRUE)
var_explained = results$sdev^2 / sum(results$sdev^2)

qplot(c(1:28), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.3)


pcr_model <- pcr(Score~., data = contClassifierData, scale = TRUE, validation = "CV")
pcr_pred <- predict(pcr_model, contClassifierData[,1:28], ncomp = 4)

pcaClassifierData <- data.frame(pcr_pred,infoSeekingFullMatrix$Condition)

###################################################
# Training and testing by case ###
# Using GLM #

classifierDataUC <- classifierData[grep("UC", rownames(classifierData)), ]
classifierDataGBS <- classifierData[grep("GBS", rownames(classifierData)), ]
classifierDataTA <- classifierData[grep("TA", rownames(classifierData)), ]
classifierDataTTP <- classifierData[grep("TTP", rownames(classifierData)), ]
classifierDataAD <- classifierData[grep("AD", rownames(classifierData)), ]
classifierDataMTB <- classifierData[grep("MTB", rownames(classifierData)), ]

#Order by difficulty
classifierDataDfs <- c()
classifierDataDfs[[1]] <- classifierDataUC
classifierDataDfs[[2]] <- classifierDataGBS
classifierDataDfs[[3]] <- classifierDataTA
classifierDataDfs[[4]] <- classifierDataTTP
classifierDataDfs[[5]] <- classifierDataAD
classifierDataDfs[[6]] <- classifierDataMTB

models <- list()

for (dfn in 1:6)
{
  models[[dfn]]<-train(Group~., method = "glm", family = binomial(link=probit), data = classifierDataDfs[[dfn]], trControl = ctrl)
}

questionClassificationROCTable <- data.frame(matrix(nrow = 6, ncol = 6))
rownames(questionClassificationROCTable) <- c("UC","GBS","TA","TTP", "AD", "MTB")
colnames(questionClassificationROCTable) <- c("UC","GBS","TA","TTP", "AD", "MTB")
for (x in 1:6)
{
  for (y in 1:6)
  {
    if (x == y)
    {
      questionClassificationROCTable[x,y] <- 0
    }
    else
    {
      currentTrainingModel <- models[[x]]
      q <- colnames(questionClassificationROCTable)[y]
      currentTestData <- classifierData[grep(q, rownames(classifierData)), ]
      testpred <-predict(currentTrainingModel,type="prob")[2]
      rocCurve <- roc.plot(x=currentTestData$Group=="1",pred=testpred,thresholds = thresh,plot=NULL)$roc.vol
      questionClassificationROCTable[x,y] <- rocCurve$Area
    }
  }
}
# Train on rows, test on columns

df2 <- questionClassificationROCTable %>%
  rownames_to_column() %>%
  gather(colname, value, -rowname)
print(ggplot(df2, aes(x = colname, y = rowname, fill = value)) +
        geom_tile(colour="white",lwd=1.5,linetype=1) +
        geom_text(aes(label=round(value,2)),colour="black",size=6) +
        scale_fill_gradient2(low = "#075AFF",
                             mid = "#FFFFCC",
                             high = "#FF0000", limits=c(0,1))+
        theme_classic()+
        theme(axis.text=element_text(size=16),
              axis.title=element_text(size=16),
              plot.title=element_text(size=18,face="bold"),
              line = element_blank()
        ))

########################
########################

# Classification of case difficulty

easyCases <- c("UC", "GBS", "TA")
hardCases <- c("TTP", "AD", "MTB")

caseDiffData <- infoSeekingFullMatrix[,c(2:29)]
caseDiffData$Condition <- infoSeekingFullMatrix$Condition
caseDiffData$Condition <- ifelse(caseDiffData$Condition %in% easyCases,0,1)
caseDiffData$Condition <- as.factor(caseDiffData$Condition)

colnames(caseDiffData)[1:29] <- c("T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                    "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                    "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                                    "T23", "T24", "T25", "T26", "T27", "T28", "T29","Condition")

# Shuffle rows in case there are order biases
caseDiffData <- caseDiffData[sample(1:nrow(caseDiffData)),]
modelcasediff<-train(Condition ~ T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 +
                       T11 + T12 + T13 + T14 + T15 + T16 + T17 +  T18 + T19 + T20 +
                       T21 + T22 + T23 + T24 + T25 + T26 + T27 + T28 + T29, method = "glm", family = binomial(link=probit), data = caseDiffData, trControl = ctrl)
predicasediff<-predict(modelcasediff,type = "prob")[2]

thresh<-seq(0,1,0.001)
# Plot all test results on one ROC curve
rocPlot <- roc.plot(x=caseDiffData$Condition=="1",pred=cbind(predicasediff),legend = T,
                    leg.text = c("GLM Classification of Case"),thresholds = thresh)$roc.vol


########################

caseInformationConsensus <- infoSeekingFullMatrix[,c(1:29)]
colnames(caseInformationConsensus)[1:29] <- c("T1","T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                    "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                    "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                                    "T23", "T24", "T25", "T26", "T27", "T28", "T29")

caseInformationConsensus$Condition <- infoSeekingFullMatrix$Condition

caseInformationConsensus <- caseInformationConsensus[!grepl("e1", rownames(caseInformationConsensus)),]
caseInformationConsensus <- caseInformationConsensus[!grepl("e2", rownames(caseInformationConsensus)),]

caseInformationConsensusUC <- caseInformationConsensus[caseInformationConsensus$Condition=="UC",]
caseInformationConsensusGBS <- caseInformationConsensus[caseInformationConsensus$Condition=="GBS",]
caseInformationConsensusAD <- caseInformationConsensus[caseInformationConsensus$Condition=="AD",]
caseInformationConsensusTTP <- caseInformationConsensus[caseInformationConsensus$Condition=="TTP",]
caseInformationConsensusMTB <- caseInformationConsensus[caseInformationConsensus$Condition=="MTB",]
caseInformationConsensusTA <- caseInformationConsensus[caseInformationConsensus$Condition=="TA",]

caseInformationConsensusUC = subset(caseInformationConsensusUC, select = -c(Condition,T1,T2,T3,T4,T5,T6))
caseInformationConsensusGBS = subset(caseInformationConsensusGBS, select = -c(Condition,T1,T2,T3,T4,T5,T6))
caseInformationConsensusAD = subset(caseInformationConsensusAD, select = -c(Condition,T1,T2,T3,T4,T5,T6))
caseInformationConsensusTTP = subset(caseInformationConsensusTTP, select = -c(Condition,T1,T2,T3,T4,T5,T6))
caseInformationConsensusMTB = subset(caseInformationConsensusMTB, select = -c(Condition,T1,T2,T3,T4,T5,T6))
caseInformationConsensusTA = subset(caseInformationConsensusTA, select = -c(Condition,T1,T2,T3,T4,T5,T6))

caseInformationConsensusUC <- colSums(caseInformationConsensusUC)/80
caseInformationConsensusGBS <- colSums(caseInformationConsensusGBS)/80
caseInformationConsensusAD <- colSums(caseInformationConsensusAD)/80
caseInformationConsensusTTP <- colSums(caseInformationConsensusTTP)/80
caseInformationConsensusMTB <- colSums(caseInformationConsensusMTB)/80
caseInformationConsensusTA <- colSums(caseInformationConsensusTA)/80

allTestsSet <- c(testSet1,testSet2,testSet3)

Ntests <- 5

commonTestsUC <- order(caseInformationConsensusUC,decreasing=TRUE)[1:Ntests]+6
commonTestsGBS <- order(caseInformationConsensusGBS,decreasing=TRUE)[1:Ntests]+6
commonTestsAD <- order(caseInformationConsensusAD,decreasing=TRUE)[1:Ntests]+6
commonTestsTTP <- order(caseInformationConsensusTTP,decreasing=TRUE)[1:Ntests]+6
commonTestsMTB <- order(caseInformationConsensusMTB,decreasing=TRUE)[1:Ntests]+6
commonTestsTA <- order(caseInformationConsensusTA,decreasing=TRUE)[1:Ntests]+6

commonTestsTextUC <- allTestsSet[c(commonTestsUC)]
commonTestsTextGBS <- allTestsSet[c(commonTestsGBS)]
commonTestsTextAD <- allTestsSet[c(commonTestsAD)]
commonTestsTextTTP <- allTestsSet[c(commonTestsTTP)]
commonTestsTextMTB <- allTestsSet[c(commonTestsMTB)]
commonTestsTextTA <- allTestsSet[c(commonTestsTA)]

for (n in 1:nrow(caseInformationConsensus)) #row
{
  for (m in 1:ncol(caseInformationConsensus)) #column
  {
    consensusSet <- c()
    if (caseInformationConsensus[n,]$Condition == "UC")
    {
      consensusSet <- caseInformationConsensusUC
    } else if (caseInformationConsensus[n,]$Condition == "GBS") {
      consensusSet <- caseInformationConsensusGBS
    } else if (caseInformationConsensus[n,]$Condition == "AD") {
      consensusSet <- caseInformationConsensusAD
    } else if (caseInformationConsensus[n,]$Condition == "TTP") {
      consensusSet <- caseInformationConsensusTTP
    } else if (caseInformationConsensus[n,]$Condition == "MTB") {
      consensusSet <- caseInformationConsensusMTB
    } else if (caseInformationConsensus[n,]$Condition == "TA") {
      consensusSet <- caseInformationConsensusTA
    }
    caseInformationConsensus[n,m] <- ifelse(caseInformationConsensus[n,m]==1,consensusSet[m],NA)
  }
}
caseInformationConsensus = subset(caseInformationConsensus, select = -c(Condition))

caseInformationConsensus$meanValue <- rowMeans(caseInformationConsensus,na.rm = TRUE)

pptMeans <- c()
group <- c()
for (ppt in 1:85)
{
  pptTrials <- caseInformationConsensus[grepl(paste("p",ppt,"-a",sep=""), rownames(caseInformationConsensus), ignore.case = FALSE),]
  pptMeans[ppt] <- mean(pptTrials$meanValue,na.rm=TRUE)
  if (classifyVar == "confidence")
  {
    group[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-a",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$ConfidenceGroup[1]
  } else {
    group[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-a",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$AccuracyGroup[1]
  }
}

accGroupInfoDf <- data.frame(pptMeans,group)

accDfMeans <- c(mean(accGroupInfoDf[accGroupInfoDf$group==1,]$pptMeans),
                mean(accGroupInfoDf[accGroupInfoDf$group==2,]$pptMeans),
                mean(accGroupInfoDf[accGroupInfoDf$group==3,]$pptMeans),
                mean(accGroupInfoDf[accGroupInfoDf$group==4,]$pptMeans))
accDfSds <- c(sd(accGroupInfoDf[accGroupInfoDf$group==1,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==1,])),
              sd(accGroupInfoDf[accGroupInfoDf$group==2,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==2,])),
              sd(accGroupInfoDf[accGroupInfoDf$group==3,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==3,])),
              sd(accGroupInfoDf[accGroupInfoDf$group==4,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==4,])))
accGroupNum <- c(1,2,3,4)
varDf <-data.frame(accGroupNum, accDfMeans,accDfSds)

p <- ggplot(varDf) +
  geom_bar(aes(x = accGroupNum, y = accDfMeans),stat="identity",fill="black",alpha=0.7) +
  geom_errorbar(aes(x = accGroupNum,ymin=accDfMeans-accDfSds, ymax=accDfMeans+accDfSds), width=.2, position=position_dodge(0.05),color="orange") +
  labs(title="Mean of Information Value",x=paste(classifyVar, " Group (Quantiles)", sep=""),y="Mean Information Value Sought") +
  theme_classic() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18,face="bold"),
        line = element_blank())

print(p)

#############
# Expert Value of Information

expTemp <- infoSeekingFullMatrix[,c(1:29)]
colnames(expTemp)[1:29] <- c("T1","T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                             "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                             "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                             "T23", "T24", "T25", "T26", "T27", "T28", "T29")

expTemp$Condition <- infoSeekingFullMatrix$Condition

expTemp <- expTemp[grepl("e1|e2|e3|e4|e5|e6", rownames(expTemp)),]

expInfoDf <- data.frame(matrix(ncol = 29,nrow = 6))
cases <- c("UC", "GBS", "TA", "TTP", "AD", "MTB")
for (x in 1:length(cases))
{
  caseSel <- expTemp[expTemp$Condition==cases[x],]
  for (y in 1:(ncol(caseSel)-1))
  {
    expInfoDf[x,y] <- mean(caseSel[,y])
  }
}
rownames(expInfoDf) <- cases

########


infoValueDf <- infoSeekingFullMatrix[,c(1:29)]
colnames(infoValueDf)[1:29] <- c("T1","T2","T3","T4","T5","T6","T7", "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                              "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                                              "T23", "T24", "T25", "T26", "T27", "T28", "T29")

infoValueDf$Correct <- infoSeekingFullMatrix$Correct
infoValueDf$Condition <- infoSeekingFullMatrix$Condition
infoValueDf$ID <- infoSeekingFullMatrix$ID


temp <- infoSeekingFullMatrix[,c(1:29)]
colnames(temp)[1:29] <- c("T1","T2","T3","T4","T5","T6","T7", "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                          "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                          "T23", "T24", "T25", "T26", "T27", "T28", "T29")

temp$Condition <- infoSeekingFullMatrix$Condition
temp$ID <- infoSeekingFullMatrix$ID

temp <- temp[!grepl("e1|e2|e3|e4|e5|e6", rownames(temp)),]

standard <- "expert" #student/expert
if (standard == "student")
{
  infoValueDf <- infoValueDf[!grepl("e1|e2|e3|e4|e5|e6", rownames(infoValueDf)),]
} else
{
  infoValueDf <- infoValueDf[grepl("e1|e2|e3|e4|e5|e6", rownames(infoValueDf)),]
}


for (n in 1:nrow(temp)) #row
{
  for (m in 1:29) #column
  {
    accSet <- c()
    currentID <- temp[n,]$ID # cross validation
    infoSelectCase <- infoValueDf[infoValueDf$Condition==temp[n,]$Condition,]
    infoSelect <- infoSelectCase[,m]
    infoSelect <- as.data.frame(infoSelect)
    infoSelect <- cbind(infoSelect,infoSelectCase$ID)
    infoSelect <- cbind(infoSelect,infoSelectCase$Correct)
    colnames(infoSelect) <- c("Info","ID","Correct")
    infoSelect <- infoSelect[infoSelect$ID!=currentID,]
    infoSelect <- infoSelect[, !(colnames(infoSelect) %in% c("ID"))] 
    accPresent <- mean(infoSelect[infoSelect$Info==1,]$Correct,na.rm=TRUE)
    accNotPresent <- mean(infoSelect[infoSelect$Info==0,]$Correct,na.rm=TRUE)
    if (nrow(infoSelect[infoSelect$Info==0,]) > 1)
    {
      temp[n,m] <- ifelse(temp[n,m]==1,accPresent-accNotPresent,NA)
      if (is.nan(temp[n,m]))
      {
        temp[n,m] <- 0
      }
    }
  }
}
temp = subset(temp, select = -c(Condition,ID))


temp$infoValue <- rowMeans(temp,na.rm = TRUE)
#temp$infoValue <- rowSums(temp,na.rm = TRUE)

################

studentInfoDf <- temp

studentInfoDf <- data.frame(matrix(ncol = 29,nrow = 6))
cases <- c("UC", "GBS", "TA", "TTP", "AD", "MTB")
for (x in 1:length(cases))
{
  caseSel <- temp[grepl(cases[x], rownames(temp)),]
  for (y in 1:(ncol(caseSel)-1))
  {
    studentInfoDf[x,y] <- max(replace(caseSel[,y], caseSel[,y] == 0, NA),na.rm=TRUE)
  }
}
rownames(studentInfoDf) <- cases

# Just Information that was sought
# temp$meanValue <- rowMeans(replace(temp, temp == 0, NA),na.rm = TRUE)

#######################

pptMeans <- c()
group <- c()
value <- c()
for (ppt in 1:85)
{
  pptTrials <- temp[grepl(paste("p",ppt,"-a",sep=""), rownames(temp), ignore.case = FALSE),]
  pptMeans[ppt] <- mean(pptTrials$infoValue,na.rm=TRUE)
  if (classifyVar == "confidence")
  {
    group[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-a",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$ConfidenceGroup[1]
    value[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-a",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$ConfidenceScore[1]
    
  } else {
    group[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-a",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$AccuracyGroup[1]
    value[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-a",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$AccuracyScore[1]
  }
}

accGroupInfoDf <- data.frame(pptMeans,group,value)

accDfMeans <- c(mean(accGroupInfoDf[accGroupInfoDf$group==1,]$pptMeans),
                mean(accGroupInfoDf[accGroupInfoDf$group==2,]$pptMeans),
                mean(accGroupInfoDf[accGroupInfoDf$group==3,]$pptMeans),
                mean(accGroupInfoDf[accGroupInfoDf$group==4,]$pptMeans))
accDfSds <- c(sd(accGroupInfoDf[accGroupInfoDf$group==1,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==1,])),
              sd(accGroupInfoDf[accGroupInfoDf$group==2,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==2,])),
              sd(accGroupInfoDf[accGroupInfoDf$group==3,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==3,])),
              sd(accGroupInfoDf[accGroupInfoDf$group==4,]$pptMeans)/sqrt(nrow(accGroupInfoDf[accGroupInfoDf$group==4,])))
accGroupNum <- c(1,2,3,4)
varDf <-data.frame(accGroupNum, accDfMeans,accDfSds)

p <- ggplot(varDf) +
  geom_bar(aes(x = accGroupNum, y = accDfMeans),stat="identity",fill="skyblue",alpha=0.7) +
  geom_errorbar(aes(x = accGroupNum,ymin=accDfMeans-accDfSds, ymax=accDfMeans+accDfSds), width=.2, position=position_dodge(0.05),color="orange") +
  labs(title="Mean Information Value",x="Accuracy Group (Quantiles)",y="Total Information Value Sought") +
  theme_classic() +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=16),
        plot.title=element_text(size=18,face="bold"),
        line = element_blank())

print(p)

lmvar <- lm(accDfMeans~accGroupNum, data = varDf)
summary(lmvar)


# Continuous
cor <- cor.test(accGroupInfoDf$value,accGroupInfoDf$pptMeans,method="pearson")

diffCon <- ggplot(data = accGroupInfoDf, aes(x=value, y=pptMeans)) +
  geom_point() +
  geom_smooth(method=lm , color=accuracyColour, fill="#69b3a2", se=TRUE) +
  theme_classic()

title <- paste("Participant ", classifyVar ,"/ Info Value: ",
               "r(", cor$parameter, ") = ",  round(cor$estimate, 2), ", p = ",  round(cor$p.value,2), sep="")

print(diffCon + 
        ggtitle(title)
      + labs(y="Information Value", x = paste("Participant ", classifyVar, sep=""))
      +theme(axis.text=element_text(size=16),
             axis.title=element_text(size=16),
             plot.title=element_text(size=14,face="bold")
      ))


####################
# What is the most consistently sought info
# for highest performers?

infoValueTable <- infoSeekingFullMatrix[,7:29]
if (classifyVar == "confidence")
{
  infoValueTable <- infoValueTable[grepl("confGroup4", rownames(infoValueTable)),]
} else {
  infoValueTable <- infoValueTable[grepl("accGroup4", rownames(infoValueTable)),]
}
infoSeekingHighAccGroup <- colSums(infoValueTable==0)
infoSeekingHighAccGroup <- nrow(infoValueTable)-infoSeekingHighAccGroup
commonTests <- allTests[order(infoSeekingHighAccGroup,decreasing=TRUE)[1:5]+6]


####################
infoVector <- integer(ncol(infoValueTable))
for (ppt in 1:85)
{
  pptTrials <- infoValueTable[grepl(paste("p",ppt,"-a",sep=""), rownames(infoValueTable), ignore.case = FALSE),]
  if (nrow(pptTrials) > 1)
  {
    for (x in 1:ncol(pptTrials))
    {
      vals <- pptTrials[,x]
      if (sum(vals>0) > 4)
      {
        infoVector[x] <- infoVector[x]+1
      }
    }
  }
}
universalTests <- allTests[order(infoVector,decreasing=TRUE)[1:5]+6]

####################
# How much is each info sought by acc group?

infoSeekingProps <- data.frame(matrix(ncol=23,nrow =5))
setStr <- ""
if (classifyVar == "confidence")
{
  setStr <- "conf"
} else {
  setStr <- "acc"
}
for (accG in 1:4)
{
  infoValueTable <- infoSeekingFullMatrix[,7:29]
  infoValueTable <- infoValueTable[grepl(paste(setStr, "Group", accG, sep=""), rownames(infoValueTable)),]
  infoSeekingProps[accG,] <- colMeans(infoValueTable)
  rownames(infoSeekingProps)[accG] <- paste("accGroup", accG, sep="")
}
colnames(infoSeekingProps) <- c("T7",  "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                             "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                             "T23", "T24", "T25", "T26", "T27", "T28", "T29")

infoValueTable <- infoValueTable[grepl(paste("-exp", sep=""), rownames(infoValueTable)),]
infoSeekingProps[5,] <- colMeans(infoValueTable)
rownames(infoSeekingProps)[5] <- "exp"



infoPropValues <- as.array(c(sort(infoSeekingProps[1,],decreasing=TRUE),
                             sort(infoSeekingProps[2,],decreasing=TRUE),
                             sort(infoSeekingProps[3,],decreasing=TRUE),
                             sort(infoSeekingProps[4,],decreasing=TRUE),
                             sort(infoSeekingProps[5,],decreasing=TRUE)))
repN <- ncol(infoSeekingProps)
accGroupL <- c(rep(1, repN),rep(2, repN),rep(3, repN),rep(4, repN),rep(5, repN))
tests <- c(colnames(sort(infoSeekingProps[1,],decreasing=TRUE)),
           colnames(sort(infoSeekingProps[2,],decreasing=TRUE)),
           colnames(sort(infoSeekingProps[3,],decreasing=TRUE)),
           colnames(sort(infoSeekingProps[4,],decreasing=TRUE)),
           colnames(sort(infoSeekingProps[5,],decreasing=TRUE)))
ranks <- c(rep(c(1:repN),5))
testIdx <- c(rep(0,length(repN*5)))
infoPropDf <- data.frame(infoPropValues,accGroupL,tests,ranks,testIdx)

testCodes <- c("PULSE", "BP", "RESP", "LUNG", "HEART", "EYES", "TEMP",
               "ABDEX", "REC", "NECK", "HEAD", "NEURO", "EXTR",
               "URINE", "ECG", "ABCT", "VBG", "ELEC", "CRP", "CLOT", "FBC",
               "BIOCH", "CHXR")

for (testNum in 7:29)
{
  testStr <- paste("T", testNum, sep="")
  infoPropDf[infoPropDf$tests==testStr,]$testIdx <- testNum-6
  infoPropDf[infoPropDf$tests==testStr,]$tests <- testCodes[testNum-6]
}

infoPropDf <- infoPropDf[infoPropDf$accGroupL<5,]

ggplot(infoPropDf, aes(x = as.numeric(accGroupL), y = as.double(infoPropValues), fill = tests, group=ranks)) +
  geom_bar(position="dodge", stat="identity") +
  #scale_fill_viridis(discrete=T) +
  geom_text(aes(label = tests),position = position_dodge(0.9), size = 2, angle = 90, hjust = 1) +
  labs(y="Information Seeking Proportion", x = paste("Participant ", classifyVar, sep="")) +
  theme_classic()

# Compute similarity of order of tests

testSimAcc <- data.frame(matrix(ncol=5,nrow=5))
colnames(testSimAcc) <- c("accGroup1","accGroup2","accGroup3","accGroup4","exp")
rownames(testSimAcc) <- c("accGroup1","accGroup2","accGroup3","accGroup4","exp")

for (x in 1:5)
{
  for (y in 1:5)
  {
    if (x == y)
    {
      testSimAcc[x,y] <- 0
    }
    else 
    {
      testSimAcc[x,y] <- kendallTauDistance(infoPropDf[infoPropDf$accGroupL==x,]$testIdx,
                                            infoPropDf[infoPropDf$accGroupL==y,]$testIdx)
    }
  }
}

