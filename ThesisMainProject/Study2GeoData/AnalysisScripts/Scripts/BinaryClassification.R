requiredPackages <- c("rpart", "caret", "tidyverse", "data.table", "verification", "glmnet",
                      "GGally", "corrplot", "verification", "ROCR", "maptree",
                      "glmnet", "gridExtra", "randomForest", "mgcv", "nnet", "pROC", "pls",
                      "gbm", "e1071", "xgboost", "DT", "NeuralNetTools", "rpart.plot", "ROCR", "scipy")
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(requiredPackages, require, character.only = TRUE)

#####################
### Binary classification of ability

# 18 and 28 are NEVER requested

# Set up data
set.seed(222)

#specify the cross-validation method
ctrl <- trainControl(method = "LOOCV", number = 100, savePredictions = TRUE)

if (classifyVar == "confidence")
{
  classifierData <- infoSeekingFullMatrix[,c(1:17,19:27,29,42)]
  classifierData$ConfidenceGroup <- as.integer(as.logical(classifierData$ConfidenceGroup>2))
  classifierData$ConfidenceGroup <- as.factor(classifierData$ConfidenceGroup)
  colnames(classifierData) <- c("T1", "T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                      "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                      "T15", "T16", "T17", "T19", "T20", "T21", "T22", 
                                      "T23", "T24", "T25", "T26", "T27", "T29","Group")
  contClassifierData <- cbind(classifierData, infoSeekingFullMatrix$ConfidenceScore)
  colnames(contClassifierData)[29] <- "Score"
} else if (classifyVar == "accuracy")
{
  classifierData <- infoSeekingFullMatrix[,c(1:17,19:27,29,40)]
  classifierData$AccuracyGroup <- as.integer(as.logical(classifierData$AccuracyGroup>2))
  classifierData$AccuracyGroup <- as.factor(classifierData$AccuracyGroup)
  colnames(classifierData) <- c("T1", "T2",  "T3",  "T4",  "T5",  "T6",  "T7",  
                                      "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                      "T15", "T16", "T17", "T19", "T20", "T21", "T22", 
                                      "T23", "T24", "T25", "T26", "T27", "T29","Group")
  contClassifierData <- cbind(classifierData, infoSeekingFullMatrix$AccuracyScore)
  colnames(contClassifierData)[29] <- "Score"
} else {
  classifierData <- infoSeekingFullMatrix[,c(1:17,19:27,29, 38)]
  classifierData$GeoKnowledgeGroup <- as.integer(as.logical(classifierData$GeoKnowledgeGroup>2))
  classifierData$GeoKnowledgeGroup <- as.factor(classifierData$GeoKnowledgeGroup)
  colnames(classifierData) <- c("T1","T2",  "T3",  "T4",  "T5",  "T6",  "T7",  "T8",  
                                "T9", "T10", "T11", "T12", "T13", "T14", "T15", 
                                "T16", "T17", "T19", "T20", "T21", "T22", "T23", "T24", "T25", 
                                "T26", "T27", "T29", "Group")
  contClassifierData <- cbind(classifierData, infoSeekingFullMatrix$GeoKnowledgeScore)
  colnames(contClassifierData)[29] <- "Score"
}

thresh<-seq(0,1,0.001)


#####################################################

set.seed(1000)

# Shuffle rows in case there are order biases
classifierData <- classifierData[sample(1:nrow(classifierData)),]
modelglm<-train(Group ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 +
                  T11 + T12 + T13 + T14 + T15 + T16 + T17 + T19 + T20 +
                  T21 + T22 + T23 + T24 + T25 + T26 + T27 + T29, method = "glm", family = binomial(link=probit), data = classifierData, trControl = ctrl)
prediglm<-predict(modelglm,type = "prob")[2]

modelrpart<-train(Group ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + T9 + T10 +
                    T11 + T12 + T13 + T14 + T15 + T16 + T17 + T19 + T20 +
                    T21 + T22 + T23 + T24 + T25 + T26 + T27 + T29, method = "rpart", data = classifierData, trControl = ctrl)
predirpart<-predict(modelrpart,type = "prob")[2]

thresh<-seq(0,1,0.001)
# Plot all test results on one ROC curve
rocPlot <- roc.plot(x=classifierData$Group=="1",pred=cbind(prediglm,predirpart),legend = T,
                    leg.text = c("GLM","ClassificationTree"),thresholds = thresh)$roc.vol


#####################################################

# LOOCV with predicting geo score as a continuous/ordinal variable

contClassifierData$Score <- as.numeric(contClassifierData$Score)
pcr_model <- pcr(Score~., data = contClassifierData, scale = TRUE, validation = "CV")

summary(pcr_model)

validationplot(pcr_model)

pcr_pred <- predict(pcr_model, contClassifierData, ncomp = 3)
mean((pcr_pred - contClassifierData$Score)^2)
# This is the average deviation between the predicted value and the observed value

results <- prcomp(contClassifierData[,1:27], scale = TRUE)
var_explained = results$sdev^2 / sum(results$sdev^2)

qplot(c(1:27), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 0.3)

#####################################################

infoValueDf <- infoSeekingFullMatrix[,c(1:17,19:27,29)]
colnames(infoValueDf)[1:27] <- c("T1","T2","T3","T4","T5","T6","T7", "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                 "T15", "T16", "T17", "T19", "T20", "T21", "T22", 
                                 "T23", "T24", "T25", "T26", "T27", "T29")

infoValueDf$Correct <- infoSeekingFullMatrix$Correct
infoValueDf$Country <- infoSeekingFullMatrix$Country
infoValueDf$ID <- infoSeekingFullMatrix$ID


temp <- infoSeekingFullMatrix[,c(1:17,19:27,29)]
colnames(temp)[1:27] <- c("T1","T2","T3","T4","T5","T6","T7", "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                          "T15", "T16", "T17", "T19", "T20", "T21", "T22", 
                          "T23", "T24", "T25", "T26", "T27", "T29")

temp$Country <- infoSeekingFullMatrix$Country
temp$ID <- infoSeekingFullMatrix$ID

for (n in 1:nrow(temp)) #row
{
  for (m in 1:27) #column
  {
    accSet <- c()
    currentID <- temp[n,]$ID # cross validation
    infoSelectCase <- infoValueDf[infoValueDf$Country==temp[n,]$Country,]
    infoSelect <- infoSelectCase[,m]
    infoSelect <- as.data.frame(infoSelect)
    infoSelect <- cbind(infoSelect,infoSelectCase$ID)
    infoSelect <- cbind(infoSelect,infoSelectCase$Correct)
    colnames(infoSelect) <- c("Info","ID","Correct")
    infoSelect <- infoSelect[infoSelect$ID!=currentID,]
    infoSelect <- infoSelect[, !(colnames(infoSelect) %in% c("ID"))] 
    accPresent <- mean(infoSelect[infoSelect$Info==1,]$Correct)
    accNotPresent <- mean(infoSelect[infoSelect$Info==0,]$Correct,na.rm=TRUE)
    if (nrow(infoSelect[infoSelect$Info==0,]) > 1)
    {
      temp[n,m] <- ifelse(temp[n,m]==1,accPresent-accNotPresent,0)
    }
  }
}
temp = subset(temp, select = -c(Country,ID))


temp$infoValue <- rowMeans(temp,na.rm = TRUE)

infoDf <- temp

infoDf <- data.frame(matrix(ncol = 27,nrow = 6))
cases <- c("KOR","MON","COL","SWI", "GRE", "BOT")
for (x in 1:length(cases))
{
  caseSel <- temp[grepl(cases[x], rownames(temp)),]
  for (y in 1:(ncol(caseSel)-1))
  {
    infoDf[x,y] <- max(replace(caseSel[,y], caseSel[,y] == 0, NA),na.rm=TRUE)
  }
}
rownames(infoDf) <- cases

# Just Information that was sought
# temp$meanValue <- rowMeans(replace(temp, temp == 0, NA),na.rm = TRUE)


#######################

pptMeans <- c()
group <- c()
value <- c()
for (ppt in 1:88)
{
  pptTrials <- temp[grepl(paste("p",ppt,"-g",sep=""), rownames(temp), ignore.case = FALSE),]
  pptMeans[ppt] <- mean(pptTrials$infoValue,na.rm=TRUE)
  if (classifyVar == "confidence")
  {
    group[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-g",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$ConfidenceGroup[1]
    value[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-g",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$ConfidenceScore[1]
    
  } else if (classifyVar == "accuracy") {
    group[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-g",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$AccuracyGroup[1]
    value[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-g",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$AccuracyScore[1]
  } else {
    group[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-g",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$GeoKnowledgeGroup[1]
    value[ppt] <- infoSeekingFullMatrix[grepl(paste("p",ppt,"-g",sep=""), rownames(infoSeekingFullMatrix), ignore.case = FALSE),]$GeoKnowledgeScore[1]
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
  geom_bar(aes(x = accGroupNum, y = accDfMeans),stat="identity",fill="black",alpha=0.7) +
  geom_errorbar(aes(x = accGroupNum,ymin=accDfMeans-accDfSds, ymax=accDfMeans+accDfSds), width=.2, position=position_dodge(0.05),color="orange") +
  labs(title="Mean of Information Value",x=paste(classifyVar, " Group (Quantiles)", sep=""),y="Mean Information Value Sought") +
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

########
####################
####################
# How much is each info sought by acc group?

infoSeekingProps <- data.frame(matrix(ncol=29,nrow =4))
for (accG in 1:4)
{
  infoValueTable <- infoSeekingFullMatrix[,1:29]
  setStr <- ""
  if (classifyVar == "confidence")
  {
    setStr <- "conf"
  } else if (classifyVar == "accuracy")  {
    setStr <- "acc"
  } else {
    setStr <- "geoKnowledge"
  }
  infoValueTable <- infoValueTable[grepl(paste(setStr, "Group", accG, sep=""), rownames(infoValueTable)),]
  infoSeekingProps[accG,] <- colMeans(infoValueTable)
  rownames(infoSeekingProps)[accG] <- paste("accGroup", accG, sep="")
}
colnames(infoSeekingProps) <- c("T1",  "T2",  "T3", "T4", "T5", "T6",
                                "T7",  "T8",  "T9", "T10", "T11", "T12", "T13", "T14", 
                                "T15", "T16", "T17", "T18", "T19", "T20", "T21", "T22", 
                                "T23", "T24", "T25", "T26", "T27", "T28", "T29")


infoPropValues <- as.array(c(sort(infoSeekingProps[1,],decreasing=TRUE),sort(infoSeekingProps[2,],decreasing=TRUE),sort(infoSeekingProps[3,],decreasing=TRUE),sort(infoSeekingProps[4,],decreasing=TRUE)))
repN <- ncol(infoSeekingProps)
accGroupL <- c(rep(1, repN),rep(2, repN),rep(3, repN),rep(4, repN))
tests <- c(colnames(sort(infoSeekingProps[1,],decreasing=TRUE)),
           colnames(sort(infoSeekingProps[2,],decreasing=TRUE)),
           colnames(sort(infoSeekingProps[3,],decreasing=TRUE)),
           colnames(sort(infoSeekingProps[4,],decreasing=TRUE)))
ranks <- c(rep(c(1:repN),4))
infoPropDf <- data.frame(infoPropValues,accGroupL,tests,ranks)

testCodes <- c("AREA", "BORD", "WATER", "UTC", "FORE", "TEMP", "LANDL",
               "ANIM", "HIGH", "RAIN", "GDP", "FOOD", "MONAR",
               "PM", "HAPPI", "DEMO", "PASSP", "OECD", "GINI", "POPO", "POPC",
               "OLYM", "UNEM", "POPDE", "FLAG", "SIDE", "INTE", "FIFA", "RELIG")

for (testNum in 1:29)
{
  testStr <- paste("T", testNum, sep="")
  infoPropDf[infoPropDf$tests==testStr,]$tests <- testCodes[testNum]
}

ggplot(infoPropDf, aes(x = as.numeric(accGroupL), y = as.double(infoPropValues), fill = tests, group=ranks)) +
  geom_bar(position="dodge", stat="identity") +
  #scale_fill_viridis(discrete=T) +
  geom_text(aes(label = tests),position = position_dodge(0.9), size = 2, angle = 90, hjust = 1) +
  labs(y="Information Seeking Proportion", x = paste("Participant ", classifyVar, sep=""))


