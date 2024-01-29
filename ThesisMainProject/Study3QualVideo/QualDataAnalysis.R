wd <- dirname(rstudioapi::getSourceEditorContext()$path)
path <- paste(wd, '/SASIQualData.csv', sep="")
dataF <- read.csv(path,header=TRUE,sep=",")
dataF <- as.data.frame(dataF)

hist(dataF$Combined.Experience,breaks = 10)

dataF$CombinedConfidence <- rowSums(dataF[,8:19])

hist(dataF$CombinedConfidence,breaks = 10)

