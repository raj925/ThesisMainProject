# Repeated Measures

medianInfoReq <- median(countryDf$caseInformationProportion)
medianInitialDiffs <- median(countryDf$initialDifferentials)

countryDf$caseInfoReqGroup <- with(countryDf, ifelse(caseInformationProportion < medianInfoReq, 1, 2))
countryDf$initialDiffGroup <- with(countryDf, ifelse(initialDifferentials < medianInitialDiffs, 1, 2))
countryDf$confidenceChangeGroup <- with(countryDf, ifelse(confidenceChange <= 0, 1, 2))

confidenceChangeAnova <- aov(confidenceChange~caseInfoReqGroup+initialDiffGroup+caseInfoReqGroup*initialDiffGroup, data=countryDf)
summary(confidenceChangeAnova)


aggData$confidenceChangeGroup <- with(aggData, ifelse(meanConfidenceOverallChange <= 0, 0, 1))
model <- glm(confidenceChangeGroup ~ laterPropOfInfo + meanInitialDiffs,family=binomial(link='logit'),data=aggData)
summary(model)

