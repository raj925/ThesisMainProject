##### Per participant (sorted by accuracy), easy vs hard variance

easyDistVar <- distVarByDiff[,!(names(distVarByDiff) %in% c("HardDistanceVariance"))]
names(easyDistVar)[4] <- "DistanceVariance"
easyDistVar <- cbind(easyDistVar,rep("easy", nrow(easyDistVar)))
names(easyDistVar)[6] <- "Difficulty"
easyDistVar$ParticipantID <- paste(easyDistVar$ParticipantAccuracy, "-", easyDistVar$ParticipantID, sep="")

hardDistVar <- distVarByDiff[,!(names(distVarByDiff) %in% c("EasyDistanceVariance"))]
names(hardDistVar)[4] <- "DistanceVariance"
hardDistVar <- cbind(hardDistVar,rep("hard", nrow(hardDistVar)))
names(hardDistVar)[6] <- "Difficulty"
hardDistVar$ParticipantID <- paste(hardDistVar$ParticipantAccuracy, "-", hardDistVar$ParticipantID, sep="")

allDistVar <- rbind(easyDistVar, hardDistVar)

p <- ggplot(allDistVar) +
  geom_segment(data = easyDistVar,
               aes(x = DistanceVariance, y = ParticipantID,
                   yend = hardDistVar$ParticipantID, xend = hardDistVar$DistanceVariance),
               color = "#aeb6bf",
               size = 4.5,
               alpha = 0.5
               ) +
  geom_point(aes(x = DistanceVariance, y = ParticipantID, color = Difficulty), size = 4, show.legend = TRUE) +
  ggtitle("Variance in MSD Distances by Case Difficulty")
p

#### Per case, positive vs negative resolution variance


