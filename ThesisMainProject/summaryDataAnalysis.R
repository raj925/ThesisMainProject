wd <- dirname(rstudioapi::getSourceEditorContext()$path)
sumFile <- paste(wd, "/DiagnosisReviewSummary.csv", sep="")
summary <- read.csv(sumFile, header=TRUE, sep=",")

p <- summary %>%
  ggplot( aes(x=Publication.Year)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Publication Years") +
  theme_classic() +
  theme(
    plot.title = element_text(size=15),
    axis.text.y = element_text(face="bold",
                               size=10),
    axis.text.x = element_text(face="bold",
                               size=10)
    ) +
  scale_x_continuous(name ="Publication Year", 
                     breaks=seq(1990,2024,5)) +
  scale_y_continuous(name ="Amount of Papers", 
                     breaks=seq(0,10,1))
p