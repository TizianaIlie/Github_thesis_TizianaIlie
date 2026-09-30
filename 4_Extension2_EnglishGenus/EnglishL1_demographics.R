  #DESCRIPTION OF DEMOGRAPHICS FOR SECTION 4.1. OF KOCHER 2025
library(ggplot2)
library(here) #TI: Added

#DATA
datanative<-read.csv(here::here("data", "annotations", "kocher_native_data_annotation.csv"), stringsAsFactors = TRUE)

surro_l2english <-read.csv(here::here("data", "annotations", "datal2english.csv"), stringsAsFactors = TRUE)

#PROFICIENCY
aggregate(surro_l2english$Score, by=list(surro_l2english$Proficiency), FUN=range)
aggregate(surro_l2english$Score, by=list(surro_l2english$Proficiency), FUN=mean)


#SCORES BY PROFICIENCY GROUP

demodata<-cbind(aggregate(surro_l2english$Proficiency, by=list(surro_l2english$Textnr), unique),aggregate(surro_l2english$Score, by=list(surro_l2english$Textnr), unique)[,2])
#View(demodata)
colnames(demodata)<-c("Textnr","Proficiency1","Score")
demodata$Proficiency<- "advanced"
head(demodata)
demodata[demodata$Score<33, ]$Proficiency<-"beginner"
demodata[demodata$Score>33 & demodata$Score<66, ]$Proficiency<-"intermediate"
demodata$Proficiency <- factor(demodata$Proficiency, levels = c("beginner", "intermediate", "advanced"))

#FIGURE
#pdf(here::here("4_Extension2_EnglishGenus", "Figures", "Scores.pdf"), height = 300, width = 300)

ggplot(demodata, aes(
  y=Score, 
  x=Proficiency, 
  color=Proficiency), 
  ylim=c(0, 100)) + 
  geom_jitter() +
  theme_classic() +
  scale_color_grey(start=0.8, end=0.2) + 
  theme(legend.position = "none") 
dev.off()

#png(here::here("4_Extension2_EnglishGenus", "Figures", "Scores.png"), height = 500, width = 500)

ggplot(demodata, aes(
  y=Score, 
  x=Proficiency, 
  color=Proficiency), 
  ylim=c(0, 100)) + 
  geom_jitter() +
  theme_classic() +
  scale_color_grey(start=0.8, end=0.2) + 
  theme(legend.position = "none") 
dev.off()
