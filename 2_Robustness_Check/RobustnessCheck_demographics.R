#DESCRIPTION OF DEMOGRAPHICS FOR SECTION 4.1. OF KOCHER 2025
library(ggplot2)
library(here) #TI: Added

#DATA #TI: Typo
#setwd("C:/Dokumente/MASTER/Köln/5_6_Semester/thesis/OSF_Kocher/Analysis")

#datanative<-read.csv("Data/datanatives.csv", stringsAsFactors = TRUE)
datanative<-read.csv(here::here("data", "annotations", "kocher_native_data_annotation.csv"), stringsAsFactors = TRUE)

#datal2<-read.csv("Data/datal2s.csv", stringsAsFactors = TRUE)
datal2<-read.csv(here::here("data", "annotations", "joined_germanL1_data_annotation.csv"), stringsAsFactors = TRUE)

#PROFICIENCY
aggregate(datal2$Score, by=list(datal2$Proficiency), FUN=range)
aggregate(datal2$Score, by=list(datal2$Proficiency), FUN=mean)


#SCORES BY PROFICIENCY GROUP

demodata<-cbind(aggregate(datal2$Proficiency, by=list(datal2$Textnr), unique),aggregate(datal2$Score, by=list(datal2$Textnr), unique)[,2])
#View(demodata)
colnames(demodata)<-c("Textnr","Proficiency1","Score")
demodata$Proficiency<- "advanced"
head(demodata)
demodata[demodata$Score<33, ]$Proficiency<-"beginner"
demodata[demodata$Score>33 & demodata$Score<66, ]$Proficiency<-"intermediate"
demodata$Proficiency <- factor(demodata$Proficiency, levels = c("beginner", "intermediate", "advanced"))

#FIGURE
#pdf("2_Robustness_Check/Figures/Scores.pdf", height = 5, width = 5)
#ggplot(demodata, aes(y=Score, x=Proficiency, color=Proficiency), ylim=c(0,100))+
#geom_jitter()+
#theme_classic() +scale_color_grey(start=0.8, end=0.2) + theme(legend.position = "none") 
#dev.off()

#ggsave("2_Robustness_Check/Figures/Scores.png",
#       plot = ggplot(demodata, aes(y=Score, x=Proficiency, color=Proficiency), ylim=c(0,100))+
#         geom_jitter()+
#         theme_classic() +scale_color_grey(start=0.8, end=0.2) + theme(legend.position = "none"),
#       width = 5,
#       height = 5,
#       dpi = 300)

