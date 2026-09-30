# MUPDARF ANALYSIS FOR SECTION 5.1-5.2 OF KOCHER 2025
library(partykit)
library(dplyr)
library(stringr)
library(lattice)
library(knitr) #to generate HTML, PDF, Word, etc. documents from .qmd file
library(here) #to declare the location of current script or project
library(tidyverse) #collection of R packages for data science
library(irr) #to calculate the inter-annotator agreement
library(performance)#to check model assumptions
library(ggplot2) #for data visualisation
library(qqplotr) #plot extensions for 'ggplot2'
library(see) #provides framework for statistical modeling, visualization, and reporting

#DATA
#setwd("C:/Dokumente/MASTER/Köln/5_6_Semester/thesis/OSF_Kocher/Analysis")

#datanative<-read.csv("Data/datanatives.csv", stringsAsFactors = TRUE)
datanative<-read.csv(here("1_Reproduction_Kocher", "Analysis", "Data", "datanatives.csv"), stringsAsFactors = TRUE)

#datal2<-read.csv("Data/datal2s.csv", stringsAsFactors = TRUE)
datal2<-read.csv(here("1_Reproduction_Kocher", "Analysis", "Data", "datal2s.csv"), stringsAsFactors = TRUE)

#DISTRIBUITONS
sd(datal2$DurationTask) #TI: not mentioned in the paper
range(datal2$DurationTask) #TI: not mentioned in the paper
mean(datal2$DurationTask) #TI: as in the paper


#MUPDARF

#STEP 1
rfpnative <- partykit::cforest(SubjectType ~ SubjectPosition+ InformationStructure + ReferentialContinuity + ClauseType, 
                weights = ifelse(datanative$SubjectType =='dp', 2,
                                 ifelse(datanative$SubjectType == "pron",4,1) ), 
                data = datanative)

#FIGURE
pdf("1_Reproduction_Kocher/Analysis/Figures/varimp_native.pdf", width = 5, height = 5)
varimp <- varimp.cforest(rfpnative) #TI: variable importance
dotplot(sort(varimp), xlab="Variable importance", col="gray51") #TI: plot is in paper
dev.off()

png("1_Reproduction_Kocher/Analysis/Figures/varimp_native.png", width = 500, height = 500)
varimp <- varimp.cforest(rfpnative) #TI: variable importance
dotplot(sort(varimp), xlab="Variable importance", col="gray51") #TI: plot is in paper
dev.off() #TI: saving varimp as pdf too

#CHECKING ACCURACY
confusionmatrixrfpnative<-table(predict(rfpnative), datanative$SubjectType)
confusionmatrixrfpnative
sum(diag(confusionmatrixrfpnative))/sum(confusionmatrixrfpnative) #TI: 64% as in the paper

#saveRDS(confusionmatrixrfpnative, "1_Reproduction_Kocher/Analysis/Data/Calculations/confusionmatrixrfpnative.rds") #TI: saving as rds to read in thesis document


#INSPECTING PREDICTIONS
datanative$Predictions<-predict(rfpnative)
datanative$Correct<-as.character(datanative$Predictions) 
datanative[datanative$Predictions==datanative$SubjectType,]$Correct<-"CORRECT"
datanative$Correct<-recode(datanative$Correct, pron="predictpron", dp="predictdp",nullsub="predictnullsub")
datanative$Correct<-as.factor(datanative$Correct)

#WRONG PREDICTIONS
corrtab<-table(datanative$Correct, datanative$SubjectType)
corrtab

#DPs
corrtab[1,1]/sum(corrtab[c(1,2,3,4),1]) # TI: 80 correct as in the paper
corrtab[3,1]/sum(corrtab[c(1,2,3,4),1]) # TI: 8 null sub
corrtab[4,1]/sum(corrtab[c(1,2,3,4),1]) # TI: 10 pronouns

#nullsubs
corrtab[1,2]/sum(corrtab[c(1,2,3,4),2]) # TI:60 correct as in the paper
corrtab[2,2]/sum(corrtab[c(1,2,3,4),2]) # TI: 24 Dps
corrtab[4,2]/sum(corrtab[c(1,2,3,4),2]) # TI: 14 pronouns

#pronouns
corrtab[1,3]/sum(corrtab[c(1,2,3,4),3]) # TI: 38 correct as in the paper
corrtab[2,3]/sum(corrtab[c(1,2,3,4),3]) # TI: 32 Dps
corrtab[3,3]/sum(corrtab[c(1,2,3,4),3]) # TI: 28 nullsub



#STEP 2 #TI: separating rows to run 'Predictnativerf' 

datal2.mini1 <- datal2[1:500,]
datal2.mini1$Predictnativerf<-predict(rfpnative, newdata=datal2.mini1)

datal2.mini2 <- datal2[501:1000,]
datal2.mini2$Predictnativerf<-predict(rfpnative, newdata=datal2.mini2)

datal2.mini3 <- datal2[1001:1500,]
datal2.mini3$Predictnativerf<-predict(rfpnative, newdata=datal2.mini3)

datal2.mini4 <- datal2[1501:2000,]
datal2.mini4$Predictnativerf<-predict(rfpnative, newdata=datal2.mini4)

datal2.mini5 <- datal2[2001:2335,]
datal2.mini5$Predictnativerf<-predict(rfpnative, newdata=datal2.mini5)

datal2_new <- bind_rows(list(datal2.mini1, datal2.mini2,datal2.mini3,datal2.mini4,datal2.mini5))
#View(datal2_new)

rm(datal2) #TI: remove old datal2
datal2 <- datal2_new #TI: renaming df
rm(datal2_new) #TI: remove datal2_new

#STEP 3

##add CORRECT + predicted value vector
datal2$ReferenceLikeCat<-as.character(datal2$Predictnativerf) 
datal2[datal2$Predictnativerf==datal2$SubjectType,]$ReferenceLikeCat<-"CORRECT"
datal2$ReferenceLikeCat<-recode(datal2$ReferenceLikeCat, pron="predictpron", dp="predictdp",nullsub="predictnullsub")
datal2$ReferenceLikeCat<-as.factor(datal2$ReferenceLikeCat)

datal2$SubjectType <- factor(datal2$SubjectType, levels = c("nullsub", "dp", "pron"))
datal2$Predictnativerf <- factor(datal2$Predictnativerf, levels = c("nullsub", "dp", "pron"))

# INSPECTION: Predicted vs observed
kocher_l2_pred_actual <- table(datal2$Predictnativerf, datal2$SubjectType) #predictions by L1 model and actual L2 data
#saveRDS(kocher_l2_pred_actual, "1_Reproduction_Kocher/Analysis/Data/Calculations/kocher_l2_pred_actual.rds") #TI: saving as rds to read in thesis document
table(datal2$ReferenceLikeCat, datal2$SubjectType)
# how much overlap (TI: Table 8 in the paper)
predictions_l2_kocher <- round(table(datal2$ReferenceLikeCat)[1] /
    sum(table(datal2$ReferenceLikeCat)) * 100, digits = 1) #TI: assigning name, 69.6% correctly predicted

#saveRDS(predictions_l2_kocher, "1_Reproduction_Kocher/Analysis/Data/Calculations/predictions_l2_kocher.rds") #TI: saving model_accuracy_l2_kocher


#nullsub
table(datal2$Predictnativerf, datal2$SubjectType)[1,1]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,1]) # 75% correct (TI: also in table 8 in the paper)

#dps
table(datal2$Predictnativerf, datal2$SubjectType)[2,2]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,2]) # 77% correct

#pronouns
table(datal2$Predictnativerf, datal2$SubjectType)[3,3]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,3]) # 34% correct

# INSPECTING PRONOUNS

#View(datal2[datal2$ReferenceLikeCat!="CORRECT" & datal2$SubjectType=="pron",]) 
table(datal2[datal2$ReferenceLikeCat!="CORRECT" & datal2$SubjectType=="pron",]$SubjectTypeOld) 
# mostly cases of personal pronouns

#ELF/TI: Returns an empty table as column 'SubjectTypeOld' does not exist!

table(datal2[datal2$SubjectType=="pron",]$SubjectTypeOld, datal2[datal2$SubjectType=="pron",]$Predictnativerf) 
#TI: returns an error

# STEP 4
#RECODE RESPONSEVARIABLE
datal2$ReferenceLike<-as.factor(temp<-ifelse(datal2$Predictnativerf==datal2$SubjectType, "TRUE", "FALSE"))

#MODEL 
rfpl2 <- party::cforest(ReferenceLike ~ ClauseType + InformationStructure + ReferentialContinuity + SubjectType + SubjectPosition + Score, 
                        data = datal2)
weights = ifelse(datal2$ReferenceLikeCat =='CORRECT', 1,
ifelse(datal2$ReferenceLikeCat == "predictdp",5,8) ) #TI: Renaming "ReferenceLikeNew" to ReferenceLikeCat" in weights as there is no "ReferenceLikeNew" and originally it returns an empty value

confusionmatrixrfpl2<-table(predict(rfpl2),datal2$ReferenceLike)
confusionmatrixrfpl2 
sum(diag(confusionmatrixrfpl2))/sum(confusionmatrixrfpl2) #99.5% whereas in the paper 99% are stated!

#saveRDS(confusionmatrixrfpl2, "1_Reproduction_Kocher/Analysis/Data/Calculations/confusionmatrixrfpl2.rds") #TI: saving as rds to read in thesis document


varimp_l2 <- party::varimp(rfpl2) # Advice ELF: Presumably we need to specify that the party package should be used for this function (as opposed to partykit).

#pdf("1_Reproduction_Kocher/Analysis/Figures/varimp_l2.pdf",width=5, height = 5)
#dotplot(sort(varimp_l2), col="gray51", xlab="Variable importance")
#dev.off() 

#png("1_Reproduction_Kocher/Analysis/Figures/varimp_l2.png",width=500, height = 500)
#dotplot(sort(varimp_l2), col="gray51", xlab="Variable importance")
#dev.off() #TI: saving plot as png too

#ELF: Added missing package installation and library loading

#install.packages("usethis") #TI: RStudio wanted me to download the package "usethis" before being able to access "devtools"
library(usethis)

#install.packages("devtools")
#devtools::install_git("https://framagit.org/nicolas-robette/moreparty", force = TRUE) #TI: To download 'moreparty' I needed to download and install Rtools 4.5 from https://cran.r-project.org/bin/windows/Rtools/
library(devtools)
library(moreparty)

#TI: Changing "surro" to "surro_l2"
surro_l2 <- SurrogateTree(rfpl2, maxdepth=4)
Rsquare_surro_kocher <- surro_l2$r.squared #TI: after installing moreparty the R^2 is 0.95 high, whereas in the paper it is 0.94 (p. 23)

saveRDS(Rsquare_surro_kocher, "1_Reproduction_Kocher/Analysis/Data/Calculations/Rsquare_surro_kocher.rds") #TI: 0.94 match, the original study

#FIGURE
#pdf("1_Reproduction_Kocher/Analysis/Figures/surro_l2.pdf", width = 21, height = 10)
#tree<-surro_l2$tree
#plot(tree)
#dev.off()

#png("1_Reproduction_Kocher/Analysis/Figures/surro_l2.png", width = 2000, height = 550) 
#tree<-surro_l2$tree
#plot(tree)
#dev.off() #TI: saving tree also as png




