# MUPDARF ANALYSIS FOR SECTION 5.1-5.2 OF KOCHER 2025
library(partykit) # For representing, summarizing, and visualizing tree-structured regression models
library(dplyr) # For working with data frame like objects
library(stringr) # Wrappers for Common String Operations
library(lattice) # For data visualisation of multivariate data
library(here) #TI: Added

#DATA
#setwd("C:/Dokumente/MASTER/Köln/5_6_Semester/thesis/OSF_Kocher/Analysis")

#datanative<-read.csv("Data/datanatives.csv", stringsAsFactors = TRUE)
datanative<-read.csv(here::here("data", "annotations", "kocher_native_data_annotation.csv"), stringsAsFactors = TRUE)

#datal2<-read.csv("Data/datal2s.csv", stringsAsFactors = TRUE)
datal2<-read.csv(here::here("data", "annotations", "joined_germanL1_data_annotation.csv"), stringsAsFactors = TRUE)


#DISTRIBUITONS (Duration)
sd(datal2$DurationTask) #TI: 253.4517 Kocher did not mentioned the number in the paper
range(datal2$DurationTask) #TI: 2-2700 Kocher did not mentioned the number in the paper
mean(datal2$DurationTask) #TI: 59.06263, in the paper 68.27338

#MUPDARF

#TI: MUPDARF stands for 'Multifactorial Prediction and Deviation Analysis Using Regression/Random Forests'. This approach attempts to improve upon traditional regression- or tree-based methods by firstly training a model on reference speakers (often native speakers in learner corpus studies or British English speakers in variety studies) and then using this model to predict what the reference speaker would produce in the target speaker's situation (often non-native or indigenized variety speakers). The third step is to determine whether the target speakers made a canonical choice and explore that variability using a second regression model or classifier (Gries et al., 2020).

#STEP 1
rfpnative <- partykit::cforest(SubjectType ~ SubjectPosition+ InformationStructure + ReferentialContinuity + ClauseType, 
                               weights = ifelse(datanative$SubjectType =='dp', 2, 
                                                ifelse(datanative$SubjectType == "pron",4,1) ), #TI: Kocher added weights as the model may focus primarily on predicting the majority class, here null subjects.
                               data = datanative) 


#FIGURE (Proficiency:Score)
#pdf("2_Robustness_Check/Figures/varimp_native.pdf",width=5, height = 5)
#varimp<-varimp.cforest(rfpnative) #TI: variable importance
#dotplot(sort(varimp),  xlab="Variable importance", col="gray51") #TI: plot is in paper
#dev.off()

#CHECKING ACCURACY
confusionmatrixrfpnative<-table(predict(rfpnative), datanative$SubjectType)
confusionmatrixrfpnative
sum(diag(confusionmatrixrfpnative))/sum(confusionmatrixrfpnative) #TI: moderate accuracy of 64%, like in the paper

#INSPECTING PREDICTIONS
datanative$Predictions<-predict(rfpnative) #TI: predicts a value of SubjectType for each observation in datanative
datanative$Correct<-as.character(datanative$Predictions) #TI: variable called 'Correct' is created and initially contains the predicted class labels
datanative[datanative$Predictions==datanative$SubjectType,]$Correct<-"CORRECT" #TI: code compares the predicted value with the true value. When they match, 'Correct' is replaced by 'CORRECT'.
datanative$Correct<-recode(datanative$Correct, pron="predictpron", dp="predictdp",nullsub="predictnullsub") #TI: incorrectly classified observations are renamed to make explicit what the model actually predicted
datanative$Correct<-as.factor(datanative$Correct)

#WRONG PREDICTIONS
corrtab<-table(datanative$Correct, datanative$SubjectType)
corrtab #different type of visualisation of the confusionmatrix

#DPs
corrtab[1,1]/sum(corrtab[c(1,2,3,4),1]) # TI: 80 correct as in the paper
corrtab[3,1]/sum(corrtab[c(1,2,3,4),1]) # TI: 8 null sub
corrtab[4,1]/sum(corrtab[c(1,2,3,4),1]) # TI: 10 pronouns

#nullsubs
corrtab[1,2]/sum(corrtab[c(1,2,3,4),2]) # TI: 60 correct as in the paper
corrtab[2,2]/sum(corrtab[c(1,2,3,4),2]) # TI: 24 Dps
corrtab[4,2]/sum(corrtab[c(1,2,3,4),2]) # TI: 14 pronouns

#pronouns
corrtab[1,3]/sum(corrtab[c(1,2,3,4),3]) # TI: 38 correct as in the paper
corrtab[2,3]/sum(corrtab[c(1,2,3,4),3]) # TI: 32 Dps
corrtab[3,3]/sum(corrtab[c(1,2,3,4),3]) # TI: 28 nullsub


#STEP 2 #TI: separating rows to run 'Predictnativerf' on the L2German data

datal2.mini1 <- datal2[1:500,]
datal2.mini1$Predictnativerf<-predict(rfpnative, newdata=datal2.mini1)

datal2.mini2 <- datal2[501:1000,]
datal2.mini2$Predictnativerf<-predict(rfpnative, newdata=datal2.mini2)

datal2.mini3 <- datal2[1001:1500,]
datal2.mini3$Predictnativerf<-predict(rfpnative, newdata=datal2.mini3)

datal2.mini4 <- datal2[1501:2000,]
datal2.mini4$Predictnativerf<-predict(rfpnative, newdata=datal2.mini4)

datal2.mini5 <- datal2[2001:2384,]
datal2.mini5$Predictnativerf<-predict(rfpnative, newdata=datal2.mini5)

datal2_new <- bind_rows(list(datal2.mini1, datal2.mini2,datal2.mini3,datal2.mini4,datal2.mini5))
#View(datal2_new)

rm(datal2) #TI: remove old datal2
datal2 <- datal2_new #TI: renaming df
rm(datal2_new) #TI: remove datal2_new

#STEP 3
#TI: This step creates a new categorical variable that describes whether the prediction from the random forest matches the observed 'SubjectType', and if not, which category was predicted.

##add CORRECT + predicted value vector
datal2$ReferenceLikeCat<-as.character(datal2$Predictnativerf) #TI:'ReferenceLikeCat' is created and initially contains the values of 'Predictnativerf'
datal2[datal2$Predictnativerf==datal2$SubjectType,]$ReferenceLikeCat<-"CORRECT" #TI: markes correctly predicted cases
datal2$ReferenceLikeCat<-recode(datal2$ReferenceLikeCat, pron="predictpron", dp="predictdp",nullsub="predictnullsub") #renaming incorrect cases (see above)
datal2$ReferenceLikeCat<-as.factor(datal2$ReferenceLikeCat)

datal2$SubjectType <- factor(datal2$SubjectType, 
                             levels = c("nullsub", "dp", "pron")) #TI: sets the order of factor levels

datal2$Predictnativerf <- factor(datal2$Predictnativerf, 
                                 levels = c("nullsub", "dp", "pron"))


# INSPECTION: Predicted vs observed

extended_l2german_pred_actual <- table(datal2$Predictnativerf, datal2$SubjectType) #predictions by L1 model and actual L2 data
#saveRDS(extended_l2german_pred_actual, "2_Robustness_Check/Calculations/kocher_l2_pred_actual.rds") #TI: saving as rds to read in thesis document

table(datal2$ReferenceLikeCat, datal2$SubjectType)
# how much overlap (TI: Table 8 in the paper)
table(datal2$ReferenceLikeCat)[1] /sum(table(datal2$ReferenceLikeCat)) # TI: (~69.5% in the paper) here 69.63% total correct agreement between the native-speaker model and the L2 data.

predictions_germanl2 <- round(table(datal2$ReferenceLikeCat)[1] /sum(table(datal2$ReferenceLikeCat)) * 100, digits = 1) #TI: assigning name and rounding for thesis.qmd
#saveRDS(predictions_germanl2, "2_Robustness_Check/Calculations/predictions_germanl2.rds") #TI: saving model_accuracy_l2_kocher for thesis.qmd

#nullsub
table(datal2$Predictnativerf, datal2$SubjectType)[1,1]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,1]) # TI: 75.1%; Kocher: 75% correct (TI: also in table 8 in the paper)

#dps
table(datal2$Predictnativerf, datal2$SubjectType)[2,2]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,2]) # TI: 77.3%; Kocher: 77% correct

#pronouns
table(datal2$Predictnativerf, datal2$SubjectType)[3,3]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,3]) # TI: 33.3%; Kocher: 34% correct

# TI Conclusion remarks: L2 speakers use pronouns substantially differently to the patterns learned from native speakers. By contrast, null subjects and DPs show much stronger alignment with the native speaker model.


# INSPECTING PRONOUNS

#View(datal2[datal2$ReferenceLikeCat!="CORRECT" & datal2$SubjectType=="pron",]) 

#table(datal2[datal2$ReferenceLikeCat!="CORRECT" & datal2$SubjectType=="pron",]$SubjectTypeOld) 
# mostly cases of personal pronouns

#ELF/TI: Returns an empty table as column 'SubjectTypeOld' does not exist!

#table(datal2[datal2$SubjectType=="pron",]$SubjectTypeOld, datal2[datal2$SubjectType=="pron",]$Predictnativerf) 
#TI: returns an error


# STEP 4

#RECODE RESPONSEVARIABLE
datal2$ReferenceLike<-as.factor(temp<-ifelse(datal2$Predictnativerf==datal2$SubjectType, "TRUE", "FALSE")) #TI: creating binary 'ReferenceLike' variable

#MODEL 
#rfpl2 <- party::cforest(ReferenceLike ~ ClauseType + InformationStructure + #ReferentialContinuity + SubjectType + SubjectPosition + Score, 
#                        data = datal2)
#                       weights = ifelse(datal2$ReferenceLikeNew =='CORRECT', 1,
#                                       ifelse(datal2$ReferenceLikeNew == "predictdp",5,8) )
#                        #TI: Returns an empty value because there is no "ReferenceLikeNew" 
#TI: Alternative model without 'ReferenceLikeNew' but 'ReferenceLike' instead

# TI: In the original study it is not explain why these weights are chosen (1,5,8)) and I am not sure whether changing 'ReferenceLikeNew' to 'ReferenceLikeCat' to make the code run and to include the weight distribution to the analysis. 
  
  
rfpl2 <- party::cforest(ReferenceLike ~ ClauseType + InformationStructure + ReferentialContinuity + SubjectType + SubjectPosition + Score, 
                        data = datal2)
      weights = ifelse(datal2$ReferenceLikeCat =='CORRECT', 1,
                 ifelse(datal2$ReferenceLikeCat == "predictdp",5,8))

##CHECKING ACCURACY
confusionmatrixrfpl2<-table(predict(rfpl2),datal2$ReferenceLike)
confusionmatrixrfpl2 #TI: only 11 observations are classified incorrectly
#saveRDS(confusionmatrixrfpl2, "2_Robustness_Check/Calculations/confusionmatrixrfp_extended.rds") #TI: saving as rds to read in thesis document
sum(diag(confusionmatrixrfpl2))/sum(confusionmatrixrfpl2) #99.5% as mentioned in the paper

varimp_l2 <- party::varimp(rfpl2) # Advice ELF: Presumably we need to specify that the party package should be used for this function (as opposed to partykit).
#pdf("2_Robustness_Check/Figures/varimp_l2.pdf",width=5, height = 5)
#dotplot(sort(varimp_l2), col="gray51", xlab="Variable importance")
#dev.off() 

#png("2_Robustness_Check/Figures/varimp_l2.png",width=500, height = 500)
#dotplot(sort(varimp_l2), col="gray51", xlab="Variable importance")
#dev.off() 

#TI: The distribution is highly right-skewed, as can be seen in the 'varimp_native.pdf' figure below, and contains some values that are much larger than the mean.


#ELF: Added missing package installation and library loading

#install.packages("usethis") #TI: RStudio wanted me to download the package "usethis" before being able to access "devtools"
library(usethis)

#install.packages("devtools")
#devtools::install_git("https://framagit.org/nicolas-robette/moreparty", force = TRUE) #TI: To download 'moreparty' I needed to download and install Rtools 4.5 from https://cran.r-project.org/bin/windows/Rtools/
library(devtools)
library(moreparty)

#TI: Creating the surrogate tree
surro <- SurrogateTree(rfpl2, maxdepth=4)
Rsquare_surro_extended <- surro$r.squared #TI: after installing moreparty the R^2 is 0.945 high, like in the paper (p. 23)
saveRDS(Rsquare_surro_extended, "2_Robustness_Check/Calculations/Rsquare_surro_extended.rds") #TI: saving as rds to read in thesis document

#FIGURE
#pdf("2_Robustness_Check/Figures/surro.pdf", width = 21, height = 10) 
#tree<-surro$tree
#plot(tree)
#dev.off()

#png("2_Robustness_Check/Figures/surro.png", width = 2000, height = 500) 
#tree<-surro$tree
#plot(tree)
#dev.off()
