# MUPDARF ANALYSIS FOR SECTION 5.1-5.2 OF KOCHER 2025

library(partykit)
library(dplyr)
library(stringr)
library(lattice)

#DATA
datanative<-read.csv("datanatives.csv", stringsAsFactors = TRUE)
datal2<-read.csv("Data/datal2s.csv", stringsAsFactors = TRUE)

#DISTRIBUITONS
sd(datal2$DurationTask)
range(datal2$DurationTask)
mean(datal2$DurationTask)


#MUPDARF

#STEP 1
rfpnative<-partykit::cforest(SubjectType~SubjectPosition+InformationStructure+ReferentialContinuity+ClauseType, weights= ifelse(datanative$SubjectType=='dp', 2,ifelse(datanative$SubjectType=="pron",4,1) ), data=datanative)

#FIGURE
pdf("Figures/varimp_native.pdf",width=5, height = 5)
varimp<-varimp.cforest(rfpnative)
dotplot(sort(varimp),  xlab="Variable importance", col="gray51")
dev.off()

#CHECKING ACCURACY
confusionmatrixrfpnative<-table(predict(rfpnative), datanative$SubjectType)
confusionmatrixrfpnative
sum(diag(confusionmatrixrfpnative))/sum(confusionmatrixrfpnative) #64

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
corrtab[1,1]/sum(corrtab[c(1,2,3,4),1]) # 80 correct
corrtab[3,1]/sum(corrtab[c(1,2,3,4),1]) # 8 null sub
corrtab[4,1]/sum(corrtab[c(1,2,3,4),1]) # 10 pronouns

#nullsubs
corrtab[1,2]/sum(corrtab[c(1,2,3,4),2]) # 60 correct
corrtab[2,2]/sum(corrtab[c(1,2,3,4),2]) #  24 Dps
corrtab[4,2]/sum(corrtab[c(1,2,3,4),2]) # 14 pronouns

#pronouns
corrtab[1,3]/sum(corrtab[c(1,2,3,4),3]) # 38 correct
corrtab[2,3]/sum(corrtab[c(1,2,3,4),3]) # 32 Dps
corrtab[3,3]/sum(corrtab[c(1,2,3,4),3]) # 28 nullsub




#STEP 2
datal2$Predictnativerf<-predict(rfpnative, newdata=datal2)

#STEP 3

##add CORRECT + predicted value vector
datal2$ReferenceLikeCat<-as.character(datal2$Predictnativerf) 
datal2[datal2$Predictnativerf==datal2$SubjectType,]$ReferenceLikeCat<-"CORRECT"
datal2$ReferenceLikeCat<-recode(datal2$ReferenceLikeCat, pron="predictpron", dp="predictdp",nullsub="predictnullsub")
datal2$ReferenceLikeCat<-as.factor(datal2$ReferenceLikeCat)

datal2$SubjectType <- factor(datal2$SubjectType, levels = c("nullsub", "dp", "pron"))
datal2$Predictnativerf <- factor(datal2$Predictnativerf, levels = c("nullsub", "dp", "pron"))

# INSPECTION: Predicted vs observed
table(datal2$Predictnativerf, datal2$SubjectType)#differences in l1 and l2
table(datal2$ReferenceLikeCat, datal2$SubjectType)
# how much overlap
table(datal2$ReferenceLikeCat)[1] /sum(table(datal2$ReferenceLikeCat)) # 69% match


#nullsub
table(datal2$Predictnativerf, datal2$SubjectType)[1,1]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,1]) # 75% correct
#dps
table(datal2$Predictnativerf, datal2$SubjectType)[2,2]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,2]) # 77% correct
#pronouns
table(datal2$Predictnativerf, datal2$SubjectType)[3,3]/sum(table(datal2$Predictnativerf, datal2$SubjectType)[,3]) # 34% correct

# INSPECTING PRONOUNS:

View(datal2[datal2$ReferenceLikeCat!="CORRECT" & datal2$SubjectType=="pron",]) 
table(datal2[datal2$ReferenceLikeCat!="CORRECT" & datal2$SubjectType=="pron",]$SubjectTypeOld) # mostly cases of personal pronouns

table(datal2[datal2$SubjectType=="pron",]$SubjectTypeOld, datal2[datal2$SubjectType=="pron",]$Predictnativerf) 

# STEP 4
#RECODE RESPONSEVARIABLE
datal2$ReferenceLike<-as.factor(temp<-ifelse(datal2$Predictnativerf==datal2$SubjectType, "TRUE", "FALSE"))

#MODEL
rfpl2<-party::cforest(ReferenceLike~ClauseType+InformationStructure+ReferentialContinuity+SubjectType+SubjectPosition+Score,  data=datal2)#weights= ifelse(datal2$ReferenceLikeNew=='CORRECT', 1,ifelse(datal2$ReferenceLikeNew=="predictdp",5,8) )
confusionmatrixrfpl2<-table(predict(rfpl2),datal2$ReferenceLike)
confusionmatrixrfpl2
sum(diag(confusionmatrixrfpl2))/sum(confusionmatrixrfpl2)
varimp<-varimp(rfpl2)
pdf("Figures/varimp_l2.pdf",width=5, height = 5)
dotplot(sort(varimp), col="gray51", xlab="Variable importance")
dev.off()

surro<-SurrogateTree(rfpl2, maxdepth=4)
surro$r.squared

#FIGURE
pdf("Figures/surro_l2.pdf", width = 21, height = 10)
tree<-surro$tree
plot(tree)
dev.off()







