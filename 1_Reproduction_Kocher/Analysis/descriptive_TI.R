#DESCRIPTIVE STATISTICS AND DISTRIBUITIONS FOR SECTION 4.2. OF KOCHER 2025

library(here) #TI: Added

#DATA
#datanative<-read.csv("Data/datanatives.csv", stringsAsFactors = TRUE)
datanative<-read.csv(here("1_Reproduction_Kocher", "Analysis", "Data", "datanatives.csv"), stringsAsFactors = TRUE)

#datal2<-read.csv("Data/datal2s.csv", stringsAsFactors = TRUE)
datal2<-read.csv(here("1_Reproduction_Kocher", "Analysis", "Data", "datal2s.csv"), stringsAsFactors = TRUE)

# DATA EXPLORATION AND CHISQUARES

chisq.test(table(datanative$SubjectPosition, datanative$Thetarole))# significant
chisq.test(table(datal2$SubjectPosition, datal2$Thetarole))# significant

sort(table(droplevels(subset(datanative, datanative$SubjectPosition=="POST"))$VERB), decreasing=TRUE)
sort(table(droplevels(subset(datal2, datal2$SubjectPosition=="POST"))$Verb), decreasing=TRUE)
sort(table(droplevels(subset(datal2, datal2$SubjectPosition=="POST"))$VerbConstruction), decreasing=TRUE)
sort(table(droplevels(subset(datanative, datal2$SubjectPosition=="POST"))$VERBCONSTRUCTION), decreasing=TRUE)

table(droplevels(datanative[datanative$SubjectPosition=="POST",])$VERB, droplevels(datanative[datanative$SubjectPosition=="POST",])$SubjectType)

table(droplevels(datal2[datal2$SubjectPosition=="POST",])$Verb, droplevels(datal2[datal2$SubjectPosition=="POST",])$SubjectType)

table(aggregate(datal2$NSLadditional, by=list(datal2$Textnr), unique)$x)
table(datal2$SubjectType,datal2$NSLadditional)[,1]/sum(table(datal2$SubjectType,datal2$NSLadditional)[,1])*100
table(datal2$SubjectType,datal2$NSLadditional)[,2]/sum(table(datal2$SubjectType,datal2$NSLadditional)[,2])*100
chisq.test(table(datal2$SubjectType,datal2$NSLadditional))

