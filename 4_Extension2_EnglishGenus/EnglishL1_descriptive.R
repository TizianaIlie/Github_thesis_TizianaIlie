#DESCRIPTIVE STATISTICS AND DISTRIBUITIONS FOR SECTION 4.2. OF KOCHER 2025

library(here) #Setting working directory

#DATA
datanative<-read.csv(here::here("data", "annotations", "kocher_native_data_annotation.csv"), stringsAsFactors = TRUE)

datal2english <-read.csv(here::here("data", "annotations", "datal2english.csv"), stringsAsFactors = TRUE)


# DATA EXPLORATION AND CHISQUARES

chisq.test(table(datanative$SubjectPosition, datanative$Thetarole))
#X-squared = 34.025, df = 1, p-value = 5.441e-09 => significant

# SubjectPosition is POST:

sort(table(droplevels(subset(datanative, datanative$SubjectPosition=="POST"))$VERB), decreasing=TRUE) #datanative

sort(table(droplevels(subset(datal2english, datal2english$SubjectPosition=="POST"))$Verb), decreasing=TRUE) #datal2english

sort(table(droplevels(subset(datal2english, datal2english$SubjectPosition=="POST"))$VerbConstruction), decreasing=TRUE) 
#datal2english: When the subject is located posterior to the verb, the verb is mostly used as simple-finite (19x)
#Kocher's original data: simple-finite 130, periphrasis-gerund 2, modal 1 

sort(table(droplevels(subset(datanative, datal2english$SubjectPosition=="POST"))$VERBCONSTRUCTION), decreasing=TRUE)
#datanative: When the subject is located posterior to the verb, the verb is mostly used as finite+simple (4x) but also as infinitive or periphrasis-gerund (1x each)
#Kocher's original data: 22x finite+simple, 3x infinitive,  1x periphrasis-lexical

#Concluding remark on reproduction data: Subjects after verbs are used much more frequently in the translated data than in the original
  
table(droplevels(datanative[datanative$SubjectPosition=="POST",])$VERB, droplevels(datanative[datanative$SubjectPosition=="POST",])$SubjectType) 
#datanative: Posterior subjects are mostly used with 'aparecer' and 'ser' (8x each).  
#Same applies to Kocher's original data.

table(droplevels(datal2english[datal2english$SubjectPosition=="POST",])$Verb, droplevels(datal2english[datal2english$SubjectPosition=="POST",])$SubjectType) #datal2english: Posterior subjects are also mostly used with 'pasar' (3x) but also with 'estar' and 'venir' (2x each)
#In Kocher's original data mostly 'venir' and 'ser'

###datal2english speaker's knowledge in additional foreign languages###

table(aggregate(datal2english$NSLadditional, by=list(datal2english$Textnr), unique)$x) 
#datal2english: No:129 vs. Yes:31 (31 speakers have knowledge in additional foreign languages)
#In Kocher's original data No:64 Yes:18
  
table(datal2english$SubjectType,datal2english$NSLadditional)[,1]/sum(table(datal2english$SubjectType,datal2english$NSLadditional)[,1])*100 
#Speakers that do not know an additional NSL
#dp:38.0705394   nullsub:42.3236515      pron:19.5020747 > similar results
#In Kocher's original data dp:37.32240   nullsub:46.88525   pron:15.79235

#Concluding remark in reproduction data: similar values

###Does the subject types differ significantly between the speakers who know an additional NSL and those who do not?###

table(datal2english$SubjectType,datal2english$NSLadditional)[,2]/sum(table(datal2english$SubjectType,datal2english$NSLadditional)[,2])*100 #Speakers that do know an additional null subject language
#dp:32.08723   nullsub:50.46729    pron:17.44548  
#In Kocher's original data dp:36.03960  nullsub:49.90099  pron:14.05941
#Concluding remark in reproduction data: similar values

chisq.test(table(datal2english$SubjectType,datal2english$NSLadditional))
#X-squared = 6.8268, df = 3, p-value = 0.07763 > not significant
#In Kocher's original data X-squared = 1.7039, df = 2, p-value = 0.4266

#Conclusion: The distribution of subject types in the data from L1 English speakers does not differ significantly between those who know an additional null subject language and those who do not. The same is true of Kocher's data on L1 German speakers!
