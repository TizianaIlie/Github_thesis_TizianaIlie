#DESCRIPTIVE STATISTICS AND DISTRIBUITIONS FOR SECTION 4.2. OF KOCHER 2025

#Running Kocher's scripts using her native data and the extended L1 German dataset!

library(here) #TI: Added

#DATA
datanative<-read.csv(here::here("data", "annotations", "kocher_native_data_annotation.csv"), stringsAsFactors = TRUE)

datal2<-read.csv(here::here("data", "annotations", "joined_germanL1_data_annotation.csv"), stringsAsFactors = TRUE)


# DATA EXPLORATION AND CHISQUARES

chisq.test(table(datanative$SubjectPosition, datanative$Thetarole))
#X-squared = 34.025, df = 1, p-value = 5.441e-09 => significant
#Kocher's original data: X-squared = 34.025, df = 1, p-value = 5.441e-09 => significant
  
#chisq.test(table(datal2$SubjectPosition, datal2$Thetarole))#TI: Thetarole was not annotated in my data (see 1_Preprocessing.qmd)


# SubjectPosition is POST:

sort(table(droplevels(subset(datanative, datanative$SubjectPosition=="POST"))$VERB), decreasing=TRUE) #datanative

sort(table(droplevels(subset(datal2, datal2$SubjectPosition=="POST"))$Verb), decreasing=TRUE) #datal2

sort(table(droplevels(subset(datal2, datal2$SubjectPosition=="POST"))$VerbConstruction), decreasing=TRUE) 
#datal2: When the subject is located posterior to the verb, the verb is mostly used as a simple-finite construction (130x), only 2x as periphrasis-gerund and only once as a modal verb.
#Kocher's original data: simple-finite 130x, periphrasis-gerund 2x, modal 1x

sort(table(droplevels(subset(datanative, datal2$SubjectPosition=="POST"))$VERBCONSTRUCTION), decreasing=TRUE)
#datanative: When the subject is located posterior to the verb, the verb is mostly used as a simple-finite construction (22x), 3x infinitive and only once as periphrasis-gerund and once as a modal verb.
#Kocher's original data: 22x finite+simple, 3x infinitive,  1x periphrasis-gerund

#Concluding remark in reproduction data: In the datal2 subjects after verbs are used more often than in the native data!

table(droplevels(datanative[datanative$SubjectPosition=="POST",])$VERB, droplevels
      
(datanative[datanative$SubjectPosition=="POST",])$SubjectType) #datanative: Posterior subjects are mostly used with 'aparecer' and 'ser 
#The same in Kocher's original data

table(droplevels(datal2[datal2$SubjectPosition=="POST",])$Verb, droplevels(datal2[datal2$SubjectPosition=="POST",])$SubjectType) 
#datal2: Post-verbal subjects are also mostly used with 'venir' and 'ser', but much more frequently than in the native ds
#The same in Kocher's original data

###datal2 speaker's knowledge in additional foreign languages###

table(aggregate(datal2$NSLadditional, by=list(datal2$Textnr), unique)$x) 
#datal2: No:75 Yes: 25 (25 speakers have knowledge in additional foreign languages)
#In Kocher's original data No:64 Yes:18
  
table(datal2$SubjectType,datal2$NSLadditional)[,1]/sum(table(datal2$SubjectType,datal2$NSLadditional)[,1])*100 #Speakers that do not know an additional null subject language
#dp:37.67215 nullsub:46.64594    pron:15.68192  
#In Kocher's original data dp:37.32240   nullsub:46.88525   pron:15.79235

#Concluding remark in reproduction data: similar values

###Does the subject types differ significantly between the speakers who know an additional null subject language, and those who do not?###

table(datal2$SubjectType,datal2$NSLadditional)[,2]/sum(table(datal2$SubjectType,datal2$NSLadditional)[,2])*100 #Speakers that do know an additional null subject language
#dp:36.41536  nullsub:49.35989   pron:14.22475 
#In Kocher's original data dp:36.03960  nullsub:49.90099  pron:14.05941
#Concluding remark in reproduction data: similar values

chisq.test(table(datal2$SubjectType,datal2$NSLadditional))
#X-squared = 1.8025, df = 2, p-value = 0.4061
#In Kocher's original data X-squared = 1.7039, df = 2, p-value = 0.4266

# Conclusion: Also in my data, the distribution of subject types does not differ significantly between the speakers who know an additional null subject language, and those who do not.
