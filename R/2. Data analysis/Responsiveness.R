##In order to report the responsiveness the data set had to be reshaped

##Creating a dataframe for each time unit the questionnaire was completed

PRWHE.t0 <- subset(PRWHEdataNoMissingDataTAB, PRWHEdataNoMissingDataTAB$rounddescription == "Intake")
PRWHE.t1 <- subset(PRWHEdataNoMissingDataTAB, PRWHEdataNoMissingDataTAB$rounddescription == "6 weken")
PRWHE.t2 <- subset(PRWHEdataNoMissingDataTAB, PRWHEdataNoMissingDataTAB$rounddescription == "3 maanden")
PRWHE.t3 <- subset(PRWHEdataNoMissingDataTAB, PRWHEdataNoMissingDataTAB$rounddescription == "6 maanden")
PRWHE.t4 <- subset(PRWHEdataNoMissingDataTAB, PRWHEdataNoMissingDataTAB$rounddescription == "12 maanden")

PRWHE.t0 <- PRWHE.t0[,c(-40:-43)]
PRWHE.t1 <- PRWHE.t1[,c(-3:-6,-31:-34,-36:-52)]
PRWHE.t2 <- PRWHE.t2[,c(-3:-6,-31:-34,-36:-52)]
PRWHE.t3 <- PRWHE.t3[,c(-3:-6,-31:-34,-36:-52)]
PRWHE.t4 <- PRWHE.t4[,c(-3:-6,-31:-34,-36:-52)]

for(i in 1:length(PRWHE.t1)){
  if(colnames(PRWHE.t1)[i] == "V30"){next}
  colnames(PRWHE.t1)[i] <- paste(colnames(PRWHE.t1[i]),"T1", sep = ".") 
}
for(i in 1:length(PRWHE.t2)){
  if(colnames(PRWHE.t2)[i] == "V30"){next}
  colnames(PRWHE.t2)[i] <- paste(colnames(PRWHE.t2[i]),"T2", sep = ".") 
}
for(i in 1:length(PRWHE.t3)){
  if(colnames(PRWHE.t3)[i] == "V30"){next}
  colnames(PRWHE.t3)[i] <- paste(colnames(PRWHE.t3[i]),"T3", sep = ".") 
}
for(i in 1:length(PRWHE.t4)){
  if(colnames(PRWHE.t4)[i] == "V30"){next}
  colnames(PRWHE.t4)[i] <- paste(colnames(PRWHE.t4[i]),"T4", sep = ".") 
}

PRWHE.tijd <- join(PRWHE.t0,PRWHE.t1, by="V30")
PRWHE.tijd <- join(PRWHE.tijd,PRWHE.t2, by="V30")
PRWHE.tijd <- join(PRWHE.tijd,PRWHE.t3, by="V30")
PRWHE.tijd <- join(PRWHE.tijd,PRWHE.t4, by="V30")

res.tijd <- join(PRWHE.t0, PRWHE.t2, by="V30")
prediction.totaal <- prediction.functie + prediction.pijn

##From this point until line 101  the code is the same as with optimization of the CHAID algorithm.
##At line 101 the predicted scores between two time units are compared to analyse the responsiveness
##At line 176 the responsiveness for the disability subscore is calculated
##responsiveness.pijn
PRWHE.res.2 <- join(PRWHE.t0, PRWHE.t2, by="V30")
PRWHE.res.2 <- PRWHE.res.2[complete.cases(PRWHE.res.2),]

chaid_control <- chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
                               minsplit = 30,
                               minbucket = 10, minprob = 0.05, stump = FALSE,
                               maxheight = 3)

##t0
#factoren voor CHAID

ps <- factor(PRWHE.res.2$V9)
pv1 <- factor(PRWHE.res.2$V11)
pv2 <- factor(PRWHE.res.2$V12)
pv3 <- factor(PRWHE.res.2$V13)
pv4 <- factor(PRWHE.res.2$V14)
pv5 <- factor(PRWHE.res.2$V15)

#runnen van algoritme
current.chaid.run <- chaid(ps ~ pv1+pv2+pv3+pv4+pv5,
                           data= PRWHE.res.2 ,
                           control= chaid_control)

#verwachtte score op basis van CHAID
current.predict <- predict(current.chaid.run, newdata = PRWHE.res.2)
#-1, want factor begint op 1 ipv 0
current.prediction <- as.numeric(current.predict)-1

prediction.T0 <- current.prediction



##T2
#factoren voor CHAID

ps <- factor(PRWHE.res.2$V9.T2)
pv1 <- factor(PRWHE.res.2$V11.T2)
pv2 <- factor(PRWHE.res.2$V12.T2)
pv3 <- factor(PRWHE.res.2$V13.T2)
pv4 <- factor(PRWHE.res.2$V14.T2)
pv5 <- factor(PRWHE.res.2$V15.T2)

#runnen van algoritme
current.chaid.run <- chaid(ps ~ pv1+pv2+pv3+pv4+pv5,
                           data= PRWHE.res.2 ,
                           control= chaid_control)

#verwachtte score op basis van CHAID
current.predict <- predict(current.chaid.run, newdata = PRWHE.res.2)
#-1, want factor begint op 1 ipv 0
current.prediction <- as.numeric(current.predict)-1

prediction.T2 <- current.prediction

##SRM.pijn!

delta.score.dt <- prediction.T0 - prediction.T2
delta.score.dt.improved <- delta.score.dt
srm.dt <- mean(delta.score.dt.improved)/sd(delta.score.dt)

delta.score.origineel <- PRWHE.res.2$V9 - PRWHE.res.2$V9.T2
delta.score.origineel.improved <- delta.score.origineel
srm.origineel <- mean(delta.score.origineel.improved)/sd(delta.score.origineel)

##responsiveness.functie
PRWHE.res.2 <- join(PRWHE.t0, PRWHE.t2, by="V30")
PRWHE.res.2 <- PRWHE.res.2[complete.cases(PRWHE.res.2),]

chaid_control <- chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
                               minsplit = 30,
                               minbucket = 10, minprob = 0.05, stump = FALSE,
                               maxheight = 3)

##t0
#factoren voor CHAID

fs <- factor(PRWHE.res.2$V10)
fv1 <- factor(PRWHE.res.2$V16)
fv2 <- factor(PRWHE.res.2$V17)
fv3 <- factor(PRWHE.res.2$V18)
fv4 <- factor(PRWHE.res.2$V19)
fv5 <- factor(PRWHE.res.2$V20)
fv6 <- factor(PRWHE.res.2$V21)
fv7 <- factor(PRWHE.res.2$V22)
fv8 <- factor(PRWHE.res.2$V23)
fv9 <- factor(PRWHE.res.2$V24)
fv10 <- factor(PRWHE.res.2$V25)

#runnen van algoritme
current.chaid.run <- chaid(fs ~ fv1+fv2+fv3+fv4+fv5+fv6+fv7+fv8+fv9+fv10,
                           data= PRWHE.res.2 ,
                           control= chaid_control)

#verwachtte score op basis van CHAID
current.predict <- predict(current.chaid.run, newdata = PRWHE.res.2)
#-1, want factor begint op 1 ipv 0
current.prediction <- as.numeric(current.predict)-1

prediction.functie.T0 <- current.prediction



##T2
#factoren voor CHAID

fs <- factor(PRWHE.res.2$V10.T2)
fv1 <- factor(PRWHE.res.2$V16.T2)
fv2 <- factor(PRWHE.res.2$V17.T2)
fv3 <- factor(PRWHE.res.2$V18.T2)
fv4 <- factor(PRWHE.res.2$V19.T2)
fv5 <- factor(PRWHE.res.2$V20.T2)
fv6 <- factor(PRWHE.res.2$V21.T2)
fv7 <- factor(PRWHE.res.2$V22.T2)
fv8 <- factor(PRWHE.res.2$V23.T2)
fv9 <- factor(PRWHE.res.2$V24.T2)
fv10 <- factor(PRWHE.res.2$V25.T2)

#runnen van algoritme
current.chaid.run <- chaid(fs ~ fv1+fv2+fv3+fv4+fv5+fv6+fv7+fv8+fv9+fv10,
                           data= PRWHE.res.2 ,
                           control= chaid_control)

#verwachtte score op basis van CHAID
current.predict <- predict(current.chaid.run, newdata = PRWHE.res.2)
#-1, want factor begint op 1 ipv 0
current.prediction <- as.numeric(current.predict)-1

prediction.functie.T2 <- current.prediction

##SRM.pijn!

delta.score.functie.dt <- prediction.functie.T0 - prediction.functie.T2
delta.score.functie.dt.improved <- delta.score.functie.dt
srm.functie.dt <- mean(delta.score.functie.dt.improved)/sd(delta.score.functie.dt.improved)

delta.score.functie.origineel <- PRWHE.res.2$V10 - PRWHE.res.2$V10.T2
delta.score.functie.origineel.improved <- delta.score.functie.origineel
srm.functie.origineel <- mean(delta.score.functie.origineel.improved)/sd(delta.score.functie.origineel.improved)

