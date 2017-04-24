##IN the original dataset no VAS scores were collected
## therefore, an additional data frama was created to validate the predicted decision tree PRWE score with a VAS score

##Creating place-holders

VAS1 <- rep(NA,2599)
VAS2 <- rep(NA,2599)
VAS3 <- rep(NA,2599)
VAS4 <- rep(NA,2599)
VAS5 <- rep(NA,2599)

VASmatchKort <- rep(NA,2599)
VAsmatchLang <- rep(NA,2599)

VAS.pijn <- rep(NA,2599)


##Creating the new data frame
PRWHE.VAS <- cbind(PRWHEtest$V33, as.character(PRWHEtest$rounddescription) ,PRWHEtest$V8, PRWHEtest$V9, PRWHEtest$V10, prediction.pijn, prediction.functie, VAS1, VAS2, VAS3, VAS4,VAS5, VASmatchKort, VAsmatchLang)

##Our data was devided between "Pols Kort" and "Pols Lang", therefore we needed to find a match in either of these data frames

for (i in 1:2599){
  
  PRWHE.VAS[i,13] <- match(paste(PRWHE.VAS[i,1],PRWHE.VAS[i,2]),
                           paste(VAS.pols.kort$X.D..Patiënt.traject.ID,VAS.pols.kort$X.D..rounddescription), nomatch=0)
  
}


for (i in 1:2599){
  
  PRWHE.VAS[i,14] <- match(paste(PRWHE.VAS[i,1],PRWHE.VAS[i,2]),
                           paste(VAS.pols.lang$X.D..Patiënt.traject.ID,VAS.pols.lang$X.D..rounddescription), nomatch=0)
  
}


##Once a match has been identified, the corresponding data was inserted in the new dataframe

for(i in 1:2599){
  
  if (PRWHE.VAS[i,13] == 0){
    
    next
    
  } else {
    
    PRWHE.VAS[i,8] <- VAS.pols.kort$X.A..Hoeveel.pijn.heeft.u.gemiddeld.gehad.in.de.afgelopen.week.....geen.pijn.hevigst.denkbare.pijn[as.integer(PRWHE.VAS[i,13])]
    
    PRWHE.VAS[i,9] <- VAS.pols.kort$X.A..Hoeveel.pijn.heeft.u.de.afgelopen.week.gehad.in.rust...N.B..Het.betreft.dus.niet.de.pijn.die.u.had.vlak.na.de..operatieve..behandeling......geen.pijn.hevigst.denkbare.pijn[as.integer(PRWHE.VAS[i,13])]
    
    PRWHE.VAS[i,10] <- VAS.pols.kort$X.A..Hoeveel.pijn.heeft.u.de.afgelopen.week.gehad.tijdens.belasten.....geen.pijn.hevigst.denkbare.pijn[as.integer(PRWHE.VAS[i,13])]
    
    PRWHE.VAS[i,11] <- VAS.pols.kort$X.A..Hoe.heeft.u.in.de.afgelopen.week.de.functie.van.uw.hand.ervaren.....Geen.functie.Volledige.functie[as.integer(PRWHE.VAS[i,13])]
    
    PRWHE.VAS[i,12] <- VAS.pols.kort$X.A..Hoe.tevreden.bent.u.op.dit.moment.met.uw.hand.....Geheel.ontevreden.Geheel.tevreden[as.integer(PRWHE.VAS[i,13])]
  }
}


for(i in 1:2599){
  
  if (PRWHE.VAS[i,14] == 0){
    
    next
    
  } else {
    
    PRWHE.VAS[i,8] <- VAS.pols.lang$X.A..Hoeveel.pijn.heeft.u.gemiddeld.gehad.in.de.afgelopen.week.....geen.pijn.hevigst.denkbare.pijn[as.integer(PRWHE.VAS[i,14])]
    
    PRWHE.VAS[i,9] <- VAS.pols.lang$X.A..Hoeveel.pijn.heeft.u.de.afgelopen.week.gehad.in.rust...N.B..Het.betreft.dus.niet.de.pijn.die.u.had.vlak.na.de..operatieve..behandeling......geen.pijn.hevigst.denkbare.pijn[as.integer(PRWHE.VAS[i,14])]
    
    PRWHE.VAS[i,10] <- VAS.pols.lang$X.A..Hoeveel.pijn.heeft.u.de.afgelopen.week.gehad.tijdens.belasten.....geen.pijn.hevigst.denkbare.pijn[as.integer(PRWHE.VAS[i,14])]
    
    PRWHE.VAS[i,11] <- VAS.pols.lang$X.A..Hoe.heeft.u.in.de.afgelopen.week.de.functie.van.uw.hand.ervaren.....Geen.functie.Volledige.functie[as.integer(PRWHE.VAS[i,14])]
    
    PRWHE.VAS[i,12] <- VAS.pols.lang$X.A..Hoe.tevreden.bent.u.op.dit.moment.met.uw.hand.....Geheel.ontevreden.Geheel.tevreden[as.integer(PRWHE.VAS[i,14])]
  }
}

PRWHE.VAS <- PRWHE.VAS[,-15]

##There where 3 different pain VAS scores, we also calculated an average VAS pain score

for(i in 1:2599){
  
  VAS.pijn[i] <- round(((as.numeric(PRWHE.VAS[i,8]))+(as.numeric(PRWHE.VAS[i,9]))+(as.numeric(PRWHE.VAS[i,10])))/3, digits = 0)
  
}

PRWHE.VAS <- cbind(PRWHE.VAS, VAS.pijn)

##THese functions calculated correlatoins between the original PRWE and VAS scors and also between the decision tree PRWE and VAS scores.

cor(as.numeric(PRWHE.VAS[,6]), as.numeric(PRWHE.VAS[,15]), use = "complete.obs", method = c("pearson"))
cor(as.numeric(PRWHE.VAS[,4]), as.numeric(PRWHE.VAS[,15]), use = "complete.obs", method = c("pearson"))

cor(as.numeric(PRWHE.VAS[,6]), as.numeric(PRWHE.VAS[,8]), use = "complete.obs", method = c("pearson"))
cor(as.numeric(PRWHE.VAS[,4]), as.numeric(PRWHE.VAS[,8]), use = "complete.obs", method = c("pearson"))

cor(as.numeric(PRWHE.VAS[,6]), as.numeric(PRWHE.VAS[,9]), use = "complete.obs", method = c("pearson"))
cor(as.numeric(PRWHE.VAS[,4]), as.numeric(PRWHE.VAS[,9]), use = "complete.obs", method = c("pearson"))

cor(as.numeric(PRWHE.VAS[,6]), as.numeric(PRWHE.VAS[,10]), use = "complete.obs", method = c("pearson"))
cor(as.numeric(PRWHE.VAS[,4]), as.numeric(PRWHE.VAS[,10]), use = "complete.obs", method = c("pearson"))


cor(as.numeric(PRWHE.VAS[,7]), as.numeric(PRWHE.VAS[,11]), use = "complete.obs", method = c("pearson"))
cor(as.numeric(PRWHE.VAS[,5]), as.numeric(PRWHE.VAS[,11]), use = "complete.obs", method = c("pearson"))

plot(as.numeric(PRWHE.VAS[,6]), as.numeric(PRWHE.VAS[,15]))


