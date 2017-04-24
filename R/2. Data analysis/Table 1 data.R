##Following code was used to merge different data sets to obtain the information provided in TABLE 1 in the article

behandelde.hand <- as.factor(rep(NA, 10394))
dominante.hand <- as.factor(rep(NA, 10394))
behandeling <- as.factor(rep(NA, 10394))
type.behandeling <- as.factor(rep(NA, 10394))
lengte.klachten <- as.factor(rep(NA, 10394))
zwaarte.beroep <- as.factor(rep(NA, 10394))

### data voor tabel 1
for(i in 1:10934){
  
  PRWHEdataNoMissingDataTAB$matchkort[i] <- match(PRWHEdataNoMissingDataTAB$V33[i], intake.pols.kort$Patiënt.traject.ID, nomatch=0)
  
  if (PRWHEdataNoMissingDataTAB$matchkort[i] == 0){
    next
  } else {
    
    PRWHEdataNoMissingDataTAB$behandelde.hand[i] <- intake.pols.kort$Welke.hand.wordt.behandeld..Selecteer.de.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchkort[i]]
    PRWHEdataNoMissingDataTAB$dominante.hand[i] <- intake.pols.kort$Wat.is.de.dominate.hand.[PRWHEdataNoMissingDataTAB$matchkort[i]]
    PRWHEdataNoMissingDataTAB$behandeling[i] <- intake.pols.kort$Wat.wordt.de.behandeling.[PRWHEdataNoMissingDataTAB$matchkort[i]]
    PRWHEdataNoMissingDataTAB$type.behandeling[i] <- intake.pols.kort$Type.behandeling[PRWHEdataNoMissingDataTAB$matchkort[i]]
    PRWHEdataNoMissingDataTAB$lengte.klachten[i] <- intake.pols.kort$Hoeveel.maanden.bestaat.de.klacht.al.[PRWHEdataNoMissingDataTAB$matchkort[i]]
    PRWHEdataNoMissingDataTAB$zwaarte.beroep[i] <- intake.pols.kort$Wat.is.de.zwaarte.van.het.beroep.van.de.patiënt..Selecteer.de.meest.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchkort[i]]
  }
}


for(i in 1:10934){
  
  PRWHEdataNoMissingDataTAB$matchlang[i] <- match(PRWHEdataNoMissingDataTAB$V33[i], intake.pols.lang$X.D..Patiënt.traject.ID, nomatch=0)
  
  if (PRWHEdataNoMissingDataTAB$matchlang[i] == 0){
    next
  } else {
    
    PRWHEdataNoMissingDataTAB$behandelde.hand[i] <- intake.pols.lang$X.A..Welke.hand.wordt.behandeld..Selecteer.de.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchlang[i]]
    PRWHEdataNoMissingDataTAB$dominante.hand[i] <- intake.pols.lang$X.A...Wat.is.de.dominate.hand.[PRWHEdataNoMissingDataTAB$matchlang[i]]
    PRWHEdataNoMissingDataTAB$behandeling[i] <- intake.pols.lang$X.A..Wat.wordt.de.behandeling.[PRWHEdataNoMissingDataTAB$matchlang[i]]
    PRWHEdataNoMissingDataTAB$type.behandeling[i] <- intake.pols.lang$X.TF..Type.behandeling[PRWHEdataNoMissingDataTAB$matchlang[i]]
    PRWHEdataNoMissingDataTAB$lengte.klachten[i] <- intake.pols.lang$X.A..Hoeveel.maanden.bestaat.de.klacht.al.[PRWHEdataNoMissingDataTAB$matchlang[i]]
    PRWHEdataNoMissingDataTAB$zwaarte.beroep[i] <- intake.pols.lang$X.A..Wat.is.de.zwaarte.van.het.beroep.van.de.patiënt..Selecteer.de.meest.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchlang[i]]
  }
}

plot(as.factor(PRWHEdataNoMissingDataTAB$behandeling))

summary(PRWHEdataNoMissingDataTAB)

PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 1] <- "overig"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 2] <- "carpal boss"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 4] <- "correctie osteotomie radius"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 5] <- "GCD"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 6] <- "GCV"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 7] <- "MCI polsprogramma"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 8] <- "LCTH"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 9] <- "overig"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 10] <- "pols artroscopie"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 11] <- "pols overig"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 12] <- "release 1e extensorloge"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 13] <- "Tendinitis of tendovaginitis pols"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 14] <- "VOSM"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 15] <- "PRC"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 16] <- "reinsertie 6e extensorlorge"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 17] <- "rsl arthrodese"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 18] <- "scaphoid osteosynthese"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 19] <- "tenoraphie extensoren pols"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 20] <- "TFCC reinsertie"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 21] <- "ulna verkorting"
PRWHEdataNoMissingDataTAB$behandeling[PRWHEdataNoMissingDataTAB$behandeling == 22] <- "ulnakopprothese"


PRWHEdataNoMissingDataTAB$dominante.hand[PRWHEdataNoMissingDataTAB$dominante.hand == 1] <- "beide"
PRWHEdataNoMissingDataTAB$dominante.hand[PRWHEdataNoMissingDataTAB$dominante.hand == 2] <- "links"
PRWHEdataNoMissingDataTAB$dominante.hand[PRWHEdataNoMissingDataTAB$dominante.hand == 3] <- "rechts"
PRWHEdataNoMissingDataTAB$dominante.hand[PRWHEdataNoMissingDataTAB$dominante.hand == 4] <- "rechts"

PRWHEdataNoMissingDataTAB$type.behandeling[PRWHEdataNoMissingDataTAB$type.behandeling == 1] <- "chirurgisch"
PRWHEdataNoMissingDataTAB$type.behandeling[PRWHEdataNoMissingDataTAB$type.behandeling == 2] <- "conservatief"
PRWHEdataNoMissingDataTAB$type.behandeling[PRWHEdataNoMissingDataTAB$type.behandeling == 3] <- "conservatief"

PRWHEdataNoMissingDataTAB$zwaarte.beroep[PRWHEdataNoMissingDataTAB$zwaarte.beroep == 1] <- "geen betaalde arbeid"
PRWHEdataNoMissingDataTAB$zwaarte.beroep[PRWHEdataNoMissingDataTAB$zwaarte.beroep == 2] <- "licht fysieke arbeid"
PRWHEdataNoMissingDataTAB$zwaarte.beroep[PRWHEdataNoMissingDataTAB$zwaarte.beroep == 3] <- "matig fysieke arbeid"
PRWHEdataNoMissingDataTAB$zwaarte.beroep[PRWHEdataNoMissingDataTAB$zwaarte.beroep == 4] <- "zwaar fysieke arbeid"
PRWHEdataNoMissingDataTAB$zwaarte.beroep[PRWHEdataNoMissingDataTAB$zwaarte.beroep == 5] <- "zwaar fysieke arbeid"

for(i in 1:10934){
  
  PRWHEdataNoMissingDataTAB$training[i] <- match(PRWHEdataNoMissingDataTAB$Kenmerk[i], PRWHEtraining$Kenmerk, nomatch=0)
  
  
}

PRWHEdataNoMissingDataTAB$training[PRWHEdataNoMissingDataTAB$training > 0] <- "Training"
PRWHEdataNoMissingDataTAB$training[PRWHEdataNoMissingDataTAB$training == 0] <- "Test"

##maken tabel

"tabel 1 data" <- matrix(data = NA, nrow = 50, ncol = 5)

## lengte klachten
"<3" = c("0", "0.2","0.25","0.5","1","1.5","2","2.5")
"3-6" = c("3", "4", "5")
"6-12" = c( "6", "7", "8" , "9", "10" ,"11")
"12-24" = c("12", "13", "14", "15","16", "17", "18", "19","20","21", "22", "23")
"24-48" = c("24" ,"25" ,"26", "27","28", "29","30","31","32","34","35","36","37", "38","39", "40" ,"42" , "43", "44" , "46")
">48" = c("48",  "49", "50","52", "54","55","56","58","60", "62" ,"65","70", "72" ,"74", "75","76" ,"78" ,"80" ,"82", "84" ,"90" ,"92", "96", "98", "99", "100",
          "106","108","110","120","124", "130" ,"132" ,"135" ,"144" ,"145" ,"150","156","160","168" ,"170" ,"180", "192", "200","204" ,"220" ,"228","240" ,"246", "250","266" ,
          "268","288", "300", "360", "372" ,"432","480", "500","510" ,"540", "600" ,"784","800")

`tabel 1 data`[36,1] <- "Other"
`tabel 1 data`[36,4] <- 26+24+224+435+11+5+1+9+23
`tabel 1 data`[35,5] <- round(49/(147+49)*100, digits = 3)

table(PRWHEdataNoMissingDataTAB$lengte.klachten[PRWHEdataNoMissingDataTAB$training=="Training"])

tabel.1 <- matrix(data =tabel.1.data, nrow = 36, ncol = 6, byrow = FALSE)

test.tabel.1 <- tabel.1.data[137:172]

28   + 2    +2  + 11 + 231   +12+  463   + 1
549 + 481+  318
767 + 218 + 273 + 240 + 178  + 79
844 +60+  100 +  65 +  56   +13  +400  +  9  + 42   + 9 +  10 +   5
595   + 5 +   4  + 10    +9 +   8  + 94  +2  +  5  +  5 +   3  +431+    1 +   7   + 5  + 15   +27   + 3  +  2   + 3

tabel.1 <- cbind(item.tabel.1, controle.tabel.1, controle.percentage.tabel.1, test.tabel.1, test.percentage.tabel.1)

colnames(tabel.1)
rownames(tabel.1) <- item.tabel.1

write.table(tabel.1, "C:/Users/mark/Documents/geneeskunde/onderzoek plastische/CAT/PRWHE/uitkomst/table 1", sep = "\t")

tabel.1 <- tabel.1[1:35,4]*3

tabel.1 <- tabel.1[1:35,-1]

table(PRWHEdataNoMissingDataTAB$training, PRWHEdataNoMissingDataTAB$behandeling)

"operated hand"<- matrix(data = c(nrow(PRWHEdataNoMissingDataTAB$behandelde.hand[PRWHEdataNoMissingDataTAB$training == "Training"]),nrow(PRWHEdataNoMissingDataTAB$behandelde.hand[PRWHEdataNoMissingDataTAB$training == "Test"])), nrow = 1, ncol = 2)

leeftijd.geslacht <- read_excel(choose.files())

PRWHEdata <- PRWHEdataNoMissingDataTAB

for(i in 1:nrow(PRWHEdataNoMissingDataTAB)){
  
  if(PRWHEdataNoMissingDataTAB$matchkort[i] > 0){
    PRWHEdataNoMissingDataTAB$behandelde.hand[i] <- intake.pols.kort$Welke.hand.wordt.behandeld..Selecteer.de.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchkort[i]]
    PRWHEdataNoMissingDataTAB$dominante.hand[i] <- as.character(intake.pols.kort$Wat.is.de.dominate.hand.[PRWHEdataNoMissingDataTAB$matchkort[i]])
    PRWHEdataNoMissingDataTAB$behandeling[i] <- as.character(intake.pols.kort$Wat.wordt.de.behandeling.[PRWHEdataNoMissingDataTAB$matchkort[i]])
    PRWHEdataNoMissingDataTAB$type.behandeling[i] <- as.character(intake.pols.kort$Type.behandeling[PRWHEdataNoMissingDataTAB$matchkort[i]])
    PRWHEdataNoMissingDataTAB$lengte.klachten[i] <- as.numeric(intake.pols.kort$Hoeveel.maanden.bestaat.de.klacht.al.[PRWHEdataNoMissingDataTAB$matchkort[i]])
    PRWHEdataNoMissingDataTAB$zwaarte.beroep[i] <- as.character(intake.pols.kort$Wat.is.de.zwaarte.van.het.beroep.van.de.patiënt..Selecteer.de.meest.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchkort[i]])
  }
  
  if (PRWHEdataNoMissingDataTAB$matchlang[i] > 0) {
    PRWHEdataNoMissingDataTAB$behandelde.hand[i] <- intake.pols.lang$X.A..Welke.hand.wordt.behandeld..Selecteer.de.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchlang[i]]
    PRWHEdataNoMissingDataTAB$dominante.hand[i] <- as.character(intake.pols.lang$X.A...Wat.is.de.dominate.hand.[PRWHEdataNoMissingDataTAB$matchlang[i]])
    PRWHEdataNoMissingDataTAB$behandeling[i] <- as.character(intake.pols.lang$X.A..Wat.wordt.de.behandeling.[PRWHEdataNoMissingDataTAB$matchlang[i]])
    PRWHEdataNoMissingDataTAB$type.behandeling[i] <- as.character(intake.pols.lang$X.TF..Type.behandeling[PRWHEdataNoMissingDataTAB$matchlang[i]])
    PRWHEdataNoMissingDataTAB$lengte.klachten[i] <- as.numeric(intake.pols.lang$X.A..Hoeveel.maanden.bestaat.de.klacht.al.[PRWHEdataNoMissingDataTAB$matchlang[i]])
    PRWHEdataNoMissingDataTAB$zwaarte.beroep[i] <- as.character(intake.pols.lang$X.A..Wat.is.de.zwaarte.van.het.beroep.van.de.patiënt..Selecteer.de.meest.toepasselijke.optie[PRWHEdataNoMissingDataTAB$matchlang[i]])
  }
}

match <- rep(NA, nrow(PRWHEdataNoMissingDataTAB))

for(i in 1:nrow(leeftijd.geslacht)){
  match[i] <- match(PRWHEdataNoMissingDataTAB$V33[i], leeftijd.geslacht$fact_respondent_track_id_report)
} 
sex <- rep(NA, nrow(PRWHEdataNoMissingDataTAB))
age <- rep(NA, nrow(PRWHEdataNoMissingDataTAB))
PRWHEdataNoMissingDataTAB <- cbind(PRWHEdataNoMissingDataTAB, sex, age)

for(i in 1:nrow(leeftijd.geslacht)){
  PRWHEdataNoMissingDataTAB$sex[i] <- leeftijd.geslacht$geslacht[match[i]]
  PRWHEdataNoMissingDataTAB$age[i] <- as.numeric(leeftijd.geslacht$leeftijd[match[i]])
}

PRWHEdataNoMissingDataTAB$lengte.klachten <- as.numeric(PRWHEdataNoMissingDataTAB$lengte.klachten)

for(i in 1:nrow(PRWHEdataNoMissingDataTAB)){
  PRWHEdataNoMissingDataTAB$age[i] <- cut(PRWHEdataNoMissingDataTAB$age[i], c(0,10,20,30,40,50,60,70,80,90))
  PRWHEdataNoMissingDataTAB$lengte.klachten[i] <- cut(PRWHEdataNoMissingDataTAB$lengte.klachten[i], c(0,3,6,12,24,48,800))
} 
