##Import the dataset. In the article and in this code the database was a .txt file and saved as 'PRWHEdataNoMissingDataTAB'

# PRWHEdataNoMissingDataTAB <- read.table(choose.file())

##The data set was split into a development group 'PRWHEtraining' and a validation group 'PRWHEtest'
  ## WARNING: only subset the date ONCE, otherwise all optimization needs to be done again
  ## DO NOT subset again if the data has already been subsetted for an other subscore

PRWHEtraining <- PRWHEdataNoMissingDataTAB[sample(1:nrow(PRWHEdataNoMissingDataTAB),
                                                  0.75*nrow(PRWHEdataNoMissingDataTAB),
                                                  replace=F),]

sub.set <- as.numeric(rownames(PRWHEtraining))
PRWHEtest <- PRWHEdataNoMissingDataTAB[-sub.set,]

##In order to optimize the chaid alorithm a design matrix was created for the parameters 'Maximal Depth', 'Minimal bucket' and 'minimal split'
##Design matrix
minbucket.variabele.functie <- as.numeric(list(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))

design.matrix.functie <- expand.grid(
  "max.height" = c(3, 4, 5, 6),
  "minbucket" = as.numeric(minbucket.variabele.functie),
  "minsplit" = NA
)

totaal.rijen.functie <- nrow(design.matrix.functie)

for(i in 1:totaal.rijen.functie) {
  design.matrix.functie$minsplit[i] <- (design.matrix.functie$minbucket[i]*3)
}

##place-holder for data overview after optimization
aantal.terminal.nodes.functie <- rep(NA, totaal.rijen.functie)
mean.predicted.functie <- rep(NA, totaal.rijen.functie)
standaard.deviatie.predicted.functie <- rep(NA, totaal.rijen.functie)
mean.error.functie <- rep(NA, totaal.rijen.functie)
standaard.deviatie.error.functie <- rep(NA, totaal.rijen.functie)
ICC.functie <- rep(NA, totaal.rijen.functie)


##begin for-loop
for (i in 1:totaal.rijen.functie) {
  
  ##Identifiing the altering parameters used in the for-loop
  chaid_control <- chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
                                 minsplit = design.matrix.functie$minsplit,
                                 minbucket = design.matrix.functie$minbucket, minprob = 0.05, stump = FALSE,
                                 maxheight = design.matrix.functie$max.height)
  
  ##Importing all questinnaire anwers as factor to be used for the CHAID algorithm
  
  fs <- factor(PRWHEtraining$V10)
  fv1 <- factor(PRWHEtraining$V16)
  fv2 <- factor(PRWHEtraining$V17)
  fv3 <- factor(PRWHEtraining$V18)
  fv4 <- factor(PRWHEtraining$V19)
  fv5 <- factor(PRWHEtraining$V20)
  fv6 <- factor(PRWHEtraining$V21)
  fv7 <- factor(PRWHEtraining$V22)
  fv8 <- factor(PRWHEtraining$V23)
  fv9 <- factor(PRWHEtraining$V24)
  fv10 <- factor(PRWHEtraining$V25)
  
  ##Running the algorithm itself
  current.chaid.run.functie <- chaid(fs ~ fv1+fv2+fv3+fv4+fv5+fv6+fv7+fv8+fv9+fv10,
                                     data= PRWHEtraining ,
                                     control= chaid_control)
  
  ##After running the optimization, a final decision tree can be created using the best parameters
  ##To test this final decision tree a predicion needs to be made based on the "PRWHEtest" data
  
  # fs <- factor(PRWHEtest$V10)
  # fv1 <- factor(PRWHEtest$V16)
  # fv2 <- factor(PRWHEtest$V17)
  # fv3 <- factor(PRWHEtest$V18)
  # fv4 <- factor(PRWHEtest$V19)
  # fv5 <- factor(PRWHEtest$V20)
  # fv6 <- factor(PRWHEtest$V21)
  # fv7 <- factor(PRWHEtest$V22)
  # fv8 <- factor(PRWHEtest$V23)
  # fv9 <- factor(PRWHEtest$V24)
  # fv10 <- factor(PRWHEtest$V25)
  
  ##Predicting the score based on the decision tree
  current.predict <- predict(current.chaid.run.functie, newdata = PRWHEtest)
  
  ##-1, during transformation to factor all answers had +1
  
  current.prediction <- as.numeric(current.predict)-1
  
  ##Creating a table of optimization outcomes
  
  ##Predicted scores
  mean.predicted.functie[i] <- c(mean(as.numeric(current.prediction)))
  standaard.deviatie.predicted.functie[i] <- c(sd(as.numeric(current.prediction)))
  
  ##Difference between the original PRWE and the decisoin tree PRWHE
  
  error <- PRWHEtest$V10-as.numeric(current.prediction)
  mean.error.functie[i] <- c(mean(error))
  standaard.deviatie.error.functie[i] <- c(sd(error))
  
  ##Counting the number of terminal nodes
  
  current.terminal.nodes  <- nodeids(current.chaid.run, terminal=TRUE)
  aantal.terminal.nodes.functie[i] <- c(length(current.terminal.nodes))
  
  ##Calculating interclass correlation coefficient
  
  ICC.functie[i]<- ICCbare(factor(PRWHEtest$V10), as.numeric(current.prediction))
  
  print(i)
}

##End of for-loop


##Creating the optimization outcome table
mean.fs <- rep(NA, totaal.rijen.functie)

for(i in 1:totaal.rijen.functie) {
  mean.fs[i] <- mean(PRWHEtest$V10)
}

standaard.deviatie.fs <- rep(NA, totaal.rijen.functie)

for(i in 1:totaal.rijen.functie) {
  standaard.deviatie.fs[i] <- sd(PRWHEtest$V10)
}

outcome.overview.functie <- cbind(design.matrix.functie,
                                  aantal.terminal.nodes.functie,
                                  mean.fs,
                                  standaard.deviatie.fs,
                                  mean.predicted.functie,
                                  standaard.deviatie.predicted.functie,
                                  mean.error.functie,
                                  standaard.deviatie.error.functie,
                                  ICC.functie)



##when needed, plot the decision tree
plot(current.chaid.run.functie, type="simple")

##When optimizing is completed, select the prefered chaid_control variables and run just the CHAID function using these parameters.

##For visualisition use the "Figures" function