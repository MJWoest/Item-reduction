##Import the dataset. In the article and in this code the database was a .txt file and saved as 'PRWHEdataNoMissingDataTAB'

# PRWHEdataNoMissingDataTAB <- read.table(choose.file())

##The data set was split into a development group 'PRWHEtraining' and a validation group 'PRWHEtest'

PRWHEtraining <- PRWHEdataNoMissingDataTAB[sample(1:nrow(PRWHEdataNoMissingDataTAB),
                                                  0.75*nrow(PRWHEdataNoMissingDataTAB),
                                                  replace=F),]

sub.set <- as.numeric(rownames(PRWHEtraining))
PRWHEtest <- PRWHEdataNoMissingDataTAB[-sub.set,]

##In order to optimize the chaid alorithm a design matrix was created for the parameters 'Maximal Depth', 'Minimal bucket' and 'minimal split'
##Design matrix
minbucket.variabele.pijn <- as.numeric(list(10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,95,100))

design.matrix.pijn <- expand.grid(
  "max.height" = c(3,4,5),
  "minbucket" = as.numeric(minbucket.variabele.pijn),
  "minsplit" = NA
  )

totaal.rijen.pijn <- nrow(design.matrix.pijn)

for(i in 1:totaal.rijen.pijn) {
  design.matrix.pijn$minsplit[i] <- (design.matrix.pijn$minbucket[i]*3)
}

##place-holder for data overview after optimization
aantal.terminal.nodes.pijn <- rep(NA, totaal.rijen.pijn)
mean.predicted.pijn <- rep(NA, totaal.rijen.pijn)
standaard.deviatie.predicted.pijn <- rep(NA, totaal.rijen.pijn)
mean.error.pijn <- rep(NA, totaal.rijen.pijn)
standaard.deviatie.error.pijn <- rep(NA, totaal.rijen.pijn)
ICC.pijn <- rep(NA, totaal.rijen.pijn)


##begin for-loop
for (i in 1:totaal.rijen.pijn) {

##Identifiing the altering parameters used in the for-loop
chaid_control <- chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
                               minsplit = design.matrix.pijn$minsplit,
                               minbucket = design.matrix.pijn$minbucket, minprob = 0.05, stump = FALSE,
                               maxheight = design.matrix.pijn$max.height)

##Importing all questinnaire anwers as factor to be used for the CHAID algorithm

ps <- factor(PRWHEtraining$V9)
pv1 <- factor(PRWHEtraining$V11)
pv2 <- factor(PRWHEtraining$V12)
pv3 <- factor(PRWHEtraining$V13)
pv4 <- factor(PRWHEtraining$V14)
pv5 <- factor(PRWHEtraining$V15)

##Running the algorithm itself
current.chaid.run.pijn <- chaid(ps ~ pv1+pv2+pv3+pv4+pv5,
                           data= PRWHEtraining ,
                           control= chaid_control)

##After running the optimization, a final decision tree can be created using the best parameters
##To test this final decision tree a predicion needs to be made based on the "PRWHEtest" data

# ps <- factor(PRWHEtest$V9)
# pv1 <- factor(PRWHEtest$V11)
# pv2 <- factor(PRWHEtest$V12)
# pv3 <- factor(PRWHEtest$V13)
# pv4 <- factor(PRWHEtest$V14)
# pv5 <- factor(PRWHEtest$V15)

##Predicting the score based on the decision tree

current.predict <- predict(current.chaid.run.pijn, newdata = PRWHEtest)

##-1, during transformation to factor all answers had +1

current.prediction <- as.numeric(current.predict)-1

##Creating a table of optimization outcomes

##Predicted scores
mean.predicted.pijn[i] <- c(mean(as.numeric(current.prediction)))
standaard.deviatie.predicted.pijn[i] <- c(sd(as.numeric(current.prediction)))

##Difference between the original PRWE and the decisoin tree PRWHE

error <- PRWHEtest$V9-as.numeric(current.prediction)
mean.error.pijn[i] <- c(mean(error))
standaard.deviatie.error.pijn[i] <- c(sd(error))

##Counting the number of terminal nodes

current.terminal.nodes  <- nodeids(current.chaid.run, terminal=TRUE)
aantal.terminal.nodes.pijn[i] <- c(length(current.terminal.nodes))

##Calculating interclass correlation coefficient

ICC.pijn[i]<- ICCbare(factor(PRWHEtest$V9), as.numeric(current.prediction))

print(i)
}

##End of for-loop


##Creating the optimization outcome table

mean.ps <- rep(NA, totaal.rijen.pijn)

for(i in 1:totaal.rijen.pijn) {
  mean.ps[i] <- mean(PRWHEtest$V9)
}

standaard.deviatie.ps <- rep(NA, totaal.rijen.pijn)

for(i in 1:totaal.rijen.pijn) {
  standaard.deviatie.ps[i] <- sd(PRWHEtest$V9)
}

outcome.overview.pijn <- cbind(design.matrix.pijn,
                          aantal.terminal.nodes.pijn,
                          mean.ps,
                          standaard.deviatie.ps,
                          mean.predicted.pijn,
                          standaard.deviatie.predicted.pijn,
                          mean.error.pijn,
                          standaard.deviatie.error.pijn,
                          ICC.pijn)



##when needed, plot the decision tree
plot(current.chaid.run, type="simple")

##When optimizing is completed, select the prefered chaid_control variables and run just the CHAID function using these parameters.

##For visualisition use the "Figures" function