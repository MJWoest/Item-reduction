##Figure 1. SD vs. Minimal bucket for the pain subscore

plot(outcome.overview.pijn.5$minbucket, outcome.overview.pijn.5$standaard.deviatie.error.pijn,
     main = "",
     xlab = "Minimal bucket size of CHAID optimization",
     ylab = c("Standaard deviation of the difference", "for the pain sub score"),
     col=rainbow(3),
     pch=c(15,16,17)
)

legend("bottomright",legend=paste(rep(c("Depth"), times=3), c("2", "3", "4")), bty = "n",
       pch=c(15,16,17), col = rainbow(3))

##Figre 2. SD vs. Mimimal bucket for the disability subscore
plot(outcome.overview.functie.5$minbucket, outcome.overview.functie.5$standaard.deviatie.error.functie,
     main = "",
     xlab = "Minimal bucket size of CHAID optimization",
     ylab = c("Standaard deviation of the difference", "for the disability sub score"),
     col=rainbow(4),
     pch=c(15,16,17,18)
)

legend("bottomright",legend=paste(rep(c("Depth"), times=4), c("3", "4", "5", "6")), bty = "n",
       pch=c(15,16,17,18), col = rainbow(4))


##Figure 3. Histograms of the error between the decision tree PRWE and the original PRWE
##Figure 3A. Pain subscore

error.pijn <- PRWHEtest$V9-prediction.pijn
mean(error.pijn)
sd(error.pijn)

hist(error.pijn, xlim =c(-20,20), ylim = c(0,800), breaks = 30, col = "ivory4",
     main = "",
     xlab = "Difference between the PRWE and DT-PRWE pain sub score"
)

text(-14.8,500, labels =paste("Standard deviation = 2.9"))
text(-17,550, labels =paste("Mean = -0.2"))

##FIgure 3B. disability subscore

error.functie <- PRWHEtest$V10-prediction.functie
mean(error.functie)
sd(error.functie)

hist(error.functie, xlim =c(-20,20), ylim = c(0,800), breaks = 30, col = "ivory4",
     main = "",
     xlab = "Difference between the PRWE and DT-PRWE disability sub score"
)

text(-14.7,500, labels =paste("Standard deviation = 4.1"))
text(-17,550, labels =paste("Mean = 0.53"))

##Figure 3C. Total RPWE

error.totaal <- PRWHEtest$V8-prediction.totaal
mean(error.totaal)
sd(error.totaal)

##SEM/ error PRWE

SEM <- (mean(error.totaal^2))^0.5

##MDC error PRWE

MDC <- ((mean(error.totaal^2))^0.5)*(2^0.5)*1.64

#histogram
hist(error.totaal, xlim =c(-25,25), ylim = c(0,800), breaks = 59, col = "ivory4",
     main = "",
     xlab = "Difference between original PRWE and DT-PRWE total score"
)

#text ICC and mean
text(-14.3,500, labels =paste(" Standard deviation = 5.1"))
text(-17,550, labels =paste("Mean = 0.4"))

#lines SEM and MDC
segments(SEM,0, x1=5.14, y1=250, lwd = 2, col ="blue4", lty = 3)
segments(-SEM,0, x1=-5.14, y1=250, lwd = 2, col ="blue4", lty = 3)
segments(MDC,0, x1=11.9, y1=250, lwd = 2, col = "red3", lty = 1)
segments(-MDC,0, x1=-11.9, y1=250, lwd = 2, col = "red3", lty = 1)


legend(12,610, legend=paste(c("SEM", "90% MDC"), c("           5.1", " 11.9")), bty = "n",
       lty = c(3,1),  col = c("blue4", "red3"))

##Figure 4. BlandAltman plots
##Can only be run when a decision tree has already been created using thes scripts
##Creates a Bland Altman plot of the latest deicsion tree that has been created

##Figure 4A. Bland altman for pain
bland.data <- cbind(PRWHEtest$V9,as.numeric(current.prediction))

##Fgure 4B. Bland altman for disabiity
bland.data <- cbind(PRWHEtest$V10,as.numeric(current.prediction))

bland.altman <- BlandAltman(bland.data[,1], bland.data[,2],
                            xlab = "mean disability score",
                            ylab = "error",
                            maintit = "Bland-Altmanplot",
                            col.points=rgb(0,0,100,55,maxColorValue=255), col.lines = rgb(1,0,0,0.5), pch=19,
                            par(mar=c(5,5,3,3.5)))

text(7,30, labels =paste("ICC = 0.9061"))