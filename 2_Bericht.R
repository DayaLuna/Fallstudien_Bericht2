#library(qboxplot)
library(scales)
library(xtable)

data <- read.csv("WinterGames.csv")
data <- data[,-1]

data$age <- data$olympics - data$birth

data$olympics <- as.factor(data$olympics)
data$discipline <- as.factor(data$discipline)
data$sex <- as.factor(data$sex)

summary(data)
# olympics       discipline  sex         birth           age       
# 2014:261   Ski      :559   f:346   Min.   :1970   Min.   :17.00  
# 2018:254   Snowboard:190   m:403   1st Qu.:1989   1st Qu.:22.00  
# 2022:234                           Median :1992   Median :26.00  
#                                    Mean   :1992   Mean   :25.88  
#                                    3rd Qu.:1996   3rd Qu.:29.00
#                                    Max.   :2005   Max.   :45.00


#### Deskriptive Analyse ###########################################

hist(data[data$discipline== "Ski",]$age, 27,xlim=c(15,45),
     col = "darkgrey", freq=F, main = "", xlab="Alter in Jahren",
     ylab = "rel. Häufigkeit", cex.axis = 1.5,
     cex.lab = 1.5)
hist(data[data$discipline== "Snowboard",]$age, 27, xlim=c(15,45),
     add = T, col= alpha("blue", 0.2), freq=F, cex.axis = 1.5,
     cex.lab = 1.5)
legend("topright",legend = c("Ski", "Snowboard"), 
       col = c("darkgrey",alpha("blue", 0.2)),
       lwd = 5, cex = 1.2, bty = 'n', inset = -0.2) 


mean(data$age[data$discipline=="Ski"], range=0, qtype=2)
# [1] 25.0805
median(data$age[data$discipline=="Ski"], range=0, qtype=2)
# [1] 25

mean(data$age[data$discipline=="Snowboard"], range=0, qtype=2)
# [1] 28.24211
median(data$age[data$discipline=="Snowboard"], range=0, qtype=2)
# [1] 28

## gibt es Gründe, warum Skifahrer jünger sind? 


## Teildatensätze Frauen

ski_f_2014 <- data[data$olympics == "2014" & 
                     data$discipline == "Ski" &
                     data$sex == "f",]
snow_f_2014 <- data[data$olympics == "2014" & 
                      data$discipline == "Snowboard" &
                      data$sex == "f",]

ski_f_2018 <- data[data$olympics == "2018" & 
                     data$discipline == "Ski" &
                     data$sex == "f",]
snow_f_2018 <- data[data$olympics == "2018" & 
                      data$discipline == "Snowboard" &
                      data$sex == "f",]

ski_f_2022 <- data[data$olympics == "2022" & 
                     data$discipline == "Ski" &
                     data$sex == "f",]

snow_f_2022 <- data[data$olympics == "2022" & 
                      data$discipline == "Snowboard" &
                      data$sex == "f",]

## Teildatensätze Männer

ski_m_2014 <- data[data$olympics == "2014" & 
                     data$discipline == "Ski" &
                     data$sex == "m",]
snow_m_2014 <- data[data$olympics == "2014" & 
                      data$discipline == "Snowboard" &
                      data$sex == "m",]

ski_m_2018 <- data[data$olympics == "2018" & 
                     data$discipline == "Ski" &
                     data$sex == "m",]
snow_m_2018 <- data[data$olympics == "2018" & 
                      data$discipline == "Snowboard" &
                      data$sex == "m",]

ski_m_2022 <- data[data$olympics == "2022" & 
                     data$discipline == "Ski" &
                     data$sex == "m",]

snow_m_2022 <- data[data$olympics == "2022" & 
                      data$discipline == "Snowboard" &
                      data$sex == "m",]

# Gruppengrößen
c(dim(snow_f_2014)[1], dim(snow_f_2018)[1], dim(snow_f_2022)[1],
  dim(snow_m_2014)[1], dim(snow_m_2018)[1], dim(snow_m_2022)[1])
# [1] 32 31 31 32 32 32

# immer 32 Snowboarder, bei 31 vermutlich kurzfristige Ausfälle

c(dim(ski_f_2014)[1], dim(ski_f_2018)[1], dim(ski_f_2022)[1],
  dim(ski_m_2014)[1], dim(ski_m_2018)[1], dim(ski_m_2022)[1])
# [1]  89  81  82 108 110  89



Anzahl<- rbind(Snowboarder = c(dim(snow_f_2014)[1], dim(snow_f_2018)[1],
                               dim(snow_f_2022)[1], dim(snow_m_2014)[1],
                               dim(snow_m_2018)[1], dim(snow_m_2022)[1]),
               Skifahrer =  c(dim(ski_f_2014)[1], dim(ski_f_2018)[1],
                              dim(ski_f_2022)[1], dim(ski_m_2014)[1], 
                              dim(ski_m_2018)[1], dim(ski_m_2022)[1]))

xtable(Anzahl)

#### Snowboard
insgesamt<- quantile(data$age[data$discipline == "Snowboard"],
                     c(0,0.25,0.5,0.75,1))
weiblich2014<- quantile(snow_f_2014$age,c(0,0.25,0.5,0.75,1))
weiblich2018<- quantile(snow_f_2018$age,c(0,0.25,0.5,0.75,1))
weiblich2022<- quantile(snow_f_2022$age,c(0,0.25,0.5,0.75,1))

männlich2014<- quantile(snow_m_2014$age, c(0,0.25,0.5,0.75,1))
männlich2018<- quantile(snow_m_2018$age, c(0,0.25,0.5,0.75,1))
männlich2022<- quantile(snow_m_2022$age, c(0,0.25,0.5,0.75,1))
datenSnow<-rbind(insgesamt, 
                 "Frauen 2014" = weiblich2014,
                 "Frauen 2018" = weiblich2018, 
                 "Frauen 2022" = weiblich2022,
                 "Männer 2014" = männlich2014, 
                 "Männer 2018" = männlich2018, 
                 "Männer 2022" = männlich2022)

xtable(datenSnow)

#### Ski
insgesamt2<- quantile(data$age[data$discipline == "Ski"],
                      c(0,0.25,0.5,0.75,1))
weiblich2014<- quantile(ski_f_2014$age,c(0,0.25,0.5,0.75,1))
weiblich2018<- quantile(ski_f_2018$age,c(0,0.25,0.5,0.75,1))
weiblich2022<- quantile(ski_f_2022$age,c(0,0.25,0.5,0.75,1))

männlich2014<- quantile(ski_m_2014$age, c(0,0.25,0.5,0.75,1))
männlich2018<- quantile(ski_m_2018$age, c(0,0.25,0.5,0.75,1))
männlich2022<- quantile(ski_m_2022$age, c(0,0.25,0.5,0.75,1))

datenSki<-rbind("insgesamt" = insgesamt2, 
                "Frauen 2014" = weiblich2014 ,
                "Frauen 2018" = weiblich2018, 
                "Frauen 2022" = weiblich2022,
                "Männer 2014" = männlich2014,
                "Männer 2018" = männlich2018, 
                "Männer 2022" = männlich2022)

xtable(datenSki)

#### Statistische Tests ############################################

# da stetig ist N-Verteilung eh unpassend

# Ski Dichte
par(mfrow = c(2,3), mar = c(5.1, 4.6, 4.1, 2.1))
plot(table(ski_f_2014$age), xlim=c(17, 45), main = "Frauen 2014",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(ski_f_2018$age), xlim=c(17, 45), main = "Frauen 2018",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(ski_f_2022$age), xlim=c(17, 45), main = "Frauen 2022",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)

plot(table(ski_m_2014$age), xlim=c(17, 45), main = "Männer 2014",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(ski_m_2018$age), xlim=c(17, 45), main = "Männer 2018",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(ski_m_2022$age), xlim=c(17, 45), main = "Männer 2022",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)

mtext("Skifahrer", side = 3, line = - 1.5, outer = TRUE, cex = 1.4)

# Snowboard Dichte
plot(table(snow_f_2014$age), xlim=c(17, 45), main = "Frauen 2014",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(snow_f_2018$age), xlim=c(17, 45), main = "Frauen 2018",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(snow_f_2022$age), xlim=c(17, 45), main = "Frauen 2022",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)

plot(table(snow_m_2014$age), xlim=c(17, 45), main = "Männer 2014",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(snow_m_2018$age), xlim=c(17, 45), main = "Männer 2018",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)
plot(table(snow_m_2022$age), xlim=c(17, 45), main = "Männer 2022",
     xlab = "Alter in Jahren", ylab = "abs. Häufigkeit",
     cex.axis = 2, cex.lab = 2, cex.main = 2)

mtext("Snowboardfahrer", side = 3, line = - 1.5, outer = TRUE,  cex = 1.4)

# sieht nicht nach Normalverteilung aus

########

# auf die y-Achse kumulierte Wahrscheinlichkeit?
par(mfrow = c(2,3), oma = c(3, 0, 0, 0), mar = c(5.1, 4.6, 4.1, 2.1))
plot.ecdf(ski_f_2014$ag, xlab="Alter in Jahren", 
          cex.axis = 1.6, cex.lab = 1.6, , cex.main=1.8,
          ylab = "Verteilungsfunktion", main = "Olympiade Frauen 2014")
plot.ecdf(snow_f_2014$age, add=T, col="blue")
#legend(16.65541,  0.9934292, legend = c("Ski", "Snowboard"),
#       col = c("black", "blue"), lwd = 3, cex=0.8)
#legend(30.9542,  0.2390144, legend = c("Ski", "Snowboard"),
#       col = c("black", "blue"), lwd = 3, cex=0.8)
plot.ecdf(ski_f_2018$age, xlab="Alter in Jahren",
          cex.axis = 1.6, cex.lab = 1.6, , cex.main=1.8,
          ylab = "Verteilungsfunktion", main = "Olympiade Frauen 2018")
plot.ecdf(snow_f_2018$age, add=T, col="blue")
plot.ecdf(ski_f_2022$age, xlab="Alter in Jahren", 
          cex.axis = 1.6, cex.lab = 1.6, , cex.main=1.8,
          ylab = "Verteilungsfunktion", main = "Olympiade Frauen 2022")
plot.ecdf(snow_f_2022$age, add=T, col="blue")

# bei Frauen verlaufen die Verteilungen ähnlciher als bei den Männern

plot.ecdf(ski_m_2014$age, xlab="Alter in Jahren", 
          cex.axis = 1.6, cex.lab = 1.6, , cex.main=1.8,
          ylab = "Verteilungsfunktion", main = "Olympiade Männer 2014")
plot.ecdf(snow_m_2014$age, add=T, col="blue")
plot.ecdf(ski_m_2018$age, xlab="Alter in Jahren", 
          cex.axis = 1.6, cex.lab = 1.6, , cex.main=1.8,
          ylab = "Verteilungsfunktion", main = "Olympiade Männer 2018")
plot.ecdf(snow_m_2018$age, add=T, col="blue")
plot.ecdf(ski_m_2022$age, xlab="Alter in Jahren",         
          cex.axis = 1.6, cex.lab = 1.6, , cex.main=1.8,
          ylab = "Verteilungsfunktion", main = "Olympiade Männer 2022")
plot.ecdf(snow_m_2022$age, add=T, col="blue")

legend(-30.14725,-0.409555 ,legend = c("Ski", "Snowboard"), 
       col = c("black","blue"),
       lwd = 5, horiz = TRUE, cex = 1.5,  xpd="NA",  bty = 'n')


##### Wilcoxon Test
# Bonferroni-Korrektur des Niveaus
niveau<-1-(0.05/6)
wilcox.test(ski_f_2014$age, snow_f_2014$age, "greater", correct = F, conf.level= niveau)
wilcox.test(ski_f_2018$age, snow_f_2018$age, "greater", correct = F, conf.level= niveau)
wilcox.test(ski_f_2022$age, snow_f_2022$age, "greater", correct = F, conf.level= niveau)

wilcox.test(ski_m_2014$age, snow_m_2014$age, "greater", correct = F, conf.level= niveau)
wilcox.test(ski_m_2018$age, snow_m_2018$age, "greater", correct = F, conf.level= niveau)
wilcox.test(ski_m_2022$age, snow_m_2022$age, "greater", correct = F, conf.level= niveau)


### Test selbst implementieren
# n1= 89, n2 = 32
sum(rank(c(ski_f_2014$age, snow_f_2014$age))[1:89])
# [1] 5111 # Wn1,n2
(5111 - 1/2*(89+32+1))/sqrt((89*32*(89+32+1))/12)
# [1] 29.67783

wtest <- function(x,y){
  n1 <- length(x)
  n2 <- length(y)
  w <- sum(rank(c(x,y))[1:n1])
  (w - 1/2*(n1+n2+1))/sqrt((n1*n2*(n1+n2+1))/12)
}
qnorm(niveau)
# [1] 2.39398

c(wtest(ski_f_2014$age, snow_f_2014$age),
  wtest(ski_f_2018$age, snow_f_2018$age),
  wtest(ski_f_2022$age, snow_f_2022$age),
  wtest(ski_m_2014$age, snow_m_2014$age),
  wtest(ski_m_2018$age, snow_m_2018$age),
  wtest(ski_m_2022$age, snow_m_2022$age))
pnorm(c(wtest(ski_f_2014$age, snow_f_2014$age),
        wtest(ski_f_2018$age, snow_f_2018$age),
        wtest(ski_f_2022$age, snow_f_2022$age),
        wtest(ski_m_2014$age, snow_m_2014$age),
        wtest(ski_m_2018$age, snow_m_2018$age),
        wtest(ski_m_2022$age, snow_m_2022$age)))
