#library(qboxplot)

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

par(mfrow=c(1,2))
boxplot(data$age[data$discipline=="Ski"], range=0, qtype=2)
boxplot(data$age[data$discipline=="Snowboard"], range=0, qtype=2)

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
dim(ski_f_2014)[1]

c(dim(ski_f_2014)[1], dim(ski_f_2018)[1], dim(ski_f_2022)[1],
  dim(ski_m_2014)[1], dim(ski_m_2018)[1], dim(ski_m_2022)[1])
# [1]  89  81  82 108 110  89

# man darf auch N(0,1) anstelle von t-Verteilung nutzen

# Frauen Dichte
par(mfrow = c(2,3))
plot(table(ski_f_2014$age), xlim=c(17, 45))
plot(table(ski_f_2018$age), xlim=c(17, 45))
plot(table(ski_f_2022$age), xlim=c(17, 45))

plot(table(snow_f_2014$age), xlim=c(17, 45))
plot(table(snow_f_2018$age), xlim=c(17, 45))
plot(table(snow_f_2022$age), xlim=c(17, 45))
# sieht nicht nach Normalverteilung aus

# Männer Dichte
plot(table(ski_m_2014$age), xlim=c(17, 45))
plot(table(ski_m_2018$age), xlim=c(17, 45))
plot(table(ski_m_2022$age), xlim=c(17, 45))

plot(table(snow_m_2014$age), xlim=c(17, 45))
plot(table(snow_m_2018$age), xlim=c(17, 45))
plot(table(snow_m_2022$age), xlim=c(17, 45))

########
par("oma")
# [1] 0 0 0 0
par("mar")
# [1] 5.1 4.1 4.1 2.1
par("fig")
# [1] 0 1 0 1
par("xpd")
# [1] FALSE
########

# auf die y-Achse kumulierte Wahrscheinlichkeit?
par(mfrow = c(2,3), oma = c(3, 0, 0, 0))
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

# legend(-50.14725,-0.909555 ,legend = c("Ski", "Snowboard"), 
#        col = c("black","blue"),
#        lwd = 5, horiz = TRUE, cex = 1,  xpd="NA",  bty = 'n')
legend(-30.14725,-0.409555 ,legend = c("Ski", "Snowboard"), 
       col = c("black","blue"),
       lwd = 5, horiz = TRUE, cex = 1.5,  xpd="NA",  bty = 'n')

##### Wilcoxon Test
wilcox.test(ski_f_2014$age, snow_f_2014$age, "greater", correct = F)
wilcox.test(ski_f_2018$age, snow_f_2018$age, "greater", correct = F)
wilcox.test(ski_f_2022$age, snow_f_2022$age, "greater", correct = F)

wilcox.test(ski_m_2014$age, snow_m_2014$age, "greater", correct = F)
wilcox.test(ski_m_2018$age, snow_m_2018$age, "greater", correct = F)
wilcox.test(ski_m_2022$age, snow_m_2022$age, "greater", correct = F)
