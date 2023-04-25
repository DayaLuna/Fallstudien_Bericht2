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
# Mean   :1992   Mean   :25.88  
# 3rd Qu.:1996   3rd Qu.:29.00
# Max.   :2005   Max.   :45.00

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

par(mfrow = c(2,3))
plot(table(ski_f_2014$age))
plot(table(ski_f_2018$age))
plot(table(ski_f_2022$age))

plot(table(snow_f_2014$age))
plot(table(snow_f_2018$age))
plot(table(snow_f_2022$age))
# sieht nicht nach Normalverteilung aus