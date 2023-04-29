WinterGames <- read.csv("C:/Users/esthe/Downloads/WinterGames.csv")

str(WinterGames)
#'data.frame':	749 obs. of  5 variables:
#$ X         : int  1 2 3 4 5 6 7 8 9 10 ...
#$ olympics  : int  2022 2022 2022 2022 2022 2022 2022 2022 2022 2022 ...
#$ discipline: chr  "snowboard" "Snowboard" "Snowboard" "Snowboard" ...
#$ sex       : chr  "f" "f" "f" "f" ...
#$ birth     : int  1998 1994 1996 1996 1996 1997 1993 2003 1983 2000 ...

anyNA(WinterGames)
#[1] FALSE
WinterGames$discipline<- as.factor(WinterGames$discipline)
WinterGames$sex<- as.factor(WinterGames$sex)
WinterGames$alter<- WinterGames$olympics-WinterGames$birth

barplot(table(WinterGames$alter))
#wäre interessant gewesen zu schauen, wann das erste mal teilgenommen wurde nicht aber nicht möglich da Personen nicht zugeordnet werden

unique(WinterGames$discipline)
snowboard<- subset(WinterGames, WinterGames$discipline=="Snowboard")
ski<- subset(WinterGames, WinterGames$discipline== "Ski")
skiW<- subset(ski, ski$sex=="f")
skiM<- subset(ski, ski$sex=="m")

skiW22<- subset(skiW, skiW$olympics==2022)
skiW18<- subset(skiW, skiW$olympics==2018)
skiW14<- subset(skiW, skiW$olympics==2014)

skiM22<- subset(skiM, skiM$olympics==2022)
skiM18<- subset(skiM, skiM$olympics==2018)
skiM14<- subset(skiM, skiM$olympics==2014)

snowW<- subset(snowboard, snowboard$sex=="f")
snowM<- subset(snowboard, snowboard$sex=="m")

snowW22<- subset(snowW, snowW$olympics==2022)
snowW18<- subset(snowW, snowW$olympics==2018)
snowW14<- subset(snowW, snowW$olympics==2014)

snowM22<- subset(snowM, snowM$olympics==2022)
snowM18<- subset(snowM, snowM$olympics==2018)
snowM14<- subset(snowM, snowM$olympics==2014)



par(mfrow= c(2,1))
barplot(table(snowboard$alter)/length(snowboard$alter))
barplot(table(ski$alter)/length(ski$alter))
#beim snowboard nehmen immer 32 Teilneher teil 

#snowboardfahrer: FIS Punkte und einmal unter top 30 bei einem Weltcup sieg, 32. Platz für den aus dem Hostcountry
#ski: 

#was erschwert die Auswertung (Probalemstellung):
#viel mehr ski fahrer auf Grund des Turnus
#snowboarder fahren parallel zu 2. aber die ski fahrer fahren alleine
#Mehrfachteilnahmen

#können wir hier eine
#teststatistik kann t-Verteilt sein, obwohl nicht Normalverteilte Daten
#kein Chi-Quadrattest

#Mann-Whitney-Test entspricht Wilcoxon-Rangsummen-Test

#Vorraussetzung: Grafik von Darya
#lets go mit Bonferronie
alpha<- 0.05
niveau<-1-(alpha/6)

#insgesmmt 6 Tests, da jedes Olympiajahr (3 an der Zahl) 2 Tests (da 2 Geschlechter) ODER doch andere Anzal?
library(stats)
wilcox.test(snowW14$alter, skiW14$alter, alternative= "less", conf.level= niveau)
wilcox.test(snowW18$alter, skiW18$alter, alternative= "less", conf.level= niveau)
wilcox.test(snowW22$alter, skiW22$alter, alternative= "less", conf.level= niveau)

wilcox.test(snowM14$alter, skiM14$alter, alternative= "less", conf.level= niveau)
wilcox.test(snowM18$alter, skiM18$alter, alternative= "less", conf.level= niveau)
wilcox.test(snowM22$alter, skiM22$alter, alternative= "less", conf.level= niveau)


#bootstraptests -> immer wieder neu ziehen und daraus ergibt sich eine Teststatistik und daraus approximative Verteilung
#boxplottest-> Grafik testen (notch= TRUE)


#Deskriptive Auswertung
insgesamt<- quantile(snowboard$alter,c(0,0.25,0.5,0.75,1))
weiblich<- quantile(snowW$alter,c(0,0.25,0.5,0.75,1))
weiblich2014<- quantile(snowW14$alter,c(0,0.25,0.5,0.75,1))
weiblich2018<- quantile(snowW18$alter,c(0,0.25,0.5,0.75,1))
weiblich2022<- quantile(snowW22$alter,c(0,0.25,0.5,0.75,1))


männlich<- quantile(snowM$alter, c(0,0.25,0.5,0.75,1))
männlich2014<- quantile(snowM14$alter, c(0,0.25,0.5,0.75,1))
männlich2018<- quantile(snowM18$alter, c(0,0.25,0.5,0.75,1))
männlich2022<- quantile(snowM22$alter, c(0,0.25,0.5,0.75,1))
datenSnow<-rbind(insgesamt, weiblich, "weiblich 2014" =weiblich2014 ,"weiblich 2018" = weiblich2018, "weiblich 2022"=weiblich2022, männlich, "männlich 2014" = männlich2014, "männlich 2018"= männlich2018, "männlich 2022" = männlich2022)


library(xtable)
xtable(datenSnow)


insgesamt<- quantile(ski$alter,c(0,0.25,0.5,0.75,1))
weiblich<- quantile(skiW$alter,c(0,0.25,0.5,0.75,1))
weiblich2014<- quantile(skiW14$alter,c(0,0.25,0.5,0.75,1))
weiblich2018<- quantile(skiW18$alter,c(0,0.25,0.5,0.75,1))
weiblich2022<- quantile(skiW22$alter,c(0,0.25,0.5,0.75,1))


männlich<- quantile(skiM$alter, c(0,0.25,0.5,0.75,1))
männlich2014<- quantile(skiM14$alter, c(0,0.25,0.5,0.75,1))
männlich2018<- quantile(skiM18$alter, c(0,0.25,0.5,0.75,1))
männlich2022<- quantile(skiM22$alter, c(0,0.25,0.5,0.75,1))
datenSki<-rbind(insgesamt, weiblich, "weiblich 2014" =weiblich2014 ,"weiblich 2018" = weiblich2018, "weiblich 2022"=weiblich2022, männlich, "männlich 2014" = männlich2014, "männlich 2018"= männlich2018, "männlich 2022" = männlich2022)


library(xtable)
xtable(datenSki)

#par(mfrow= c(2,3))
#qqnorm(snowW14$alter, ylim= c(10,50))
#qqline(snowW14$alter)
#qqnorm(snowW18$alter, ylim= c(10,50))
#qqline(snowW18$alter)
#qqnorm(snowW22$alter, ylim= c(10,50))
#qqline(snowW22$alter)

#qqnorm(skiW14$alter, ylim= c(10,50))
#qqline(skiW14$alter)
#qqnorm(skiW18$alter, ylim= c(10,50))
#qqline(skiW18$alter)
#qqnorm(skiW22$alter, ylim= c(10,50))
#qqline(skiW22$alter)


pdf("C:/Users/esthe/Documents/Studium/Statistik/6. Semester/Fallstudien/2/barplotSnow.pdf")
par(mfrow= c(2,3))
barplot(table(snowM14$alter)/length(snowM14$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(snowM18$alter)/length(snowM18$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(snowM22$alter)/length(snowM22$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")

barplot(table(snowW14$alter)/length(snowW14$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(snowW18$alter)/length(snowW18$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(snowW22$alter)/length(snowW22$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
dev.off()

pdf("C:/Users/esthe/Documents/Studium/Statistik/6. Semester/Fallstudien/2/barplotSki.pdf")
par(mfrow= c(2,3))
barplot(table(skiM14$alter)/length(skiM14$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(skiM18$alter)/length(skiM18$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(skiM22$alter)/length(skiM22$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")

barplot(table(skiW14$alter)/length(skiW14$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(skiW18$alter)/length(skiW18$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
barplot(table(skiW22$alter)/length(skiW22$alter),ylim= c(0,0.2),  xlab= "Alter (Jahre)", ylab= "relative Häufigkeit")
dev.off()

table(WinterGames$discipline)
#Ski Snowboard 
#559       190 
#xtable(table(WinterGames$discipline))

Anzahl<- rbind(Snowboarder=c(length(snowW14$alter), length(snowW18$alter), length(snowW22$alter), length(snowM14$alter), length(snowM18$alter), length(snowM22$alter)),
skifahrer=c(length(skiW14$alter), length(skiW18$alter), length(skiW22$alter), length(skiM14$alter), length(skiM18$alter), length(skiM22$alter)))

xtable(Anzahl)

plot(table(WinterGames$alter), xlab= "Alter (Jahre)", ylab= "Absolute Häufigkeit")


