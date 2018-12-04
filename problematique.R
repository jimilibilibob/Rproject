setwd("C:/Users/timti/Documents/R/Rproject")

library(dplyr)
library(readxl)
library(xlsx)

mag <- read.csv("DATA_complet/REF_MAGASIN.CSV",sep="|",stringsAsFactors = FALSE)
clients <- read.csv("DATA_complet/CLIENT.CSV",sep="|",stringsAsFactors = FALSE)
entetes <- read.csv("DATA_complet/ENTETE.CSV",sep="|",stringsAsFactors = FALSE,dec=',', skipNul = T)
#lignes <- read.csv("LIGNES_TICKET_V4.CSV",sep="|",stringsAsFactors = FALSE)
entetes$TIC_TOTALTTC <- as.numeric(entetes$TIC_TOTALTTC)
#entetes <- sample_n(entetes, 800000)

#mag[mag$LIBELLEREGIONCOMMERCIALE=="Vente en ligne",]$ï..CODESOCIETE


#Vente en ligne
entetes[entetes$MAG_CODE=="EST",]
nrow(clients[clients$MAGASIN=="EST",])
length(sort(unique(entetes$MAG_CODE)))

#VIP
nrow(clients[clients$VIP==1,])/nrow(clients)

summary(entetes)

temp <- entetes %>% group_by(IDCLIENT,TIC_DATE) %>% summarize(sum = sum(TIC_TOTALTTC)) 
temp$YEAR <- format(as.Date(temp$TIC_DATE),"%Y")
temp$MONTH <- format(as.Date(temp$TIC_DATE),"%m")
temp$count <- 1

test <- temp %>% group_by(IDCLIENT,YEAR,MONTH) %>% summarize(count = sum(count)) 

curve <- test %>% group_by(YEAR,MONTH) %>% summarize(MIN = min(count),
                                                MAX = max(count), 
                                                MOY = mean(count))

curve$DAT <- 1:24

plot.ts(curve[,c("MIN","MOY","MAX")])

curve[,c("MONTH","MIN","MOY","MIN")]

plot(curve$DAT,ylim=range(min(entetes$TIC_TOTALTTC):max(entetes$TIC_TOTALTTC)),
     curve$MIN,type="l",col="red")
lines(curve$DAT,curve$MAX,col="green")
lines(curve$DAT,curve$MOY,col="blue")

plot(curve$DAT, curve$MOY,type="l",col="red")
