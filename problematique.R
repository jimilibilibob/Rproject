#setwd("C:/Users/Juliette/Desktop/MBA/Data Projet/")

library(dplyr)
library(readxl)
library(xlsx)
library(ggplot2)
library(reshape2)

mag <- read.csv("REF_MAGASIN.CSV",sep="|",stringsAsFactors = FALSE)
clients <- read.csv("CLIENT.CSV",sep="|",stringsAsFactors = FALSE)
entetes <- read.csv("ENTETES_TICKET_V4.CSV",sep="|",stringsAsFactors = FALSE,dec=',')
#lignes <- read.csv("LIGNES_TICKET_V4.CSV",sep="|",stringsAsFactors = FALSE)
entetes$TIC_TOTALTTC <- as.numeric(entetes$TIC_TOTALTTC)
#entetes <- sample_n(entetes, 800000)

mag[mag$LIBELLEREGIONCOMMERCIALE=="Vente en ligne",]$ï..CODESOCIETE


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
temp$DAY <- format(as.Date(temp$TIC_DATE),"%d")


curve <- temp %>% group_by(YEAR,MONTH) %>% summarize(MIN = min(sum),
                                                MAX = max(sum), 
                                                MOY = mean(sum))

curve$DAT <- 1:24

plot.ts(curve[,c("MIN","MOY","MAX")])

curve[,c("MONTH","MIN","MOY","MIN")]

plot(curve$DAT,ylim=range(min(entetes$TIC_TOTALTTC):max(entetes$TIC_TOTALTTC)),
     curve$MIN,type="l",col="red")
lines(curve$DAT,curve$MAX,col="green")
lines(curve$DAT,curve$MOY,col="blue")

