#setwd("C:/Users/timti/Documents/R/Rproject")

library(dplyr)
library(readxl)
library(xlsx)

mag <- read.csv("DATA/REF_MAGASIN.CSV",sep="|",stringsAsFactors = FALSE)
clients <- read.csv("DATA/SUBCLIENT.CSV",sep="|",stringsAsFactors = FALSE)
entetes <- read.csv("DATA/SUBENTETE.CSV",sep="|",stringsAsFactors = FALSE,dec=',',skipNul = T)
lignes <- read.csv("DATA/SUBLIGNES.CSV",sep="|",stringsAsFactors = FALSE,skipNul = T,dec=',')
articles <- read.csv("DATA/REF_ARTICLE.CSV",sep="|",stringsAsFactors = FALSE)

entetes$TIC_TOTALTTC <- as.numeric(entetes$TIC_TOTALTTC)
#entetes <- sample_n(entetes, 800000)

#mag[mag$LIBELLEREGIONCOMMERCIALE=="Vente en ligne",]$ï..CODESOCIETE


#--------------------------Vente en ligne
entetes[entetes$MAG_CODE=="EST",]
nrow(clients[clients$MAGASIN=="EST",])
length(sort(unique(entetes$MAG_CODE)))

#------------------------------VIP
nrow(clients[clients$VIP==1,])/nrow(clients)

summary(entetes)

#------------------------------SAISONNALITE DE LA CONSOMMATION CLIENT (PANIER MOYEN)
#COURBE COUNT
temp <- entetes %>% group_by(IDCLIENT,TIC_DATE) %>% summarize(sum = sum(TIC_TOTALTTC)) 
temp$YEAR <- format(as.Date(temp$TIC_DATE),"%Y")
temp$MONTH <- format(as.Date(temp$TIC_DATE),"%m")
temp$count <- 1


test <- temp %>% group_by(IDCLIENT,YEAR,MONTH) %>% summarize(count = sum(count)) 

curveCOUNT <- test %>% group_by(YEAR,MONTH) %>% summarize(MIN = min(count),
                                                MAX = max(count), 
                                                MOY = mean(count))
axe <- paste(curveCOUNT$MONTH,curveCOUNT$YEAR,sep="/")

curveCOUNT$DAT <- 1:24
plot(curveCOUNT$DAT, curveCOUNT$MOY,type="l",col="red",main="Moyenne de visite par clients",
     xlab="Mois", ylab="Prix")

#COURBE CA
temp <- entetes %>% group_by(IDCLIENT,TIC_DATE) %>% summarize(sum = sum(TIC_TOTALTTC)) 
temp$YEAR <- format(as.Date(temp$TIC_DATE),"%Y")
temp$MONTH <- format(as.Date(temp$TIC_DATE),"%m")
temp$count <- 1

curveCA <- temp %>% group_by(YEAR,MONTH) %>% summarize(MIN = min(sum),
                                                     MAX = max(sum), 
                                                     MOY = mean(sum))

curveCA$DAT <- 1:24

plot(curveCA$DAT,ylim=range(min(entetes$TIC_TOTALTTC):max(entetes$TIC_TOTALTTC)),
     curveCA$MIN,type="l",col="red",main="Moyenne/MIN/MAX du CA par clients",
     xlab="Mois", ylab="Prix")
lines(curveCA$DAT,curveCA$MAX,col="green")
lines(curveCA$DAT,curveCA$MOY,col="blue")

plot(curveCA$DAT, curveCA$MOY,type="l",col="red",main="Moyenne du CA par clients",
     xlab="Mois", ylab="Prix")

plot(curveCA$DAT,curveCA$MOY/curveCOUNT$MOY,type="l",main="Panier Moyen",
     xlab="Mois", ylab="Prix du panier moyen",col="red")



#------------------------------Prix différents des articles selon leur magasins
names(entetes)
merge_table <- merge(lignes, entetes, by="ÿþIDTICKET",all.x = TRUE)
#all.x=TRUE signifie qu'on veut garder toutes les lignes de la table lignes

#merge_table <- temp[is.na(temp$TIC_DATE)==FALSE,]

table_diff_price <- merge_table %>% group_by(IDARTICLE,MAG_CODE,TIC_DATE) %>% 
  summarize(PRICE = TOTAL/QUANTITE)

table_variance <- table_diff_price %>% group_by(IDARTICLE,MAG_CODE) %>% 
  summarize(VARIANCE = var(PRICE))