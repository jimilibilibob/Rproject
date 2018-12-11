#setwd("C:/Users/jlemains/Documents/projet_R_2018")

library(quadprog)
library(matlib)
library(restriktor)
library(data.table)
library(readxl)
library(rlist)
library(plyr)
library(xlsx)
library(prob)
library(MASS)
library(plotly)

clients <- read.csv("DATA/CLIENT.CSV",sep="|")
lignes <- read.csv("DATA/LIGNES_TICKET_V4.CSV",sep="|",dec=",")
articles <- read.csv("DATA/REF_ARTICLE.CSV",sep="|",dec=",")
names(articles)<-c("IDARTICLE",names(articles)[2:4])
  
summary(clients)

clients_VIP <- clients[clients$VIP==1,]

price_articles <- lignes %>% group_by(IDARTICLE) %>% summarise(MOY_MONTANT = mean(TOTAL/QUANTITE))

prix <- merge(price_articles,articles,by="IDARTICLE",all.y=TRUE)

prix_par_univers <- prix %>% group_by(CODEUNIVERS) %>% summarise(MontantMoyen = mean(MOY_MONTANT,na.rm = TRUE))
prix_par_famille <- prix %>% group_by(CODEFAMILLE) %>% summarise(MontantMoyen = mean(MOY_MONTANT,na.rm = TRUE))
