# Chargement des libraries.
library(dplyr)
library(data.table)
library(readxl)
library(assertthat)
library(xlsx)
library(stringr)
library(rAmCharts)
library(formattable)
library(RSQLite)
library(proto)
library(gsubfn)
library(sqldf)
library(ggplot2)

## IMPORT DES DONNEES
#import de la table article source
articles_in <- read.csv2("C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet transverse R/Projet R a rendre/DATA/REF_ARTICLE.csv", header = T, na.strings = "",
                      stringsAsFactors = FALSE, sep = "|", dec =",")

#import de la table ligne ticket
lignes_in <- read.csv2("C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet transverse R/Projet R a rendre/DATA/LIGNES_TICKET.csv", header = T, na.strings = "",
                    stringsAsFactors = FALSE, sep = "|", dec =",")

#import de la table entete ticket
entete_in <- read.csv2("C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet transverse R/Projet R a rendre/DATA/ENTETES_TICKET.csv", header = T, na.strings = "",
                    stringsAsFactors = FALSE, sep = "|", dec =",")


#copie de la table article et renommer l'id code article
article<-rename(articles_in, IDARTICLE = ï..CODEARTICLE)



## TRAITEMENT DES DONNEES
#Il y a un article acheté dans la table ligne ticket qui n'existe pas dans la table article
# ajout de l ID article 395460 dans la table article
newarticle <- data.frame(IDARTICLE='395460',CODEUNIVERS='unknown',CODEFAMILLE='unknown', CODESOUSFAMILLE='unknown')
article<- rbind(article,newarticle)



#jointure interne entre la table ligne ticket et la table article
#exclusion de tous les articles qui n'ont jamais ete acheté
ligne_article<-merge(lignes_in,article, by="IDARTICLE", all=FALSE)



#agregation de la table ligne article pour faire un regroupement par code univers et ID ticket
ligne_univers<-ligne_article[, .(TOTAL=sum(TOTAL))
                        ,by=.(IDTICKET,CODEUNIVERS)]
ligne_univers<-ligne_article[, .(TOTAL=sum(TOTAL))
                             ,by=.(IDTICKET,CODEUNIVERS)]

#jointure interne entre la table entete ticket et la table ligne univers
entete_ligne_univers<-merge(entete_in,ligne_univers, by="IDTICKET", all=FALSE)


#transformation de la date du ticket en format date et ajout de l'année d'achat
setDT(entete_ligne_univers)
#Modification du type pour le champs TIC_DATE en format date
entete_ligne_univers[,TIC_DATE:= as.Date(entete_ligne_univers$TIC_DATE)]
#recuperer l'année d'achat à partir du champs TIC_DATE transformé en format date
entete_ligne_univers[,TIC_YEAR:= year(entete_ligne_univers$TIC_DATE)]

ca_univers_toto<-entete_ligne_univers[, .(TOTAL=sum(TOTAL)),
                                    by=.(CODEUNIVERS, TIC_YEAR)]

#table agrégé des CA par univers produit pour l'année N-2 c'est à dire 2016
ca_univers_N2<-entete_ligne_univers[TIC_YEAR==2016, .(TOTAL=sum(TOTAL), 
                                                   MARGE=sum(MARGE),
                                                   REMISE=sum(REMISE)),
                                 by=.(CODEUNIVERS, TIC_YEAR)]


#table agrégé des CA par univers produit pour l'année N-1 c'est à dire 2017
ca_univers_N1<-entete_ligne_univers[TIC_YEAR==2017, .(TOTAL=sum(TOTAL), 
                                                      MARGE=sum(MARGE),
                                                      REMISE=sum(REMISE)),
                                    by=.(CODEUNIVERS,TIC_YEAR )]

#union des tables agrégés des CA par univers pour l'année N-2 et l'année N-1
ca_univers<-rbind(ca_univers_N2,ca_univers_N1)

#renommer le champ tic_year en année
ca_univers<-rename(ca_univers, ANNEE = TIC_YEAR)

library(ggplot2)
X11()
ggplot(ca_univers,aes(x=CODEUNIVERS,y=TOTAL,fill=factor(ANNEE)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="ANNEE")+
  xlab("UNIVERS")+ylab("TOTAL CA")

#autre fonction pour renommer
#ggplot(ca_univers,aes(x=CODEUNIVERS,y=TOTAL,fill=factor(TIC_YEAR)))+
 # geom_bar(stat="identity",position="dodge")+
  #scale_fill_discrete(name="TIC_YEAR",
   #                   breaks=c(2016, 2017),
    #                  labels=c("2016", "2017"))+
  #xlab("UNIVERS")+ylab("TOTAL CA")





#renommer le champ tic_year en tic_year_N1
#ca_univers_N1<-rename(ca_univers_N1, TIC_YEAR_N1 = TIC_YEAR)

#fusion par année du ca par univers
ca_univers<-merge(ca_univers_N2,ca_univers_N1, by="CODEUNIVERS", all=TRUE)




## autre calcul et fonction
#update valeur 0 quand NA
setDT(ca_univers)
ca_univers[is.na(ca_univers)]<-0



#calcul des variations
ca_univers[,VAR_CA:= (
  ((TOTAL_N1-TOTAL_N2)/TOTAL_N2)*100.00
                                )]
ca_univers[,VAR_MARGE:=(
  ((MARGE_N1-MARGE_N2)/MARGE_N2)*100.00
)]
ca_univers[,VAR_REMISE:=(
  ((REMISE_N1-REMISE_N2)/REMISE_N2)*100.00
)]


X11()
