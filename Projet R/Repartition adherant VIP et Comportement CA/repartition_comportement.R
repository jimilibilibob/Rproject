setwd("C:/Users/Juliette/Desktop/MBA/Data Projet")

library(dplyr)
library(readxl)
library(plotly)
library(data.table)

#Import sample tables

client <- fread("CLIENT.csv",sep="|")
entetes <- fread("ENTETES_TICKET_V4.CSV",sep="|",dec=",")

#Convert all date variables
client$DATENAISSANCE<- as.Date(client$DATENAISSANCE, format="%d/%m/%Y")
client$DATEDEBUTADHESION<- as.Date(client$DATEDEBUTADHESION, format="%d/%m/%Y")
client$DATEREADHESION<- as.Date(client$DATEREADHESION, format="%d/%m/%Y")
client$DATEFINADHESION<- as.Date(client$DATEFINADHESION, format="%d/%m/%Y")

#------------------------1. ETUDE GLOBAL

#------------1.1 Repartition Adherant/VIP


#VIP : client etant VIP (VIP = 1)
VIP<-client[client$VIP==1,]

N <- 2018
#NEW_N2 : client ayant adhere au cours de l'annee N-2 (date d?but adhesion)
NEW_N2 <-client[(client$VIP!=1)&(format(client$DATEDEBUTADHESION,"%Y")==(N-2)),]
#NEW_N1 : client ayant adhere au cours de l'annee N-1 (date debut adhesion)
NEW_N1 <-client[(client$VIP!=1)&(format(client$DATEDEBUTADHESION,"%Y")==(N-1)),]

#ADHERANT : client toujours en cours d'adhesion (date de fin d'adhesion > 2018/01/01)
ADHERANT <-client[(client$VIP!=1)&format(client$DATEDEBUTADHESION,"%Y")<(N-2)&
                     (format(client$DATEFINADHESION,"%Y-%m-%d"))>as.Date("2018-01-01"),]

#CHURNER : client ayant churner (date de fin d'adhesion < 2018/01/01)
CHURNER <-client[((format(client$DATEFINADHESION,"%Y-%m-%d"))<as.Date("2018-01-01"))&
                        (client$VIP!=1)&format(client$DATEDEBUTADHESION,"%Y")<(N-2),]

slices <- c(nrow(VIP),nrow(NEW_N2),nrow(NEW_N1),nrow(ADHERANT),nrow(CHURNER))
levels <- c("# VIP","# adherant au cours de N-2","# adherant au cours de N-1",
          "# Toujours adherant","Churner")

plot_ly(data.frame(cbind(slices,levels)), labels = ~levels,values=~slices, type="pie")%>%
  layout(title = "Repartition Adherant/VIP",legend = list(x = -1, y = 0.9))


#------------1.2	Comportement du CA GLOBAL par client N-2 vs N-1
#Constituer une boite à moustache pour chaque année (N-2 et N-1) comparant 
#le CA TOTAL (TTC) des clients (sommer les achats par client par années)
entetes <- data.table(entetes)
client_CA <- entetes[,list(TOTALCA = sum(TIC_TOTALTTC)),by=list(IDCLIENT,year(TIC_DATE))]

#REMOVE OUTLIERS
client_CA_not_OUT <- subset(client_CA,(TOTALCA>quantile(client_CA$TOTALCA,c(0.01)))&
                            (TOTALCA<quantile(client_CA$TOTALCA,c(0.99))))

plot_ly(type="box") %>%
  add_boxplot(y=~client_CA_not_OUT[client_CA_not_OUT$year==2016]$TOTALCA,
              boxpoints=FALSE,name="2016") %>%
  add_boxplot(y=~client_CA_not_OUT[client_CA_not_OUT$year==2017]$TOTALCA,
              boxpoints=FALSE,name="2017") %>%
  layout(title="Boite à moustache du CA TOTAL des clients par année d'achat",
         yaxis=list(title="CA Total des clients"))
