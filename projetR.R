setwd("C:/Users/Juliette/Desktop/MBA/Data Projet")

library(dplyr)
library(readxl)
library(plotly)
library(sqldf)

#Import sample tables

client <- read.csv("CLIENT.csv",sep="|")

#Convert all date variables
client$DATENAISSANCE<- as.Date(client$DATENAISSANCE, format="%d/%m/%Y")
client$DATEDEBUTADHESION<- as.Date(client$DATEDEBUTADHESION, format="%d/%m/%Y")
client$DATEREADHESION<- as.Date(client$DATEREADHESION, format="%d/%m/%Y")
client$DATEFINADHESION<- as.Date(client$DATEFINADHESION, format="%d/%m/%Y")

#------------------------1. ETUDE GLOBAL

#------------1.1 R?partition Adh?rant/VIP


#VIP : client ?tant VIP (VIP = 1)
VIP<-nrow(client[client$VIP==1,])

N <- 2018
#NEW_N2 : client ayant adh?r? au cours de l'ann?e N-2 (date d?but adh?sion)
NEW_N2 <-nrow(client[(client$VIP!=1)&(format(client$DATEDEBUTADHESION,"%Y")==(N-2)),])
#NEW_N1 : client ayant adh?r? au cours de l'ann?e N-1 (date d?but adh?sion)
NEW_N1 <-nrow(client[(client$VIP!=1)&(format(client$DATEDEBUTADHESION,"%Y")==(N-1)),])

#ADHERANT : client toujours en cours d'adh?sion (date de fin d'adh?sion > 2018/01/01)
ADHERANT <-nrow(client[(client$VIP!=1)&format(client$DATEDEBUTADHESION,"%Y")<(N-2)&
                     (format(client$DATEFINADHESION,"%Y-%m-%d"))>as.Date("2018-01-01"),])

#CHURNER : client ayant churner (date de fin d'adh?sion < 2018/01/01)
CHURNER <-nrow(client[((format(client$DATEFINADHESION,"%Y-%m-%d"))<as.Date("2018-01-01"))&
                        (client$VIP!=1)&format(client$DATEDEBUTADHESION,"%Y")<(N-2),])

slices <- c(VIP,NEW_N2,NEW_N1,ADHERANT,CHURNER)
levels <- c("# VIP","# adherant au cours de N-2","# adherant au cours de N-1",
          "# Toujours adherant","Churner")

plot_ly(data.frame(cbind(slices,levels)), labels = ~levels,values=~slices, type="pie")%>%
  layout(legend = list(x = -1, y = 0.9))


