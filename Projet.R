source("Parametres.R")

# ---------------------------------------------------------------------------------------
#
#1. Etude Global (fonctions)
#
#---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#    Fonction : convert_date_client()
# Développeur : Juliette Lemains.
#        Date : 28 décembre 2018.
#  Paramètres : Data Frame
# Description : Cette fonction réalise convertit les dates en objet "Date".
# ---------------------------------------------------------------------------------------

convert_date_client <- function(data){
  for (i in names(data)){
    if (substr(i,1,4)=="DATE") {data[[i]]<- as.Date(data[[i]], format="%d/%m/%Y")}
    }
  return(data)
}


# ---------------------------------------------------------------------------------------
#    Fonction : repartition_adherant_vip_1.1()
# Développeur : Juliette Lemains.
#        Date : 28 décembre 2018.
#  Paramètres : 
# Description : Cette fonction réalise un camembert 
#               représentant la repartition entre differents clients.
# ---------------------------------------------------------------------------------------

repartition_adherant_vip_1.1 <- function(ANNEE,table_client){
  
  #VIP : client etant VIP (VIP = 1)
  VIP <- table_client[table_client$VIP==1,]

  #NEW_N2 : client ayant adhere au cours de l'annee N-2 (date debut adhesion)
  NEW_N2 <-table_client[(table_client$VIP!=1)&
                          (format(table_client$DATEDEBUTADHESION,"%Y")==(ANNEE-2)),]
  
  #NEW_N1 : client ayant adhere au cours de l'annee N-1 (date debut adhesion)
  NEW_N1 <-table_client[(table_client$VIP!=1)&
                          (format(table_client$DATEDEBUTADHESION,"%Y")==(ANNEE-1)),]
  
  #ADHERANT : client toujours en cours d'adhesion (date de fin d'adhesion > 2018/01/01)
  ADHERANT <-table_client[(client$VIP!=1)&
                            format(table_client$DATEDEBUTADHESION,"%Y")<(ANNEE-2)&
                            (format(table_client$DATEFINADHESION,"%Y-%m-%d"))>as.Date("2018-01-01"),]
  
  #CHURNER : client ayant churner (date de fin d'adhesion < 2018/01/01)
  CHURNER <-table_client[((format(table_client$DATEFINADHESION,"%Y-%m-%d"))<as.Date("2018-01-01"))&
                           (table_client$VIP!=1)&
                           format(table_client$DATEDEBUTADHESION,"%Y")<(ANNEE-2),]
  
  slices <- c(nrow(VIP),nrow(NEW_N2),nrow(NEW_N1),nrow(ADHERANT),nrow(CHURNER))
  
  levels <- c("# VIP","# adherant au cours de N-2","# adherant au cours de N-1",
              "# Toujours adherant","Churner")
  
  plot_ly(data.frame(cbind(slices,levels)), labels = ~levels,values=~slices, type="pie")%>%
    layout(title = "Repartition Adherant/VIP",legend = list(x = -1, y = 0.9))
}

# ---------------------------------------------------------------------------------------
#    Fonction : comportement_CA_1.2()
# Développeur : Juliette Lemains.
#        Date : 28 décembre 2018.
#  Paramètres : 
# Description : Cette fonction constitue une boite a moustache 
#               comparant le CA des clients.
# ---------------------------------------------------------------------------------------

comportement_CA_1.2 <- function(table_entetes){
  
  client_CA <- table_entetes[,list(TOTALCA = sum(TIC_TOTALTTC)),by=list(IDCLIENT,year(TIC_DATE))]
  
  #Supression des valeurs aberrantes
  client_CA_not_OUT <- subset(client_CA,(TOTALCA>quantile(client_CA$TOTALCA,c(0.01)))&
                                (TOTALCA<quantile(client_CA$TOTALCA,c(0.99))))
  
  plot_ly(type="box") %>%
    add_boxplot(y=~client_CA_not_OUT[client_CA_not_OUT$year==2016]$TOTALCA,
                boxpoints=FALSE,name="2016") %>%
    add_boxplot(y=~client_CA_not_OUT[client_CA_not_OUT$year==2017]$TOTALCA,
                boxpoints=FALSE,name="2017") %>%
    layout(title="Boite à moustache du CA TOTAL des clients par année d'achat",
           yaxis=list(title="CA Total des clients"))
  }









#Autres fonctions des autres exos







# ---------------------------------------------------------------------------------------
#
#Execution du code
#
#---------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------
#
#1. Etude Global (fonctions)
#
#---------------------------------------------------------------------------------------

repartition_adherant_vip_1.1(2018,convert_date_client(client))
comportement_CA_1.2(entetes)
