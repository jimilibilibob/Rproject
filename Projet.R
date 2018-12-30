source("Parametres.R")

# =======================================================================================
#
# FONCTIONS - BOITE A OUTILS DU PROJET
#
# =======================================================================================

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
#    Fonction : toRadians()
# Développeur : Nicolas Robin.
#        Date : 9 décembre 2018.
#  Paramètres : degree:float
# Description : Cette fonction convertit une valeur exprimée en degrés en radians.
# ---------------------------------------------------------------------------------------
toRadians <- function (degree)
{
  return (degree * pi / 180)
}

# ---------------------------------------------------------------------------------------
#    Fonction : distanceGeo()
# Développeur : Nicolas Robin.
#        Date : 9 décembre 2018.
#  Paramètres : lat1:float, lon1:float, lat2:float, lon2:float
# Description : La fonction permet de calculer la distance entre 2 points géographiques.
#               Elle prendre 4 variable en compte : 
#               - lat1 et lon1 soit la lattitude et la longitude de l'origine.
#               - lat2 et lon2 soit la lattitude et la longitude de la destination.
#               Nous aurions pu par exemple utiliser la fonction distGeo de library(geosphere)
#               mais celle-ci ne fournit pas exactement les mêmes valeurs que celles rendues sur
#               le site http://www.lexilogos.com/calcul_distances.htm
#               Pour que la fonction soit parfaitement conforme à la référence,
#               nous avons décidé d'utiliser la FORMULE DE HAVERSINE en l'état.
#               Cette formule permet de déterminer la distance entre deux points d'une sphère,
#               en fonction des valeurs de longitude et latitude des 2 points géographiques en degrée.
#               Voir: https://fr.wikipedia.org/wiki/Formule_de_haversine
# ---------------------------------------------------------------------------------------
distanceGeo <- function(lat1, lon1, lat2, lon2) 
{
  rayonTerre <- 6371e3
  φ1 <- toRadians(lat1)
  φ2 <- toRadians(lat2)
  Δφ <- toRadians(lat2-lat1)
  Δλ <- toRadians(lon2-lon1)
  a <- (sin(Δφ/2)^2) + cos(φ1) * cos(φ2) * (sin(Δλ/2)^2)
  unMoinsA <- 1 - a
  c <- (2 * atan2(sqrt(a),sqrt(unMoinsA)))
  
  distanceCalculée <- rayonTerre * c / 1000
  
  return(distanceCalculée)
}

# =======================================================================================
#
# LES FONCTIONS LISTEES DANS L'ORDRE DES ENONCES DU PROJET R
#
# =======================================================================================


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
  ADHERANT <-table_client[(table_client$VIP!=1)&
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
#  Paramètres : table_entetes
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

# ---------------------------------------------------------------------------------------
#    Fonction : proportion_sexe_age_1.3
# Développeur : Thomas Fontaine
#        Date : 29 décembre 2018.
#  Paramètres : table_clients
# Description : Cette fonction etudie la repartition par sexe et par age et la represente par
#                 un graphique en bar.
# ---------------------------------------------------------------------------------------
proportion_sexe_age_1.3 <- function(table_clients){
    
  # Creation d'une table resultats a partir de la table clients. La table est cree avec une normalisation
  # de la civilite.
  clients_r<-table_clients%>%mutate(CIVILITE_r=recode(`CIVILITE`,
                                                "MADAME" = "femme",
                                                "Mme" = "femme",
                                                "madame" = "femme",
                                                "monsieur" = "homme",
                                                "Mr" = "homme",
                                                "MONSIEUR" = "homme"))
  # Suppression des lignes clients sans date de naissance.
  clients_r<- subset(clients_r, !is.na(DATENAISSANCE))
  # creation d'une colonne age.
  clients_r$age <- 2018 - as.numeric(format(as.Date(clients_r$DATENAISSANCE, tryFormats = c("%d/%m/%Y")),"%Y"))
  # Suppression des clients agees de plus de 98ans.
  clients_r<- subset(clients_r, age < 99 ) 
  # Suppression des clients agees de moins de 18ans.
  clients_r<- subset(clients_r, age > 17 ) 
  # la population etudier sera donc les personnes qui ont renseiger leurs age et qui on actuellement entre 
  # 18 et 98 ans (inclus).
  # Creation des groupe client suivant leurs ages. 8 groupes de 18 � 98 ans.
  clients_r$age_group <- cut(clients_r$age,seq(18,98,10), include.lowest= TRUE, right = FALSE)
  #Creation d'une label pour l'affichage des resultats.
  labels <- c(seq(100, 0, by=-2),seq(0, 100, by=2))
  # Creation d'une table pour l'affichage des resultats.
  data <- data.frame(sexe = clients_r$CIVILITE_r,age = clients_r$age, age_group = clients_r$age_group)
  # Nombre de ligne dans la table data.
  row_data <- nrow(data)
  # Table data des femmes.
  data_femme <- subset(data,sexe=="femme")
  # Table data des hommes.
  data_homme <- subset(data,sexe=="homme")
  # Premier plot, repartition par sexe, ici les 18.89% en rose � gauche, repr�sente la proportion de femme
  # de 58 � 67 ans par rapport au nombre total de femme.
  gg_par_sexe <-  ggplot(data) +
    aes(x=age_group,fill=sexe) +
    geom_bar(data = subset(data,sexe=="femme"),aes(y=((..count..)/sum(..count..)*-100) )) + 
    geom_text(data = subset(data,sexe=="femme"), stat = "count", aes(label = round(..count../sum(..count..)*100,2), y = (..count..)/sum(..count..)*-100))+
    geom_bar(data = subset(data,sexe=="homme"),aes(y = (..count..)/sum(..count..)*100)) +
    geom_text(data = subset(data,sexe=="homme"),stat = "count", aes(label = round(..count../sum(..count..)*100,2), y = (..count..)/sum(..count..)*100))+
    scale_fill_manual(values = c("pink","blue")) + 
    ylab("Proportion par sexe")+
    coord_flip()
  plot(gg_par_sexe)
  
  # Second plot, repartition total, ici les 11.47% en rose � gauche, repr�sente la proportion de femme
  # de 58 � 67 ans par rapport au nombre total de la population
  gg_global<-  ggplot(data) +
    aes(x=age_group,fill=sexe) +
    geom_bar(data = subset(data,sexe=="femme"),aes(y=((..count..)/row_data*-100) )) + 
    geom_text(data = subset(data,sexe=="femme"), stat = "count", aes(label = round(..count../row_data*100,2), y = (..count..)/row_data*-100),  hjust = -0.01)+
    geom_bar(data = subset(data,sexe=="homme"),aes(y = (..count..)/row_data*100)) +
    geom_text(data = subset(data,sexe=="homme"),stat = "count", aes(label = round(..count../row_data*100,2), y = (..count..)/row_data*100),  hjust = -0.4)+
    scale_fill_manual(values = c("pink","blue")) + 
    ylab("Proportion global")+
    coord_flip()
  plot(gg_global)
}

# ---------------------------------------------------------------------------------------
#    Fonction : resultat_magasin_2.1
# Développeur : Thomas Fontaine
#        Date : 29 décembre 2018.
#  Paramètres : clients, magasins, entetes
# Description : Cette fonction etudie les revenues et le nombre de client des magasins, ainsi que
#                 l'evolution entre l'annee 2016 et 2017.
# ---------------------------------------------------------------------------------------
resultat_magasin_2.1 <- function(table_clients, table_magasins, table_entetes) {
  # Comptage du nombre de client adherent par magasin
  nombre_client_magasin <- table_clients %>% group_by(MAGASIN) %>%   summarise(nombreClient = n())
  # Creation de la table resultat
  cat("Creation de la table resultat...","\n")
  resultat <- merge(x=table_magasins, y=nombre_client_magasin, by.x="CODESOCIETE", by.y="MAGASIN")
  # Creation de la table representant l'activite de chaque magasin sur les deux annees
  cat("Creation de la table activite_magasin...","\n")
  activite_magasin <- table_entetes %>% group_by(MAG_CODE,IDCLIENT,year = format(as.Date(table_entetes$TIC_DATE),'%Y'))  
  # Creation de la table representant l'activite de chaque magasin sur l'annee n-2
  activite_magasin_n2 <- subset(activite_magasin, activite_magasin$year == '2016') %>% group_by(MAG_CODE) %>% summarise(clientActifN2 = n())
  # Creation de la table representant l'activite de chaque magasin sur l'annee n-1
  activite_magasin_n1 <- subset(activite_magasin, activite_magasin$year == '2017') %>% group_by(MAG_CODE) %>% summarise(clientActifN1 = n())
  # Ajout de l'activite des magasins en n-2 au resultat
  resultat <- merge(x=resultat, y=activite_magasin_n2, by.x="CODESOCIETE", by.y="MAG_CODE")
  # Ajout de l'activite des magasins en n-1 au resultat
  resultat <- merge(x=resultat, y=activite_magasin_n1, by.x="CODESOCIETE", by.y="MAG_CODE")
  # Creation d'une colonne mesurant la variation de cette activite
  resultat$evolutionClientActif <- round(resultat$clientActifN2/resultat$clientActifN1 * 100 -100,2)
  # Creation de la table representant le total ttc de chaque magasin sur les deux annees
  cat("Creation de la table TOTALTCC_magasin...","\n")
  TOTALTCC_magasin <- table_entetes %>% group_by(MAG_CODE,year = format(as.Date(table_entetes$TIC_DATE),'%Y'))  %>%   summarise(TOTAL_TTC = sum(TIC_TOTALTTC))
  # Creation de la table representant le total ttc de chaque magasin sur l'annee n-2
  TOTALTTC_magasin_n2 <- subset(TOTALTCC_magasin, TOTALTCC_magasin$year == '2016') %>% summarise(TOTAL_TTCN1 = TOTAL_TTC )
  # Creation de la table representant le total ttc de chaque magasin sur l'annee n-1
  TOTALTTC_magasin_n1 <- subset(TOTALTCC_magasin, TOTALTCC_magasin$year == '2017')  %>% summarise(TOTAL_TTCN2 = TOTAL_TTC )
  # Ajout du total ttc des magasins en n-2 au resultat
  resultat <- merge(x=resultat, y=TOTALTTC_magasin_n2, by.x="CODESOCIETE", by.y="MAG_CODE")
  # Ajout du total ttc des magasins en n-1 au resultat
  resultat <- merge(x=resultat, y=TOTALTTC_magasin_n1, by.x="CODESOCIETE", by.y="MAG_CODE")
  # Creation d'une colonne mesurant la variation du total total ttc
  resultat$evolutionTOTALTTC <- resultat$TOTAL_TTCN2 - resultat$TOTAL_TTCN1 
  # Boucle permettetant la creation de l'indice d'evolution de chaque magasins
  cat("Creation de la table TOTALTCC_magasin'index...","\n")
  for(i in c(1:nrow(resultat))){
    if(resultat$evolutionClientActif[i] >= 0 & resultat$evolutionTOTALTTC[i] >= 0){
      resultat$indices[i] <- 1
    }else if(resultat$evolutionClientActif[i] < 0 & resultat$evolutionTOTALTTC[i] < 0){
      resultat$indices[i] <- -1
    }else{
      resultat$indices[i] <- 0
    }
  }
  #Tri du la table resultat en fonction de cette index
  resultat <- resultat[order(-rank(resultat$indices))]
  # Calcul des valeurs necessaire a l'ajotu d'une ligne total
  cat("Creation de la ligne total...","\n")
  total_Client <- sum(as.integer(resultat$nombreClient), na.rm = TRUE)
  total_clientActifN2 <- sum(as.integer(resultat$clientActifN2), rm.na= TRUE)
  total_clientActifN1 <- sum(as.integer(resultat$clientActifN1), rm.na= TRUE)
  total_evolutionClientActif <- round(total_clientActifN2/total_clientActifN1 * 100 -100,2)
  total_TOTALTTC_magasin_n2 <- sum(as.numeric(resultat$TOTAL_TTCN2), rm.na= TRUE)
  total_TOTALTTC_magasin_n1 <- sum(as.numeric(resultat$TOTAL_TTCN1), rm.na= TRUE)
  total_evolutionTOTALTTC <- total_TOTALTTC_magasin_n2 - total_TOTALTTC_magasin_n1
  total_indice <- NULL
  if(total_evolutionClientActif >= 0 & total_evolutionTOTALTTC >= 0){
    total_indice <- 1
  }else if(total_evolutionClientActif < 0 & total_evolutionTOTALTTC < 0){
    total_indice <- -1
  }else{
    total_indice <- 0
  }
  total <- data.frame("Total",
                      "France",
                      as.integer(00),
                      "France", 
                      total_Client,
                      total_clientActifN2,
                      total_clientActifN1,
                      total_evolutionClientActif,
                      total_TOTALTTC_magasin_n2, 
                      total_TOTALTTC_magasin_n1,
                      total_evolutionTOTALTTC,
                      total_indice
  )
  # Chagement du nom des colonnes de la table resultat
  colnames(resultat) <- c("Code Magasin", "Ville", "Departement","Region","Nombre Adherent", "Client Actif N-2","Client Actif N-1", "Evolution client Actif", "Total TTC N-2","Total TTC N-1", "Evolution Total TTC", "Indices ?volutions")
  # Chagement du nom des colonnes de la table total
  names(total) <- c("Code Magasin", "Ville", "Departement","Region","Nombre Adherent", "Client Actif N-2","Client Actif N-1", "Evolution client Actif", "Total TTC N-2","Total TTC N-1", "Evolution Total TTC", "Indices ?volutions")
  # Ajout de la ligne total
  resultat <- rbind(resultat,total)
  # creation du texte formatter utiliser sur l'evolution des clients actif et du total ttc
  color_text_formatter <- formatter("span", 
                                    style = x ~ formattable::style(color = ifelse(x > 0, "green", 
                                                                                  ifelse(x < 0, "red", "black"))))
  #  creation du  formatter utiliser pour representer l'index
  improvement_formatter <- formatter("span", 
                                     style = x ~ formattable::style(font.weight = "bold", 
                                                                    color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))), 
                                     x ~ icontext(ifelse(x > 0, "arrow-up", ifelse(x < 0, "arrow-down", "arrow-right")), text = list(NULL))
  )
  #affichage du formattable
  cat("Affichage du formattable...","\n")
  formattable(resultat, list("Evolution client Actif" = color_text_formatter, "Evolution Total TTC"= color_text_formatter,  "Nombre Adherent" = color_bar("lightblue"), "Indices ?volutions" = improvement_formatter))
  
}

# ---------------------------------------------------------------------------------------
#    Fonction : distance_Client_Magasin_2.2
#  Paramètres : 
# Développeur : Nicolas Robin.
#        Date : 14 décembre 2018.
# Description : Ce script renferme l'ensemble des étapes nécessaires demandées 
#               dans la partie : 2.2	Distance CLIENT / MAGASIN.
#
#    Objectif : Calculer la distance qui existe entre le magasin et le client.
#      Enoncé : Les infos disponibles pour le moment sont : 
#               -	la ville du magasin
#               -	le code insee du client
#               Il faut télécharger les données GPS des villes et code-insee pour pouvoir calculer la distance :
#                -	https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/
#
#               Une fois les données acquises, il faut lier les données GPS composé de la latitude et de la 
#               longitude au client et au magasin.
#               (Constituer pour chaque client et chaque magasin 2 colonnes : latitude et longitude).
#
#               Créer une fonction qui détermine la distance entre 2 points.
#               La fonction doit prendre 4 variable en compte : latitude1, longitude1, latitude2, longitude2.
#
#               Pour savoir si la fonction est correcte : http://www.lexilogos.com/calcul_distances.htm
#
#               Constituer une représentation (tableau ou graphique --> au choix) représentant le nombre de client par 
#               distance : 0 à 5km, 5km à 10km, 10km à 20km, 20km à 50km, plus de 50km.
# ---------------------------------------------------------------------------------------

distance_Client_Magasin_2.2 <- function(table_insee, table_magasins, table_clients) {

  #------------------------------------------------------------------------------------
  # ETAPE 2a - CONSTRUCTION ET PREPARATION DE LA "TABLE_DE_TRAVAIL"
  #------------------------------------------------------------------------------------
  # Cette table est une version aménagée et simplifiée de la table de correspondance de l'INSEE.
  
  # On récupère uniquement les 4 colonnes qui nous intéressent :
  # - CODEINSEE, Code Postal, Commune et geo_point_2d.
  # TABLE_DE_TRAVAIL <- table_insee[, list('Code INSEE', 'Code Postal', Commune, geo_point_2d)]
  TABLE_DE_TRAVAIL <- table_insee[, c(1,2,3, 10)]
  
  # Pour pouvoir faire les jointures avec les tables magasinss et clients :
  # 2a1 - on renomme la colonne "Code INSEE" en "CODEINSEE".
  setnames(TABLE_DE_TRAVAIL, old=c("Code INSEE"), new=c("CODEINSEE"))
  # 2a2 - on renomme la colonne "Code Postal" en "NUMDEPT".
  setnames(TABLE_DE_TRAVAIL, old=c("Code Postal"), new=c("NUMDEPT"))
  # 2a3 - on renomme la colonne "Commune" en "VILLE".
  setnames(TABLE_DE_TRAVAIL, old=c("Commune"), new=c("VILLE"))
  # 2a4 - On "splite" les informations contenues dans geo_point_2d et on les insère dans les 2 colonnes "LATITUDE" et "LONGITUDE".
  TABLE_DE_TRAVAIL[, c("LATITUDE", "LONGITUDE") := tstrsplit(geo_point_2d, ", ", fixed=TRUE)]
  TABLE_DE_TRAVAIL[,geo_point_2d:=NULL] # on élimine la colonne "geo_point_2d" devenue inutile.
  # On tronque le numéro de code postal pour obtenir le numéro de département qui nous permmettra
  # plus tard de discriminer plusieurs communes françaises avec le même nom.
  TABLE_DE_TRAVAIL[,NUMDEPT:=str_trunc(NUMDEPT, 2, "right", "")]
  TABLE_DE_TRAVAIL[,NUMDEPT :=as.integer(NUMDEPT)] 
  
  #------------------------------------------------------------------------------------
  # ETAPE 2b - PHASE DE TRAITEMENT DES DONNEES - RETABLISSEMENT DE LA COHERENCE
  #------------------------------------------------------------------------------------
  # Cette phase permet de rétablir la cohérence entre les tables 
  # TABLE_DE_TRAVAIL, magasins et clients.
  
  # On retire tous les "-" entre les mots des villes afin de ramener
  # de la cohérence dans le champ VILLE entre la table de travail et celui du fichier magasins.
  TABLE_DE_TRAVAIL[,VILLE:=str_replace_all(VILLE, pattern = "-", replacement = " ")]
  
  # On retire tous les "-" entre les mots des villes afin de ramener
  # de la cohérence avec les villes du fichier magasins.
  magasins[, VILLE:=str_replace_all(VILLE, pattern = "-", replacement = " ")]
  
  # On remplace tous les "ST" par "SAINT", toujours pour amener de la cohérence
  # avec les villes du fichier CODE INSEE et le fichier magasins.
  magasins[, VILLE:=str_replace(VILLE, pattern = "^[S-T,s-t][S-T,s-t][ ]", replacement = "SAINT ")]
  
  # On retire le mot "CEDEX" trouvé dans certains noms de villes.
  magasins[, VILLE:=str_replace(VILLE, pattern = " CEDEX", replacement = " ")]
  magasins[, VILLE:=str_replace(VILLE, pattern = "^\\s+|\\s+$", replacement = "")]
  
  # Dans l'objectif de pouvoir faire une jointure entre la TABLE_DE_TRAVAIL et la TABLE magasins
  setnames(magasins, old=c("CODESOCIETE"), new=c("MAGASINS"))
  setnames(magasins, old=c("LIBELLEDEPARTEMENT"), new=c("NUMDEPT"))
  
  # Dans l'objectif de pouvoir faire une jointure entre la TABLE_DE_TRAVAIL et la TABLE magasins
  setnames(clients, old=c("MAGASIN"), new=c("MAGASINS"))
  
  #------------------------------------------------------------------------------------
  # ETAPE 3 - JOINTURE ENTRE LES TABLES "MAGASINS" et "TABLE DE TRAVAIL"
  #------------------------------------------------------------------------------------
  
  # Création d'une table avec jointure entre la table MAGASINS et la TABLE DE TRAVAIL
  jointureGeoMagasins <- table_magasins %>% 
    left_join(TABLE_DE_TRAVAIL, by = c("VILLE", "NUMDEPT")) %>% 
    select(MAGASINS, NUMDEPT, VILLE, LATITUDE, LONGITUDE)
  
  #------------------------------------------------------------------------------------
  # ETAPE 3a - PREPARATION DE LA TABLE jointureGeoMagasins
  #------------------------------------------------------------------------------------
  
  #Transformation de la table en DATA TABLE
  setDT(jointureGeoMagasins)
  
  # Renommage des colonnes
  setnames(jointureGeoMagasins, old=c("LATITUDE"), new=c("LATITUDEMAG"))
  setnames(jointureGeoMagasins, old=c("LONGITUDE"), new=c("LONGITUDEMAG"))
  
  # La ville "LES MILLES" dans La table MAGASINS n'est pas répertoriée dans le
  # Tableau INSEE. Nous avons rajouté l'information "manuellement", 
  # puisque c'était la seule ville qui n'était pas répertoriée.
  jointureGeoMagasins[VILLE=="LES MILLES", LATITUDEMAG := "43.5042670000"]
  jointureGeoMagasins[VILLE=="LES MILLES", LONGITUDEMAG := "5.3916980000"]
  
  #------------------------------------------------------------------------------------
  # ETAPE 4 - JOINTURE ENTRE LES TABLES "CLIENTS" et "TABLE DE TRAVAIL"
  #------------------------------------------------------------------------------------
  
  # Création d'une table avec jointure entre la table CLIENTS et la TABLE DE TRAVAIL
  positionGeoClients <- table_clients %>% 
    left_join(TABLE_DE_TRAVAIL, by = c("CODEINSEE")) %>% 
    select(IDCLIENT, CODEINSEE, VILLE, MAGASINS, LATITUDE, LONGITUDE)
  
  #Transformation de la table en DATA TABLE
  setDT(positionGeoClients)
  
  # Renommage des colonnes
  setnames(positionGeoClients, old=c("LATITUDE"), new=c("LATITUDECLIENT"))
  setnames(positionGeoClients, old=c("LONGITUDE"), new=c("LONGITUDECLIENT"))
  
  #------------------------------------------------------------------------------------
  # ETAPE 5 - JOINTURE ENTRE LES TABLES "jointureGeoMagasins" et "jointureGeoClients"
  #------------------------------------------------------------------------------------
  
  # Creation d'une table avec jointure entre la table jointureGeoMagasins" et la "jointureGeoClients"
  # Cette table enfin finalisée va permettre de calculer la distance client <-> magasin pour chacun
  # des clients de la table clients.
  jointureGeoMagasinsClients <- positionGeoClients %>% 
    left_join(jointureGeoMagasins, by = c("MAGASINS")) %>% 
    select(IDCLIENT, LATITUDECLIENT, LONGITUDECLIENT, MAGASINS, LATITUDEMAG, LONGITUDEMAG)
  
  #Transformation de la table en DATA TABLE
  setDT(jointureGeoMagasinsClients)
  
  # Les paramètres de latitude et de longitude étaient pour l'instant disponibles en tant
  # que chaînes de caractères. Il convient donc de convertir chacun d'eux en valeur décimale
  # avant de lancer le calcul de la distance entre la ville du client et celle du magasin.
  jointureGeoMagasinsClients[,LATITUDECLIENT :=as.double(LATITUDECLIENT)] 
  jointureGeoMagasinsClients[,LONGITUDECLIENT :=as.double(LONGITUDECLIENT)] 
  jointureGeoMagasinsClients[,LATITUDEMAG :=as.double(LATITUDEMAG)] 
  jointureGeoMagasinsClients[,LONGITUDEMAG :=as.double(LONGITUDEMAG)] 
  
  #--------------------------------------------------------------------------------------------
  # ETAPE 6 - AFFECTATION DE LA DISTANCE AVEC LA FONCTION distanceGeo(lat1, long1, lat2, long2)
  #--------------------------------------------------------------------------------------------
  
  # C'est ici que l'on calcule la distance entre les 2 localités.
  jointureGeoMagasinsClients[,DISTANCE_CLIENT_magasins
                             :=distanceGeo(LATITUDECLIENT, LONGITUDECLIENT, LATITUDEMAG, LONGITUDEMAG)]
  
  #------------------------------------------------------------------------------------
  # Ensuite j'aurais aimé utiliser la fonction retourneValeurBorne(distance) qui renvoie une valeur bornée
  # sous forme de chaînes de caractères, mais la fonction ne fonctionne pas correctement lorsqu'on l'utilise
  # avec la library data.table.
  # jointureGeoMagasinsClients[,BORNE_DISTANCE:=retourneValeurBorne(DISTANCE_CLIENT_magasins)]
  #------------------------------------------------------------------------------------
  # A la place j'ai utilisé cette stratégie.
  # Cela fonctionne, mais c'est beaucoup moins élégant !
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins > 50), BORNE_DISTANCE:="plus de 50km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins <= 50), BORNE_DISTANCE:="20 à 50km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 20), BORNE_DISTANCE:="10 à 20km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 10), BORNE_DISTANCE:="5 à 10km"]
  jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 5), BORNE_DISTANCE:="0 à 5km"]
  
  
  #--------------------------------------------------------------------------------------------
  # ETAPE 7 - AFFICHAGE DES RESULTATS
  #--------------------------------------------------------------------------------------------
  
  # Récupération du nombre total des clients.
  nb_total_clients <- jointureGeoMagasinsClients %>%
    filter(!is.na(BORNE_DISTANCE)) %>%
    summarise(NB_TOTAL_CLIENTS = n())
  
  #--------------------------------------------------------------------------------------------
  # ETAPE 7a - AFFICHAGE DES RESULTAS AU FORMAT GRAPHIQUE : amPie
  #--------------------------------------------------------------------------------------------
  
  # Création d'un data frame pour une utilisation spécifique avec amPie.
  # avec le pourcentage de clients pour chacune des bornes définies plus haut.
  info_distance_pour_amPie <- jointureGeoMagasinsClients %>%
    # Faut-il retirer les "NA" ou les inclure dans les résultats ?
    filter(!is.na(BORNE_DISTANCE)) %>%
    # Avec group_by on regroupe les clients en fonction des 5 intervalles de distances
    group_by(BORNE_DISTANCE) %>%
    # On compte avec n() le nombre de clients et on calcule le pourcentage de chacun pour chaque intervalle
    summarise(value = n(), 
              POURCENTAGE_CLIENTS = round((value/nb_total_clients$NB_TOTAL_CLIENTS*100),2)) %>% 
    arrange(desc(POURCENTAGE_CLIENTS)) %>%
    # Ici on "créé/sélectionne" 2 colonnes obligatoire pour un amPie : la colonne label et la colonne value.
    select(label = BORNE_DISTANCE, value)
  
  # Affiche un "beignet" avec un trou en son centre de 50 et une épaisseur de 10.
  # Ce graphique affiche le pourcentage de chacune des populations selon la distance avec leur magasin.
  amPie(data = info_distance_pour_amPie, inner_radius = 50, depth = 10, show_values = TRUE, legend = TRUE)
  
  #--------------------------------------------------------------------------------------------
  # ETAPE 7b - AFFICHAGE DES RESULTAS AU FORMAT TABLEAU : formattable
  #--------------------------------------------------------------------------------------------
  
  # Cette fois-ci création d'un data frame pour une utilisation spécifique avec formattable
  # avec le pourcentage de clients pour chacune des bornes définies plus haut.
  info_distance_pour_formattable <- jointureGeoMagasinsClients %>%
    # Faut-il retirer les "NA" ou les inclure dans les résultats ?
    filter(!is.na(BORNE_DISTANCE)) %>%
    # Avec group_by on regroupe les clients en fonction des 5 intervalles de distances
    group_by(BORNE_DISTANCE) %>%
    # On compte avec n() le nombre de clients et on calcule le pourcentage de chacun pour chaque intervalle
    summarise(NB_CLIENTS = n(), 
              POURCENTAGE_CLIENTS = round((NB_CLIENTS/nb_total_clients$NB_TOTAL_CLIENTS*100),2)) %>% 
    arrange(desc(NB_CLIENTS)) %>% 
    # Ici on "créé/sélectionne" 3 colonnes "DISTANCE", "CLIENTS" et "POURCENTAGE DE CLIENTS".
    select(DISTANCE = BORNE_DISTANCE, CLIENTS = NB_CLIENTS, 'POURCENTAGE DE CLIENTS' = POURCENTAGE_CLIENTS)
  
  # Affiche un tableau avec un barre permettant de mieux visualiser le pourcenatge de la population.
  formattable(info_distance_pour_formattable,
              list('POURCENTAGE DE CLIENTS' = normalize_bar("cornflowerblue", 0.14)),
              check.rows = FALSE,
              check.names = TRUE,
              align=c("l","l","r")
  )
  
}

#Autres fonctions des autres exos

# ---------------------------------------------------------------------------------------
#    Fonction : etude_par_univers_3.1()
# Développeur : Dan Goldman.
#        Date : 30 décembre 2018.
#  Paramètres : 
# Description : Cette fonction réalise histogramme du CA par code univers et année
#               
# ---------------------------------------------------------------------------------------

etude_par_univers_3.1<- function(table_article,table_ligne_ticket, table_entete_ticket){
  #renommer l'id code article correctement
  article<-rename(table_article, IDARTICLE = ï..CODEARTICLE)
  setDT(article)
  
  #Il y a un article acheté dans la table ligne ticket qui n'existe pas dans la table article
  # ajout de l ID article 395460 dans la table article
  newarticle <- data.frame(IDARTICLE='395460',CODEUNIVERS='unknown',CODEFAMILLE='unknown', CODESOUSFAMILLE='unknown')
  article<- rbind(article,newarticle)
  setDT(article)
  
  #jointure interne entre la table ligne ticket et la table article
  #exclusion de tous les articles qui n'ont jamais ete acheté
  ligne_article<-merge(table_ligne_ticket,article, by="IDARTICLE", all=FALSE)
  setDT(ligne_article)
  
  #agregation de la table ligne article pour faire un regroupement par code univers et ID ticket
  ligne_group_by_univers<-ligne_article[, .(TOTAL=sum(TOTAL))
                                        ,by=.(IDTICKET,CODEUNIVERS)]
  setDT(ligne_group_by_univers)
  
  #jointure interne entre la table entete ticket et la table ligne univers
  entete_ligne_univers<-merge(table_entete_ticket,ligne_group_by_univers, by="IDTICKET", all=FALSE)
  setDT(entete_ligne_univers)
  
  #transformation de la date du ticket en format date et ajout de l'année d'achat
  #Modification du type pour le champs TIC_DATE en format date
  entete_ligne_univers[,TIC_DATE:= as.Date(entete_ligne_univers$TIC_DATE)]
  #recuperer l'année d'achat à partir du champs TIC_DATE transformé en format date
  entete_ligne_univers[,TIC_YEAR:= year(entete_ligne_univers$TIC_DATE)]
  #renommer le champ tic_year en année
  entete_ligne_univers<-rename(entete_ligne_univers, ANNEE = TIC_YEAR)
  
  #agregation du CA par code univers et par année
  univers<-entete_ligne_univers[, .(TOTAL=sum(TOTAL)),
                                by=.(CODEUNIVERS, ANNEE)]
  
  
  #afficher le graphe
  ggplot(univers,aes(x=CODEUNIVERS,y=TOTAL,fill=factor(ANNEE)))+
    geom_bar(stat="identity",position="dodge")+
    scale_fill_discrete(name="ANNEE")+
    xlab("UNIVERS")+ylab("TOTAL CA")+ 
    scale_y_continuous(labels = c("0","2 M","4 M","6 M","8 M"))
}


# ---------------------------------------------------------------------------------------
#    Fonction : top_par_univers_3.2()
# Développeur : Dan Goldman.
#        Date : 30 décembre 2018.
#  Paramètres : 
# Description : Cette fonction réalise un graphe de label presentant le top 5 de produit famille par univers
#               
# ---------------------------------------------------------------------------------------

top_par_univers_3.2<- function(table_article,table_ligne_ticket){
  #copie de la table article et renommer l'id code article
  article<-rename(table_article, IDARTICLE = ï..CODEARTICLE)
  
  #Il y a un article acheté dans la table ligne ticket qui n'existe pas dans la table article
  # ajout de l ID article 395460 dans la table article
  newarticle <- data.frame(IDARTICLE='395460',CODEUNIVERS='unknown',CODEFAMILLE='unknown', CODESOUSFAMILLE='unknown')
  article<- rbind(article,newarticle)
  setDT(article)
  
  #jointure en left outer join entre article et ligne
  #on recupere tous les articles
  article_ligne<-merge(article,table_ligne_ticket, by="IDARTICLE", all.x=TRUE)
  setDT(article_ligne)
  
  #remplacer le NA par 0 dans la margesortie
  article_ligne<-article_ligne[is.na(MARGESORTIE)==TRUE, MARGESORTIE:=0]
  
  #agregation de la marge par code univers et code famille
  top_univers_famille<-article_ligne[, .(MARGE=sum(MARGESORTIE)), by=.(CODEUNIVERS, CODEFAMILLE)]
  
  #trie de la table top_univers_famille
  top_univers_famille<-top_univers_famille[order(CODEUNIVERS,MARGE, decreasing = TRUE),]
  
  #créé un rank de la marge par code univers
  top_univers_famille <- within(top_univers_famille, rank <- ave(MARGE, CODEUNIVERS,
                                                                 FUN=function(x)rev(order(x))))
  
  #afficher le resultat du rank par code univers et code famille
  top_univers_famille<-subset(top_univers_famille, rank<=5)
  
  #afficher le graphe
  ggplot(top_univers_famille, aes(x = CODEUNIVERS, y = MARGE, label = CODEFAMILLE, fill=CODEUNIVERS)) +
    geom_label()
}

# =======================================================================================
#
# EXECUTION DU CODE
#
# =======================================================================================

# Chargement des tables :
# L'ensemble des tables définies et nécessaires au projet, sont chargées depuis le
# fichier "Paramètres.R".

# ---------------------------------------------------------------------------------------
# 1 -	ETUDE GLOBALE
# ---------------------------------------------------------------------------------------

# 1.1	Répartition Adhérant / VIP.
repartition_adherant_vip_1.1(2018,convert_date_client(clients))

# 1.2	Comportement du CA GLOBAL par client N-2 vs N-1.
comportement_CA_1.2(entetes)

# 1.3	Répartition par age x sexe.

proportion_sexe_age_1.3(clients)

# ---------------------------------------------------------------------------------------
# 2 -	ETUDE PAR MAGASIN
# ---------------------------------------------------------------------------------------

# 2.1	Résultat par magasin (+1 ligne Total).
resultat_magasin_2.1(clients, magasins, entetes) 
  
# 2.2	Distance CLIENT <-> MAGASIN.
distance_Client_Magasin_2.2(insee, magasins, clients)

# ---------------------------------------------------------------------------------------
# 3 -	ETUDE PAR UNIVERS
# ---------------------------------------------------------------------------------------

# 3.1 - Affichage d'un histogramme N-2 / N-1 évolution du CA par univers.
etude_par_univers_3.1(articles,lignes,entetes)

# 3.2 - Affichage du top 5 des familles les plus rentable par univers.
top_par_univers_3.2(articles,lignes)
