# ---------------------------------------------------------------------------------------
#     Fichier : DistanceClientMagasin.r
# Développeur : Nicolas Robin.
#        Date : 14 décembre 2018.
# Description : Ce script renferme l'ensemble des étapes nécessaires demandées 
#               dans la partie : 2.2	Distance CLIENT / MAGASIN.
#
# Objectif    : Calculer la distance qui existe entre le magasin et le client.
# Enoncé      : Les infos disponibles pour le moment sont : 
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

# Inclure le chargement du fichier paramètres qui contient :
# - Le chargement des biliothèques nécessaires à l'éxecutuon des scripts R de ce fichier.
source("Parametres.R")
source("DistanceGeo.r")

setwd(chemin_access_environnement_du_projet)

#------------------------------------------------------------------------------------
# ETAPE 1 - LECTURE DES FICHIERS
#------------------------------------------------------------------------------------

# Chargement du fichier des articles en 2 secondes
#articles <- read.csv(chemin_fichier_articles, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier des articles en 1 secondes
#articles <- fread(chemin_fichier_articles, sep="|", header = TRUE, stringsAsFactors = FALSE)

# Cette ligne de code retire la ligne COUPON.
# Faut-il la retirer ou pas ? N'aura-t-on pas besoin de cette information plus tard ?
#articles <- articles[-c(1),]

# Chargement du fichier magasins en moins d'une seconde
#magasins <- read.csv(chemin_fichier_magasins, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier magasins en moins d'une seconde
magasins <- fread(chemin_fichier_magasins, sep="|", header = TRUE, stringsAsFactors = FALSE)

# Chargement du fichier clients en 6 secondes
#clients <- read.csv(chemin_fichier_clients, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier magasins en 2 secondes
clients <- fread(chemin_fichier_clients, sep="|", header = TRUE, stringsAsFactors = FALSE)
# On met tous les clients vides à "NA".
clients[clients == ""] <- NA

# Chargement du fichier entetes en 30 secondes.
#entetes <- read.csv(chemin_fichier_entetes, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier entetes en 12 secondes.
#entetes <- fread(chemin_fichier_entetes, sep="|", header = TRUE, stringsAsFactors = FALSE)
# Suppression de la colonne entete_num. 
#[entete_num!=0]

# Chargement du fichier magasins en 120 secondes
#lignes <- read.csv(chemin_fichier_lignes, sep="|", header=TRUE, skipNul = T)
# Chargement du fichier magasins en 30 secondes
#lignes <- fread(chemin_fichier_lignes, sep="|", header = TRUE, stringsAsFactors = FALSE)

#------------------------------------------------------------------------------------
# LECTURE DU FICHIER PUBLIC OPEN DATA DE L'INSEE.
#------------------------------------------------------------------------------------

# Il s'agit d'un fichier de correspondance entre le code-insee, le code-postal, la longitude et la latitude.
# Thèmes : Administration, Gouvernement, Finances publiques, Citoyenneté
# Licence : Licence Ouverte (Etalab)
# Langue : Français
# Dernière date de mise à jour : 22 avril 2016 16:47
# Producteur : OpenDataSoft
INSEE_CP_FULL_DATATABLE <- fread("DATA/correspondance-code-insee-code-postal.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

#------------------------------------------------------------------------------------
# ETAPE 2a - CONSTRUCTION ET PREPARATION DE LA "TABLE_DE_TRAVAIL"
#------------------------------------------------------------------------------------
# Cette table est une version aménagée et simplifié de la table de correspondance de l'INSEE.

# On récupère uniquement les 4 colonnes qui nous intéressent :
# - CODEINSEE, Code Postal, Commune et geo_point_2d.
# TABLE_DE_TRAVAIL <- INSEE_CP_FULL_DATATABLE[, list('Code INSEE', 'Code Postal', Commune, geo_point_2d)]
TABLE_DE_TRAVAIL <- INSEE_CP_FULL_DATATABLE[, c(1,2,3, 10)]

# Pour pouvoir faire les jointures avec les tables magasinss et clients :
# 2a1 - on renomme la colonne "Code INSEE" en "CODEINSEE".
setnames(TABLE_DE_TRAVAIL, old=c("Code INSEE"), new=c("CODEINSEE"))
# 2a2 - on renomme la colonne "Code Postal" en "VILLE".
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

# Creation d'une table avec jointure entre la table MAGASINS et la TABLE DE TRAVAIL
jointureGeoMagasins <- magasins %>% 
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
# Tableau INSEE. Je me suis permis de rajouter l'information personnellement,
# dans le sens où c'était la seule ville qui n'était pas répertoriée.
jointureGeoMagasins[VILLE=="LES MILLES", LATITUDEMAG := "43.5042670000"]
jointureGeoMagasins[VILLE=="LES MILLES", LONGITUDEMAG := "5.3916980000"]

#------------------------------------------------------------------------------------
# ETAPE 4 - JOINTURE ENTRE LES TABLES "CLIENTS" et "TABLE DE TRAVAIL"
#------------------------------------------------------------------------------------

# Creation d'une table avec jointure entre la table CLIENTS et la TABLE DE TRAVAIL
positionGeoClients <- clients %>% 
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
jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins > 50), BORNE_DISTANCE:="+ de 50km"]
jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins <= 50), BORNE_DISTANCE:="20 à 50km"]
jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 20), BORNE_DISTANCE:="10 à 20km"]
jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 10), BORNE_DISTANCE:="5 à 10km"]
jointureGeoMagasinsClients[(DISTANCE_CLIENT_magasins < 5), BORNE_DISTANCE:="0 à 5km"]


#--------------------------------------------------------------------------------------------
# ETAPE 7 - AFFICHAGE DES RESULTAS
#--------------------------------------------------------------------------------------------

# Récupération du nombre total des clients.
nb_total_clients <- jointureGeoMagasinsClients %>%
  filter(!is.na(BORNE_DISTANCE)) %>%
  summarise(NB_TOTAL_CLIENTS = n())

#--------------------------------------------------------------------------------------------
# ETAPE 7a - AFFICHAGE DES RESULTAS AU FORMAT GRAPHIQUE : amPie
#--------------------------------------------------------------------------------------------

# Création d'un data frame pour une utilisation spécifique avec amPie.
# avec le pourcentage de cleints pour chacune des bornes définies plus haut.
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

