# Chargement des libraries.
require(assertthat)
require(data.table)
require(dplyr)
require(formattable)
require(ggplot2)
require(plotly)
require(rAmCharts)
require(stringr)

# Declaration du répertoire de travail.
chemin_access_environnement_du_projet <- "/Users/nrobin/Documents/GitHub/Rproject/"

# Declaration du chemin du dossier dans lequel se trouve les fichiers de donnees.
chemin_dossier_donnees <- paste0(chemin_access_environnement_du_projet, "DATA/")

# La liste des fichiers de donnees necessaires au projet.
nom_fichier_articles <- "REF_ARTICLE.CSV"
nom_fichier_magasins <- "REF_MAGASIN.CSV"
nom_fichier_clients <- "CLIENT.CSV"
nom_fichier_entetes <- "ENTETES_TICKET_V4.CSV"
nom_fichier_lignes <- "LIGNES_TICKET_V4.CSV"
nom_fichier_INSEE <- "correspondance-code-insee-code-postal.csv"

# Le chemin d'accès complet des fichiers de donnees.
chemin_fichier_articles <- paste0(chemin_dossier_donnees, nom_fichier_articles)
chemin_fichier_magasins <- paste0(chemin_dossier_donnees, nom_fichier_magasins)
chemin_fichier_clients <- paste0(chemin_dossier_donnees, nom_fichier_clients)
chemin_fichier_entetes <- paste0(chemin_dossier_donnees, nom_fichier_entetes)
chemin_fichier_lignes <- paste0(chemin_dossier_donnees, nom_fichier_lignes)
chemin_fichier_INSEE <- paste0(chemin_dossier_donnees, nom_fichier_INSEE)


# Chargement du fichier des articles en 2 secondes
# articles <- read.csv(chemin_fichier_articles, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier des articles en 1 secondes
articles <- fread(chemin_fichier_articles, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)

# Cette ligne de code retire la ligne COUPON.
# Faut-il la retirer ou pas ? N'aura-t-on pas besoin de cette information plus tard ?
#articles <- articles[-c(1),]

# Chargement du fichier magasins en moins d'une seconde
#magasins <- read.csv(chemin_fichier_magasins, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier magasins en moins d'une seconde
magasins <- fread(chemin_fichier_magasins, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)

# Chargement du fichier clients en 6 secondes
#clients <- read.csv(chemin_fichier_clients, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier magasins en 2 secondes
clients <- fread(chemin_fichier_clients, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
clients[clients == ""] <- NA # On met tous les clients vides à "NA".

# Chargement du fichier entetes en 30 secondes.
#entetes <- read.csv(chemin_fichier_entetes, sep="|", header=TRUE, skipNul = T, stringsAsFactors = FALSE)
# Chargement du fichier entetes en 12 secondes.
entetes <- fread(chemin_fichier_entetes, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)

# Chargement du fichier magasins en 120 secondes
#lignes <- read.csv(chemin_fichier_lignes, sep="|", header=TRUE, skipNul = T)
# Chargement du fichier magasins en 30 secondes
lignes <- fread(chemin_fichier_lignes, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)

#------------------------------------------------------------------------------------
# LECTURE DU FICHIER PUBLIC OPEN DATA DE L'INSEE.
#------------------------------------------------------------------------------------

# Il s'agit d'un fichier de correspondance entre le code-insee, le code-postal, la longitude et la latitude.
# Thèmes : Administration, Gouvernement, Finances publiques, Citoyennete
# Licence : Licence Ouverte (Etalab)
# Langue : Français
# Dernière date de mise à jour : 22 avril 2016 16:47
# Producteur : OpenDataSoft
INSEE_CP_FULL_DATATABLE <- fread(chemin_fichier_INSEE, sep = ";", header = TRUE, dec = ",", stringsAsFactors = FALSE)
