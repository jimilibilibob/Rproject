# ---------------------------------------------------------------------------------------
#      Fichier : Parametres.R
# Développeurs : Thomas FONTAINE, Dan GOLDMAN, Juliette LEMAINS, Nicolas ROBIN
#         Date : 28 décembre 2018.
#  Description : Ce script inclut :
#                 - Le chargement de l'ensemble des libraries nécessaires au projet.
#                 - La déclaration du répertoires de travail et des fichiers
#                 - Le chargement des fichiers d'entrée.
#
# ---------------------------------------------------------------------------------------

# Chargement des libraries.
require(assertthat)
require(data.table)
require(dplyr)
require(formattable)
require(ggplot2)
require(plotly)
require(rAmCharts)
require(stringr)
require(tcltk)


# La variable booléene "utilise_des_extraits_de_fichier" permet de sélectionner des extraits de fichiers
# plutôt que les fichiers d'entrée volumineux du projet. Cela permet de travailler sur des machines moins véloces.
# Mettre la valeur à TRUE lorsque l'on désire travailler avec des fichiers plus petits.
# Ne pas oublier de mettre cette valeur à FALSE lorsque le projet est finalisé.
utilise_des_extraits_de_fichier <- FALSE

# Chemins de dossier des fichiers de donnée respectifs dans le cas où on n'utilise pas la boite de dialogue.
chemin_dossier_donnees_Thomas <- "C:/Users/timti/Documents/R/Rproject/DATA/"
chemin_dossier_donnees_Dan <- "C:/Users/dgoldman/Desktop/MBA - BIG DATA CDO/DataMining/projet transverse R/Projet R a rendre/DATA/"
chemin_dossier_donnees_Juliette <- "/Users/Juliette/Desktop/MBA/Data Projet/"
chemin_dossier_donnees_Nicolas <- "/Users/nrobin/Documents/GitHub/Rproject/DATA/"

# Fonction "dossierFichiersDonnees".
# Cette fonction renvoie le chemin du dossier où se trouve les fichiers de données d'entrée.
# Si aucun dossier n'est sélectionné, la fonction retourne "NA".
cheminDossierFichiersDonnees <- function(chemin_dossier_donnees_par_defaut) {
  
  # Declaration du chemin du dossier dans lequel se trouve les fichiers de donnees.
  chemin <- tk_choose.dir(getwd(), "Sélectionner le dossier où se trouvent les fichiers d'entrée :")
  
  if (is.na(chemin)) {
    
    chemin <- chemin_dossier_donnees_par_defaut
    
  }else{
    # Ajout d'un "/" pour le dossier sélectionné par l'utilisateur.
    chemin <- paste0(chemin, "/")
  }
  
  # Si la valeur de "chemin_dossier_donnees" n'a pas été renseignée, on affiche un message d'erreur.
  if (is.na(chemin)) {
    tk_messageBox("ok", "Le dossier contenant les fichiers d'entrée n'a pas été sélectionné", caption = "Erreur", default = "")
  }
  
  return(chemin)
}

# Affectation de la variable globale "chemin_dossier_donnees".
# Cette valeur indique le chemin du dossier dans lequel se trouvent tous les fichiers de donnée.
chemin_dossier_donnees <- cheminDossierFichiersDonnees(chemin_dossier_donnees_Thomas)

# La liste des fichiers de donnees necessaires au projet.
nom_fichier_INSEE <- "correspondance-code-insee-code-postal.csv"
nom_fichier_articles <- "REF_ARTICLE.CSV"
nom_fichier_magasins <- "REF_MAGASIN.CSV"

if (utilise_des_extraits_de_fichier == TRUE) {
  nom_fichier_clients <- "SUBCLIENT.CSV"
}else{
  nom_fichier_clients <- "CLIENT.CSV"
}

if (utilise_des_extraits_de_fichier == TRUE) {
  nom_fichier_entetes <- "SUBENTETE.CSV"
}else{
  nom_fichier_entetes <- "ENTETES_TICKET_V4.CSV"
}

if (utilise_des_extraits_de_fichier == TRUE) {
  nom_fichier_lignes <- "SUBLIGNES.CSV"
}else{
  nom_fichier_lignes <- "LIGNES_TICKET_V4.CSV"
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableArticles"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableArticles <- function() {
  
  # information pour la console
  cat("Chargement de la table Articles...","\n")
  
  tableArticles <- NULL
  
  if (!is.na(chemin_dossier_donnees)) {
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier_donnees, nom_fichier_articles)

    # Chargement du fichier des articles en 1 secondes
    tableArticles <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
    # Cette ligne de code retire la ligne COUPON.
    # Faut-il la retirer ou pas ? N'aura-t-on pas besoin de cette information plus tard ?
    # tableArticles <- tableArticles[-c(1),]
  }
  
  returnValue(tableArticles)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableMagasins"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableMagasins <- function() {
  
  # information pour la console
  cat("Chargement de la table Magasins...","\n")

    tableMagasins <- NULL
  
  if (!is.na(chemin_dossier_donnees)) {
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier_donnees, nom_fichier_magasins)
    
    # Chargement du fichier des articles en 1 secondes
    tableMagasins <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }

  returnValue(tableMagasins)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableClients"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableClients <- function() {
  
  # information pour la console
  cat("Chargement de la table Clients","\n")

    tableClients <- NULL
  
  if (!is.na(chemin_dossier_donnees)) {
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier_donnees, nom_fichier_clients)
    
    # Chargement du fichier des articles en 1 secondes
    tableClients <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    tableClients[tableClients == ""] <- NA
    
  }
  
  returnValue(tableClients)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableEntetes"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableEntetes <- function() {
  
  # information pour la console
  cat("Chargement de la table Entetes","\n")

    tableEntetes <- NULL
  
  if (!is.na(chemin_dossier_donnees)) {
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier_donnees, nom_fichier_entetes)
    
    # Chargement du fichier des articles en 1 secondes
    tableEntetes <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }
  
  returnValue(tableEntetes)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableLignes"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableLignes <- function() {
  
  # information pour la console
  cat("Chargement de la table Lignes...","\n")

    tableLignes <- NULL
  
  if (!is.na(chemin_dossier_donnees)) {
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier_donnees, nom_fichier_lignes)
    
    # Chargement du fichier des articles en 1 secondes
    tableLignes <- fread(chemin_fichier, sep="|", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }
  
  returnValue(tableLignes)
}

# ------------------------------------------------------------------------------------
# Fonction "chargementTableInsee"
# Cette fonction charge la table dans la mesure où le dossier a été sélectionné.
# ------------------------------------------------------------------------------------
chargementTableInsee <- function() {
  
  # information pour la console
  cat("Chargement de la table Insee...","\n")

    tableInsee <- NULL
  
  if (!is.na(chemin_dossier_donnees)) {
    
    # Le chemin d'accès complet des fichiers de donnees.
    chemin_fichier <- paste0(chemin_dossier_donnees, nom_fichier_INSEE)
    
    # Chargement du fichier des articles en 1 secondes
    tableInsee <- fread(chemin_fichier, sep=";", header = TRUE, dec = ",", stringsAsFactors = FALSE)
    
  }
  
  returnValue(tableInsee)
}

# Chargement des 5 tables à utiliser en entrée, plus la table Insee.
# Les données peuvent maintenant être traitées pour chacun dees exercices du projet.
articles <- chargementTableArticles()
magasins <- chargementTableMagasins()
clients <- chargementTableClients()
entetes <- chargementTableEntetes()
lignes <- chargementTableLignes()
insee <- chargementTableInsee()

  