# Chargement des libraries.
library(dplyr)
library(data.table)
library(readxl)
library(assertthat)
library(stringr)
library(rAmCharts)
library(formattable)

# Déclaration de l'environnment de travail.
chemin_access_environnement_du_projet <- "/Users/nrobin/Documents/GitHub/Rproject/"

# Déclaration du chemin du dossier dans lequel se trouve les fichiers de données.
chemin_dossier_données <- paste0(chemin_access_environnement_du_projet, "DATA/")

# La liste des fichiers de données nécessaires au projet.
nom_fichier_articles <- "REF_ARTICLE.CSV"
nom_fichier_magasins <- "REF_MAGASIN.CSV"
nom_fichier_clients <- "CLIENT.CSV"
nom_fichier_entetes <- "ENTETES_TICKET_V4.CSV"
nom_fichier_lignes <- "LIGNES_TICKET_V4.CSV"
nom_fichier_INSEE <- "correspondance-code-insee-code-postal.csv"

# Le chemin d'accès complet des fichiers de données.
chemin_fichier_articles <- paste0(chemin_dossier_données, nom_fichier_articles)
chemin_fichier_magasins <- paste0(chemin_dossier_données, nom_fichier_magasins)
chemin_fichier_clients <- paste0(chemin_dossier_données, nom_fichier_clients)
chemin_fichier_entetes <- paste0(chemin_dossier_données, nom_fichier_entetes)
chemin_fichier_lignes <- paste0(chemin_dossier_données, nom_fichier_lignes)
chemin_fichier_INSEE <- paste0(chemin_dossier_données, nom_fichier_INSEE)

