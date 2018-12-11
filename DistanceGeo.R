# ---------------------------------------------------------------------------------------
#     Fichier : DistanceGeo.r
# Développeur : Nicolas Robin.
#        Date : 9 décembre 2018.
# Description : Ce fichier renferme la fonction DistanceGeo() qui permet de calculer
#               la distance entre 2 points géographiques.
#               La fonction DistanceGeo2() est une autre façon de calculer la distance
#               entre 2 points géographiques. Elle existe pour information seulement et
#               ne sera pas conservée dans la version finale du fichier.
# ---------------------------------------------------------------------------------------

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



# ---------------------------------------------------------------------------------------
#    Fonction : distanceGeo2()
# Développeur : Nicolas Robin.
#        Date : 9 décembre 2018.
#  Paramètres : lat1:float, lon1:float, lat2:float, lon2:float
# Description : La fonction distanceGeo2 permet de calculer la distance entre 2 points géographiques.
#               Elle prendre 4 variable en compte : 
#               - lat1 et lon1 soit la lattitude et la longitude de l'origine.
#               - lat2 et lon2 soit la lattitude et la longitude de la destination.
#               Cette fonction utilise la fonction distGeo de la library "geosphere",
#               mais celle-ci ne fournit pas exactement les mêmes valeurs que celles rendues sur
#               le site http://www.lexilogos.com/calcul_distances.htm.
#               Nous avons donc préféré utiliser la fonction distanceGeo à distanceGeo2 qui renvoie exactement
#               les mêmes valeurs que celle rendues sur le site http://www.lexilogos.com/calcul_distances.htm
# ---------------------------------------------------------------------------------------
distanceGeo2 <- function(lat1, lon1, lat2, lon2) 
{
  coord_Ville_1 <- matrix(c(lon1, lat1), ncol = 2)
  coord_Ville_2 <- matrix(c(lon2, lat2), ncol = 2)
  
  library(geosphere)
  distanceCalculée <- distGeo(coord_Ville_1, coord_Ville_2)/1000
  
  return(distanceCalculée)
}

# -------------------- PARTIE TEST --------------------

# Calcule de la distance entre les villes de Paris, Bar-le-Duc et Arras.

# Bar-le-Duc : 48.7642280465, 5.16346492169
# Arras : 50.2771637475, 2.71345127608
# Paris : 48.8594154976, 2.37874106024

# Distance Paris et Bar-le-Duc : 204.176 km = la fonction distanceGeo retourne 204.176
# Distance Paris et Arras      : 159.483 km = la fonction distanceGeo retourne 159.483
# Distance Bar-le-Duc et Arras : 159.483 km = la fonction distanceGeo retourne 244.063
# --------------------
# Bar-le-Duc --> Paris
distanceParisBar <- distanceGeo(
  lat1 = 48.7642280465, lon1 = 5.16346492169, 
  lat2 = 48.8594154976, lon2 = 2.37874106024)
distanceParisBar2 <- distanceGeo2(
  lat1 = 48.7642280465, lon1 = 5.16346492169, 
  lat2 = 48.8594154976, lon2 = 2.37874106024)
# --------------------
# Arras --> Paris
distanceParisArras <- distanceGeo(
  lat1 = 50.2771637475, lon1 = 2.71345127608, 
  lat2 = 48.8594154976, lon2 = 2.37874106024)
distanceParisArras2 <- distanceGeo2(
  lat1 = 50.2771637475, lon1 = 2.71345127608, 
  lat2 = 48.8594154976, lon2 = 2.37874106024)
# --------------------
# Bar-le-Duc --> Arras
distanceBarArras <- distanceGeo(
  lat1 = 48.7642280465, lon1 = 5.16346492169,
  lat2 = 50.2771637475, lon2 = 2.71345127608)
distanceBarArras2 <- distanceGeo2(
  lat1 = 48.7642280465, lon1 = 5.16346492169,
  lat2 = 50.2771637475, lon2 = 2.71345127608)
