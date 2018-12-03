require(dplyr)

articles <- read.csv("DATA/REF_ARTICLE.CSV",sep="|",
                      header=TRUE,skipNul = T,encoding = "UTF-8")
 articles <- articles[-c(1),]
 magasin <- read.csv("DATA/REF_MAGASIN.CSV",sep="|",
                     header=TRUE,skipNul = T,encoding = "UTF-8")
 clients <- read.csv("DATA/SUBCLIENT.CSV",sep="|",
                     header=TRUE,skipNul = T,encoding = "UTF-8", stringsAsFactors = FALSE)
 entete <- read.csv("DATA/SUBENTETE.CSV",sep="|",
                     header=TRUE,skipNul = T,encoding = "UTF-8")
 lignes <- read.csv("DATA/SUBLIGNES.CSV",sep="|",
                     header=TRUE,skipNul = T,encoding = "UTF-8")
 
 Civilite <- clients %>% 
   select(CIVILITE, VIP) %>%  
   group_by(CIVILITE)  %>% 
   count(CIVILITE)

 Civilite_C