require(dplyr)

articles <- read.csv("DATA_UTF-8/REF_ARTICLE.CSV",sep="|",
                     header=TRUE,skipNul = T,stringsAsFactors = FALSE)
articles <- articles[-c(1),]
magasin <- read.csv("DATA/REF_MAGASIN.CSV",sep="|",
                    header=TRUE,skipNul = T,stringsAsFactors = FALSE)
clients <- read.csv("C:\\Users\\timti\\Downloads\\projet_R_2018\\DATA\\CLIENT.CSV",sep="|",
                    header=TRUE,skipNul = T,stringsAsFactors = FALSE)
clients[clients == ""] <- NA
entete <- read.csv("C:\\Users\\timti\\Downloads\\projet_R_2018\\DATA\\a.CSV",sep="|",
                    header=TRUE,skipNul = T,stringsAsFactors = FALSE)
lignes <- read.csv("C:\\Users\\timti\\Downloads\\projet_R_2018\\DATA\\LIGNES_TICKET_V4.CSV",sep="|",
                    header=TRUE,skipNul = T)
 
 #test <- clients %>%    group_by(dose) %>%   summarise(note =  count("CIVILITE")  )  
  
 

CODEUNIVERS_tab <- count(articles, CODEUNIVERS) 

for(i in 1:nrow(CODEUNIVERS_tab)){
  CODEUNIVERS_tab[i,3] <- CODEUNIVERS_tab[i,2]/count(articles)*100
}

clients_r<-clients%>%mutate(CIVILITE_r=recode(`CIVILITE`,
                                          "MADAME" = "Madame",
                                          "Mme" = "Madame",
                                          "madame" = "Madame",
                                          "monsieur" = "Monsieur",
                                          "Mr" = "Monsieur",
                                          "MONSIEUR" = "Monsieur"))

CIVILITE_tab <- count(clients_r, CIVILITE_r)

for(i in 1:nrow(CIVILITE_tab)){
  CIVILITE_tab[i,3] <- CIVILITE_tab[i,2]/count(clients_r)*100
}


MAG_CODE_tab <- count(entete,MAG_CODE)
for(i in 1:nrow(MAG_CODE_tab)){
  MAG_CODE_tab[i,3] <- MAG_CODE_tab[i,2]/count(entete)*100
}

IDCLIENT_tab <- count(entete,IDCLIENT)
for(i in 1:nrow(IDCLIENT_tab)){
  IDCLIENT_tab[i,3] <- IDCLIENT_tab[i,2]/count(entete)*100
}

test <- count(lignes,MONTANTREMISE)
