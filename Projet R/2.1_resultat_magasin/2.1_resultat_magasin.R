

nombre_client_magasin <- clients %>% group_by(MAGASIN) %>%   summarise(nombreClient = n())

resultat <- merge(x=magasins, y=nombre_client_magasin, by.x="CODESOCIETE", by.y="MAGASIN")

activite_magasin <- entetes %>% group_by(MAG_CODE,IDCLIENT,year = format(as.Date(entetes_tickets$TIC_DATE),'%Y'))  

activite_magasin_n2 <- subset(activite_magasin, activite_magasin$year == '2016') %>% group_by(MAG_CODE) %>% summarise(clientActifN2 = n())

activite_magasin_n1 <- subset(activite_magasin, activite_magasin$year == '2017') %>% group_by(MAG_CODE) %>% summarise(clientActifN1 = n())

resultat <- merge(x=resultat, y=activite_magasin_n2, by.x="CODESOCIETE", by.y="MAG_CODE")

resultat <- merge(x=resultat, y=activite_magasin_n1, by.x="CODESOCIETE", by.y="MAG_CODE")

resultat$evolutionClientActif <- round(resultat$clientActifN2/resultat$clientActifN1 * 100 -100,2)


TOTALTCC_magasin <- entetes_tickets %>% group_by(MAG_CODE,year = format(as.Date(entetes_tickets$TIC_DATE),'%Y'))  %>%   summarise(TOTAL_TTC = sum(TIC_TOTALTTC))

TOTALTTC_magasin_n2 <- subset(TOTALTCC_magasin, TOTALTCC_magasin$year == '2016') %>% summarise(TOTAL_TTCN1 = TOTAL_TTC )

TOTALTTC_magasin_n1 <- subset(TOTALTCC_magasin, TOTALTCC_magasin$year == '2017')  %>% summarise(TOTAL_TTCN2 = TOTAL_TTC )

resultat <- merge(x=resultat, y=TOTALTTC_magasin_n2, by.x="CODESOCIETE", by.y="MAG_CODE")

resultat <- merge(x=resultat, y=TOTALTTC_magasin_n1, by.x="CODESOCIETE", by.y="MAG_CODE")


resultat$evolutionTOTALTTC <- resultat$TOTAL_TTCN2 - resultat$TOTAL_TTCN1 

for(i in c(1:nrow(resultat))){
  if(resultat$evolutionClientActif[i] >= 0 & resultat$evolutionTOTALTTC[i] >= 0){
    resultat$indices[i] <- 1
  }else if(resultat$evolutionClientActif[i] < 0 & resultat$evolutionTOTALTTC[i] < 0){
    resultat$indices[i] <- -1
  }else{
    resultat$indices[i] <- 0
  }
}

resultat <- resultat[order(-rank(resultat$indices))]

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

colnames(resultat) <- c("Code Magasin", "Ville", "Departement","Region","Nombre Adherent", "Client Actif N-2","Client Actif N-1", "Evolution client Actif", "Total TTC N-2","Total TTC N-1", "Evolution Total TTC", "Indices évolutions")

names(total) <- c("Code Magasin", "Ville", "Departement","Region","Nombre Adherent", "Client Actif N-2","Client Actif N-1", "Evolution client Actif", "Total TTC N-2","Total TTC N-1", "Evolution Total TTC", "Indices évolutions")

resultat <- rbind(resultat,total)

color_text_formatter <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))
color_text_formatter(c(-1, 0, 1))

improvement_formatter <- formatter("span", 
                                   style = x ~ style(font.weight = "bold", 
                                                     color = ifelse(x > 0, "green", ifelse(x < 0, "red", "black"))), 
                                   x ~ icontext(ifelse(x > 0, "arrow-up", ifelse(x < 0, "arrow-down", "arrow-right")), text = list(NULL))
)

formattable(resultat, list("Evolution client Actif" = color_text_formatter, "Evolution Total TTC"= color_text_formatter,  "Nombre Adherent" = color_bar("lightblue"), "Indices évolutions" = improvement_formatter))

