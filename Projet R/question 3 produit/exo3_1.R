unique(articles$)
library(data.table)


#jointure et analyse entre la table article et la table ligne ticket
# il y a plein d'article qui n'ont pas ete acheté
articles<-setDT(articles)
articles[, .(number_of_distinct = length(unique(ï..CODEARTICLE)))] # 665 946

lignes<-setDT(lignes)

lignes[, .(number_of_distinct = length(unique(IDARTICLE)))] # 134 828

ref_article_lignes<-lignes[,.(unique(IDARTICLE))]

head(ref_article_lignes)



ligne_article<-merge(lignes,articles, by.x="IDARTICLE", by.y="ï..CODEARTICLE", all.x=TRUE)
ligne_article[is.na(CODEUNIVERS)==TRUE, unique(IDARTICLE)]
head(ligne_article)

ref_article_lignes<-rename(ref_article_lignes, IDARTICLE = V1)

merge_article<-merge(ref_article_lignes,articles, by.x="IDARTICLE", by.y="ï..CODEARTICLE", all.x=TRUE)

head(merge_article)
merge_article[, .(number_of_distinct = length(unique(IDARTICLE)))] # 134 828
merge_article[is.na(CODEUNIVERS)==TRUE, unique(IDARTICLE)]


merge_article[IDARTICLE==395460,]
articles[ï..CODEARTICLE==395460,]
395460
# ajout de l ID article 395460 pour ne pas perdre l'info
newarticle <- data.frame(ï..CODEARTICLE='395460',CODEUNIVERS='unknown',CODEFAMILLE='unknown', CODESOUSFAMILLE='unknown')
articles<- rbind(articles,newarticle)
merge_article<-merge(ref_article_lignes,articles, by.x="IDARTICLE", by.y="ï..CODEARTICLE", all.x=TRUE)
ligne_article<-merge(lignes,articles, by.x="IDARTICLE", by.y="ï..CODEARTICLE", all.x=TRUE)

#aucune ligne vide pour la table lignes_articles
ligne_article[is.na(TOTAL)==TRUE,]
ligne_article[is.na(MONTANTREMISE)==TRUE,]
ligne_article[is.na(MARGESORTIE)==TRUE,]
ligne_article[is.na(CODEUNIVERS)==TRUE,]

#agregation de la table ligne ticket pour faire un regroupement par code univers et article
agg_ligne_univers<-ligne_article[, .(TOTAL=sum(TOTAL)
                            , MARGE=sum(MARGESORTIE)
                            , REMISE=sum(MONTANTREMISE))
                        ,by=.(IDTICKET,CODEUNIVERS)]
head(agg_ligne_univers)

#analyse de la table agreger pour savoir si il y a plusieurs code univers par IDticket
agg_ligne_univers[,length(IDTICKET)]
agg_ligne_univers[,length(unique(IDTICKET))]
Doublon_idticket <- sqldf("SELECT IDTICKET,COUNT(*) AS DOUBLON
                              
                              FROM agg_ligne_univers
                              
                              GROUP BY
                              IDTICKET having count(*) > 1 ")
agg_ligne_univers[IDTICKET==50100150515,]


merge_ticket_univers<-merge(entete,agg_ligne_univers, by="IDTICKET", all=FALSE)

head(merge_ticket_univers)
#transformation de la date du ticket en format date et ajout de l'année d'achat
setDT(merge_ticket_univers)
merge_ticket_univers[,TIC_DATE2:= as.Date(merge_ticket_univers$TIC_DATE)]
merge_ticket_univers[,TIC_YEAR:= year(merge_ticket_univers$TIC_DATE2)]

merge_ticket_univers[,unique(TIC_YEAR)]

#aggr indicateur par univers pour N-2 c'est à dire 2016
ca_univers_N2<-merge_ticket_univers[TIC_YEAR==2016, .(TOTAL_N2=sum(TOTAL), 
                                                   MARGE_N2=sum(MARGE),
                                                   REMISE_N2=sum(REMISE)),
                                 by=.(CODEUNIVERS)]

head(ca_univers_N2)

#aggr indicateur par univers pour N-1 c'est à dire 2017
ca_univers_N1<-merge_ticket_univers[TIC_YEAR==2017, .(TOTAL_N1=sum(TOTAL), 
                                                      MARGE_N1=sum(MARGE),
                                                      REMISE_N1=sum(REMISE)),
                                    by=.(CODEUNIVERS)]

head(ca_univers_N1)

#fusion par année du ca par univers
ca_variation_univers<-merge(ca_univers_N2,ca_univers_N1, by="CODEUNIVERS", all=TRUE)
head(ca_variation_univers)

#update valeur 0 quand NA
setDT(ca_variation_univers)
ca_variation_univers[is.na(ca_variation_univers)]<-0

#calcul des variations
ca_variation_univers[,VAR_CA:= (
  ((TOTAL_N1-TOTAL_N2)/TOTAL_N2)*100.00
                                )]
ca_variation_univers[,VAR_MARGE:=(
  ((MARGE_N1-MARGE_N2)/MARGE_N2)*100.00
)]
ca_variation_univers[,VAR_REMISE:=(
  ((REMISE_N1-REMISE_N2)/REMISE_N2)*100.00
)]

library(ggplot2)
X11()
ggplot(ca_variation_univers, aes(x=VAR_CA)) + geom_histogram()

ggplot(ca_variation_univers, aes(x=TOTAL_N2)) + geom_histogram()
