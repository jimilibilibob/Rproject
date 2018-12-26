

head(ligne_article)
#agregation de la marge par code univers et code famille
agg_famille_univers<-ligne_article[, .(MARGE=sum(MARGESORTIE)), by=.(CODEUNIVERS, CODEFAMILLE)]
head(agg_famille_univers)
#trie de la table agg_famille_univers
agg_famille_univers<-agg_famille_univers[order(CODEUNIVERS,MARGE, decreasing = TRUE),]

#créé un rank de la marge par code univers
agg_famille_univers <- within(agg_famille_univers, rank <- ave(MARGE, CODEUNIVERS,
                                                               FUN=function(x)rev(order(x))))

#afficher le resultat du rank par code univers et code famille
result<-subset(agg_famille_univers, rank<=5)

X11()
ggplot(data = result, aes(x=CODEUNIVERS, y=CODEFAMILLE, fill=rank)) + 
  geom_tile()


                                  