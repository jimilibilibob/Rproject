


clients_r<-clients%>%mutate(CIVILITE_r=recode(`CIVILITE`,
                                              "MADAME" = "femme",
                                              "Mme" = "femme",
                                              "madame" = "femme",
                                              "monsieur" = "homme",
                                              "Mr" = "homme",
                                              "MONSIEUR" = "homme"))

clients_r$age <- 2018 - as.numeric(format(as.Date(clients_r$DATENAISSANCE, tryFormats = c("%d/%m/%Y")),"%Y"))
  
clients_r<- subset(clients_r, !is.na(DATENAISSANCE))
                   
clients_r<- subset(clients_r, age < 99 ) 

clients_r<- subset(clients_r, age > 17 ) 


clients_r$age_group <- cut(clients_r$age,seq(18,98,10), include.lowest= TRUE, right = FALSE)



labels <- c(seq(100, 0, by=-2),seq(0, 100, by=2))

data <- data.frame(sexe = clients_r$CIVILITE_r,age = clients_r$age, age_group = clients_r$age_group)

row_data <- nrow(data)

data_femme <- subset(data,sexe=="femme")
data_homme <- subset(data,sexe=="homme")
  

gg_par_sexe <-  ggplot(data) +
  aes(x=age_group,fill=sexe) +
  
  geom_bar(data = subset(data,sexe=="femme"),aes(y=((..count..)/sum(..count..)*-100) )) + 
  geom_text(data = subset(data,sexe=="femme"), stat = "count", aes(label = round(..count../sum(..count..)*100,2), y = (..count..)/sum(..count..)*-100))+
  
  
  geom_bar(data = subset(data,sexe=="homme"),aes(y = (..count..)/sum(..count..)*100)) +
  geom_text(data = subset(data,sexe=="homme"),stat = "count", aes(label = round(..count../sum(..count..)*100,2), y = (..count..)/sum(..count..)*100))+
  
  scale_fill_manual(values = c("pink","blue")) + 
  ylab("Proportion par sexe")+
  coord_flip()
plot(gg_par_sexe)

gg_global<-  ggplot(data) +
  aes(x=age_group,fill=sexe) +
  
  geom_bar(data = subset(data,sexe=="femme"),aes(y=((..count..)/row_data*-100) )) + 
  geom_text(data = subset(data,sexe=="femme"), stat = "count", aes(label = round(..count../row_data*100,2), y = (..count..)/row_data*-100),  hjust = -0.01)+
  
  
  geom_bar(data = subset(data,sexe=="homme"),aes(y = (..count..)/row_data*100)) +
  geom_text(data = subset(data,sexe=="homme"),stat = "count", aes(label = round(..count../row_data*100,2), y = (..count..)/row_data*100),  hjust = -0.4)+
  
  scale_fill_manual(values = c("pink","blue")) + 

  ylab("Proportion global")+
  coord_flip()
plot(gg_global)
