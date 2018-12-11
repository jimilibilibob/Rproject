setwd("C:/Users/timti/Documents/R/Rproject")
require(dplyr)
require(ggplot2)


clients <- read.csv("DATA_COMPLET/CLIENT.CSV",sep="|",stringsAsFactors = FALSE)

clients_r<-clients%>%mutate(CIVILITE_r=recode(`CIVILITE`,
                                              "MADAME" = "femme",
                                              "Mme" = "femme",
                                              "madame" = "femme",
                                              "monsieur" = "homme",
                                              "Mr" = "homme",
                                              "MONSIEUR" = "homme"))

sexe <- c()
age <- c() 
c <- 1

#for(i in c(1:nrow(clients_r))){
#  if(!is.na(clients_r$CIVILITE_r[i]) && clients_r$DATENAISSANCE[i] != "" ){
#    sexe[c] <- clients_r$CIVILITE_r[i]
#    age[c] <- 2018 - as.numeric(format(as.Date(clients_r$DATENAISSANCE[i], tryFormats = c("%d/%m/%Y")),"%Y"))
#    c <- c+1
#    }
#}

for(i in c(1:nrow(clients_r))){
  if(clients_r$DATENAISSANCE[i] != ""){
    sexe_c <- 2018 - as.numeric(format(as.Date(clients_r$DATENAISSANCE[i], tryFormats = c("%d/%m/%Y")),"%Y"))
    if(!is.na(clients_r$CIVILITE_r[i]) &&  sexe_c >17 && sexe_c < 99){
      sexe[c] <- clients_r$CIVILITE_r[i]
      age[c] <- sexe_c
      c <- c+1
    }
  }
}

data <- data.frame(sexe = sexe,age = age, age_group = age_group)

row_data <- as.numeric(nrow(data))

age_group <- cut(age,seq(18,98,10), include.lowest= TRUE, right = FALSE)
limits <- seq(row_data*-1, row_data, by=row_data/50)
labels <- c(seq(100, 0, by=-2),seq(0, 100, by=2))
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
