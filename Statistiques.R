require(dplyr)
require(readxl)
require(assertthat)
require(xlsx)


wb<-createWorkbook(type="xlsx")
# Define some cell styles
#++++++++++++++++++++
# Title and sub title styles
TITLE_STYLE <- CellStyle(wb)+ Font(wb,heightInPoints=16,color="blue", isBold=TRUE, underline=1)
SUB_TITLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=TRUE, isBold=FALSE)
TITLE_TABLE_STYLE <- CellStyle(wb) + Font(wb,  heightInPoints=14, isItalic=FALSE, isBold=TRUE,color="chocolate1")

# Styles for the data table row/column names
TABLE_ROWNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE)
TABLE_COLNAMES_STYLE <- CellStyle(wb) + Font(wb, isBold=TRUE) +
  Alignment(wrapText=TRUE)

xlsx.addTitle<-function(sheet, rowIndex, title, titleStyle){
  rows <-createRow(sheet,rowIndex=rowIndex)
  sheetTitle <-createCell(rows, colIndex=1)
  setCellValue(sheetTitle[[1,1]], title)
  xlsx::setCellStyle(sheetTitle[[1,1]], titleStyle)
}


articles <- read.csv("DATA/REF_ARTICLE.CSV",sep="|",
                     header=TRUE,skipNul = T,encoding = "UTF-8")
articles <- articles[-c(1),]
magasin <- read.csv("DATA/REF_MAGASIN.CSV",sep="|",
                    header=TRUE,skipNul = T,encoding = "UTF-8")
clients <- read.csv("DATA/SUBCLIENT.CSV",sep="|",
                    header=TRUE,skipNul = T,encoding = "UTF-8")
entete <- read.csv("DATA/SUBENTETE.CSV",sep="|",
                    header=TRUE,skipNul = T,encoding = "UTF-8")
lignes <- read.csv("DATA/SUBLIGNES.CSV",sep="|",
                    header=TRUE,skipNul = T,encoding = "UTF-8")

#ARTICLES

# Add title
xlsx.addTitle(sheet1, rowIndex=1, title="Statistiques sur ARTICLES",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=2,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

stat <- data.frame(matrix(nrow = 2,ncol=length(colnames(articles))))
colnames(stat)<- colnames(articles)

for (i in colnames(articles)){
  stat[[i]]<- c(length(unique(articles[[i]])),
                sum(is.na(articles[[i]]))/length(articles[[i]])*100)
}
row.names(stat) <- c("Unique values","Taux de valeurs manquantes")

# Creation de la feuille Statistiques"
addDataFrame(stat, sheet1, startRow=4, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)



#MAGASIN
# Add title
xlsx.addTitle(sheet1, rowIndex=10, title="Statistiques sur MAGASIN",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=11,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

stat <- data.frame(matrix(nrow = 2,ncol=length(colnames(magasin))))
colnames(stat)<- colnames(magasin)

for (i in colnames(magasin)){
  stat[[i]]<- c(length(unique(magasin[[i]])),
                sum(is.na(magasin[[i]]))/length(magasin[[i]])*100)
}
row.names(stat) <- c("Unique values","Taux de valeurs manquantes")

# Creation de la feuille Statistiques"
addDataFrame(stat, sheet1, startRow=15, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
xlsx::setColumnWidth(sheet1, colIndex=c(1:ncol(stat)), colWidth=20)


#CLIENTS
# Add title
xlsx.addTitle(sheet1, rowIndex=20, title="Statistiques sur CLIENTS",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=21,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

colcat <- colnames(clients)[!colnames(clients) %in% c("DATEDEBUTADHESION",
                                                      "DATEREADHESION",
                                                      "DATEFINADHESION",
                                                      "DATENAISSANCE")]
coldate <- c("DATEDEBUTADHESION","DATEREADHESION","DATEFINADHESION",
             "DATENAISSANCE")

statcat <- data.frame(matrix(nrow = 2,ncol=length(colcat)))
colnames(statcat)<- colcat

for (i in colcat){
  statcat[[i]]<- c(length(unique(clients[[i]])),
                sum(is.na(clients[[i]]))/length(clients[[i]])*100)
}

statdate <- data.frame(matrix(nrow = 3,ncol=length(coldate)))
colnames(statdate)<- coldate


for (i in coldate){
  statdate[[i]]<- c(min(as.Date(clients[[i]],"%d/%m/%Y"),na.rm = TRUE),max(as.Date(clients[[i]],"%d/%m/%Y"),na.rm = TRUE),
                sum(is.na(clients[[i]]))/length(clients[[i]])*100)
}

row.names(statcat) <- c("Unique values","Taux de valeurs manquantes")
row.names(statdate) <- c("Minimum","Maximum","Taux de valeurs manquantes")

# Creation de la feuille Statistiques"
addDataFrame(statcat, sheet1, startRow=25, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statdate, sheet1, startRow=30, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)

xlsx::setColumnWidth(sheet1, colIndex=c(1:15), colWidth=20)


#ENTETE

# Add title
xlsx.addTitle(sheet1, rowIndex=37, title="Statistiques sur SUBENTETE",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=38,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)

colcat_entete <- colnames(entete)[!colnames(entete) %in% c("TIC_DATE","TIC_TOTALTTC")]
coldate_entete <- c("TIC_DATE")
colquali_entete <- c("TIC_TOTALTTC")

statcat_entete <- data.frame(matrix(nrow = 2,ncol=length(colcat_entete)))
colnames(statcat_entete)<- colcat_entete

for (i in colcat_entete){
  statcat_entete[[i]]<- c(length(unique(entete[[i]])),
                   sum(is.na(entete[[i]]))/length(entete[[i]])*100)
}

statdate_entete <- data.frame(matrix(nrow = 3,ncol=length(coldate_entete)))
colnames(statdate_entete)<- coldate_entete


for (i in coldate_entete){
  statdate_entete[[i]]<- c(
    min(as.Date(entete[[i]]),na.rm = TRUE),
    max(as.Date(entete[[i]]),na.rm = TRUE),
    as.numeric(sum(is.na(entete[[i]]))/length(entete[[i]])*100))

  

}

statquanti_entete <- data.frame(matrix(nrow = 10,ncol=length(colquali_entete)))
colnames(statquanti_entete)<- colquali_entete



for (i in colquali_entete){
  entete_num <- as.numeric(sub(",", ".", levels(entete[[i]])[entete[[i]]]))
  statquanti_entete[[i]]<- c(
    length(entete_num),
    min(entete_num,na.rm = TRUE),
    sort(unique(entete_num,na.rm = TRUE))[2],
    max(entete_num,na.rm = TRUE),
    sort(unique(entete_num,na.rm = TRUE),decreasing = TRUE)[2],
    round(mean(entete_num,na.rm = TRUE),1),
    median(entete_num,na.rm = TRUE),
    round(sd(entete_num,na.rm = TRUE),1),
    round(sd(entete_num,na.rm = TRUE)/mean(entete_num,na.rm = TRUE),1),
    sum(is.na(entete_num))/length(entete_num)*100
  
  )
  
}

row.names(statcat_entete) <- c("Unique values","Taux de valeurs manquantes")
row.names(statdate_entete) <- c("Minimum","Maximum","Taux de valeurs manquantes")
row.names(statquanti_entete) <- c("Effectif","Minimum","Min1","Maximum","Max1","Moyenne","Mediane","Ecart type","coefficient de variation","Taux de valeurs manquantes")

# Creation de la feuille Statistiques"
addDataFrame(statcat_entete, sheet1, startRow=42, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statdate_entete, sheet1, startRow=47, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statquanti_entete, sheet1, startRow=53, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)

xlsx::setColumnWidth(sheet1, colIndex=c(1:15), colWidth=20)


#Ligne
sheet1 <- xlsx::createSheet(wb, sheetName = "Statistiques")
# Add title
xlsx.addTitle(sheet1, rowIndex=66, title="Statistiques sur ARTICLES",titleStyle = TITLE_STYLE)
# Add sub title
xlsx.addTitle(sheet1, rowIndex=67,title="Tableau descriptif",titleStyle = SUB_TITLE_STYLE)


colcat_ligne <- colnames(entete)[!colnames(entete) %in% c("QUANTITE","MONTANTREMISE","TOTAL","MARGESORTIE")]
colquali_ligne <- c("QUANTITE","MONTANTREMISE","TOTAL","MARGESORTIE")


statcat_ligne <- data.frame(matrix(nrow = 2,ncol=length(colnames(colcat_ligne))))
colnames(statcat_ligne)<- colnames(colcat_ligne)


for (i in colnames(colcat_ligne)){
  statcat_ligne[[i]]<- c(length(unique(lignes[[i]])),
                sum(is.na(lignes[[i]]))/length(lignes[[i]])*100)
}


statquali_ligne <- data.frame(matrix(nrow = 10,ncol=length(colnames(colquali_ligne))))
colnames(statquali_ligne)<- colnames(colquali_ligne)

for (i in colquali_ligne){
  ligne_num <- as.numeric(sub(",", ".", levels(lignes[[i]])[lignes[[i]]]))
  statquali_ligne[[i]]<- c(
    length(ligne_num),
    min(ligne_num,na.rm = TRUE),
    sort(unique(ligne_num,na.rm = TRUE))[2],
    max(ligne_num,na.rm = TRUE),
    sort(unique(ligne_num,na.rm = TRUE),decreasing = TRUE)[2],
    round(mean(ligne_num,na.rm = TRUE),1),
    median(ligne_num,na.rm = TRUE),
    round(sd(ligne_num,na.rm = TRUE),1),
    round(sd(ligne_num,na.rm = TRUE)/mean(ligne_num,na.rm = TRUE),1),
    sum(is.na(ligne_num))/length(ligne_num)*100
    
  )
  
}


row.names(statcat_ligne) <- c("Unique values","Taux de valeurs manquantes")
row.names(statquali_ligne) <- c("Effectif","Minimum","Min1","Maximum","Max1","Moyenne","Mediane","Ecart type","coefficient de variation","Taux de valeurs manquantes")


# Creation de la feuille Statistiques"
addDataFrame(statcat_ligne, sheet1, startRow=71, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)
addDataFrame(statquali_ligne, sheet1, startRow=76, startColumn=1, 
             colnamesStyle = TABLE_COLNAMES_STYLE,
             rownamesStyle = TABLE_ROWNAMES_STYLE)












xlsx::saveWorkbook(wb, "temp.xlsx")







