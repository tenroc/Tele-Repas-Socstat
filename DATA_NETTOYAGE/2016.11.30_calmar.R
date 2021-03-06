
rm(list=ls())
setwd(chemin)

e<- read.csv2("./DATA_BDD-RECONCILIE/enq final_03.csv",encoding="UTF-8")

#
round(prop.table(table(e[,"age_calage"])),2)
round(prop.table(table(e[,"D13_re"])),2)
round(prop.table(table(e[,"D5_re"])),2)

round(prop.table(table(e[,"age_calage"],e[,"D13_re"])),2)

#INSTALLATION DU PACKAGE ICARUS
#install.packages("icarus")
library(icarus)

##Calage sur marge SEXE AGE CSP ## 
###########ON NE PEUT PAS L'EXECUTER SUR L'ACTUEL BASE DE DONNEES ###########################
# il faut d'abord nettoyer et recoder les variables sexe, âge et CSP #

ech <- data.frame(cbind(e[,"id_r"],e[,"D13_re"],e[,"age_calage"],e[,"D5_re"],1))
colnames(ech)<-c("id","sexe","age","csp","poids")
ech[,2]<- as.factor(ech[,2])
ech[,3]<-as.factor(ech[,3])
ech[,4]<-as.factor(ech[,4])


class(ech[,2])
class(ech[,3])
class(ech[,4])
class(ech[,5])
str(ech)
#suppression des NAs 
ech<- na.omit(ech)


summary(ech[,"sexe"])
summary(ech[,"age"])
summary(ech[,"csp"])

levels(ech[,"age"])


#poids de calage pour les trois variables 
#�a ne marche pas

#Ecriture des marges
mar1 <- c("sexe",2,0.48,0.52,0,0,0,0,0)
mar2 <- c("age",6,0.08,0.19,0.20,0.13,0.12,0.06,0)
mar3 <- c("csp",7,0.01,0.03,0.07,0.12,0.13,0.11,0.35)

marges1 <- rbind(mar1)
marges2 <- rbind(mar2)
marges3 <- rbind(mar3)
marges4<-rbind(mar1,mar2,mar3)
marges4



calibration(data=ech, marginMatrix=marges4,colWeights="poids", 
            method="raking",description=T,bounds=c(0.4,2.1),
            popTotal=1,pct=T)


#Seulement pour le sexe 
#ca marche

mar1 <- c("sexe",2,0.48,0.52)
marges1 <- rbind(mar1)
marges1
calibration(data=ech, marginMatrix=marges1,colWeights="poids", 
            method="raking",description=T,bounds=c(0.4,2.1),
            popTotal=1,pct=T)

#Seulement pour l'�ge 
#ca ne marche pas

mar2 <- c("age",6,0.08,0.19,0.20,0.13,0.12,0.06)
marges2 <- rbind(mar2)
marges2
calibration(data=ech, marginMatrix=marges2,colWeights="poids", 
            method="raking",description=T,bounds=c(0.4,2.1),
            popTotal=1,pct=T)

#Seulement pour l'�ge MAIS avec les �ge recod�s en 4 categ
#ca ne marche pas

levels(ech[,"age"])
levels(ech$age)[c(1,2)] <- "1"
levels(ech$age)[c(2)] <- "2"
levels(ech$age)[c(3)] <- "3"
levels(ech$age)[c(4,5)] <- "4"
summary(ech[,"age"])
levels(ech$age)[c(1)]<- "18-39"
levels(ech$age)[c(2)]<- "40-54"
levels(ech$age)[c(3,4)]<- "55+"
levels(ech$age)[c(4)]<- "60+"

mar2 <- c("age",3,0.27,0.20,0.31)
marges2 <- rbind(mar2)
marges2
calibration(data=ech, marginMatrix=marges2,colWeights="poids", 
            method="raking",description=T,bounds=c(0.4,2.1),
            popTotal=1,pct=T)
#Seulement pour la CSP 
#ca ne marche pas

mar3 <- c("csp",7,0.01,0.03,0.07,0.12,0.13,0.11,0.35)
marges3 <- rbind(mar3)
marges3
calibration(data=ech, marginMatrix=marges3,colWeights="poids", 
            method="raking",description=T,bounds=c(0.4,2.1),
            popTotal=1,pct=T)
