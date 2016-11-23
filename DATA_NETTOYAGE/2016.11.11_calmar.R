
rm(list=ls())
setwd("C:/Users/sarah/Google Drive/Enquête statistique 3")

enq.final <- read.csv("enq final.csv", encoding="UTF-8")
etiquettes <- read.csv("etiquettes.csv", encoding = "UTF-8")

#INSTALLATION DU PACKAGE ICARUS
#install.packages("icarus")
library(icarus)


#TEST sur le sexe
data <- data.frame(cbind(enq.final[,"id_r"],enq.final[,"D13"],enq.final[,"D1"],enq.final[,"D5"],1))
colnames(data)<-c("id","sexe","age","csp","poids")
data[,2]<- as.factor(data[,2])
data[,3]<-as.factor(data[,3])
data[,4]<-as.factor(data[,4])
data[,5]<-as.factor(data[,5])
class(data[,2])
class(data[,3])
class(data[,4])
class(data[,5])
str(data)



data2<- data.frame(cbind(data[,1],na.exclude(data[,2]),na.exclude(data[,5])))
colnames(data2)<-c("id","sexe","poids")

#CONVERSION DES VARIABLES EN FACTEURS
data2[,2]<- as.factor(data2[,2])
data2[,3]<-as.factor(data2[,3])
#VERIFICATION
class(data2[,2])
class(data2[,3])

#CONSTITUTION DE LA BASE DES MARGES (ici on prend les % ATTENDUS)

marA<-c("sexe",2,0.48,0.52)
marges2<-rbind(marA)

#UTILISATION DU PACKAGE POUR LE CALCUL DE POIDS
calibration(data=data2,marginMatrix=marges2,colWeights="poids", method="raking",description=T,
            bounds=c(0.4,2.1),popTotal=1,pct=TRUE)
#AUTRE POSSIBILITE [CETTE FOIS AVEC LES EFFECTIFS ATTENDUS]
marA<-c("sexe",2,30853246,32844619)
marges2<-rbind(marA)
calibration(data=data2,marginMatrix=marges2,colWeights="poids", method="raking",description=T,
            bounds=c(0.4,2.1),popTotal=63697865)


##Calage sur marge SEXE AGE CSP ## 
###########ON NE PEUT PAS L'EXECUTER SUR L'ACTUEL BASE DE DONNEES ###########################
# il faut d'abord nettoyer et recoder les variables sexe, âge et CSP #

data <- data.frame(cbind(enq.final[,"id_r"],enq.final[,"D13"],enq.final[,"D1"],enq.final[,"D5"],1))
colnames(data)<-c("id","sexe","age","csp","poids")
data[,2]<- as.factor(data[,2])
data[,3]<-as.factor(data[,3])
data[,4]<-as.factor(data[,4])
data[,5]<-as.factor(data[,5])
class(data[,2])
class(data[,3])
class(data[,4])
class(data[,5])
str(data)

mar1 <- c("sexe",2,0.484368599,0.515631401,0,0,0,0,0,0)
mar2 <- c("age",6,0.083767834,0.186311702,0.204297153,0.127147863,0.121330241,0.057810841,0,0)
mar3 <- c("csp",8,0.007023795,0.028331137,0.074515761,0.115641707,0.134657491,0.105744586,0.21963372,0.131001746)
marges <- rbind(mar1, mar2, mar3)


calibration(data=data, marginMatrix=marges, colWeights="poids", method="raking",description=T,
            bounds=c(0.4,2.1),popTotal=1,pct=TRUE)


