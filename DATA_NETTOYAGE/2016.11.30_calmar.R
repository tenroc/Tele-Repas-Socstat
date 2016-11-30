
rm(list=ls())
setwd(chemin)

enq.final <- read.csv("./_Data/BDD réconciliée/BDD/enq final_v2.csv",encoding="UTF-8")

#INSTALLATION DU PACKAGE ICARUS
#install.packages("icarus")
library(icarus)

##Calage sur marge SEXE AGE CSP ## 
###########ON NE PEUT PAS L'EXECUTER SUR L'ACTUEL BASE DE DONNEES ###########################
# il faut d'abord nettoyer et recoder les variables sexe, âge et CSP #

ech <- data.frame(cbind(enq.final[,"id_r"],enq.final[,"D13_re"],enq.final[,"age_calage"],enq.final[,"D5_re"],1))
colnames(ech)<-c("id","sexe","age","csp","poids")
ech[,2]<- as.factor(ech[,2])
ech[,3]<-as.factor(ech[,3])
ech[,4]<-as.factor(ech[,4])
ech[,5]<-as.factor(ech[,5])
class(ech[,2])
class(ech[,3])
class(ech[,4])
class(ech[,5])
str(ech)
summary(ech[,"age"])
mar1 <- c("sexe",2,0.48,0.52,0,0,0,0)
mar2 <- c("age",6,0.08,0.19,0.20,0.13,0.12,0.06)
#mar3 <- c("csp",7,0.01,0.03,0.07,0.12,0.13,0.11,0.35)
marges <- rbind(mar1, mar2)
marges
#bounds=c(0.4,2.1)
calibration(data=ech, marginMatrix=marges,colWeights="poids", 
            method="raking",description=T,bounds=c(0.4,2.1),
            pct=T,popTotal=63697865)

#autre possibilité avec effectifs
mar1 <- c("sexe",2,30853246,32844619,0,0,0,0)
mar2 <- c("age",6,5335832,11867658,13013292,8099047,7728477,3682427)

marges <- rbind(mar1, mar2)
marges
#bounds=c(0.4,2.1)
calibration(data=ech, marginMatrix=marges,colWeights="poids", 
            method="raking",description=T)





