rm(list=ls())
setwd(chemin)
enq.final <- read.csv2("./DATA_BDD-RECONCILIE/enq final_04.csv", encoding="UTF-8")

#chargement des packages 
library(questionr)
library(R2HTML)
library(FactoMineR)
library(GDAtools)
str(enq.final)

#s?lection d'un ?chantillon de l'enqu?te avec les variables souhait?es
e <- subset(enq.final,select=c(id_r,A12_re,A14_re,A13_1_re,A13_3_re,A13_4_re,A13_5_re,
                               A15_re,A16_re,A7_re,HA3_re,
                               A11_1_re,A11_2_re,A11_3_re,A11_4_re
                               ,A3_re,A4_re,A8_re,A9_re,A10_re,D13_re,
                               D1_re,D3_re,D6_re,D9_re, age_calage,D13_re,D6_re,D8_re,weight))



#recodage des modalites rares pour l'acm 

#gros probleme sur la variable A4_re, je sais pas ce qu'il s'est passe dans le recodage
#mais y a que 50personnes qui ont repondu
#Recodage a8
t<-prop.table(table(e$A8_re))*100
t
levels(e$A8_re)
e$A8_acm <-e$A8_re
levels(e$A8_acm)[c(1,2,3,5,6)]<-"A l'exterieur"
levels(e$A8_acm)
t<-prop.table(table(e$A8_acm))*100
t
#Recodage a10
t<-prop.table(table(e$A10_re))*100
t
e$A10_acm <-e$A10_re
levels(e$A10_acm)[c(1,2,4,5,6)]<-"Autre"
levels(e$A10_acm)
#Probl?me avec A4_re
table(enq.final$A4_re)
#Recodage A3
levels(e$A3_re)[c(2,3,5)]<-"Plus de 20min"
table(e$A3_re)
#age_calage
table(e$age_calage)
levels(e$age_calage)[c(1,2)]<-"18-39"
levels(e$age_calage)[c(2,3)]<-"40-64"
levels(e$age_calage)[c(3,4)]<-"65+"
table(e$age_calage)
#D6_re
table(e$D6_re)
levels(e$D6_re)[c(1,2)]<-"Autre"


#trop de modalit?s rares dans A14_re
#trop de NAS dans la CSP
which(is.na(e$D8_re))
#definition du jeu de donnees pour l'ACM avec les variables recodees
jeu <- subset(e,select=c(A11_1_re,A11_2_re,A7_re,A12_re,A13_4_re,A13_5_re,
                         A11_3_re,A8_acm,A9_re,A10_acm,A3_re,
                         HA3_re,age_calage,D13_re,D6_re))


#variables omises :  A15_re , A3_re,
jeu<-na.exclude(jeu)

str(jeu) 

#attention on a seulement 428 observations quand on enl?ve les NAs 
#les gens de cette ACM sont seulement ceux qui ont r?pondu ?tre avec quelquu'n
dev.off()
res.acm <- MCA(jeu,quali.sup=12:15, ncp=3, graph=T)

summary(res.acm)
valprop.acm <- res.acm$eig[1:10,]
valprop.acm
barplot(res.acm$eig[1:10,2], main="Histogramme des valeurs propres", names.arg=1:10,        
        xlab="Axes", ylab="Pourcentage d'inertie", cex.axis=0.8, font.lab=3, ylim=c(0, 18),
        col="orange")
#on ne retient que deux axes

# Contribution moyenne - quelles sont les modalites qui contribuent 
#                         a la construction des axes ?
seuil <- 100/nrow(res.acm$var$contrib)
seuil
modatot <- which(res.acm$var$contrib[, 1]>seuil 
                 | res.acm$var$contrib[, 2]>seuil)
modatot


# Mise en forme et export des res.acm
dim1 <- cbind(res.acm$var$contrib[,1], res.acm$var$coord[,1], res.acm$var$cos2[,1])
colnames(dim1) <- c("dim1_contrib","dim1_coord","dim1_cos2")
dim1

dim2 <- cbind(res.acm$var$contrib[,2], res.acm$var$coord[,2], res.acm$var$cos2[,2])
colnames(dim2) <- c("dim2_contrib","dim2_coord","dim2_cos2")
dim2


res <- round(cbind(dim1, dim2),3)
res
write.infile(res, file="./Travaux Groupes/Repas Principal Sociabilit?s/res_acm2.xls", sep="\t")
rownames(res)

varact <- res[modatot,] # res.acm pour toutes les modalites contributives
varact
write.infile(varact, file="./Travaux Groupes/Repas Principal Sociabilit?s/res_acm2_varact.xls", sep="\t")

summary(varact)
nrow(varact)
colnames(varact)


# Une fois que l'on a lu les res.acm stats, on peut essayer de faire un graphique lisible

plot.MCA(res.acm, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=modatot)

# Classification ascendante hierarchique

res.hcpc <- HCPC(res.acm,6)
jeu$cluster<- as.factor(as.character(res.hcpc$data.clust$clust))
summary(jeu$cluster)
str(jeu)
dev.off()
res.acm.Avecgroupes <- MCA(jeu,quali.sup=12:16,graph=T)

#recuperer les resultats de l'ACM dans un fichier :
dim1_act <- cbind(res.acm.Avecgroupes$var$contrib[,1], res.acm.Avecgroupes$var$coord[,1], res.acm.Avecgroupes$var$cos2[,1])
dim1_sup <- cbind(0,res.acm.Avecgroupes$quali.sup$coord[,1], res.acm.Avecgroupes$quali.sup$cos2[,1])
dim1<-rbind(dim1_act,dim1_sup)
colnames(dim1) <- c("dim1_contrib","dim1_coord","dim1_cos2")
dim1

dim2_act <- cbind(res.acm.Avecgroupes$var$contrib[,2], res.acm.Avecgroupes$var$coord[,2], res.acm.Avecgroupes$var$cos2[,2])
dim2_sup <- cbind(0,res.acm.Avecgroupes$quali.sup$coord[,2], res.acm.Avecgroupes$quali.sup$cos2[,2])
dim2<-rbind(dim2_act,dim2_sup)
colnames(dim2) <- c("dim2_contrib","dim2_coord","dim2_cos2")
dim2

res2 <- round(cbind(dim1, dim2),3)
res2
write.infile(res2, file="./Travaux Groupes/Repas Principal Sociabilit?s/res_acm2_avecCluster.xls", sep="\t")
varact <- res2[modatot,] # res.acm pour toutes les modalites contributives
varact
write.infile(varact, file="./Travaux Groupes/Repas Principal Sociabilit?s/res_acm2_varact_avecCluster.xls", sep="\t")



plot.MCA(res.acm.Avecgroupes, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=modatot)
# Representer le nuage des individus
dev.off()
plot(res.acm.Avecgroupes$var$coord[modatot, 1:2]*1.2, type="n", 
     xlab=paste0("Axe 1 (", round(res.acm$eig[1,2], 1), "%)"), 
     ylab=paste0("Axe 2 (", round(res.acm$eig[2,2], 1), "%)"), 
     main="Premier plan factoriel", 
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1)
abline(h=0, v=0, col="grey", lty=3, lwd=1)

points(res.acm.Avecgroupes$ind$coord[, 1:2], col = 1, pch = 3)

# Habiller le nuage de points selon les groupes
plot(res.acm.Avecgroupes$var$coord[modatot, 1:2]*1.2, type="n", 
     xlab=paste0("Axe 1 (", round(res.acm.Avecgroupes$eig[1,2], 1), "%)"), ylab=paste0("Axe 2 (", round(res.acm.Avecgroupes$eig[2,2], 1), "%)"), 
     main="Premier plan factoriel", 
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1)
abline(h=0, v=0, col="grey", lty=3, lwd=1)


points(res.acm.Avecgroupes$ind$coord[,1:2], col=as.numeric(jeu$cluster), pch=3)
legend("topright", legend=levels(jeu$cluster), bty="o", 
       text.col=1:6, col=1:6, pch=19, cex=0.8)

res.acm.Avecgroupes <- MCA(jeu,quali.sup=12:16,graph=T)

######## Sarah: j'ai mis mon code dans la section suivante, si ça te vas on peut l'intégrer a la partie Classif hierarchique

# On a besoin de la variable id_r pour le merge

jeu_2 <- subset(e,select=c(id_r, A11_1_re,A11_2_re,A7_re,A12_re,A13_4_re,A13_5_re,
                         A11_3_re,A8_acm,A9_re,A10_acm,A3_re))
jeu_2<-na.exclude(jeu_2)


res.acm2 <- MCA(jeu_2,quali.sup=1, ncp=3, graph=T)

res.hcpc2 <- HCPC(res.acm2,6)

cluster_jeu2 <- subset(res.hcpc2$data.clust, select=c(id_r,clust))
enq.final2 <- merge(enq.final, cluster_jeu2, by="id_r", all=T)
table(enq.final2$clust,useNA="ifany")
is.na(enq.final2$clust)
str(enq.final2)
enq.final2$X.2<-NULL
enq.final2$X.1<-NULL
enq.final2$X<-NULL



#ou ça :
#jeu_3 <- subset(e,select=c(id_r, A11_1_re,A11_2_re,A7_re,A12_re,A13_4_re,A13_5_re,
#                           A11_3_re,A8_acm,A9_re,A10_acm,A3_re))
#jeu_3<-na.exclude(jeu_3)

#jeu_3$cluster <- jeu$cluster
#enq.final3 <- merge(enq.final, jeu_3, by="id_r", all=T)
#table(enq.final3$cluster, useNA="ifany")





#creer un fichier enq final avec une variable cluster

write.csv2(enq.final2, file="./DATA_BDD-RECONCILIE/enq final_05.csv")

#caractérisation des groupes

levels(enq.final.cluster$A3_re.x)[c(2,3,5)]<-"Plus de 20min"
table(enq.final.cluster$A3_re.x)

t<-table(enq.final.cluster$enq.final.cluster$age_calage.x)
t
t<-round(prop.table(t,1),2)*100
t
t<-addmargins(t,2)
t
copie(t)

t<-table(enq.final$A11_2_re,enq.final$A11_3_re)
t
chisq.test(t)
t<-round(prop.table(t,1),2)*100
t
t<-addmargins(t,2)
t
copie(t)




# CAH - essai avec 4 groupes ----------------------------------------------

# Classification ascendante hierarchique

res.hcpc <- HCPC(res.acm,4)
jeu$cluster<- as.factor(as.character(res.hcpc$data.clust$clust))
summary(jeu$cluster)

str(jeu)
dev.off()
res.acm.Avecgroupes <- MCA(jeu,quali.sup=12:16,graph=T)


#recuperer les resultats de l'ACM dans un fichier :
dim1_act <- cbind(res.acm.Avecgroupes$var$contrib[,1], res.acm.Avecgroupes$var$coord[,1], res.acm.Avecgroupes$var$cos2[,1])
dim1_sup <- cbind(0,res.acm.Avecgroupes$quali.sup$coord[,1], res.acm.Avecgroupes$quali.sup$cos2[,1])
dim1<-rbind(dim1_act,dim1_sup)
colnames(dim1) <- c("dim1_contrib","dim1_coord","dim1_cos2")
dim1

dim2_act <- cbind(res.acm.Avecgroupes$var$contrib[,2], res.acm.Avecgroupes$var$coord[,2], res.acm.Avecgroupes$var$cos2[,2])
dim2_sup <- cbind(0,res.acm.Avecgroupes$quali.sup$coord[,2], res.acm.Avecgroupes$quali.sup$cos2[,2])
dim2<-rbind(dim2_act,dim2_sup)
colnames(dim2) <- c("dim2_contrib","dim2_coord","dim2_cos2")
dim2

res2 <- round(cbind(dim1, dim2),3)
res2
write.infile(res2, file="./Travaux Groupes/Repas Principal Sociabilités/res_acm2_avec4Cluster.xls", sep="\t")



plot.MCA(res.acm.Avecgroupes, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=)
res.acm.Avecgroupes <- MCA(jeu,quali.sup=12:16,graph=T)
# Representer le nuage des individus
dev.off()
plot(res.acm.Avecgroupes$var$coord[, 1:2]*1.2, type="n", 
     xlab=paste0("Axe 1 (", round(res.acm$eig[1,2], 1), "%)"), 
     ylab=paste0("Axe 2 (", round(res.acm$eig[2,2], 1), "%)"), 
     main="Premier plan factoriel", 
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1)
abline(h=0, v=0, col="grey", lty=3, lwd=1)

points(res.acm.Avecgroupes$ind$coord[, 1:2], col = 1, pch = 3)

# Habiller le nuage de points selon les groupes
plot(res.acm.Avecgroupes$var$coord[, 1:2]*1.2, type="n", 
     xlab=paste0("Axe 1 (", round(res.acm.Avecgroupes$eig[1,2], 1), "%)"), ylab=paste0("Axe 2 (", round(res.acm.Avecgroupes$eig[2,2], 1), "%)"), 
     main="Premier plan factoriel", 
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1)
abline(h=0, v=0, col="grey", lty=3, lwd=1)


points(res.acm.Avecgroupes$ind$coord[,1:2], col=as.numeric(jeu$cluster), pch=3)
legend("topright", legend=levels(jeu$cluster), bty="o", 
       text.col=1:6, col=1:6, pch=19, cex=0.8)




# ecriture fichier --------------------------------------------------------

jeu_3 <- subset(e,select=c(id_r, A11_1_re,A11_2_re,A7_re,A12_re,A13_4_re,A13_5_re,
                           A11_3_re,A8_acm,A9_re,A10_acm,A3_re))
jeu_3<-na.exclude(jeu_3)

dev.off()
res.acm2 <- MCA(jeu_2,quali.sup=1, ncp=3, graph=T)

res.hcpc_1 <- HCPC(res.acm2,4)
res.hcpc_2 <- HCPC(res.acm2,6)

cluster_jeu3 <- subset(res.hcpc_1$data.clust, select=c(id_r,clust))
cluster_jeu3<-cbind(cluster_jeu3,res.hcpc_2$data.clust$clust)
cluster_jeu3$`res.hcpc_2$data.clust$clust`->cluster_jeu3$clust6
cluster_jeu3$clust->cluster_jeu3$clust4
cluster_jeu3$`res.hcpc_2$data.clust$clust`<-NULL
cluster_jeu3$clust<-NULL
table(cluster_jeu3$clust6,useNA = "ifany")
table(cluster_jeu3$clust4,useNA = "ifany")

enq.final3 <- merge(enq.final, cluster_jeu3, by="id_r", all=T)
table(enq.final3$clust6,useNA="ifany")
table(enq.final3$clust4,useNA="ifany")

str(enq.final3)
enq.final3$X.2<-NULL
enq.final3$X.1<-NULL
enq.final3$X<-NULL



#creer un fichier enq final_06 avec une variable cluster

write.csv2(enq.final3, file="./DATA_BDD-RECONCILIE/enq final_06.csv")
