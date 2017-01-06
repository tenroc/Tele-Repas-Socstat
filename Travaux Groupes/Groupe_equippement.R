## stats univariées:

str(enq.final)

# Qualité de la connection

table(enq.final$E1_re)
round(prop.table(table(enq.final$E1_re))*100,3)

# Smartphone: nombre

table(enq.final$E2_re)
round(prop.table(table(enq.final$E2_re))*100,3)

# Smartphone: utilisation perso

table(enq.final$E2_a_re)
round(prop.table(table(enq.final$E2_a_re))*100,3)

# Smartphone: fréquence d'utilisation

table(enq.final$E2_b_re)
round(prop.table(table(enq.final$E2_b_re))*100,3)

# Tel mobile non smartphone: nombre

table(enq.final$E3_re)
round(prop.table(table(enq.final$E3_re))*100,3)

# Tel mobile non smartphone: utilisation perso

table(enq.final$E3_a_re)
round(prop.table(table(enq.final$E3_a_re))*100,3)

# Tel mobile non smartphone: fréquence d'utilisation

table(enq.final$E3_b_re)
round(prop.table(table(enq.final$E3_b_re))*100,3)

# Tablettes: nombre

table(enq.final$E4)
round(prop.table(table(enq.final$E4_re))*100,3)

# Tablettes: usage perso

table(enq.final$E4_a_re)
round(prop.table(table(enq.final$E4_a_re))*100,3)

# Tablettes: fréquence d'utilisation

table(enq.final$E4_b_re)
round(prop.table(table(enq.final$E4_b_re))*100,3)

# Télé: nombre

table(enq.final$E5_re)
round(prop.table(table(enq.final$E5_re))*100,3)

# Télé: utilisation perso

table(enq.final$E5_a_re)
round(prop.table(table(enq.final$E5_a_re))*100,3)

# Télé: fréquence d'utilisation

table(enq.final$E5_b_re)
round(prop.table(table(enq.final$E5_b_re))*100,3)

# Consoles: nombre

table(enq.final$E6_re)
round(prop.table(table(enq.final$E6_re))*100,3)

# Consoles: Utilisation Perso

table(enq.final$E6_a_re)
round(prop.table(table(enq.final$E6_a_re))*100,3)

# Consoles: Frequence

table(enq.final$E6_b_re)
round(prop.table(table(enq.final$E6_b_re))*100,3)

# Ordi portable: nombre

table(enq.final$E7_re)
round(prop.table(table(enq.final$E7_re))*100,3)

# Ordi portable: utilisation perso

table(enq.final$E7_a_re)
round(prop.table(table(enq.final$E7_a_re))*100,3)

# Ordi portable: frequence

table(enq.final$E7_b_re)
round(prop.table(table(enq.final$E7_b_re))*100,3)

# Ordi fixe: nombre

table(enq.final$E8_re)
round(prop.table(table(enq.final$E8_re))*100,3)

# Ordi fixe: utilisation perso

table(enq.final$E8_a_re)
round(prop.table(table(enq.final$E8_a_re))*100,3)

# Ordi fixe: frequence

table(enq.final$E8_b_re)
round(prop.table(table(enq.final$E8_b_re))*100,3)

## Stats bivariées / recodage à faire

# Smartphone / mobile

table(enq.final$E2_a_re,enq.final$E3_a_r)
round(prop.table(table(enq.final$E2_a_re,enq.final$E3_a_r))*100,3)
a <- chisq.test(table(enq.final$E2_a_re,enq.final$E3_a_r))

round(prop.table(table(enq.final$E2_b_re,enq.final$E3_b_r))*100,3)
a <- chisq.test(table(enq.final$E2_b_re,enq.final$E3_b_r))

# Smartphone / tablette

table(enq.final$E2_a_re,enq.final$E4_a_r)
round(prop.table(table(enq.final$E2_a_re,enq.final$E4_a_r))*100,3)
a <- chisq.test(table(enq.final$E2_a_re,enq.final$E4_a_r))

table(enq.final$E2_b_re,enq.final$E4_b_r)
round(prop.table(table(enq.final$E2_b_re,enq.final$E4_b_r))*100,3)
a <- chisq.test(table(enq.final$E2_b_re,enq.final$E4_b_r))

# mobile / tablette

table(enq.final$E3_a_re,enq.final$E4_a_r)
round(prop.table(table(enq.final$E3_a_re,enq.final$E4_a_r))*100,3)
a <- chisq.test(table(enq.final$E3_a_re,enq.final$E4_a_r))

table(enq.final$E3_b_re,enq.final$E4_b_r)
round(prop.table(table(enq.final$E3_b_re,enq.final$E4_b_r))*100,3)
a <- chisq.test(table(enq.final$E3_b_re,enq.final$E4_b_r))

# smartphone / Télé

table(enq.final$E2_a_re,enq.final$E5_a_r)
round(prop.table(table(enq.final$E2_a_re,enq.final$E5_a_r))*100,3)
a <- chisq.test(table(enq.final$E2_a_re,enq.final$E5_a_r))

table(enq.final$E2_b_re,enq.final$E4_b_r)
round(prop.table(table(enq.final$E2_b_re,enq.final$E5_b_r))*100,3)
a <- chisq.test(table(enq.final$E2_b_re,enq.final$E5_b_r))

# mobile / Télé

table(enq.final$E3_a_re,enq.final$E5_a_r)
round(prop.table(table(enq.final$E3_a_re,enq.final$E5_a_r))*100,3)
a <- chisq.test(table(enq.final$E3_a_re,enq.final$E5_a_r))

table(enq.final$E3_b_re,enq.final$E5_b_r)
round(prop.table(table(enq.final$E3_b_re,enq.final$E5_b_r))*100,3)
a <- chisq.test(table(enq.final$E3_b_re,enq.final$E5_b_r))

# ordi portable / ordi fixe

table(enq.final$E7_a_re,enq.final$E8_a_r)
round(prop.table(table(enq.final$E7_a_re,enq.final$E8_a_r))*100,3)
a <- chisq.test(table(enq.final$E7_a_re,enq.final$E8_a_r))

table(enq.final$E7_b_re,enq.final$E8_b_r)
round(prop.table(table(enq.final$E7_b_re,enq.final$E8_b_r))*100,3)
a <- chisq.test(table(enq.final$E7_b_re,enq.final$E8_b_r))

# Smartphone / ordi portable

table(enq.final$E2_a_re,enq.final$E7_a_r)
round(prop.table(table(enq.final$E2_a_re,enq.final$E7_a_r))*100,3)
a <- chisq.test(table(enq.final$E2_a_re,enq.final$E7_a_r))

table(enq.final$E2_b_re,enq.final$E7_b_r)
round(prop.table(table(enq.final$E2_b_re,enq.final$E7_b_r))*100,3)
a <- chisq.test(table(enq.final$E2_b_re,enq.final$E7_b_r))

# mobile / ordi portable

table(enq.final$E3_a_re,enq.final$E7_a_r)
round(prop.table(table(enq.final$E3_a_re,enq.final$E7_a_r))*100,3)
a <- chisq.test(table(enq.final$E3_a_re,enq.final$E7_a_r))

table(enq.final$E3_b_re,enq.final$E7_b_r)
round(prop.table(table(enq.final$E3_b_re,enq.final$E7_b_r))*100,3)
a <- chisq.test(table(enq.final$E3_b_re,enq.final$E7_b_r))

## Indicateurs

# Types d'utilisateurs

# Smartphone

for(i in 1:nrow(enq.final)){
  if(enq.final$E2_re[i] %in% c('1','2','3 ou plus')){
    if(enq.final$E2_a_re[i] %in% c('1','2','3 ou plus')){
      if(enq.final$E2_b_re[i] %in% c('Souvent', 'Très souvent')){
        enq.final$E2_Typologie_utilisateurs[i] <- 'Utilisateur régulier'
      } else {
        enq.final$E2_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
      }
    } else {
      enq.final$E2_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
    }
  } else {
    enq.final$E2_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
  }
}

# Mobile

for(i in 1:nrow(enq.final)){
  if(enq.final$E3_re[i] %in% c('1','2','3 ou plus')){
    if(enq.final$E3_a_re[i] %in% c('1','2','3 ou plus')){
      if(enq.final$E3_b_re[i] %in% c('Souvent', 'Très souvent')){
        enq.final$E3_Typologie_utilisateurs[i] <- 'Utilisateur régulier'
      } else {
        enq.final$E3_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
      }
    } else {
      enq.final$E3_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
    }
  } else {
    enq.final$E3_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
  }
}

# Tablette

for(i in 1:nrow(enq.final)){
  if(enq.final$E4_re[i] %in% c('1','2','3 ou plus')){
    if(enq.final$E4_a_re[i] %in% c('1','2','3 ou plus')){
      if(enq.final$E4_b_re[i] %in% c('Souvent', 'Très souvent')){
        enq.final$E4_Typologie_utilisateurs[i] <- 'Utilisateur régulier'
      } else {
        enq.final$E4_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
      }
    } else {
      enq.final$E4_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
    }
  } else {
    enq.final$E4_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
  }
}

# Télé

for(i in 1:nrow(enq.final)){
  if(enq.final$E5_re[i] %in% c('1','2','3 ou plus')){
    if(enq.final$E5_a_re[i] %in% c('1','2','3 ou plus')){
      if(enq.final$E5_b_re[i] %in% c('Souvent', 'Très souvent')){
        enq.final$E5_Typologie_utilisateurs[i] <- 'Utilisateur régulier'
      } else {
        enq.final$E5_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
      }
    } else {
      enq.final$E5_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
    }
  } else {
    enq.final$E5_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
  }
}
# Console

for(i in 1:nrow(enq.final)){
  if(enq.final$E6_re[i] %in% c('1','2','3 ou plus')){
    if(enq.final$E6_a_re[i] %in% c('1','2','3 ou plus')){
      if(enq.final$E6_b_re[i] %in% c('Souvent', 'Très souvent')){
        enq.final$E6_Typologie_utilisateurs[i] <- 'Utilisateur régulier'
      } else {
        enq.final$E6_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
      }
    } else {
      enq.final$E6_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
    }
  } else {
    enq.final$E6_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
  }
}

# Ordinateur portable

for(i in 1:nrow(enq.final)){
  if(enq.final$E7_re[i] %in% c('1','2','3 ou plus')){
    if(enq.final$E7_a_re[i] %in% c('1','2','3 ou plus')){
      if(enq.final$E7_b_re[i] %in% c('Souvent', 'Très souvent')){
        enq.final$E7_Typologie_utilisateurs[i] <- 'Utilisateur régulier'
      } else {
        enq.final$E7_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
      }
    } else {
      enq.final$E7_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
    }
  } else {
    enq.final$E7_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
  }
}

# Ordinateur fixe

for(i in 1:nrow(enq.final)){
  if(enq.final$E8_re[i] %in% c('1','2','3 ou plus')){
    if(enq.final$E8_a_re[i] %in% c('1','2','3 ou plus')){
      if(enq.final$E8_b_re[i] %in% c('Souvent', 'Très souvent')){
        enq.final$E8_Typologie_utilisateurs[i] <- 'Utilisateur régulier'
      } else {
        enq.final$E8_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
      }
    } else {
      enq.final$E8_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
    }
  } else {
    enq.final$E8_Typologie_utilisateurs[i] <- 'Non utilisateur ou utilisateur occasionel'
  }
}

# Verifications effectifs

table(enq.final$E2_Typologie_utilisateurs)
round(prop.table(table(enq.final$E2_Typologie_utilisateurs))*100,3)

table(enq.final$E3_Typologie_utilisateurs)
round(prop.table(table(enq.final$E3_Typologie_utilisateurs))*100,3)

table(enq.final$E4_Typologie_utilisateurs)
round(prop.table(table(enq.final$E4_Typologie_utilisateurs))*100,3)

table(enq.final$E5_Typologie_utilisateurs)
round(prop.table(table(enq.final$E5_Typologie_utilisateurs))*100,3)

table(enq.final$E6_Typologie_utilisateurs)
round(prop.table(table(enq.final$E6_Typologie_utilisateurs))*100,3)

table(enq.final$E7_Typologie_utilisateurs)
round(prop.table(table(enq.final$E7_Typologie_utilisateurs))*100,3)

table(enq.final$E8_Typologie_utilisateurs)
round(prop.table(table(enq.final$E8_Typologie_utilisateurs))*100,3)


## ACM

library(FactoMineR)
library(GDAtools)

# Substet (sans E6: pas assez d'individus en utilisateurs réguliers, pas fondamental de toutes manières)

acm <- subset(enq.final, select=c(E2_Typologie_utilisateurs, E3_Typologie_utilisateurs, E4_Typologie_utilisateurs, E5_Typologie_utilisateurs,
                                  E7_Typologie_utilisateurs, E8_Typologie_utilisateurs))

acm <- as.data.frame(lapply(acm, factor))

res.acm <- MCA(acm, ncp=3, graph=T)

# Axes à conserver:

valprop.acm <- res.acm$eig[1:10,]

barplot(res.acm$eig[1:10,2], main="Histogramme des valeurs propres", names.arg=1:10,        
        xlab="Axes", ylab="Pourcentage d'inertie", cex.axis=0.8, font.lab=3, ylim=c(0, 50),
        col="orange")

res.acm$var$contrib
res.acm$var$coord
res.acm$var$cos2

# Contribution moyenne
seuil <- 100/nrow(res.acm$var$contrib)

modatot <- which(res.acm$var$contrib[, 1]>seuil ## which est une condition, on ne prends ici que les valeurs de res.acm$var$contrib, dans la dimension 1, qui sont supÃ¨rieurs au seuil.
                 | res.acm$var$contrib[, 2]>seuil
                 | res.acm$var$contrib[, 3]>seuil)
modatot

moda12 <- which(res.acm$var$contrib[, 1]>seuil 
                | res.acm$var$contrib[, 2]>seuil)
moda12

moda23 <- which(res.acm$var$contrib[, 2]>seuil 
                | res.acm$var$contrib[, 3]>seuil)
moda23


# Mise en forme et export des res.acm
dim1 <- cbind(res.acm$var$contrib[,1], res.acm$var$coord[,1], res.acm$var$cos2[,1])
colnames(dim1) <- c("dim1_contrib","dim1_coord","dim1_cos2")
dim1

dim2 <- cbind(res.acm$var$contrib[,2], res.acm$var$coord[,2], res.acm$var$cos2[,2])
colnames(dim2) <- c("dim2_contrib","dim2_coord","dim2_cos2")
dim2

dim3 <- cbind(res.acm$var$contrib[,3], res.acm$var$coord[,3], res.acm$var$cos2[,3])
colnames(dim3) <- c("dim3_contrib","dim3_coord","dim3_cos2")
dim3

varact <- round(cbind(dim1, dim2, dim3),3)
varact # res.acm pour toutes les modalites actives
varact[modatot,] # res.acm pour les modalites contributives: indexe varact en fonction de modadot (ligne).



# Graphiques

plot.MCA(res.acm, invisible=c("ind","quali.sup"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=c(moda12))
plot.MCA(res.acm, invisible=c("ind","quali.sup"), title="Nuage des modalites actives Plan 2-3", axes=c(2,3), 
         autoLab="yes", unselect=1,
         selectMod=c(moda23))