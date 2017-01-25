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

## Recodages

# Sexe

table(enq.final$D13_re)

# Age en tranches

enq.final$D1_tr[enq.final$D1_re < 25] <- "moins de 25 ans"
enq.final$D1_tr[enq.final$D1_re %in% c(25:45)] <- "25-45 ans"
enq.final$D1_tr[enq.final$D1_re %in% c(45:65)] <- "46-65 ans"
enq.final$D1_tr[enq.final$D1_re > 65] <- "plus de 65 ans"
summary(enq.final$D1_tr)
table(enq.final$D1_tr)


# Diplome simplifié

table(enq.final$D3_re)
enq.final$D3_re2[as.character(enq.final$D3_re) %in% c("Aucun diplôme", "BEPC, brevet", "CAP, BEP","Certificat d’études primaires")] <- "Inférieur au bac"
enq.final$D3_re2[as.character(enq.final$D3_re) == "Baccalauréat, BP"] <- "Bac"
enq.final$D3_re2[as.character(enq.final$D3_re) %in% c("Deug, DUT, BTS, diplômes des professions sociales ou de la santé","Licence, Bac+3","Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc.")] <- "Supérieur au bac"
enq.final$D3_re2[as.character(enq.final$D3_re) == "Refus"] <- NA
summary(enq.final$D3_re2)
table(enq.final$D3_re2)

# Diplome du conjoin simplifié:

table(enq.final$D3_bis_re)
enq.final$D3_re2[as.character(enq.final$D3_bis_re) %in% c("Aucun diplôme", "BEPC, brevet", "CAP, BEP","Certificat d’études primaires")] <- "Inférieur au bac"
enq.final$D3_re2[as.character(enq.final$D3_bis_re) == "Baccalauréat, BP"] <- "Bac"
enq.final$D3_re2[as.character(enq.final$D3_bis_re) %in% c("Deug, DUT, BTS, diplômes des professions sociales ou de la santé","Licence, Bac+3","Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc.")] <- "Supérieur au bac"
enq.final$D3_re2[as.character(enq.final$D3_bis_re) == "Refus"] <- NA
summary(enq.final$D3_re2)
table(enq.final$D3_re2)

# activité (utilité? plutot utiliser la CSP?)

table(enq.final$D4_re)
enq.final$D4_re2[as.character(enq.final$D4_re) %in% c("Etudiant avec emploi", "Etudiant sans emploi")] <- "Etudiant"
enq.final$D4_re2[as.character(enq.final$D4_re) %in% c("Emploi à plein temps", "Emploi à temps partiel")] <- "Emploi"
enq.final$D4_re2[as.character(enq.final$D4_re) == "A la retraite"] <- "retraite"
enq.final$D4_re2[as.character(enq.final$D4_re) == "En recherche d'emploi"] <- "recherche emploi"
enq.final$D4_re2[as.character(enq.final$D4_re) == "Au foyer"] <- "Au foyer"
enq.final$D4_re2[as.character(enq.final$D4_re) == "Autre (voir D4_other_re)"] <- NA
summary(enq.final$D4_re2)
table(enq.final$D4_re2)

# activité du conjoint - utilité?

table(enq.final$D7_re)
enq.final$D7_re2[as.character(enq.final$D7_re) %in% c("Etudiant avec emploi", "Etudiant sans emploi")] <- "étudiants"
enq.final$D7_re2[as.character(enq.final$D7_re) %in% c("Emploi à plein temps", "Emploi à temps partiel")] <- "Emploi"
enq.final$D7_re2[as.character(enq.final$D7_re) == "A la retraite"] <- "retraite"
enq.final$D7_re2[as.character(enq.final$D7_re) == "En recherche d’emploi"] <- "recherche emploi"
enq.final$D7_re2[as.character(enq.final$D7_re) == "Au foyer"] <- "Au foyer"
enq.final$D7_re2[as.character(enq.final$D7_re) == "Autre (voir D4_other_re)"] <- NA
summary(enq.final$D7_re2)
table(enq.final$D7_re2)

# Composition du menage aggregée

table(enq.final$D6_re)
enq.final$D6_re2[as.character(enq.final$D6_re) == "Vous uniquement"] <- "Seul"
enq.final$D6_re2[as.character(enq.final$D6_re) %in% c("Vous et votre conjoint(e), avec un ou plusieurs enfant(s)", "Vous et un ou plusieurs enfant(s)")] <- "Parent(s) avec enfants"
enq.final$D6_re2[as.character(enq.final$D6_re) %in% c("Autre (voir D6_other_re)","Vos parents et éventuels frères et sœurs")] <- "Autres"
enq.final$D6_re2[as.character(enq.final$D6_re) == "Vous et votre conjoint(e) (sans enfant)"] <- "Couples sans enfants"
summary(enq.final$D6_re2)
table(enq.final$D6_re2)

# Revenu tranche aggrégées (5 niveaux):

table(enq.final$D12_re)
enq.final$D12_re2 <- as.character(enq.final$D12_re)
enq.final$D12_re2[as.character(enq.final$D12_re) %in% c("De 0 à 1 100€ /mois  (soit 0 à 14 000 € par an)", "De 1 100 à 1 400€ /mois  (soit 14 000 à 17 000 € par an)")] <- "moins de 1400"
enq.final$D12_re2[as.character(enq.final$D12_re) %in% c("De 1 400 à 1 700€ /mois  (soit 17 000 à 21 000 € par an)", "De 1 700 à 2 000€ /mois  (soit 21 000 à 25 000 € par an)")] <- "1400-2000"
enq.final$D12_re2[as.character(enq.final$D12_re) %in% c("De 2 000 à 2 500€ /mois  (soit 25 000 à 30 000 € par an)", "De 2 500 à 2 900€ /mois  (soit 30 000 à 35 000 € par an)")] <- "2000-2900"
enq.final$D12_re2[as.character(enq.final$D12_re) %in% c("De 2 900 à 3 400€ /mois  (soit 35 000 à 41 000 € par an)", "De 3 400 à 4 100€ /mois  (soit 41 000 à 50 000 € par an)")] <- "2900-4100"
enq.final$D12_re2[as.character(enq.final$D12_re) %in% c("De 4 100 à 5 300€ /mois  (soit 50 000 à 63 000 € par an)", "5 300€ et plus /mois  (soit 63 000 € et plus par an)")] <- "plus de 4100"
summary(enq.final$D12_re2)
table(enq.final$D12_re2)



# Unité urbaine: <- Que faire des 0?

table(enq.final$UU_re)
enq.final$UU_re_2 <- enq.final$UU_re
enq.final$UU_re_2[enq.final$UU_re %in% c(1,2)] <- "moins de 10 000 hab"
enq.final$UU_re_2[enq.final$UU_re %in% c(3,4,5)] <- "10 000 - 100 000 hab"
enq.final$UU_re_2[enq.final$UU_re %in% c(6,7,8)] <- "plus de 100 000 hab et Paris"
enq.final$UU_re_2[enq.final$UU_re == 0] <- NA
summary(enq.final$UU_re_2)
table(enq.final$UU_re_2)

# CSP <- 431 NA?

table(enq.final$D8_re)
summary(enq.final$D8_re)

## ACM

library(FactoMineR)

# Substet (sans E6: pas assez d'individus en utilisateurs réguliers, pas fondamental de toutes manières)

acm <- subset(enq.final, select=c(E2_Typologie_utilisateurs, E3_Typologie_utilisateurs, E4_Typologie_utilisateurs, E5_Typologie_utilisateurs,
                                  E7_Typologie_utilisateurs, E8_Typologie_utilisateurs, D1_tr, D3_re2, D4_re2, D6_re2, D12_re2, UU_re_2, D13_re))

acm <- as.data.frame(lapply(acm, factor))

res.acm <- MCA(acm, quali.sup=c(7:13), ncp=3, graph=T)

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

modatot <- which(res.acm$var$contrib[, 1]>seuil 
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

# Mieux axe 1-2: cadre vide

plot(res.acm$var$coord[modatot, 1:2]*1.2, type="n",
     xlab=paste0("Axe 1 (", round(res.acm$eig[1,2], 1), "%)"),
     ylab=paste0("Axe 2 (", round(res.acm$eig[2,2], 1), "%)"),
     main="Premier plan factoriel",
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1)
abline(h=0, v=0, col="grey", lty=3, lwd=1)

# Points et etiquettes
points(res.acm$var$coord[modatot, 1:2],
       col="black",
       pch=c(15, 15, 16, 16, 17, 18, 5, 5, 6, 6))

etiquettes <- rownames(res.acm$var$coord)
print(etiquettes)
etiquettes2 <- c("Smartphone: Non ou peu", "Smartphone: régulier", "Mobile: Non ou peu", "Mobile: régulier",
                 "Tablettes: régulier", "Télé: Non ou peu", "Ordi portable: Non ou peu", "Ordi portable: régulier",
                 "Ordi fixe: Non ou peu", "Ordi fixe: régulier")
col <- c(1,2,1,2,2,1,1,2,1,2)

# Projection etiquettes

text(res.acm$var$coord[modatot,1:2], labels=etiquettes2, 
     col=col, cex=1, pos=c(1, 1, 1, 1, 1, 1, 1, 2, 1, 1))

# Variables supplementaires

print(res.acm$quali.sup$coord[, 1:2])
modasup=c("25-45 ans", "46-65 ans", "<25 ans", ">65 ans", "Bac", "<bac", ">bac", 
          "activite_foyer", "activite_emploi", "activite_etudiant", "activite_recherche_emploi","activite_retraite",
          "menage_autre","menage_couples_sans_enfants","menage_parent(s)_enfants","menage_seul","1400-2000€","2000-2900€",
          "2900-4100€","<1400€",">1400€","10 000- 100 000 hab","<10 000 hab",">100 000 hab","Femme","Homme")

text(res.acm$quali.sup$coord[c(1:5,7:9,11:24,26:28,30:31), 1:2]*1.2,
     labels=modasup,
     cex=0.8, col="blue", font=3)

# Mieux axe 2-3: cadre vide

plot(res.acm$var$coord[modatot, 2:3]*1.2, type="n", 
     xlab=paste0("Axe 2 (", round(res.acm$eig[2,2], 1), "%)"),
     ylab=paste0("Axe 3 (", round(res.acm$eig[3,2], 1), "%)"),
     main="Second plan factoriel",
     cex.main=1, cex.axis=0.8, cex.lab=0.7, font.lab=3, 
     asp=1)
abline(h=0, v=0, col="grey", lty=3, lwd=1)

# Points
points(res.acm$var$coord[modatot, 2:3],
       col="black",
       pch=c(15, 15, 16, 16, 17, 18, 5, 5, 6, 6))


text(res.acm$var$coord[modatot,2:3], labels=etiquettes2,
     col=col, cex=1, pos=c(4,1,1,3,1,2,4,3,1,4))

## Classification ascendante hiérarchique

acm2 <- subset(enq.final, select=c(id_r, E2_Typologie_utilisateurs, E3_Typologie_utilisateurs, E4_Typologie_utilisateurs, E5_Typologie_utilisateurs,
                                  E7_Typologie_utilisateurs, E8_Typologie_utilisateurs))

acm2 <- as.data.frame(lapply(acm2, factor))

res.acm2 <- MCA(acm2, quali.sup=1, ncp=3, graph=T)

classif_equippements <- HCPC(res.acm2, nb.clust =4)
summary(classif_equippements)
cluster <- subset(classif_equippements$data.clust, select=c(id_r,clust))
enq.final <- merge(enq.final, cluster, by="id_r")
table(enq.final$clust)

## Caractérisations des clusters:

# Cluster / sexe

table (enq.final$clust, enq.final$D13_re)
round(prop.table(table(enq.final$clust, enq.final$D13_re), margin = 1)*100, 3)
round(prop.table(table(enq.final$clust, enq.final$D13_re), margin = 2)*100, 3)

# Cluster / age en tranches

table (enq.final$clust, enq.final$D1_tr)
round(prop.table(table(enq.final$clust, enq.final$D1_tr), margin = 1)*100, 3)
round(prop.table(table(enq.final$clust, enq.final$D1_tr), margin = 2)*100, 3)