library ('FactoMineR')

## Intégration à l'ACM /repas principal des catégories d'équippement (faire tourner le code équippement et repas principal dans leur totalité avant de lancer ce script)

# Subset ACM

jeu_acm_repas_equippement <- subset(e,select=c(A11_1_re,A11_2_re,A7_re,A12_re,A13_4_re,A13_5_re,
                         A11_3_re,A8_acm,A9_re,A10_acm,A3_re,
                         E2_Typologie_utilisateurs, E3_Typologie_utilisateurs,
                         E4_Typologie_utilisateurs, E5_Typologie_utilisateurs, E7_Typologie_utilisateurs,
                         E8_Typologie_utilisateurs))

jeu_acm_repas_equippement <- as.data.frame(lapply(jeu_acm_repas_equippement, factor))

jeu_acm_repas_equippement <-na.exclude(jeu_acm_repas_equippement)

res.acm <- MCA(jeu_acm_repas_equippement, quali.sup=c(12:17), ncp=3, graph=T)

# On retiens les deux premiers axes

# Contribution moyenne
seuil <- 100/nrow(res.acm$var$contrib)

modatot <- which(res.acm$var$contrib[, 1]>seuil 
                 | res.acm$var$contrib[, 2]>seuil)
modatot

# Grpahique / avec modasupp

plot.MCA(res.acm, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=c(modatot))


## Intégration à l'ACM /repas principal des cluster équippement

jeu_acm_repas_equippement <- subset(e,select=c(A11_1_re,A11_2_re,A7_re,A12_re,A13_4_re,A13_5_re,
                                               A11_3_re,A8_acm,A9_re,A10_acm,A3_re,
                                               cluster_equippement))

jeu_acm_repas_equippement <- as.data.frame(lapply(jeu_acm_repas_equippement, factor))

jeu_acm_repas_equippement <-na.exclude(jeu_acm_repas_equippement)

res.acm <- MCA(jeu_acm_repas_equippement, quali.sup=c(12), ncp=3, graph=T)

# On retiens les deux premiers axes

# Contribution moyenne
seuil <- 100/nrow(res.acm$var$contrib)

modatot <- which(res.acm$var$contrib[, 1]>seuil 
                 | res.acm$var$contrib[, 2]>seuil)
modatot

# Grpahique / avec modasupp

plot.MCA(res.acm, invisible=c("ind"), 
         title="Nuage des modalites actives Plan 1-2", axes=c(1,2), 
         autoLab="yes", unselect=1,
         selectMod=c(modatot))
