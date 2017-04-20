# -> Partie 4: 
  
#  Usage des écrans au quotidien et pendant les repas. 
#  regression logit: Y(écran pendant repas)= X(Indicateurs equippement + sexe + age + diplome + mange seul+ éventuellement à préparé)
#  + Régréssion Y = attention portée à l'écran (sur sous population qui possède un écran) = X (same)
#               Y (télé pendant repas) = same 

library(lmtest)
library(ggplot2)

# Premier modèle (Y = ecran pendant repas (oui/non), X= socio-demo + mange seul + indicateurs d'equippement)

## Variables:

# Ecrans pendant repas (oui/non selectionne)

table(enq.final$A11_3_re)
round(prop.table(table(enq.final$A11_3_re))*100,1)

table(enq.final$A21_3_re)
round(prop.table(table(enq.final$A21_3_re[enq.final$A1 != "1 fois"]))*100,1)

table(enq.final$A26_3_re)
round(prop.table(table(enq.final$A26_3_re[enq.final$A1 == "3 fois ou plus"]))*100,1)

# Indicateurs d'equippement

table(enq.final$E2_Typologie_utilisateurs)
table(enq.final$E3_Typologie_utilisateurs)
table(enq.final$E4_Typologie_utilisateurs)
table(enq.final$E5_Typologie_utilisateurs)
table(enq.final$E7_Typologie_utilisateurs)
table(enq.final$E8_Typologie_utilisateurs)

# Clusters equippement

table(enq.final$cluster_equippement)

# Sexe

table(enq.final$D13_re)

# Age en tranche

table(enq.final$D1_tr)

# Dipome (simplifié)

table(enq.final$D3_re2)

# Mange seul: premiere prise

table(enq.final$A12_re)
round(prop.table(table(enq.final$A12_re))*100,1)

# Mange seul / regarde la tele (premiere prise)

round(prop.table(table(enq.final$A12_re, enq.final$r3_1_re), margin = 1)*100,1)

# Attention portée a l'ecran:

table(enq.final$r2_1_re)
table(enq.final$r2_2_re) # ! effectifs faibles (attention a l'interpretation)
table(enq.final$r2_3_re) # effectifs pas assez importants

# Regarder la tele en mangeant (oui/non)

table(enq.final$r3_1_re)
round(prop.table(table(enq.final$r3_1_re))*100,1)


# Ecran pendant repas oui/non (sous population)

# seconde prise:

table(enq.final$A21_3_re[enq.final$A1_re %in% c("2 fois", "3 fois ou plus")])

# troisieme prise:

table(enq.final$A26_3_re[enq.final$A1_re == "3 fois ou plus"]) # Effectifs insuffisants

# Creation d'une variable qui rassemble tous les invidus ayant déclaré avoir regardé un écran quelque soit le repas

enq.final$repas_ecran_meta <- ifelse(enq.final$A11_3_re == "Oui" | enq.final$A21_3_re == "Oui" | enq.final$A26_3_re == "Oui", 
       "Ecran", "Pas d'ecran")

table(enq.final$repas_ecran_meta)
enq.final$repas_ecran_meta <- as.factor(enq.final$repas_ecran_meta)

## Regression première prise 

reg_repecran_1 <- glm(A11_3_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr + D3_re2 + A12_re, family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_1)

# Regression seconde prise

reg_repecran_2 <- glm(A21_3_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr + D3_re2 , family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_2)

# Toutes prises

reg_repecran_all <- glm(repas_ecran_meta ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                          E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                          E8_Typologie_utilisateurs + D13_re + D1_tr + D3_re2 , family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_all)

## Regression attention: 1ere prise

reg_attecran_1 <- glm(r2_1_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr + D3_re2 + A12_re, family=binomial(link = "logit"), data = enq.final,
                      subset= enq.final$id_r[enq.final$A11_3_re == "Oui"])
summary(reg_attecran_1)

# seconde prise:

reg_attecran_2 <- glm(r2_2_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr + D3_re2 + A12_re, family=binomial(link = "logit"), data = enq.final, 
    subset= enq.final$id_r[enq.final$A21_3_re == "Oui"])
summary(reg_attecran_2)

## Regression Télé pendant repas / pas télé

reg_telerepas <- glm(r3_1_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                       E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                       E8_Typologie_utilisateurs + D13_re + D1_tr + D3_re2 + A12_re, family=binomial(link = "logit"), data = enq.final)
summary(reg_telerepas)

# Meme chose mais en faisant varier les modalites de reference

# Age en tranche

table(enq.final$D1_tr)
enq.final$D1_tr <- as.factor(enq.final$D1_tr)
enq.final$D1_tr_2 <- relevel(enq.final$D1_tr, ref="46-65 ans")
enq.final$D1_tr_3 <- relevel(enq.final$D1_tr, ref="plus de 65 ans")

# Dipome (simplifié)

table(enq.final$D3_re2)
enq.final$D3_re2 <- as.factor(enq.final$D3_re2)
enq.final$D3_re2_2 <- relevel(enq.final$D3_re2, ref="Inférieur au bac")
enq.final$D3_re2_3 <- relevel(enq.final$D3_re2, ref="Supérieur au bac")


## Regression première prise 

reg_repecran_1 <- glm(A11_3_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr_2 + D3_re2_2 + A12_re, family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_1)

reg_repecran_1 <- glm(A11_3_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr_3 + D3_re2_3 + A12_re, family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_1)

# Regression seconde prise

reg_repecran_2 <- glm(A21_3_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr_2 + D3_re2_2 , family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_2)

reg_repecran_2 <- glm(A21_3_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr_3 + D3_re2_3 , family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_2)

# Toutes prises

reg_repecran_all <- glm(repas_ecran_meta ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                          E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                          E8_Typologie_utilisateurs + D13_re + D1_tr_2 + D3_re2_2 , family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_all)

reg_repecran_all <- glm(repas_ecran_meta ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                          E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                          E8_Typologie_utilisateurs + D13_re + D1_tr_3 + D3_re2_3 , family=binomial(link = "logit"), data = enq.final)
summary(reg_repecran_all)

## Regression attention: 1ere prise

reg_attecran_1 <- glm(r2_1_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr_2 + D3_re2_2 + A12_re, family=binomial(link = "logit"), data = enq.final,
                      subset= enq.final$id_r[enq.final$A11_3_re == "Oui"])
summary(reg_attecran_1)

reg_attecran_1 <- glm(r2_1_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                        E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                        E8_Typologie_utilisateurs + D13_re + D1_tr_3 + D3_re2_3 + A12_re, family=binomial(link = "logit"), data = enq.final,
                      subset= enq.final$id_r[enq.final$A11_3_re == "Oui"])
summary(reg_attecran_1)


## Regression Télé pendant repas / pas télé

reg_telerepas <- glm(r3_1_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                       E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                       E8_Typologie_utilisateurs + D13_re + D1_tr_2 + D3_re2_2 + A12_re, family=binomial(link = "logit"), data = enq.final)
summary(reg_telerepas)

reg_telerepas <- glm(r3_1_re ~  E2_Typologie_utilisateurs + E3_Typologie_utilisateurs + 
                       E4_Typologie_utilisateurs + E5_Typologie_utilisateurs + E7_Typologie_utilisateurs +
                       E8_Typologie_utilisateurs + D13_re + D1_tr_3 + D3_re2_3 + A12_re, family=binomial(link = "logit"), data = enq.final)
summary(reg_telerepas)



############################

# Graphique: Pourcentage de gens qui regardent différents écrans.
# (quels écrans pendant les repas: intro de la fiche de synthèse)

graphe_usage_repas <- c(length(which(enq.final$r3_1_re == "Oui")), length(which(enq.final$r3_2_re == "Oui")),
                        length(which(enq.final$r3_3_re == "Oui")), length(which(enq.final$r3_4_re == "Oui")),
                        length(which(enq.final$r3_5_re == "Oui")), length(which(is.na(enq.final$R3_other) != T)))

colors <- rainbow(length(graphe_usage_repas), s = 0.5, v=1)

pie(round(prop.table(graphe_usage_repas)*100,1), main = "Quels écrans pendant le repas?",
    labels = c("Télévision", "Téléphone/Smartphone", "Tablette", "Ordinateur", "Console", "Autre"), col = colors)

barplot(round(prop.table(graphe_usage_repas)*100,1), main = "Quels écrans pendant le repas?", col = colors,
        legend.text = c("Télévision", "Téléphone/Smartphone", "Tablette", "Ordinateur", "Console", "Autre"))
