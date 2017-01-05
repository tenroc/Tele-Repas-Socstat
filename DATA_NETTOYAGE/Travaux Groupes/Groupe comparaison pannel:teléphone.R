##On repart de enq03##
setwd("/Users/Timothee/Documents/EHEESS/2-TeleRepas/")
enq.final <- read.csv("./_Data/BDD réconciliée/BDD/enq final_03.csv", encoding = "UTF-8", sep=";")
etiquettes <- read.csv("./_Data/BDD réconciliée/Etiquettes/etiquettes.csv", encoding = "UTF-8")
etiquettes <- as.character(etiquettes$x)
enq.final$X <- NULL
attributes(enq.final)$variable.labels <- etiquettes
attributes(enq.final)$variable.labels[120]


colnames(enq.final)

#Réflexion générale. Le problème de la question sur l'utilisation,
#c'est qu'elle est conditionnelle à la possession de l'outil.
#Autrement dit, on peut parfaitement imaginer un  biais de sélection
#(il y en a 10 qui en ont un, et ils l'utilisent très fréquemment,
#et donc on a l'impression que le groupe l'utilise très fréquemment#
#Même si en pratique, la diff d'utilisation est souvnet pas significative#

#CONCL GENERALE#
#1. Pannelistes : ménages mieux équipés (mais sans doute plus nombreux)
#2. Le nb d'outils utilisé est cependant le plus souvent identique à celui
# des téléphones.
#3. Les pannelistes utilisent plus fréquemment tous les outils/médias,
#sauf : jeux vidéo (sans doute lié à la présence de jeunes), et tablettes (diff
#non significative).



#*****E1*****#
colnames(enq.final)[94]
print(attributes(enq.final)$variable.labels[94])


table.e1 <- round(prop.table(table(enq.final$E1_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.e1) <- c("Pannel","Téléphone")
#"Dans votre ménage, combien y a-t-il de téléphones portables de type smartphone ?"#
print(table.e1)
#On compare expected versus observed
chisq.test(table(enq.final$E1_re,enq.final$dummy.tel))$residuals 	
chisq.test(table(enq.final$E1_re,enq.final$dummy.tel))
#Rappel : Pearson residuals, (observed - expected) / sqrt(expected)#
##Test du chi deux significatifs, donc la satisfaction de la connexion et 
#l'origine de l'enquêté ne sont pas indépendants. L'absence de connexion à internet
# est surreprésentée chez les téléphones#


#*****E2*****#
#Dans votre ménage, combien y a-t-il de téléphones portables de type smartphone ?#
table.e2 <- round(prop.table(table(enq.final$E2_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.e2) <- c("Pannel","Téléphone")
print(table.e2)
chisq.test(table(enq.final$E2_re,enq.final$dummy.tel))$residuals 
chisq.test(table(enq.final$E2_re,enq.final$dummy.tel))
##Non indep. Les pannelistes ont plus de smartphones dans leur ménage (mais pas forcémment par pers)#

#*****E2_a_re*****#
#Vous personnellement, combien en utilisez-vous ?#
table.E2_a_re <- round(prop.table(table(enq.final$E2_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E2_a_re) <- c("Pannel","Téléphone")
print(table.E2_a_re)
chisq.test(table(enq.final$E2_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E2_a_re,enq.final$dummy.tel), simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E2_a_re,enq.final$dummy.tel)
#Pas de diff significative
#Diff utilisation/foyer à investiguer : signifie que le differentiel
#de smartphone est sans doute lié à la présence des enfants (car foyer
#significatif, mais pas nb utilisé)#

#*****E2_b_re*****#
#Et vous personnellement, à quelle fréquence l’utilisez vous ?#
table.E2_b_re <- round(prop.table(table(enq.final$E2_b_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E2_b_re) <- c("Pannel","Téléphone")
print(table.E2_b_re)
chisq.test(table(enq.final$E2_b_re,enq.final$dummy.tel), simulate.p.value = TRUE)
chisq.test(table(enq.final$E2_b_re,enq.final$dummy.tel), simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E2_b_re,enq.final$dummy.tel)
##Diff significative. "Très souvent" plus fréquent dans le pannel, "Jamais"/"Occasionnellement" plus rare.

#*****E3_re*****#
#Dans votre ménage, combien y a-t-il de téléphones portables non smartphone ?#
table.E3_re <- round(prop.table(table(enq.final$E3_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E3_re) <- c("Pannel","Téléphone")
print(table.E3_re)
chisq.test(table(enq.final$E3_re,enq.final$dummy.tel))
chisq.test(table(enq.final$E3_re,enq.final$dummy.tel))$residuals 
##Pas de diff significative##

#*****E3_a_re******#
#Vous personnellement, combien en utilisez-vous ?#
table.E3_a_re <- round(prop.table(table(enq.final$E3_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E3_a_re) <- c("Pannel","Téléphone")
print(table.E3_a_re)
chisq.test(table(enq.final$E3_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E3_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E3_a_re,enq.final$dummy.tel)
##Pas de diff significative##

#*****E3_b_re******#
#Et vous personnellement, à quelle fréquence l’utilisez vous ?
table.E3_b_re <- round(prop.table(table(enq.final$E3_b_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E3_b_re) <- c("Pannel","Téléphone")
print(table.E3_b_re)
chisq.test(table(enq.final$E3_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E3_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E3_b_re,enq.final$dummy.tel)
##Diff significative. Contrairement à ce qu'on pourrait penser, les panelistes
#utilisent plus souvent leur téléphone portable non smartphones que les autres#
#La différence est dans la fréquence d'utilisation (plus que le nb utilisé)#

#****E4*****#
#[E4]Dans votre ménage, combien y a-t-il de tablettes ?
table.E4_re <- round(prop.table(table(enq.final$E4_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E4_re) <- c("Pannel","Téléphone")
print(table.E4_re)
chisq.test(table(enq.final$E4_re,enq.final$dummy.tel))
chisq.test(table(enq.final$E4_re,enq.final$dummy.tel))$residuals 
##Le nb de tablette est significativement diff, notamment le nb >2##

#[E4_b]Vous personnellement, combien en utilisez-vous ?##
table.E4_a_re <- round(prop.table(table(enq.final$E4_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E4_a_re) <- c("Pannel","Téléphone")
print(table.E4_a_re)
chisq.test(table(enq.final$E4_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E4_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E4_a_re,enq.final$dummy.tel)
##Pas de diff sign dans le nb de de tablettes utilisé##

#Et vous personnellement, à quelle fréquence l’utilisez vous ?##
table.E4_b_re <- round(prop.table(table(enq.final$E4_b_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E4_b_re) <- c("Pannel","Téléphone")
print(table.E4_b_re)
chisq.test(table(enq.final$E4_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E4_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E4_b_re,enq.final$dummy.tel)
##Pas de diff significative dans la freq d'utilisation tablette. Surprenant !##

#[E5]Dans votre ménage, combien y a-t-il de télévisions ?#
table.E5_re <- round(prop.table(table(enq.final$E5_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E5_re) <- c("Pannel","Téléphone")
print(table.E5_re)
chisq.test(table(enq.final$E5_re,enq.final$dummy.tel))
chisq.test(table(enq.final$E5_re,enq.final$dummy.tel))$residuals 
##Diff sign. Les telephones ont plus souvent 0 ou 1 télé, les pannelistes 2 ou 3 ou plus#

#[E5_a]Vous personnellement, combien en utilisez-vous ?#
table.E5_a_re <- round(prop.table(table(enq.final$E5_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E5_a_re) <- c("Pannel","Téléphone")
print(table.E5_a_re)
chisq.test(table(enq.final$E5_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E5_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
#Pas de diff sign


#[E5_b]Et vous personnellement, à quelle fréquence l’utilisez vous ?
table.E5_b_re <- round(prop.table(table(enq.final$E5_b_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E5_b_re) <- c("Pannel","Téléphone")
print(table.E5_b_re)
chisq.test(table(enq.final$E5_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E5_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E5_b_re,enq.final$dummy.tel)
#Diff sign. Les pannelistes utilisent plus souvent la télévision que les autres !#
#Omnimedia#

#55 [E6]Dans votre ménage, combien y a-t-il de consoles de jeux ? 
table.E6_re <- round(prop.table(table(enq.final$E6_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E6_re) <- c("Pannel","Téléphone")
print(table.E6_re)
chisq.test(table(enq.final$E6_re,enq.final$dummy.tel))
chisq.test(table(enq.final$E6_re,enq.final$dummy.tel))$residuals 
fisher.test(enq.final$E6_re,enq.final$dummy.tel)
## Dif sign. Bcp plus de consoles de jeux chez les pannelistes##

##[E6_a]Vous personnellement, combien en utilisez-vous ?##
table.E6_a_re <- round(prop.table(table(enq.final$E6_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E6_a_re) <- c("Pannel","Téléphone")
print(table.E6_a_re)
chisq.test(table(enq.final$E6_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E6_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E6_a_re,enq.final$dummy.tel)
##Pas de diff sign#

#[E6_b]Et vous personnellement, à quelle fréquence l’utilisez vous ?#
table.E6_b_re <- round(prop.table(table(enq.final$E6_b_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E6_b_re) <- c("Pannel","Téléphone")
print(table.E6_b_re)
chisq.test(table(enq.final$E6_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E6_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E6_b_re,enq.final$dummy.tel)
##Diff sign. Plus de très souvent chez les téléphones. 
#Les personnes âgées et les jeunes se contrebalanceraient-ils ?#

# [E7]Dans votre ménage, combien y a-t-il d'ordinateurs portables ?
table.E7_re <- round(prop.table(table(enq.final$E7_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E7_re) <- c("Pannel","Téléphone")
print(table.E7_re)
chisq.test(table(enq.final$E7_re,enq.final$dummy.tel))
chisq.test(table(enq.final$E7_re,enq.final$dummy.tel))$residuals 
fisher.test(enq.final$E7_re,enq.final$dummy.tel)
##Les ménages des pannelistes sont significativement mieux (en valeur
##absolue, pas forcément en ratio/individu) équipés que les téléphones,
#(1,2 ou 3 ou plus) ordinateurs portables.

#[E7_a]Vous personnellement, combien en utilisez-vous ?#
table.E7_a_re <- round(prop.table(table(enq.final$E7_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E7_a_re) <- c("Pannel","Téléphone")
print(table.E7_a_re)
chisq.test(table(enq.final$E7_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E7_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E7_a_re,enq.final$dummy.tel)
##Pas de diff significative dans le nb utilisé##

#[E7_b]Et vous personnellement, à quelle fréquence l’utilisez vous ?#
table.E7_b_re <- round(prop.table(table(enq.final$E7_b_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E7_b_re) <- c("Pannel","Téléphone")
print(table.E7_b_re)
chisq.test(table(enq.final$E7_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E7_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E7_b_re,enq.final$dummy.tel)
##Diff significative. Les panelistes l'utilisent plus souvent (souvent ou très souvent)
# que les téléphones#

#[E8]Dans votre ménage, combien y a-t-il d'ordinateurs fixes ?#
table.E8_re <- round(prop.table(table(enq.final$E8_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E8_re) <- c("Pannel","Téléphone")
print(table.E8_re)
chisq.test(table(enq.final$E8_re,enq.final$dummy.tel))
chisq.test(table(enq.final$E8_re,enq.final$dummy.tel))$residuals 
fisher.test(enq.final$E8_re,enq.final$dummy.tel)
##Diff significative. Pannelistes mieux équipés#

#[E8_a]Vous personnellement, combien en utilisez-vous ?#
table.E8_a_re <- round(prop.table(table(enq.final$E8_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E8_a_re) <- c("Pannel","Téléphone")
print(table.E8_a_re)
chisq.test(table(enq.final$E8_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E8_a_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E8_a_re,enq.final$dummy.tel)
##Pas de diff dans le nb utilisé##

#[E8_b]Et vous personnellement, à quelle fréquence l’utilisez vous ?#
table.E8_b_re <- round(prop.table(table(enq.final$E8_a_re,enq.final$dummy.tel), 2),digit=2)
colnames(table.E8_b_re) <- c("Pannel","Téléphone")
print(table.E8_b_re)
chisq.test(table(enq.final$E8_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)
chisq.test(table(enq.final$E8_b_re,enq.final$dummy.tel),simulate.p.value = TRUE)$residuals 
fisher.test(enq.final$E8_b_re,enq.final$dummy.tel)
#Diff sign. Les téléphones l'utilisent plus souvent entre pas du tout et fréquemment,
#tandis qu'un plus grand nombre de panellistes l'utilisent très souvent (extrême)#






















1+1






?chisq.test
##On ne peut pas rejeter l'hypothèse d'indep#






colnames(enq.final)[94]
print(attributes(enq.final)$variable.labels[94])





table(enq.final$E1_re,enq.final$dummy.tel))

##Test du chi deux significatifs, donc la satisfaction de la connexion et 
#l'origine de l'enquêté ne sont pas indépendants.
#On voit qu'il y a 14 points d'écarts pour le tel, 





summary(enq.final$E1)
colnames(enq.final)[94]
attributes(enq.final)$variable.labels[93]

94



Significant



?round
?prop.table
table(tel$E1_re)
table(panel$E1_re)


enq.fi
table(enq.final$E1)
attributes(enq.final)$variable.labels[233]