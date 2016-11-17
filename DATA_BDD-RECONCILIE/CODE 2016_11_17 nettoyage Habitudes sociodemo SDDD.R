##Importation BDD + Etiquettes##
##PB A CHAQUE FOIS QUE J'UTILISE FACTOR, il y a une modalité non reconnue
setwd("C:/Users/emanuel/Documents/Cours Master 1 SocStat/Alimentation")
enq.final <- read.csv("enq final 11.06 TC.csv")
etiquettes <- read.csv("etiquettes.csv")
etiquettes <- as.character(etiquettes$x)
attributes(enq.final)$variable.labels <- etiquettes
attributes(enq.final)$variable.labels[120]
enq.final$X <- NULL
####

####
#H1 : En général, combien de soirées passez-vous chez vous sur une semaine ?#
colnames(enq.final)[115]
attributes(enq.final)$variable.labels[115]
summary(enq.final$H1)
#Déjà dans l'ordre
typeof(enq.final$H1)
class(enq.final$H1)
table(enq.final$H1)
sum(is.na(enq.final$H1))
enq.final$H1_re <- enq.final$H1
#Fait : rien #
#23 NA - s'en occuper#)
####

####
#H2 "Considérez-vous que la soirée d'hier était une soirée inhabituelle ?"
colnames(enq.final)[116]
attributes(enq.final)$variable.labels[116]
typeof(enq.final$H2)
summary(enq.final$H2)

table(enq.final$H2)
sum(is.na(enq.final$H2))
enq.final$H2_re <- enq.final$H2
enq.final$H2_re<-relevel(enq.final$H2_re,"Oui")
summary(enq.final$H2)#Remis dans l'ordre
##.Fait: Rien
##Eventuellement penser à regarder les commentaire en fin d'enquête

#HA3 : Habituellement, prenez-vous votre repas du soir en regardant un écran : #
colnames(enq.final)[117]
attributes(enq.final)$variable.labels[117]
summary(enq.final$HA3)
typeof(enq.final$HA3)
class(enq.final$HA3)
table(enq.final$HA3)
sum(is.na(enq.final$HA3))
enq.final$HA3_re <- enq.final$HA3
enq.final$HA3_re<-relevel(enq.final$HA3_re,"Jamais ou rarement")
summary(enq.final$HA3_re)#Remis dans l'ordre
#Fait : rien #
#23 NA - s'en occuper#)

#HA4_1 : [Avec des amis] \tHabituellement, vous prenez votre repas du soir...\t(Plusieurs réponses possibles) #
#Que faire avec les questions à choix multiple?
colnames(enq.final)[118]
attributes(enq.final)$variable.labels[118]
summary(enq.final$HA4_1)
typeof(enq.final$HA4_1)
class(enq.final$HA4_1)
table(enq.final$HA4_1)
sum(is.na(enq.final$HA4_1))
enq.final$HA4_re <- enq.final$HA4
#Fait : rien #
#23 NA - s'en occuper#)
#HA4_2 : [Avec votre famille ou conjoint(e)] \tHabituellement, vous prenez votre repas du soir...\t(Plusieurs réponses possibles) #
colnames(enq.final)[119]
attributes(enq.final)$variable.labels[119]
summary(enq.final$HA4_2)
typeof(enq.final$HA4_2)
class(enq.final$HA4_2)
table(enq.final$HA4_2)
sum(is.na(enq.final$HA4_2))
enq.final$HA4_2_re <- enq.final$HA4_2
#Fait : rien #
#HA4_2 : [Seul(e)] \tHabituellement, vous prenez votre repas du soir...\t(Plusieurs réponses possibles) #
colnames(enq.final)[120]
attributes(enq.final)$variable.labels[120]
summary(enq.final$HA4_3)
typeof(enq.final$HA4_3)
class(enq.final$HA4_3)
table(enq.final$HA4_3)
sum(is.na(enq.final$HA4_3))
enq.final$HA4_3_re <- enq.final$HA4_3
#Fait : rien #
#D13 : Vous êtes :  #
colnames(enq.final)[121]
attributes(enq.final)$variable.labels[121]
summary(enq.final$D13)#dejà dans l'ordre
typeof(enq.final$D13)
class(enq.final$D13)
table(enq.final$D13)
sum(is.na(enq.final$D13))
enq.final$D13_re <- enq.final$D13
#Fait : rien #
#D13 : Quel âge avez-vous ?  #
colnames(enq.final)[122]
attributes(enq.final)$variable.labels[122]
summary(enq.final$D1)
typeof(enq.final$D1)
class(enq.final$D1)
table(enq.final$D1)
sum(is.na(enq.final$D1))
enq.final$D1_re <- enq.final$D1
#Deux 0 et un 9999, sont-ce des non-réponses? Je les supprime. Tous les autres semblent plausibles
enq.final$D1_re [enq.final$D1 %in% c(0,9999999)]<-NA
#vérification
table(enq.final$D1, useNA = "ifany")
table(enq.final$D1_re, useNA = "ifany")
#Fait : valeurs abjectes supprimées #

#D2 : Quel est le nom de votre commune ?  #
colnames(enq.final)[123]
attributes(enq.final)$variable.labels[123]
summary(enq.final$D2)
typeof(enq.final$D2)
class(enq.final$D2)
table(enq.final$D2)
sum(is.na(enq.final$D2))
enq.final$D2_re <- enq.final$D2
#Fait : rien, Voir Alejandra #

#D3 :  Quel est le diplôme le plus élevé que vous ayez obtenu ?  #
colnames(enq.final)[124]
attributes(enq.final)$variable.labels[124]
summary(enq.final$D3)
typeof(enq.final$D3)
class(enq.final$D3)
table(enq.final$D3)
sum(is.na(enq.final$D3))
enq.final$D3_re<-enq.final$D3
levels(enq.final$D3_re)
enq.final$D3_re <- factor(enq.final$D3_re,levels=c("Aucun diplôme","Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc.",
                                                   "Baccalauréat, BP","BEPC, brevet","CAP, BEP",
                                                   "Certificat d'études primaires",
                                                   "Deug, DUT, BTS, diplômes des professions sociales ou de la santé"
                                                   ,"Licence, Bac+3", "Refus"))

table(enq.final$D3_re)
#PB NE RECONNAIT pas le certificat d'etudes primaires #
#Je pense que l'idéal pour remettre les niveaux dans l'ordre c'est que les niveaux soient fixés

#D3_other : [Autre] Quel est le diplôme le plus élevé que vous ayez obtenu ?  #
colnames(enq.final)[125]
attributes(enq.final)$variable.labels[125]
summary(enq.final$D3_other)
typeof(enq.final$D3_other)
class(enq.final$D3_other)
table(enq.final$D3_other)
sum(is.na(enq.final$D3_other))
enq.final$D3_other_re [enq.final$D3_other %in% c("14 ans au maroc, ne sait pas quel niveau")]<-"14 ans au Maroc (à reclasser)"
enq.final$D3_other_re [enq.final$D3_other %in% c("à l'étranger jusqu'au lycée", "permis de conduire")]<-NA
#A soumettre: pour moi on ne peut rien tirer de "à l'étranger jusqu'au lycée"
enq.final$D3_other_re [enq.final$D3_other %in% c("brevet professionel")]<-"BEPC, brevet"
enq.final$D3_other_re [enq.final$D3_other %in% c("diplome auxiliaire de puericulture", "niveau cap")]<-"CAP, BEP"
enq.final$D3_other_re [enq.final$D3_other %in% c("Diplome d'état")]<-"Diplome d'état (à reclasser)"
#Je comptais le mettre en NA
enq.final$D3_other_re [enq.final$D3_other %in% c("Diplôme de retoucherie")]<-"CAP,BEP? (à reclasser)"
# si retoucherie signifie retouche j'ai cherché et il suffit d'un CAP ou BEP pour y acceder
enq.final$D3_other_re [enq.final$D3_other %in% c("educatrice spécialisée")]<-"Licence, Bac+3" 
enq.final$D3_other_re [enq.final$D3_other %in% c("Je n'ai pas passé mon certificat d'étude donc je ne sais pas ce que ça vaut maintenant")]<-"Aucun diplôme?"
enq.final$D3_other_re [enq.final$D3_other %in% c("niveau 4")]<-"Baccalauréat, BP"#j'ai vérifié
enq.final$D3_other_re [enq.final$D3_other %in% c("passe le bac")]<-"BEPC, brevet"
enq.final$D3_other_re [enq.final$D3_other %in% c("premier prix du conservatoire de paris (20ans)")]<-"premier prix conservatoire"
enq.final$D3_other_re [enq.final$D3_other %in% c("Thèse")]<-"Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc."
table(enq.final$D3_other_re)# il y en a 12 avec les 2 NA, ce qui est normal

#Attention, j'ai un problème avec les accents que je ne parviens pas à corriger,REVENir
#Fait : rien #

#Lien D3 DE_other
enq.final$D3_re <- ifelse(is.na(enq.final$D3_other_re)==FALSE,enq.final$D3_other_re , as.character(enq.final$D3_re))

table(enq.final$D3_re, useNA = "ifany")

enq.final$D3_re <- as.factor(enq.final$D3_re)
typeof(enq.final$D3_re)
class(enq.final$D3_re)
table(enq.final$D3_re)

#D4 : Actuellement, quelle est votre situation ?  # /dans le questionnaire: "exercez-vous une activité rémunérée?
colnames(enq.final)[126]
attributes(enq.final)$variable.labels[126]
summary(enq.final$D4)
typeof(enq.final$D4)
class(enq.final$D4)
table(enq.final$D4)
##Problème de modalités répétées, revoir dictionnaire des variables (Etudiant(s) absents...)
sum(is.na(enq.final$D4))
levels(enq.final$D4)
enq.final$D4 [enq.final$D4 %in% c("Non, à la retraite","A la retraite")]<-"A la retraite"
enq.final$D4 [enq.final$D4 %in% c("Non, en recherche d'emploi","En recherche d'emploi")]<-"En recherche d'emploi"
##PROBLEME, Non en recherche d'emploi ne se range pas## JE NE COMPRENDS PAS DU TOUT
table(enq.final$D4, useNA = "ifany")


enq.final$D4_re <- enq.final$D4
enq.final$D4_re <- factor(enq.final$D4_re,levels=c("A la retraite","Au foyer"
                                                   , "Emploi à plein temps","Emploi à temps partiel",
                                                   "En recherche d'emploi","Etudiant sans emploi", "Non, en recherche d'emploi"))
table(enq.final$D4_re, useNA = "ifany")

#NE FONCTIONNE PAS, (en recherche d'emploi)
#Fait : rien #


#D4_other :  Quel est le diplôme le plus élevé que vous ayez obtenu ?  #
colnames(enq.final)[127]
attributes(enq.final)$variable.labels[127]
summary(enq.final$D4_other)
typeof(enq.final$D4_other)
class(enq.final$D4_other)
table(enq.final$D4_other)
sum(is.na(enq.final$D4_other))

enq.final$D4_other_re [enq.final$D4_other %in% c("autoentrepreneur","autoenrtrepreneur","Indépendant")]<-"Autoentrepreneur"
enq.final$D4_other_re [enq.final$D4_other %in% c("en invalidité","maladie","en invalidite","invalide","INVALIDE","invalidité","Invalidité","invalidite","INVALIDITE","unvalidité")]<-"En invalidité"
enq.final$D4_other_re [enq.final$D4_other %in% c("SANS EMPLOI","sans emploi pas retraité")]<-"En recherche d'emploi"
enq.final$D4_other_re [enq.final$D4_other %in% c("autre")]<-"Autre"
enq.final$D4_other_re [enq.final$D4_other %in% c("femme au foyer")]<-"Au foyer"
enq.final$D4_other_re [enq.final$D4_other %in% c("congé parentale")]<-"En congé parentale"
enq.final$D4_other_re [enq.final$D4_other %in% c("Formation adulte")]<-"En formation"
enq.final$D4_other_re [enq.final$D4_other %in% c("intérim")]<-"Interim"
enq.final$D4_other_re [enq.final$D4_other %in% c("rentiste")]<-"Rentier"




#Maladie est-ce invalidité? Intérim? Rentiste est-ce rentier?


#CE QUI VIENT A LA SUITE JUSQU'A LA VARIABLE SUIVANTE NE FONCTIONNE PAS
##En fait ça marche, mais PB: en recherche d'emploi ne s'accumule pas=> Reglé, je ne sais pas pourquoi.
table(enq.final$D4_other_re, useNA = "ifany")

enq.final$D4_re <- ifelse(is.na(enq.final$D4_other_re)==FALSE,enq.final$D4_other_re , as.character(enq.final$D4_re))

table(enq.final$D4_re, useNA = "ifany")

enq.final$D4_re <- as.factor(enq.final$D4_re)
typeof(enq.final$D4_re)
class(enq.final$D4_re)
table(enq.final$D4_re)

#Fait : rien #

#Quelle est votre profession ou la derniÃ¨re que vous ayez exercÃ©e ?  # Voir Antoine et Nadia
colnames(enq.final)[128]
attributes(enq.final)$variable.labels[128]
summary(enq.final$D5)
typeof(enq.final$D5)
class(enq.final$D5)
table(enq.final$D5)
sum(is.na(enq.final$D5))
#Fait rien

#Qui compose votre foyer ? 
colnames(enq.final)[129]
attributes(enq.final)$variable.labels[129]
summary(enq.final$D6)
typeof(enq.final$D6r)
class(enq.final$D6)
table(enq.final$D6)
sum(is.na(enq.final$D6))
enq.final$D6_re<-enq.final$D6
levels(enq.final$D6_re)
enq.final$D6_re <- factor(enq.final$D6_re,levels=c("Vos parents et éventuels frères et soeurs","Vous et un ou plusieurs enfant(s)",
                                                   "Vous et votre conjoint(e) (sans enfant)","Vous et votre conjoint(e), avec un ou plusieurs enfant(s)",
                                                   "Vous uniquement"
                                                   ))
##PB avec Vos parents et éventuels frères et sours NON RECONNU
table(enq.final$D6_re)


#[Autre] Qui compose votre foyer ? 
colnames(enq.final)[130]
attributes(enq.final)$variable.labels[130]
summary(enq.final$D6_other)
typeof(enq.final$D6r)
class(enq.final$D6)
table(enq.final$D6)
sum(is.na(enq.final$D6))
levels(enq.final$D6_other)
enq.final$D6_other_re [enq.final$D6_other %in% c("5 colocataires","collocation","colocataire","Colocataires","colocation","COLOCATION")]<-"vous et un ou plusieurs colocataire(s)"
enq.final$D6_other_re [enq.final$D6_other %in% c("amis")]<-"vous et un ou plusieurs ami(s)"
enq.final$D6_other_re [enq.final$D6_other %in% c("moi et mes grands-parents", "Grands parents et moi")]<-"Vous et vos grand parents"
enq.final$D6_other_re [enq.final$D6_other %in% c("Seule avec mon chat")]<-"Vous uniquement"
enq.final$D6_other_re [enq.final$D6_other %in% c("2 enfants en accueuil et petit(e)s filles")]<-"vous , un ou plusieurs enfant(s) d'accueil et un ou plusieurs petits enfants"
enq.final$D6_other_re [enq.final$D6_other %in% c("Conjoint petits enfants enfant")]<-"conjoint avec enfant(s) et petit(s) enfant(s)"
enq.final$D6_other_re [enq.final$D6_other %in% c("mere conjointe fils")]<-"conjoint(e) avec enfant(s) et parent(s)"

table(enq.final$D6_other_re,useNA = "ifany")

#Lien D6 D6_other
enq.final$D6_re <- ifelse(is.na(enq.final$D6_other_re)==FALSE,enq.final$D6_other_re , as.character(enq.final$D6_re))

table(enq.final$D6_re, useNA = "ifany")

enq.final$D6_re <- as.factor(enq.final$D6_re)
typeof(enq.final$D6_re)
class(enq.final$D6_re)
table(enq.final$D6_re)

#Actuellement, quelle est la situation de votre conjoint ? 
colnames(enq.final)[131]
attributes(enq.final)$variable.labels[131]
summary(enq.final$D7)
typeof(enq.final$D7)
class(enq.final$D7)
table(enq.final$D7)
sum(is.na(enq.final$D7))
levels(enq.final$D7)
enq.final$D7_re <-enq.final$D7
enq.final$D7_re <- factor(enq.final$D7_re,levels=c("A la retraite","Au foyer"
                                                   , "Emploi à plein temps","Emploi à temps partiel",
                                                   "En recherche d'emploi","Etudiant avec emploi","Etudiant sans emploi"))
table(enq.final$D7_re,useNA="ifany")
##PB, le programme ne reconnaît pas "en recherche d'emploi"

#[Autre] Actuellement, quelle est la situation de votre conjoint ? 
colnames(enq.final)[132]
attributes(enq.final)$variable.labels[132]
summary(enq.final$D7_other)
typeof(enq.final$D7_other)
class(enq.final$D7_other)
table(enq.final$D7_other)
sum(is.na(enq.final$D7_other))
levels(enq.final$D7_other)
#QUID DE RESTAURATEUR?
enq.final$D7_other_re [enq.final$D7_other %in% c("agriculteur (auto entrepreneur)","Chef d'entreprise","patron","restaurateur")]<-"Autoentrepreneur"
enq.final$D7_other_re [enq.final$D7_other %in% c("en invalidité","Handicapés","invalidite","invalide","INVALIDE","invalidité","Invalidité","invalidite","INVALIDITE","unvalidité")]<-"Non, en invalidité"
enq.final$D7_other_re [enq.final$D7_other %in% c("refus")]<-"Refus"
enq.final$D7_other_re [enq.final$D7_other %in% c("variable")]<-"variable"
enq.final$D7_other_re [enq.final$D7_other %in% c("SANS EMPLOI")]<-"En recherche d'emploi"
enq.final$D7_other_re [enq.final$D7_other %in% c("Conger maternité")]<-"congé maternité"
enq.final$D7_other_re [enq.final$D7_other %in% c("etudiante")]<-"Etudiant"
enq.final$D7_other_re [enq.final$D7_other %in% c("Formation")]<-"en formation"
enq.final$D7_other_re [enq.final$D7_other %in% c("intercontrat","interim")]<-"interim"
enq.final$D7_other_re [enq.final$D7_other %in% c("medecin")]<-"Emploi"
table(enq.final$D7_other_re,useNA="ifany")

#Lien D7 D7_other
enq.final$D7_re <- ifelse(is.na(enq.final$D7_other_re)==FALSE,enq.final$D7_other_re , as.character(enq.final$D7_re))

table(enq.final$D7_re, useNA = "ifany")

enq.final$D7_re <- as.factor(enq.final$D7_re)
typeof(enq.final$D7_re)
class(enq.final$D7_re)
table(enq.final$D7_re)

#Quelle est cette profession ?  #NADIA ET ANTOINE?
colnames(enq.final)[133]
attributes(enq.final)$variable.labels[133]
summary(enq.final$D8)
typeof(enq.final$D8)
class(enq.final$D8)
table(enq.final$D8)
sum(is.na(enq.final$D8))
levels(enq.final$D8)

#Quel est le diplÃ´me le plus Ã©levÃ© de votre conjoint ?
colnames(enq.final)[134]
attributes(enq.final)$variable.labels[134]
summary(enq.final$D3_bis)
typeof(enq.final$D3_bis)
class(enq.final$D3_bis)
table(enq.final$D3_bis)
sum(is.na(enq.final$D3_bis))
levels(enq.final$D3_bis)

enq.final$D3_bis_re<-enq.final$D3_bis

enq.final$D3_bis_re <- factor(enq.final$D3_bis_re,levels=c("Aucun diplôme","Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc.",
                                                   "Baccalauréat, BP","BEPC, brevet","CAP, BEP",
                                                   "Certificat d'études primaires",
                                                   "Deug, DUT, BTS, diplômes des professions sociales ou de la santé"
                                                   ,"Licence, Bac+3", "Refus"))

table(enq.final$D3_bis_re)
##Meme pb certificat d'etude primaire




#[autre] Quel est le diplÃ´me le plus Ã©levÃ© de votre conjoint ?
colnames(enq.final)[135]
attributes(enq.final)$variable.labels[135]
summary(enq.final$D3_bis_other)
typeof(enq.final$D3_bis)
class(enq.final$D3_bis)
table(enq.final$D3_bis)
sum(is.na(enq.final$D3_bis))
levels(enq.final$D3_bis)
enq.final$D3_bis_other_re [enq.final$D3_bis_other %in% c("certificat de capacite professionnelle taxi")]<-"CAP, BEP"
enq.final$D3_bis_other_re [enq.final$D3_bis_other %in% c("lycée à l'étranger")]<-"NA ou BAC?"
enq.final$D3_bis_other_re [enq.final$D3_bis_other %in% c("Artiste")]<-"???"
enq.final$D3_bis_other_re [enq.final$D3_bis_other %in% c("refus")]<-"Refus"
table(enq.final$D3_bis_other_re)

#Lien D3_bis D3_bis_other
enq.final$D3_bis_re <- ifelse(is.na(enq.final$D3_bis_other_re)==FALSE,enq.final$D3_bis_other_re , as.character(enq.final$D3_bis_re))

table(enq.final$D3_bis_re, useNA = "ifany")

enq.final$D3_bis_re <- as.factor(enq.final$D3_bis_re)
typeof(enq.final$D3_bis_re)
class(enq.final$D3_bis_re)
table(enq.final$D3_bis_re)




#Combien dâ???Tenfants vivent avec vous au moins la moitiÃ© du temps?
colnames(enq.final)[136]
attributes(enq.final)$variable.labels[136]
summary(enq.final$D9)
typeof(enq.final$D9)
class(enq.final$D9)
table(enq.final$D9)
sum(is.na(enq.final$D9))
levels(enq.final$D9)
enq.final$D9_re<-enq.final$D9
#Rien à faire

#Quel Ã¢ge a votre enfant ?
colnames(enq.final)[137]
attributes(enq.final)$variable.labels[137]
summary(enq.final$D10_bis)
typeof(enq.final$D10_bis)
class(enq.final$D10_bis)
table(enq.final$D10_bis)#ça me paraît correct, pas de valeurs absurdes
sum(is.na(enq.final$D10_bis))
levels(enq.final$D10_bis)
enq.final$D10_bis_re<-enq.final$D10_bis

#Quel Ã¢ge a le plus jeune ?
colnames(enq.final)[138]
attributes(enq.final)$variable.labels[138]
summary(enq.final$D10)
typeof(enq.final$D10)
class(enq.final$D10)
table(enq.final$D10)#ça me paraît correct, pas de valeurs absurdes
sum(is.na(enq.final$D10))
levels(enq.final$D10)
enq.final$D10_re<-enq.final$D10

#Quel Ã¢ge a le plus jeune ?
colnames(enq.final)[139]
attributes(enq.final)$variable.labels[139]
summary(enq.final$D11)
typeof(enq.final$D11)
class(enq.final$D11)
table(enq.final$D11)#ça me paraît correct, pas de valeurs absurdes
sum(is.na(enq.final$D11))
levels(enq.final$D11)
enq.final$D11_re<-enq.final$D11

#Quel est le revenu mensuel moyen de votre mÃ©nage ? (en revenu net)
colnames(enq.final)[140]
attributes(enq.final)$variable.labels[140]
summary(enq.final$D12)
typeof(enq.final$D12)
class(enq.final$D12)
table(enq.final$D12)
sum(is.na(enq.final$D12))
levels(enq.final$D12)
enq.final$D12_re<-as.character(enq.final$D12)
enq.final$D12_re <- factor(enq.final$D12_re,levels=c("De 0 à 1 100??? /mois  (soit 0 à 14 000 ??? par an)","De 1 100 à 1 400??? /mois  (soit 14 000 à 17 000 ??? par an)",
                                                           "De 1 400 à 1 700??? /mois  (soit 17 000 à 21 000 ??? par an)","De 1 700 à 2 000??? /mois  (soit 21 000 à 25 000 ??? par an)",
                                                           "De 2 000 à 2 500??? /mois  (soit 25 000 à 30 000 ??? par an)",
                                                           "De 2 500 à 2 900??? /mois  (soit 30 000 à 35 000 ??? par an)"
                                                           ,"De 2 900 à 3 400??? /mois  (soit 35 000 à 41 000 ??? par an)", "De 3 400 à 4 100??? /mois  (soit 41 000 à 50 000 ??? par an)",
                                                           "De 4 100 à 5 300??? /mois  (soit 50 000 à 63 000 ??? par an)",
                                                           "5 300??? et plus /mois  (soit 63 000 ??? et plus par an)","Refus","Ne sait pas"))
table(enq.final$D12_re)
#Ne reconnait pas les niveaux, c'est pas possible que j'ai mal écrit pcq je copie/colle
?factor

#D14 c'est les commentaires. Comment les traiter

