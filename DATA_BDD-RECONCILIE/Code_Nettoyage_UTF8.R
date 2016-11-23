##Importation BDD + Etiquettes##
setwd("C:/Users/sarah/Google Drive/2-TeleRepas")
enq.final <- read.csv("_Data/BDD réconciliée/BDD/enq final.csv", encoding = "UTF-8")
etiquettes <- read.csv("_Data/BDD réconciliée/Etiquettes/etiquettes.csv", encoding = "UTF-8")
etiquettes <- as.character(etiquettes$x)
attributes(enq.final)$variable.labels <- etiquettes
attributes(enq.final)$variable.labels[120]
enq.final$X <- NULL

####

####
#A1 : Hier, à partir de 18h...#
colnames(enq.final)[8]
attributes(enq.final)$variable.labels[8]
summary(enq.final$A1)
typeof(enq.final$A1)
class(enq.final$A1)
table(enq.final$A1)
sum(is.na(enq.final$A1))
enq.final$A1_re <- enq.final$A1
#Fait : rien (la variable est déjà de type factor)#
#(A faire : transformer en variable quanti pourrait donner une idée du nb moyen de prise alimentaire (borne inférieure). Pas sûr que ça soit à faire cela dit.
#15 NA - s'en occuper#)
####

####
#A2_1_1 "[Prise alimentaire] [Heure] À quelle heure a eu lieu votre prise alimentaire  ?"
colnames(enq.final)[9]
attributes(enq.final)$variable.labels[9]
typeof(enq.final$A2_1_1)
summary(enq.final$A2_1_1)
table(enq.final$A2_1_1)
sum(is.na(enq.final$A2_1_1))
#FAIT/A FAIRE : voir plus bas (j'en fais deux d'un coup)#

#"A2_1_2 "[Prise alimentaire] [Minute] À quelle heure a eu lieu votre prise alimentaire  ?"#
colnames(enq.final)[10]
attributes(enq.final)$variable.labels[10]
typeof(enq.final$A2_1_2)
table(enq.final$A2_1_2)
sum(is.na(enq.final$A2_1_2))

enq.final$A2_re <- paste(enq.final$A2_1_1, enq.final$A2_1_2, sep=',')
enq.final$A2_re <- gsub("h",",",enq.final$A2_re, ignore.case =TRUE)
enq.final$A2_re <- gsub("mn","",enq.final$A2_re, ignore.case =TRUE)
enq.final$A2_re <- gsub(",,",",",enq.final$A2_re, ignore.case =TRUE)
enq.final$A2_re <- gsub(",00,",",",enq.final$A2_re, ignore.case =TRUE)
enq.final$A2_re <- gsub(".00,",",",enq.final$A2_re, ignore.case =TRUE)
enq.final$A2_re <- ifelse(enq.final$A2_re=="19,0","19,00",ifelse(enq.final$A2_re=="18,0","18,00",ifelse(enq.final$A2_re=="19,0","19,00",ifelse(enq.final$A2_re=="20,0","20,00",ifelse(enq.final$A2_re=="21,0","21,00",ifelse(enq.final$A2_re=="23,0","23,00",ifelse(enq.final$A2_re=="22,0","22,00",enq.final$A2_re)))))))
enq.final$A2_re <- ifelse(enq.final$A2_re=="6,00","06,00",ifelse(enq.final$A2_re=="7,00","07,00",ifelse(enq.final$A2_re=="7,15","07,15",ifelse(enq.final$A2_re=="7,20","07,20",ifelse(enq.final$A2_re=="8,00","08,00",enq.final$A2_re)))))

table(enq.final$A2_re)
typeof(enq.final$A2_re)
class(enq.final$A2_re)

#A FAIRE :
#(
#(1)id_r = 479 a une réponse aberrante ("12,30-19,00") - C'est un pannel#
#(2)il existe un certain nombre de réponses problématiques à recoder/vérifier : 05,30 /07,30/6,00/7,00/7,15/7,20/8,00.#
#2-1 - 5h30 : pain grillé (ID_R 499)
#2-2 ID_R 437-439 "UN BOL DE CHOCOLAT DES TARTINES BEURRE UN VERRE DE JUS D ORANGE" & 07h30....PANNEL
#2-3 ID_R 567 MUESLI à 6h00....
#2-3 Que fait-on des 16h à 16H30 ?
#(3) J'ai laissé cette variable en charactère. En quoi faut-il la convertir ? Il est possible de la convertir en objet #time# à l'aide du code as.POSIXct(enq.final$A2_re, format = "%H,%M"), mais "it turns out there's no pure "time" object, so every time must also have a date" donc ça donne forcément une date##
#(4) NA à enquêter (231 ????!!!!!) = même nombre de NA que dans les variables d'origine, c'est étrange - peut être problème d'importation ?#
#(5) Il est sans doute faux de considérer que lorsque qqn dit "19h", il voulait dire "19h,00")
#)
####


####
#A2_bis_1_1 [Heure] À quelle heure a eu lieu votre prise alimentaire principale (celle que vous considérez comme la plus importante) ?""
colnames(enq.final)[11]
attributes(enq.final)$variable.labels[11]
typeof(enq.final$A2_bis_1_1)
table(enq.final$A2_bis_1_1)
sum(is.na(enq.final$A2_bis_1_1))
####

####
#"A2_bis_1_2" "[Prise alimentaire] [Minute] À quelle heure a eu lieu votre prise alimentaire principale (celle que vous considérez comme la plus importante) ?"
colnames(enq.final)[12]
attributes(enq.final)$variable.labels[12]
typeof(enq.final$A2_bis_1_2)
table(enq.final$A2_bis_1_2)
sum(is.na(enq.final$A2_bis_1_2))
####


####
enq.final$A2_bis_re <- paste(enq.final$A2_bis_1_1, enq.final$A2_bis_1_2, sep=',')
table(enq.final$A2_bis_re)
enq.final$A2_bis_re <- gsub("h",",",enq.final$A2_bis_re, ignore.case =TRUE)
enq.final$A2_bis_re <- gsub(",,",",",enq.final$A2_bis_re, ignore.case =TRUE)
enq.final$A2_bis_re <-ifelse(enq.final$A2_bis_re=="00,0","00,00",ifelse(enq.final$A2_bis_re=="1,0","01,00",ifelse(enq.final$A2_bis_re=="12,0","12,00",ifelse(enq.final$A2_bis_re=="13,0","13,00",ifelse(enq.final$A2_bis_re=="13,5","13,05",ifelse(enq.final$A2_bis_re=="16,0","16,00",ifelse(enq.final$A2_bis_re=="17,0","17,00",ifelse(enq.final$A2_bis_re=="18,0","18,00",ifelse(enq.final$A2_bis_re=="19,0","19,00",ifelse(enq.final$A2_bis_re=="19,30,30","19,30",ifelse(enq.final$A2_bis_re=="1930,30","19,30",ifelse(enq.final$A2_bis_re=="21,0","21,00",ifelse(enq.final$A2_bis_re=="22,0","22,00",ifelse(enq.final$A2_bis_re=="23,0","23,00",ifelse(enq.final$A2_bis_re=="8,0","8,00",ifelse(enq.final$A2_bis_re=="20,0","20,00",enq.final$A2_bis_re))))))))))))))))
enq.final$A2_bis_re <- ifelse(enq.final$A2_bis_re=="8,15","08,15",ifelse(enq.final$A2_bis_re=="8,30","08,30",ifelse(enq.final$A2_bis_re=="9,10","09,10",ifelse(enq.final$A2_bis_re=="9,5","09,5",ifelse(enq.final$A2_bis_re=="7,10","07,10", ifelse(enq.final$A2_bis_re=="7,30","07,30",ifelse(enq.final$A2_bis_re==".              8,40","08,40",ifelse(enq.final$A2_bis_re=="8,00","08,00",enq.final$A2_bis_re))))))))
table(enq.final$A2_bis_re)
##
#NOTE : Je n'ai clairement pas pris le chemin le plus court pour faire ce que j'ai fait, mais néanmoins je l'ai fait.
#Note 2 : ce truc bizarre dans mon code ".              8,40" s'explique par la présence de ce truc dans la bdd#

# A FAIRE -----OU PAS-----#
#1. Un certain nombre de valeurs problématiques : 13h, 16h, etc.
#2. Recoder "13,5"/22,9/9,5 ( sans doute 13,05/22,09/09,05) mais à voir
#3. + même pb que pour la question précédente (i.e quel type de variable))
####

####
#A3 "Combien de temps a-t-elle duré ?"
colnames(enq.final)[13]
attributes(enq.final)$variable.labels[13]
typeof(enq.final$A3)
class(enq.final$A3)
table(enq.final$A3)
sum(is.na(enq.final$A3))

enq.final$A3_re <- enq.final$A3

#FAIT : rien (déjà factor)#

#A faire : rien à signaler (mais je me dis qu'on va galérer pour faire des moyennes vu qu'on a que des intervalles (sauf à attribuer à l'intervalle la moyenne de l'intervalle))
####

####
#A4 "Qui a préparé ce que vous avez mangé ?"
colnames(enq.final)[14]
attributes(enq.final)$variable.labels[14]
typeof(enq.final$A4)
class(enq.final$A4)
table(enq.final$A4)
sum(is.na(enq.final$A4))

enq.final$A4_re <- as.factor(enq.final$A4)

#FAIT : rien (déjà en factor) #
#Remarque Gael : possible de regrouper les modalités 2& 3#


#A FAIRE : rien à signaler#
####

####
#A5 "Combien de temps vous a pris la préparation approximativement ?"
colnames(enq.final)[15]
attributes(enq.final)$variable.labels[15]
typeof(enq.final$A5)
class(enq.final$A5)
table(enq.final$A5)
sum(is.na(enq.final$A5))

enq.final$A5_re <- enq.final$A5
table(enq.final$A5_re)
sum(is.na(enq.final$A5_re))
#FAIT : rien (déjà en factor)#

#A FAIRE : rien à signaler#
####

####
#A5 "Combien de temps vous a pris la préparation approximativement ?"
colnames(enq.final)[15]
attributes(enq.final)$variable.labels[15]
typeof(enq.final$A5)
class(enq.final$A5)
table(enq.final$A5)
sum(is.na(enq.final$A5))

enq.final$A5_re <- enq.final$A5
table(enq.final$A5_re)
sum(is.na(enq.final$A5_re))
#FAIT :rien (déjà factor)#

#A FAIRE : rien à signaler#
####

####
#A6 "Veuillez lister tout ce que vous avez mangé à cette occasion :"
colnames(enq.final)[16]
attributes(enq.final)$variable.labels[16]
typeof(enq.final$A6)
class(enq.final$A6)
table(enq.final$A6)
sum(is.na(enq.final$A6))

enq.final$A6_re <- enq.final$A6

#FAIT : rien (déjà en caractère)#
####

####
#A7 "Considérez-vous que c'était un repas ?"
colnames(enq.final)[17]
attributes(enq.final)$variable.labels[17]
typeof(enq.final$A7)
class(enq.final$A7_re)
table(enq.final$A7)
sum(is.na(enq.final$A7))

enq.final$A7_re <- enq.final$A7


#FAIT : rien (déjà en factor)#
####

####
#A8 "Pouvez-vous me dire où vous étiez à ce moment-là ?
colnames(enq.final)[18]
attributes(enq.final)$variable.labels[18]
typeof(enq.final$A8)
class(enq.final$A8)
table(enq.final$A8)
sum(is.na(enq.final$A8))

#Je la passe en caractère avant de la repasser en factor parce que sinon j'ai un problème lorsque j'essaie d'ajouter une modalité (les modalités sont stockées sous forme de 1,2,...)"
enq.final$A8_re <- as.character(enq.final$A8)


#A8_other [Autre] Pouvez-vous me dire où vous étiez à ce moment-là ? 
colnames(enq.final)[19]
attributes(enq.final)$variable.labels[19]
typeof(enq.final$A8_other)
class(enq.final$A8_other)
table(enq.final$A8_other)
sum(is.na(enq.final$A8_other))

#Je nettoie rapidement les modalités (regroupement + forme)#
enq.final$A8_other_re <- ifelse(enq.final$A8_other=="a la danse","A la danse", ifelse(enq.final$A8_other=="hopital"|enq.final$A8_other=="HOPITAL","A l'hopital",enq.final$A8_other))

table(enq.final$A8_other_re)

#Je les re-bascule dans la variable A8_re : les individus ayant choisi "Autre, précisez" sont sinon codés comme NA#
enq.final$A8_re <- ifelse(is.na(enq.final$A8_other_re)==FALSE,"Autre, précisez (voir A8_other_re)", enq.final$A8_re)

#Je rebascule la variable en factor#
enq.final$A8_re <- as.factor(enq.final$A8_re)
typeof(enq.final$A8_re)
class(enq.final$A8_re)
table(enq.final$A8_re)
#FAIT : regroupement A8 & A8 other#
#A faire : ras#
##Remarque Mme Plessz : ordonner les modalités (là autre s'est fouttu en 2ème position)#
#Question : que fait-on de danse et hopital ? Catégorie Autre semble pas mal#
####



####
#A9 "Est-ce que vous étiez à table ? "
colnames(enq.final)[20]
attributes(enq.final)$variable.labels[20]
typeof(enq.final$A9)
class(enq.final$A9)
table(enq.final$A9)
sum(is.na(enq.final$A9))

enq.final$A9_re <- enq.final$A9
#FAIT : rien#
#A faire : ras#
####

####
#A10  "Dans quelle pièce vous-trouviez-vous ?"
colnames(enq.final)[21]
attributes(enq.final)$variable.labels[21]
typeof(enq.final$A10)
class(enq.final$A10)
table(enq.final$A10)
sum(is.na(enq.final$A10))

#Je la passe en caractère avant de la repasser en factor parce que sinon j'ai un problème lorsque j'essaie d'ajouter une modalité (les modalités sont stockées sous forme de 1,2,...)"
enq.final$A10_re <- as.character(enq.final$A10)

colnames(enq.final)[22]
attributes(enq.final)$variable.labels[22]
typeof(enq.final$A10_other)
class(enq.final$A10_other)
table(enq.final$A10_other)
sum(is.na(enq.final$A10_other))

enq.final$A10_other_re <- as.character(enq.final$A10_other)


#Je nettoie rapidement les modalités (regroupement + forme)#

enq.final$A10_other_re <-  ifelse(enq.final$A10_other_re=="bureau"|enq.final$A10_other_re=="BUREAU","Bureau",ifelse(enq.final$A10_other_re=="CHAMBRE FILS","Chambre Fils",ifelse(enq.final$A10_other_re=="dehors","Dehors",ifelse(enq.final$A10_other_re=="salle a manger"|enq.final$A10_other_re=="salle à manger","Salle à manger", ifelse(enq.final$A10_other_re=="terrasse","Terrasse",ifelse(enq.final$A10_other_re=="veranda"|enq.final$A10_other_re=="véranda","Veranda",enq.final$A10_other_re))))))
table(enq.final$A10_other_re)


#Je les re-bascule dans la variable#

enq.final$A10_re <- ifelse(is.na(enq.final$A10_other_re)==FALSE,"Autre (réponse ouverte)", enq.final$A10_re)
enq.final$A10_re <- as.factor(enq.final$A10_re)
table(enq.final$A10_re)
table(enq.final$A10_other_re)

#FAIT : regroupement A10 et A10other#
#A FAIRE : réflexion sur le thème: faut-il rebasculer des modalité de A10_other dans A10 ? on pourrait regrouper Terrasse + véranda + Dehors, faire basculer sale à manger (presque autant que 1 pièce),...Que faire de "bureau" ?#
## *PROBLEME* : il y a une modalité 7## Problème reglé : j'avais oublié de rebasculer la variabel en caractère#
####




####
#A11_1 "[Non, je ne faisais rien d'autre] Est-ce que vous faisiez quelque chose d'autre en mangeant ? "

colnames(enq.final)[23]
attributes(enq.final)$variable.labels[23]
typeof(enq.final$A11_1)
class(enq.final$A11_1)
table(enq.final$A11_1)
sum(is.na(enq.final$A11_1))

table(enq.final$A11_1)
table(enq.final$A11_2)
table(enq.final$A11_3)
table(enq.final$A11_4)
table(enq.final$A11_5)

#Je pense qu'il est mieux de laisser ça sous la forme de variables dichotomiques (vu que QCM)#

enq.final$A11_1_re <- enq.final$A11_1
enq.final$A11_2_re <- enq.final$A11_2
enq.final$A11_3_re <- enq.final$A11_3
enq.final$A11_4_re <- enq.final$A11_4
enq.final$A11_5_re <- enq.final$A11_5

#FAIT : RIEN#
#A FAIRE : NSP#
####




####
#A11_1 "[Non, je ne faisais rien d'autre] Est-ce que vous faisiez quelque chose d'autre en mangeant ? "

colnames(enq.final)[23]
attributes(enq.final)$variable.labels[23]
typeof(enq.final$A11_1)
class(enq.final$A11_1)
table(enq.final$A11_1)
sum(is.na(enq.final$A11_1))

table(enq.final$A11_1)
table(enq.final$A11_2)
table(enq.final$A11_3)
table(enq.final$A11_4)
table(enq.final$A11_5)

#Je pense qu'il est mieux de laisser ça sous la forme de variables dichotomiques (vu que QCM)#

enq.final$A11_1_re <- enq.final$A11_1
enq.final$A11_2_re <- enq.final$A11_2
enq.final$A11_3_re <- enq.final$A11_3
enq.final$A11_4_re <- enq.final$A11_4
enq.final$A11_5_re <- enq.final$A11_5

#FAIT : RIEN#
#A FAIRE : NSP#
####



####
colnames(enq.final)[28]
attributes(enq.final)$variable.labels[28]
typeof(enq.final$A11_other)
class(enq.final$A11_other)
table(enq.final$A11_other)

enq.final$A11_other_re <- enq.final$A11_other

enq.final$A11_other_re <-
  ifelse(enq.final$A11_other_re=="ecoutait la radio"|enq.final$A11_other_re=="ecouter la radio"|enq.final$A11_other_re=="radio","Oui, j'écoutais la radio",ifelse(enq.final$A11_other_re=="donner a manger a ma fille","Oui, je donnais à manger à ma fille",ifelse(enq.final$A11_other_re=="écouter de la musique"|enq.final$A11_other_re=="musique","Oui, j'écoutais de la musique",ifelse(enq.final$A11_other_re=="regarder la télévision", "Oui, je regardais la télévision", enq.final$A11_other_re))))

table(enq.final$A11_other_re)

#Création d'une nouvelle variable
enq.final$A11_6_other_re <- ifelse(is.na(enq.final$A11_other_re)==FALSE,"Oui","Non sélectionné")
table(enq.final$A11_6_other_re)

#FAIT : créé une dummy A11_6_other pour les réponses autres ; nettoyé les réponses autres dans A11_6_other #
#A faire : 
# *probleme* modalité non pour a11_other_6_re 
#-Rebasculer le " Oui, je regardais la télévision" en oui je regardais un écran
-#Regarder ce qu'il faut rebasculer en créant une dummmy (tout ce qui a trait au son et à la musique notamment)#
  ####
  
  
  
####
colnames(enq.final)[29]
attributes(enq.final)$variable.labels[29]
typeof(enq.final$A12)
class(enq.final$A12)
table(enq.final$A12)
sum(is.na(enq.final$A12))

enq.final$A12_re <- enq.final$A12

#FAIT : rien#
#A faire : rien ?#
####





####
#A13_1 " "[Ami(s)] Avec qui étiez-vous ? "

table(enq.final$A13_1)
table(enq.final$A13_2)
table(enq.final$A13_3)
table(enq.final$A13_4)
table(enq.final$A13_5)

enq.final$A13_1_re <- enq.final$A13_1
enq.final$A13_2_re <- enq.final$A13_2
enq.final$A13_3_re <- enq.final$A13_3
enq.final$A13_4_re <- enq.final$A13_4
enq.final$A13_5_re <- enq.final$A13_5

colnames(enq.final)[35]
attributes(enq.final)$variable.labels[35]
typeof(enq.final$A13_other)
class(enq.final$A13_other)
table(enq.final$A13_other)
sum(is.na(enq.final$A12))


enq.final$A13_5_other_re <- as.character(enq.final$A13_other)

enq.final$A13_5_other_re <- ifelse(enq.final$A13_5_other_re=="2 Petits enfants"|enq.final$A13_5_other_re=="mon petit fils de 3 ans","Petit enfant",ifelse(enq.final$A13_5_other_re=="aide menagere","Aide Ménagère",ifelse(enq.final$A13_5_other_re=="camarades de fac","Camarades de fac",enq.final$A13_5_other_re)))
table(enq.final$A13_5_other_re)

enq.final$A13_6_re <- ifelse(is.na(enq.final$A13_5_other_re)==FALSE, "Oui","Non sélectionné")

table(enq.final$A13_6_re)

#FAIT : créé une dummy A13_6 pour les réponses autres ; nettoyé les réponses autres dans A13_6_other #
# A FAIRE#
#Recodages
#*probleme* ça nous a encore fait un 4 wtf* #PROBLEME RESOlU : fallait rebasculer en caractère#
#"petit fils". Très étrange d'avoir mis "famille"" et enfant. Où mettre petit-fils (à la fois "famille" et enfants (mais pas les siens !)) ? Si ce qui nous intéresse, c'est l'âge, alors recoder en enfant ; si c'est la nature du lien, alors recoder en famille.
#présences forcées non prof ("collègues") : colocataire/camarades de fac (non-amis)
#
####




####
"#A14 Cette ou ces personne(s) avec vous mangeaient-elles ?"#
colnames(enq.final)[36]
attributes(enq.final)$variable.labels[36]
typeof(enq.final$A14)
class(enq.final$A14)
table(enq.final$A14)
sum(is.na(enq.final$A14))

enq.final$A14_re <- enq.final$A14

#FAIT : rien#
#A faire : ?#
####



####
"#A15 Les personne(s) qui étaient avec vous regardaient-elles un écran ?"#
colnames(enq.final)[37]
attributes(enq.final)$variable.labels[37]
typeof(enq.final$A15)
class(enq.final$A15)
table(enq.final$A15)
sum(is.na(enq.final$A15))

enq.final$A15_re <- enq.final$A15

#FAIT : rien#
#A faire : ?#
####


####
#A16 Parmi ces personnes qui regardaient un écran, certaines regardaient-elles le même écran que vous ? (Tv, Ordinateur, Tablette, Téléphone/Smartphone, Console)"#
colnames(enq.final)[38]
attributes(enq.final)$variable.labels[38]
typeof(enq.final$A16)
class(enq.final$A16)
table(enq.final$A16)
sum(is.na(enq.final$A16))

enq.final$A16_re <- enq.final$A16
#FAIT : rien#
#A faire : ?#
####



####
#A17_SQ001_SQ001 Parmi ces personnes qui regardaient un écran, certaines regardaient-elles le même écran que vous ? (Tv, Ordinateur, Tablette, Téléphone/Smartphone, Console)"#
colnames(enq.final)[39]
attributes(enq.final)$variable.labels[40]
typeof(enq.final$A17_SQ001_SQ002)
class(enq.final$A17_SQ001_SQ002)
table(enq.final$A16)
sum(is.na(enq.final$A16))

table(enq.final$A17_SQ001_SQ001)
table(enq.final$A17_SQ001_SQ002)
enq.final$A17_re <- paste(enq.final$A17_SQ001_SQ001, enq.final$A17_SQ001_SQ002, sep=',')
enq.final$A17_re <- gsub("h","",enq.final$A17_re, ignore.case =TRUE)
enq.final$A17_re <- gsub(",,",",",enq.final$A17_re, ignore.case =TRUE)
enq.final$A17_re <- ifelse(enq.final$A17_re=="0,0","00,00",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="0,30","00,30",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="2,0","02,00",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="3,0","03,00",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="6,0","06,00",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="8,10","08,10",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="8,30","08,30",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="9,15","09,15",enq.final$A17_re)
enq.final$A17_re <- ifelse(enq.final$A17_re=="9,30","09,30",enq.final$A17_re)
table(enq.final$A17_re)

#A FAIRE : RECODER 0,0 en 12h30 etc.# #FAIT#
####



####
####
#A18 "Combien de temps a-t-elle duré ? "#
colnames(enq.final)[43]
attributes(enq.final)$variable.labels[43]
typeof(enq.final$A18)
class(enq.final$A18)
table(enq.final$A18)
sum(is.na(enq.final$A18))

enq.final$A18_re <- enq.final$A18

#FAIT : RIEN#
#A FAIRE : ??#

#FAIT : rien#
#A faire : ?#
####



####
#A19 "Qu'est-ce que vous avez mangé ? "#
colnames(enq.final)[44]
attributes(enq.final)$variable.labels[44]
typeof(enq.final$A19)
class(enq.final$A19)
table(enq.final$A19)
sum(is.na(enq.final$A19))
table(enq.final$A19)
enq.final$A19_re <- enq.final$A19
#A RECOLLER AVEC CEUX QUI S'EN OCCUPENT#
####





####
#A20 "Pouvez-vous me dire où vous étiez à ce moment-là ?"#
colnames(enq.final)[45]
attributes(enq.final)$variable.labels[45]
typeof(enq.final$A20)
class(enq.final$A20)
table(enq.final$A20)
sum(is.na(enq.final$A20))
table(enq.final$A20)
enq.final$A20_re <- as.character(enq.final$A20)
####



####
#A21 "Pouvez-vous me dire où vous étiez à ce moment-là ?"#
colnames(enq.final)[46]
attributes(enq.final)$variable.labels[46]
typeof(enq.final$A20_other)
class(enq.final$A20_other)
table(enq.final$A20_other)
sum(is.na(enq.final$A20_other))
table(enq.final$A20_other)
enq.final$A20_other_re <- enq.final$A20_other

enq.final$A20_other_re <-ifelse(enq.final$A20_other_re=="opéra","Opéra",ifelse(enq.final$A20_other_re=="terrasse","Terrasse",ifelse(enq.final$A20_other_re=="théâtre","Théâtre",enq.final$A13_5_other_re)))

table(enq.final$A20_other_re)

enq.final$A20_re <- ifelse(is.na(enq.final$A20_other_re)==FALSE,"Autre",enq.final$A20_re)
table(enq.final$A20_re)
enq.final$A20_re <- as.factor(enq.final$A20_re)
table(enq.final$A20_re)
#HMMM 
#saucisson sec	NA	terrasse
#cake	NA	théâtre
#sandwich	NA	opéra
# Ici, on est dans du repas sur le pouce/grignotage à l'extérieur (non assis sans être "en marchant"), et nous n'avions pas de catégorie#
####





####
#A20 "Pouvez-vous me dire où vous étiez à ce moment-là ?"#

table(enq.final$A21_1)
table(enq.final$A21_2)
table(enq.final$A21_3)
table(enq.final$A21_4)
table(enq.final$A21_5)

enq.final$A21_1_re <- enq.final$A21_1
enq.final$A21_2_re <- enq.final$A21_2
enq.final$A21_3_re <- enq.final$A21_3
enq.final$A21_4_re <- enq.final$A21_4
enq.final$A21_5_re <- enq.final$A21_5

colnames(enq.final)[52]
attributes(enq.final)$variable.labels[52]
table(enq.final$A21_other)
enq.final$A21_other_re <- enq.final$A21_other


enq.final$A21_other_re <- ifelse(enq.final$A21_other_re=="Cuisine"|enq.final$A21_other_re=="Je préparai le gaspacho du soir"|enq.final$A21_other_re=="je prepare la suite du repas petit pois diende"|enq.final$A21_other_re=="sauter les crepes","Oui, je faisais la cuisine", ifelse(enq.final$A21_other_re=="nettoyer"|enq.final$A21_other_re=="ménage"|enq.final$A21_other_re=="rangement, devoirs, menage", "Oui, je faisais du nettoyage et du ménage", ifelse(enq.final$A21_other_re=="musique","Oui, j'écoutais de la musique",enq.final$A21_other_re)))

table(enq.final$A21_other_re)

enq.final$A21_6_re <- ifelse(is.na(enq.final$A21_other_re)==FALSE,"Oui","Non sélectionné")

#FAIT : CLEANAGE VAR OTHER (AVEC REGROUPEMENT) / + CREATION NVELLE DUMMY#

#A FAIRE : DISCUTER RECODAGE#
#Même prb que précédemment pour musique + Questions des travaux ménagers (nettoyage, ménage, cuisine)#
#2 cents...anti-passivité....Ces gens remangent parce qu'ils font qqchose (activité) et qu'ils sont en contact avec de la nourriture...approche par le corps (activité + proximité avec bouffe comme prédicteur)...mais minoritaire (versus télé)#
####





####
#A22_SQ001_SQ001 [Heure] \tNous allons maintenant parler de votre dernière prise alimentaire de la soirée. S'il s'agit de votre prise principale, parlez-nous de l'avant dernière.\tÀ quelle heure a-t-elle eu lieu ?"#
colnames(enq.final)[53]
attributes(enq.final)$variable.labels[53]
colnames(enq.final)[54]

enq.final$A22_re <- paste(enq.final$A22_SQ001_SQ001, enq.final$A22_SQ001_SQ002, sep=',')
enq.final$A22_re <- gsub("h","",enq.final$A22_re, ignore.case =TRUE)
enq.final$A22_re <- gsub(",,",",",enq.final$A22_re, ignore.case =TRUE)

table(enq.final$A22_re)
enq.final$A22_re <-ifelse(enq.final$A22_re=="0,0","00,00",ifelse(enq.final$A22_re=="00,0","00,00",enq.final$A22_re))
enq.final$A22_re <-ifelse(enq.final$A22_re=="1,0","01,00",enq.final$A22_re)
enq.final$A22_re <-ifelse(enq.final$A22_re=="11,0","11,00",enq.final$A22_re)
enq.final$A22_re <-ifelse(enq.final$A22_re=="2,0","02,00",enq.final$A22_re)
enq.final$A22_re <-ifelse(enq.final$A22_re=="7,30","07,30",enq.final$A22_re)
table(enq.final$A22_re)
#FAIT : CREATION D'UNE VAR REGROUPANT H/MIN#
#A FAIRE : REGROUPER LES HEURES 7h30/19h30...Interroger les valeurs aberrantes#
####





####
#A24  "Qu'est-ce que vous avez mangé ?"#
colnames(enq.final)[55]
attributes(enq.final)$variable.labels[55]
typeof(enq.final$A24)
class(enq.final$A24)
table(enq.final$A24)
sum(is.na(enq.final$A24))
table(enq.final$A24)
enq.final$A24_re <- enq.final$A24

#A VOIR AVEC CEUX QUI S'EN OCCUPENT
#RAS#
####









####
#A25 "Pouvez-vous me dire où vous étiez à ce moment-là ? "
colnames(enq.final)[57]
attributes(enq.final)$variable.labels[57]
typeof(enq.final$A25)
class(enq.final$A25)
table(enq.final$A25)
sum(is.na(enq.final$A25))
table(enq.final$A25)
enq.final$A25_re <- enq.final$A25

#RAS#
####








####
#A25 "Pouvez-vous me dire où vous étiez à ce moment-là ? "
colnames(enq.final)[57]
attributes(enq.final)$variable.labels[57]
typeof(enq.final$A25)
class(enq.final$A25)
table(enq.final$A25)
sum(is.na(enq.final$A25))
table(enq.final$A25)
enq.final$A25_re <- enq.final$A25
#RAS#
####


####
#A25_other "Pouvez-vous me dire où vous étiez à ce moment-là ? "
colnames(enq.final)[58]
attributes(enq.final)$variable.labels[58]
typeof(enq.final$A25_other)
class(enq.final$A25_other)
table(enq.final$A25_other)
sum(is.na(enq.final$A25_other))
table(enq.final$A25_other)
enq.final$A25_other_re <- enq.final$A25_other
#RAS - VAR NULLE 




####
#A25_other "Pouvez-vous me dire où vous étiez à ce moment-là ? "

table(enq.final$A26_1)
table(enq.final$A26_2)
table(enq.final$A26_3)
table(enq.final$A26_4)
table(enq.final$A26_5)
table(enq.final$A26_6)

enq.final$A26_1_re <- enq.final$A26_1
enq.final$A26_2_re <- enq.final$A26_2
enq.final$A26_3_re <- enq.final$A26_3
enq.final$A26_4_re <- enq.final$A26_4
enq.final$A26_5_re <- enq.final$A26_5
enq.final$A26_6_re <- enq.final$A26_6


table(enq.final$A26_other)
enq.final$A26_other_re <- enq.final$A26_other
enq.final$A26_other_re <- ifelse(enq.final$A26_other_re=="Cuisine","Oui, je faisais la cuisine", ifelse(enq.final$A26_other_re=="écouter un podcast radio","Oui, j'écoutais un podcast radio",ifelse(enq.final$A26_other_re=="musique","Oui, j'écoutais de la musique",enq.final$A26_other_re)))
table(enq.final$A26_other_re)

enq.final$A26_7_re <-ifelse(is.na(enq.final$A26_other_re)==FALSE,"Oui","Non sélectionné")
table(enq.final$A26_7_re)

#FAiT : cREATION d'une dummy supp#
#A faire : checker les recodages#
####







####TIMOTHEE 11/09 - REMETTRE LES MODALITES DANS L'ORDRE####

str(enq.final[,148:202])

#A1_RE#
colnames(enq.final)[148]
class(enq.final$A1_re)
levels(enq.final$A1_re)
#GOOD#

#A2_RE#
colnames(enq.final)[149]
class(enq.final$A2_re)
enq.final$A2_re <- as.factor(enq.final$A2_re)
levels(enq.final$A2_re)
#GOOD#

#A2_RE#
colnames(enq.final)[150]
class(enq.final$A2_bis_re)
enq.final$A2_bis_re <- as.factor(enq.final$A2_bis_re)
levels(enq.final$A2_bis_re)
#GOOD#

#A3_re#
colnames(enq.final)[151]
class(enq.final$A3_re)
enq.final$A3_re <- as.factor(enq.final$A3_re)
enq.final$A3_re <- relevel(enq.final$A3_re, "Moins de 10 min")
levels(enq.final$A3_re)
#GOOD#

#A4_re#
colnames(enq.final)[152]
class(enq.final$A4_re)
table(enq.final$A4_re)
levels(enq.final$A4_re)
enq.final$A4_re <- factor(enq.final$A4_re,levels=c("Je l’ai préparé","Une autre personne du foyer l’a préparé","Plat à emporter ou livraison à domicile","Restaurant ou autre repas extérieur au domicile"))
table(enq.final$A4_re)
#GOOD#

#A5_re#
colnames(enq.final)[153]
class(enq.final$A5_re)
table(enq.final$A5_re)
levels(enq.final$A5_re)
enq.final$A5_re <- relevel(enq.final$A5_re, "Moins de 10 minutes")

#A6_re#
colnames(enq.final)[154]
class(enq.final$A6_re)
table(enq.final$A6_re)
levels(enq.final$A6_re)
#Pasmonaffaire#

#A7_re#
colnames(enq.final)[155]
class(enq.final$A7_re)
table(enq.final$A7_re)
levels(enq.final$A7_re)
enq.final$A7_re <- relevel(enq.final$A7_re,"Oui")
table(enq.final$A7_re)
#GOOD#

#A8_re#
colnames(enq.final)[156]
class(enq.final$A8_re)
table(enq.final$A8_re)
levels(enq.final$A8_re)
enq.final$A8_re <- factor(enq.final$A8_re,levels=c("Chez vous","Au travail", "Dans les transports ou en marchant","Dans un restaurant/fast-food/brasserie etc.","Chez des amis","Chez de la famille","Autre, précisez (voir A8_other_re)"))
table(enq.final$A8_re)

#A8_re#
colnames(enq.final)[157]
class(enq.final$A8_other_re)
enq.final$A8_other_re <- as.factor(enq.final$A8_other_re)
table(enq.final$A8_other_re)
levels(enq.final$A8_other_re)

#A9_re#
colnames(enq.final)[158]
class(enq.final$A9_re)
table(enq.final$A9_re)
enq.final$A9_re <- relevel(enq.final$A9_re,"Oui")
table(enq.final$A9_re)
#GOOD#

#A10_re#
colnames(enq.final)[159]
class(enq.final$A10_re)
table(enq.final$A10_re)
enq.final$A10_re <- factor(enq.final$A10_re,levels=c("Cuisine","Salon / Salle de séjour", "Chambre","Mon logement a une seule pièce","Autre (réponse ouverte)"))
table(enq.final$A10_re)
#GOOD#

#A10_other_re#
colnames(enq.final)[160]
class(enq.final$A10_other_re)
enq.final$A10_other_re <- as.factor(enq.final$A10_other_re)
table(enq.final$A10_other_re)
#GOOD#

#A10_other_re#
colnames(enq.final)[161]
class(enq.final$A11_1_re)
table(enq.final$A11_1_re)
enq.final$A11_1_re <- relevel(enq.final$A11_1_re,"Oui")
table(enq.final$A11_1_re)
#GOOD#

#A11_2_re#
colnames(enq.final)[162]
class(enq.final$A11_2_re)
table(enq.final$A11_2_re)
enq.final$A11_2_re <- relevel(enq.final$A11_2_re,"Oui")
table(enq.final$A11_2_re)
#GOOD#

#A11_3_re#
colnames(enq.final)[163]
class(enq.final$A11_3_re)
table(enq.final$A11_3_re)
enq.final$A11_3_re <- relevel(enq.final$A11_3_re,"Oui")
table(enq.final$A11_3_re)
#GOOD#

#A11_4_re#
colnames(enq.final)[164]
class(enq.final$A11_4_re)
table(enq.final$A11_4_re)
enq.final$A11_4_re <- relevel(enq.final$A11_4_re,"Oui")
table(enq.final$A11_4_re)
#GOOD#

#A11_5_re#
colnames(enq.final)[165]
class(enq.final$A11_5_re)
table(enq.final$A11_5_re)
table(enq.final$A11_5_re)
#GOOD BUT EMPTY - A VERIF#

#A11_other_re#
colnames(enq.final)[166]
class(enq.final$A11_other_re)
enq.final$A11_other_re <- as.factor(enq.final$A11_other_re)
table(enq.final$A11_other_re)
#PB : QUEL ORDRE ?? MAJ DICO DES VARIABLES#

#A11_6_other_re#
colnames(enq.final)[167]
class(enq.final$A11_6_other_re)
enq.final$A11_6_other_re <- as.factor(enq.final$A11_6_other_re)
table(enq.final$A11_6_other_re)
enq.final$A11_6_other_re <- relevel(enq.final$A11_6_other_re,"Oui")
table(enq.final$A11_6_other_re)

#A12_re#
colnames(enq.final)[168]
class(enq.final$A12_re)
table(enq.final$A12_re)
enq.final$A12_re <- relevel(enq.final$A12_re,"Oui")
table(enq.final$A12_re)
#GOOD#

#A13_1_re#
colnames(enq.final)[169]
class(enq.final$A13_1_re)
table(enq.final$A13_1_re)
enq.final$A13_1_re <- relevel(enq.final$A13_1_re,"Oui")
table(enq.final$A13_1_re)
#GOOD#

#A13_2_re#
colnames(enq.final)[170]
class(enq.final$A13_2_re)
table(enq.final$A13_2_re)
table(enq.final$A13_2)
#PB EMPTY- A VERIFIER#

#A13_3_re#
colnames(enq.final)[171]
class(enq.final$A13_3_re)
table(enq.final$A13_3_re)
enq.final$A13_3_re <- relevel(enq.final$A13_3_re,"Oui")
table(enq.final$A13_3_re)
#GOOD#

#A13_4_re#
colnames(enq.final)[172]
class(enq.final$A13_4_re)
table(enq.final$A13_4_re)
enq.final$A13_4_re <- relevel(enq.final$A13_4_re,"Oui")
table(enq.final$A13_4_re)
#GOOD#

#A13_5_re#
colnames(enq.final)[173]
class(enq.final$A13_5_re)
table(enq.final$A13_5_re)
enq.final$A13_5_re <- relevel(enq.final$A13_5_re,"Oui")
table(enq.final$A13_5_re)
#GOOD#

#A13_5_other_re#
colnames(enq.final)[174]
class(enq.final$A13_5_other_re)
enq.final$A13_5_other_re <- as.factor(enq.final$A13_5_other_re)
table(enq.final$A13_5_other_re)
#GOOD#

#A13_6_re#
colnames(enq.final)[175]
class(enq.final$A13_6_re)
table(enq.final$A13_6_re)
enq.final$A13_6_re <- as.factor(enq.final$A13_6_re)
enq.final$A13_6_re <- relevel(enq.final$A13_6_re,"Oui")
table(enq.final$A13_6_re)
#GOOD#

#A14_re#
colnames(enq.final)[176]
class(enq.final$A14_re)
table(enq.final$A14_re)
enq.final$A14_re <- relevel(enq.final$A14_re,"Oui certaines")
table(enq.final$A14_re)
enq.final$A14_re <- relevel(enq.final$A14_re,"Oui toutes")
table(enq.final$A14_re)
levels(enq.final$A14_re) <- c("Oui, toutes","Oui, certaines","Non, aucune")
table(enq.final$A14_re)
#GOOD (j'ai aussi mis des virgules qui manquaient)#

#A15_re#
colnames(enq.final)[177]
class(enq.final$A15_re)
table(enq.final$A15_re)
enq.final$A15_re <- relevel(enq.final$A15_re,"Oui, certaines")
table(enq.final$A15_re)
enq.final$A15_re <- relevel(enq.final$A15_re,"Oui, toutes")
table(enq.final$A15_re)
#GOOD#

#A16_re#
colnames(enq.final)[178]
class(enq.final$A16_re)
table(enq.final$A16_re)
enq.final$A16_re <- relevel(enq.final$A16_re,"Oui")
table(enq.final$A16_re)
#GOOD#

#A17_re#
colnames(enq.final)[179]
class(enq.final$A17_re)
table(enq.final$A17_re)
enq.final$A17_re <- as.factor(enq.final$A17_re)
table(enq.final$A17_re)
#GOOD#

#A18_re#
colnames(enq.final)[180]
class(enq.final$A18_re)
table(enq.final$A18_re)
enq.final$A18_re <- as.factor(enq.final$A18_re)
table(enq.final$A18_re)
#GOOD#

#A19_re#
colnames(enq.final)[181]
class(enq.final$A19_re)
table(enq.final$A19_re)
#M'EN OCCUPE PAS#

#A20_re#
colnames(enq.final)[182]
class(enq.final$A20_re)
table(enq.final$A20_re)
levels(enq.final$A20_re)
enq.final$A20_re <- factor(enq.final$A20_re,levels=c("Chez vous","Au travail","Dans les transports, en marchant etc.","Dans un restaurant/fast-food/bar/brasserie etc.","Chez des amis","Chez de la famille","Autre"))
table(enq.final$A20_re )
#GOOD#

#A20_other_re#
colnames(enq.final)[183]
class(enq.final$A20_other_re)
table(enq.final$A20_other_re)
enq.final$A20_other_re <- as.factor(enq.final$A20_other_re)
table(enq.final$A20_other_re)
#GOOD#

#A21_1_re#
colnames(enq.final)[184]
class(enq.final$A21_1_re)
table(enq.final$A21_1_re)
enq.final$A21_1_re <- relevel(enq.final$A21_1_re,"Oui")
table(enq.final$A21_1_re)
#A21_1_re#

#A21_2_re#
colnames(enq.final)[185]
class(enq.final$A21_2_re)
table(enq.final$A21_2_re)
enq.final$A21_2_re <- relevel(enq.final$A21_2_re,"Oui")
table(enq.final$A21_2_re)
#GOOD#

#A21_3_re#
colnames(enq.final)[186]
class(enq.final$A21_3_re)
table(enq.final$A21_3_re)
enq.final$A21_3_re <- relevel(enq.final$A21_3_re,"Oui")
table(enq.final$A21_3_re)
#GOOD#

#A21_4_re#
colnames(enq.final)[187]
class(enq.final$A21_4_re)
table(enq.final$A21_4_re)
enq.final$A21_4_re <- relevel(enq.final$A21_4_re,"Oui")
table(enq.final$A21_4_re)
#GOOD#

#A21_5_re#
colnames(enq.final)[188]
class(enq.final$A21_5_re)
table(enq.final$A21_5_re)
enq.final$A21_5_re <- relevel(enq.final$A21_5_re,"Oui")
table(enq.final$A21_5_re)
#GOOD#

#A21_other_re#
colnames(enq.final)[189]
class(enq.final$A21_other_re)
enq.final$A21_other_re <- as.factor(enq.final$A21_other_re)
table(enq.final$A21_other_re)
#GOOD#

#A21_6_re#
colnames(enq.final)[190]
class(enq.final$A21_6_re)
enq.final$A21_6_re <- as.factor(enq.final$A21_6_re)
table(enq.final$A21_6_re)
enq.final$A21_6_re <- relevel(enq.final$A21_6_re,"Oui")
table(enq.final$A21_6_re)
#GOOD#

#A22_re#
colnames(enq.final)[191]
class(enq.final$A22_re)
enq.final$A22_re <- as.factor(enq.final$A22_re)
table(enq.final$A22_re)
#GOOD#

#A24_re#
colnames(enq.final)[192]
class(enq.final$A24_re)
table(enq.final$A24_re)
#GOOD#

#A25_re#
colnames(enq.final)[193]
class(enq.final$A25_re)
table(enq.final$A25_re)
enq.final$A25_re <- relevel(enq.final$A25_re,"Chez vous")
table(enq.final$A25_re)
enq.final$A25_re <- relevel(enq.final$A25_re,"Chez vous")
#PB : manquent modalités vides p/r au dico des variables#

#A25_other_re#
colnames(enq.final)[194]
class(enq.final$A25_other_re)
table(enq.final$A25_other_re)
#VAR VIDE#

#A26_1_re#
colnames(enq.final)[195]
class(enq.final$A26_1_re)
table(enq.final$A26_1_re)
enq.final$A26_1_re <- relevel(enq.final$A26_1_re,"Oui")
table(enq.final$A26_1_re)

#A26_2_re#
colnames(enq.final)[196]
class(enq.final$A26_2_re)
table(enq.final$A26_2_re)
enq.final$A26_2_re <- relevel(enq.final$A26_2_re,"Oui")
table(enq.final$A26_2_re)

#A26_3_re#
colnames(enq.final)[197]
class(enq.final$A26_3_re)
table(enq.final$A26_3_re)
enq.final$A26_3_re <- relevel(enq.final$A26_3_re,"Oui")
table(enq.final$A26_3_re)


##A26_5_re#
colnames(enq.final)[198]
class(enq.final$A26_5_re)
table(enq.final$A26_5_re)
enq.final$A26_5_re <- relevel(enq.final$A26_5_re,"Oui")
table(enq.final$A26_5_re)
##PB : QUID A26_4 ?? Vu que même vide, a créé A26_6##

#A26_6_re#
colnames(enq.final)[199]
class(enq.final$A26_6_re)
table(enq.final$A26_6_re)
#GOOD#

#A26_other_re#
colnames(enq.final)[200]
class(enq.final$A26_other_re)
enq.final$A26_other_re <- as.factor(enq.final$A26_other_re)
table(enq.final$A26_other_re)
#GOOD#

#A26_7_re#
colnames(enq.final)[201]
class(enq.final$A26_7_re)
enq.final$A26_7_re <- as.factor(enq.final$A26_7_re)
table(enq.final$A26_7_re)
enq.final$A26_7_re <- relevel(enq.final$A26_7_re,"Oui")
table(enq.final$A26_7_re)
#GOOD#


##############################
###### Partie 2 : Ecran ######
##############################



#R1_1#
colnames(enq.final)[66]
attributes(enq.final)$variable.labels[66]
typeof(enq.final$R1_1)
class(enq.final$R1_1)
table(enq.final$R1_1)
sum(is.na(enq.final$R1_1))

enq.final$R1_1_re <- enq.final$R1_1

#R2_1#
colnames(enq.final)[67]
attributes(enq.final)$variable.labels[67]
typeof(enq.final$r2_1)
class(enq.final$r2_1)
table(enq.final$r2_1)
sum(is.na(enq.final$r2_1))

enq.final$r2_1_re <- enq.final$r2_1

#R6_1#
colnames(enq.final)[68]
attributes(enq.final)$variable.labels[68]
typeof(enq.final$r6_1)
class(enq.final$r6_1)
table(enq.final$r6_1)
sum(is.na(enq.final$r6_1))

enq.final$r6_1_re <- enq.final$r6_1

#R1_2#
colnames(enq.final)[69]
attributes(enq.final)$variable.labels[69]
typeof(enq.final$R1_2)
class(enq.final$R1_2)
table(enq.final$R1_2)
sum(is.na(enq.final$R1_2))

enq.final$R1_2_re <- enq.final$R1_2

#R2_2#
colnames(enq.final)[70]
attributes(enq.final)$variable.labels[70]
typeof(enq.final$r2_2)
class(enq.final$r2_2)
table(enq.final$r2_2)
sum(is.na(enq.final$r2_2))

enq.final$r2_2_re <- enq.final$r2_2

#R6_2#
colnames(enq.final)[71]
attributes(enq.final)$variable.labels[71]
typeof(enq.final$r6_2)
class(enq.final$r6_2)
table(enq.final$r6_2)
sum(is.na(enq.final$r6_2))

enq.final$r6_2_re <- enq.final$r6_2
### il manque la 1ere modalite "aucune attention", est confondu avec les NA

#R1_3#
colnames(enq.final)[72]
attributes(enq.final)$variable.labels[72]
typeof(enq.final$R1_3)
class(enq.final$R1_3)
table(enq.final$R1_3)
sum(is.na(enq.final$R1_3))

enq.final$R1_3_re <- enq.final$R1_3

#R2_3#
colnames(enq.final)[73]
attributes(enq.final)$variable.labels[73]
typeof(enq.final$r2_3)
class(enq.final$r2_3)
table(enq.final$r2_3)
sum(is.na(enq.final$r2_3))

enq.final$r2_3_re <- as.factor(enq.final$r2_3)
### idem R2_2 

#R6_3#
colnames(enq.final)[80]
attributes(enq.final)$variable.labels[74]
typeof(enq.final$r6_3)
class(enq.final$r6_3)
table(enq.final$r6_3)
sum(is.na(enq.final$r6_3))

enq.final$r6_3_re <- enq.final$r6_3

#R3#
enq.final$r3_1_re <- as.character(enq.final$r3_1)
enq.final$r3_2_re <- as.character(enq.final$r3_2)
enq.final$r3_3_re <- as.character(enq.final$r3_3)
enq.final$r3_4_re <- as.character(enq.final$r3_4)
enq.final$r3_5_re <- as.character(enq.final$r3_5)

colnames(enq.final)[80]
attributes(enq.final)$variable.labels[80]
typeof(enq.final$r3_other)
class(enq.final$r3_other)
table(enq.final$r3_other)
sum(is.na(enq.final$r3_other))
# 1 seul obs dans cette modalite : reponse aberante "amis" Supprimer cet observation ??
#enq.final$A13_5_other_re <- as.character(enq.final$A13_other)

#R4#

colnames(enq.final)[81] ## r4_14 ??? moi pas comprendre, il n y a que 12 modalite pour cette var 
enq.final$r4_1_re <- as.character(enq.final$r4_1)
enq.final$r4_2_re <- as.character(enq.final$r4_2)
enq.final$r4_3_re <- as.character(enq.final$r4_3)
enq.final$r4_4_re <- as.character(enq.final$r4_4)
enq.final$r4_5_re <- as.character(enq.final$r4_5)
enq.final$r4_6_re <- as.character(enq.final$r4_6)
enq.final$r4_7_re <- as.character(enq.final$r4_7)
enq.final$r4_8_re <- as.character(enq.final$r4_8)
enq.final$r4_9_re <- as.character(enq.final$r4_9)
enq.final$r4_10_re <- as.character(enq.final$r4_10)
##enq.final$r4_11_re <- as.character(enq.final$r4_11)
table(enq.final$r4_11)                   ########## R_4_11 n existe pas ...
enq.final$r4_12_re <- as.character(enq.final$r4_12)
enq.final$r4_13_re <- as.character(enq.final$r4_13)
enq.final$r4_14_re <- as.character(enq.final$r4_14)
enq.final$r4_15_re <- as.character(enq.final$r4_15)


table(enq.final$r4_other) #Quelle idee de demander de preciser le jeu video ! 

#R5#
colnames(enq.final)[92]
attributes(enq.final)$variable.labels[92]
typeof(enq.final$r5)
class(enq.final$r5)
table(enq.final$r5)
sum(is.na(enq.final$r5))

enq.final$r5_re <- enq.final$r5

##########

str(enq.final[,202:227])

#R1_1_re 
colnames(enq.final)[202]
class(enq.final$R1_1_re)
table(enq.final$R1_1_re)
enq.final$R1_1_re <- relevel(enq.final$R1_1_re,"Oui")
table(enq.final$R1_1_re)

#R2_1_re 
colnames(enq.final)[203]
class(enq.final$r2_1_re)
table(enq.final$r2_1_re)
enq.final$r2_1_re <- as.factor(enq.final$r2_1_re)
table(enq.final$r2_1_re)

#R6_1_re 
colnames(enq.final)[204]
class(enq.final$r6_1_re)
table(enq.final$r6_1_re)
enq.final$r6_1_re <- relevel(enq.final$r6_1_re,"Oui")
table(enq.final$r6_1_re)

#R1_2_re 
colnames(enq.final)[205]
class(enq.final$r1_2_re)
table(enq.final$r1_2_re)
enq.final$r1_2_re <- relevel(enq.final$r1_2_re, "oui") ### ne fonctionne point 
table(enq.final$r1_2_re)

#R2_2_re ### en attente ####
colnames(enq.final)[206]
class(enq.final$r2_2_re)
table(enq.final$r2_2_re)
enq.final$r2_2_re <- as.factor(enq.final$r2_2_re)  
table(enq.final$r2_2_re)

#R6_2_re 
colnames(enq.final)[207]
class(enq.final$r6_2_re)
table(enq.final$r6_2_re)
enq.final$r6_2_re <- relevel(enq.final$r6_2_re,"Oui")
table(enq.final$r6_2_re)

#R1_3_re 
colnames(enq.final)[208]
class(enq.final$r1_3_re)
table(enq.final$r1_3_re)
enq.final$r1_3_re <- relevel(enq.final$r1_3_re,"Oui") ## ne fonctionne tj point !
table(enq.final$r1_3_re)

#R2_3_re ### idem R2_2
colnames(enq.final)[209]
class(enq.final$r2_3_re)
table(enq.final$r2_3_re)
enq.final$r2_3_re <- as.factor(enq.final$r2_3_re)
table(enq.final$r2_3_re)

#R6_3_re 
colnames(enq.final)[210]
class(enq.final$r6_3_re)
table(enq.final$r6_3_re)
enq.final$r6_3_re <- relevel(enq.final$r6_3_re,"Oui")
table(enq.final$r6_3_re)

#R3_1_re
colnames(enq.final)[211]
class(enq.final$r3_1_re)
table(enq.final$r3_1_re)
enq.final$r3_1_re <- relevel(enq.final$r3_1_re,"Oui")
table(enq.final$r3_1_re)
#R3_2_re
colnames(enq.final)[212]
class(enq.final$r3_2_re)
table(enq.final$r3_2_re)
enq.final$r3_2_re <- relevel(enq.final$r3_2_re,"Oui")
table(enq.final$r3_2_re)
######### meme pb que pour R1_2_re et R1_3_re 

#R4_re en attente de résoudre le pb des modalites

#R5_re
colnames(enq.final) #### je sais pas ou il est mais il existe 
class(enq.final$r5_re)
table(enq.final$r5_re)
enq.final$r5_re <- as.factor(enq.final$r5_re)
table(enq.final$r5_re)

# Partie Cesar -equippement #

####

#################################
###### Parti 3: Equipement ######
#################################

#E1#
colnames(enq.final)[93]
attributes(enq.final)$variable.labels[93]
typeof(enq.final$E1)
class(enq.final$E1)
table(enq.final$E1)
sum(is.na(enq.final$E1))

enq.final$E1_re <- enq.final$E1
#Vérification
table (enq.final$E1_re , enq.final$E1)
#E2#
colnames(enq.final)[94]
attributes(enq.final)$variable.labels[94]
typeof(enq.final$E2)
class(enq.final$E2)
table(enq.final$E2)
sum(is.na(enq.final$E2))

enq.final$E2_re <- enq.final$E2
#Vérification
table (enq.final$E2_re , enq.final$E2)

#E2_a#
colnames(enq.final)[95]
attributes(enq.final)$variable.labels[95]
typeof(enq.final$E2_a)
class(enq.final$E2_a)
table(enq.final$E2_a)
sum(is.na(enq.final$E2_a))
enq.final$E2_a_re <- enq.final$E2_a
#Vérification
table (enq.final$E2_a_re , enq.final$E2_a_re)


#E2_b#
colnames(enq.final)[96]
attributes(enq.final)$variable.labels[96]
typeof(enq.final$E2_b)
class(enq.final$E2_b)
table(enq.final$E2_b)
sum(is.na(enq.final$E2_b))

enq.final$E2_b_re <- enq.final$E2_b
#Vérification
table (enq.final$E2_b_re , enq.final$E2_b)


#E3#
colnames(enq.final)[97]
attributes(enq.final)$variable.labels[97]
typeof(enq.final$E3)
class(enq.final$E3)
table(enq.final$E3)
sum(is.na(enq.final$E3))

enq.final$E3_re <- enq.final$E3
#Vérification
table (enq.final$E3_re , enq.final$E3)

#E3_a#
colnames(enq.final)[98]
attributes(enq.final)$variable.labels[98]
typeof(enq.final$E3_a)
class(enq.final$E3_a)
table(enq.final$E3_a)
sum(is.na(enq.final$E3_a))

enq.final$E3_a_re <- enq.final$E3_a
#Vérification
table (enq.final$E3_a_re , enq.final$E3_a)

#E3_b#
colnames(enq.final)[99]
attributes(enq.final)$variable.labels[99]
typeof(enq.final$E3_b)
class(enq.final$E3_b)
table(enq.final$E3_b)
sum(is.na(enq.final$E3_b))

enq.final$E3_b_re <- enq.final$E3_b
#Vérification
table (enq.final$E3_b_re , enq.final$E3_b)

#E4#
colnames(enq.final)[100]
attributes(enq.final)$variable.labels[100]
typeof(enq.final$E4)
class(enq.final$E4)
table(enq.final$E4)
sum(is.na(enq.final$E4))

enq.final$E4_re <- enq.final$E4
#vérification
table (enq.final$E4_re  , enq.final$E4)


#E4_a#
colnames(enq.final)[101]
attributes(enq.final)$variable.labels[101]
typeof(enq.final$E4_a)
class(enq.final$E4_a)
table(enq.final$E4_a)
sum(is.na(enq.final$E4_a))

enq.final$E4_a_re <- enq.final$E4_a
#vérification
table (enq.final$E4_a_re, enq.final$E4_a)


#E4_b#
colnames(enq.final)[102]
attributes(enq.final)$variable.labels[102]
typeof(enq.final$E4_b)
class(enq.final$E4_b)
table(enq.final$E4_b)
sum(is.na(enq.final$E4_b))

enq.final$E4_b_re <- enq.final$E4_b
#vérification
table (enq.final$E4_b_re, enq.final$E4_b)


#E5#
colnames(enq.final)[103]
attributes(enq.final)$variable.labels[103]
typeof(enq.final$E5)
class(enq.final$E5)
table(enq.final$E5)
sum(is.na(enq.final$E5))

enq.final$E5_re <- enq.final$E5
#vérification
table (enq.final$E5_re, enq.final$E5)


#E5_a#
colnames(enq.final)[104]
attributes(enq.final)$variable.labels[104]
typeof(enq.final$E5_a)
class(enq.final$E5_a)
table(enq.final$E5_a)
sum(is.na(enq.final$E5_a))

enq.final$E5_a_re <- enq.final$E5_a
#vérification
table (enq.final$E5_a_re, enq.final$E5_a)


#E5_b#
colnames(enq.final)[105]
attributes(enq.final)$variable.labels[105]
typeof(enq.final$E5_b)
class(enq.final$E5_b)
table(enq.final$E5_b)
sum(is.na(enq.final$E5_b))

enq.final$E5_b_re <- enq.final$E5_b
#vérification
table (enq.final$E5_b_re, enq.final$E5_b)


#E6#
colnames(enq.final)[106]
attributes(enq.final)$variable.labels[106]
typeof(enq.final$E6)
class(enq.final$E6)
table(enq.final$E6)
sum(is.na(enq.final$E6))

enq.final$E6_re <- enq.final$E6
#vérification
table (enq.final$E6_re, enq.final$E6)


#E6_a#
colnames(enq.final)[107]
attributes(enq.final)$variable.labels[107]
typeof(enq.final$E6_a)
class(enq.final$E6_a)
table(enq.final$E6_a)
sum(is.na(enq.final$E6_a))

enq.final$E6_a_re <- enq.final$E6_a
#vérification
table (enq.final$E6_a_re, enq.final$E6_a)


#E6_b#
colnames(enq.final)[108]
attributes(enq.final)$variable.labels[108]
typeof(enq.final$E6_b)
class(enq.final$E6_b)
table(enq.final$E6_b)
sum(is.na(enq.final$E6_b))

enq.final$E6_b_re <- enq.final$E6_b
#vérification
table (enq.final$E6_b_re, enq.final$E6_b)


#E7#
colnames(enq.final)[109]
attributes(enq.final)$variable.labels[109]
typeof(enq.final$E7)
class(enq.final$E7)
table(enq.final$E7)
sum(is.na(enq.final$E7))

enq.final$E7_re <- enq.final$E7
#vérification
table (enq.final$E7_re, enq.final$E7)


#E7_a#
colnames(enq.final)[110]
attributes(enq.final)$variable.labels[110]
typeof(enq.final$E7_a)
class(enq.final$E7_a)
table(enq.final$E7_a)
sum(is.na(enq.final$E7_a))

enq.final$E7_a_re <- enq.final$E7_a
#vérification
table (enq.final$E7_a_re, enq.final$E7_a)


#E7_b#
colnames(enq.final)[111]
attributes(enq.final)$variable.labels[111]
typeof(enq.final$E7_b)
class(enq.final$E7_b)
table(enq.final$E7_b)
sum(is.na(enq.final$E7_b))

enq.final$E7_b_re <- enq.final$E7_b
#vérification
table (enq.final$E7_b_re, enq.final$E7_b)


#E8#
colnames(enq.final)[112]
attributes(enq.final)$variable.labels[112]
typeof(enq.final$E8)
class(enq.final$E8)
table(enq.final$E8)
sum(is.na(enq.final$E8))

enq.final$E8_re <- enq.final$E8
#vérification
table (enq.final$E8_re, enq.final$E8)


#E8_a#
colnames(enq.final)[113]
attributes(enq.final)$variable.labels[113]
typeof(enq.final$E8_a)
class(enq.final$E8_a)
table(enq.final$E8_a)
sum(is.na(enq.final$E8_a))

enq.final$E8_a_re <- enq.final$E8_a
#vérification
table (enq.final$E8_a_re, enq.final$E8_a)


#E8_b#
colnames(enq.final)[114]
attributes(enq.final)$variable.labels[114]
typeof(enq.final$E8_b)
class(enq.final$E8_b)
table(enq.final$E8_b)
sum(is.na(enq.final$E8_b))

enq.final$E8_b_re <- enq.final$E8_b
#vérification
table (enq.final$E8_b_re, enq.final$E8_b)


###################################
##Ordonner selon le questionnaire##
###################################

str(enq.final[,228:249])

#E1_re#
colnames(enq.final)[228]
class(enq.final$E1_re)
table(enq.final$E1_re)
enq.final$E1_re <- factor(enq.final$E1_re,levels = c("Vous n'avez pas internet à la maison", "Oui", "Non"))
table(enq.final$E1_re)

#E2_re#
colnames(enq.final)[229]
class(enq.final$E2_re)
table(enq.final$E2_re) ##Déjà ordonnée selon le questionaire

#E2_a_re#
colnames(enq.final)[230]
class(enq.final$E2_a_re)
table(enq.final$E2_a_re) ##Déjà ordonnée selon le questionaire

#E2_b_re#
colnames(enq.final)[231]
class(enq.final$E2_b_re)
table(enq.final$E2_b_re)
enq.final$E2_b_re <- factor(enq.final$E2_b_re, levels = c("Très souvent", "Souvent", "Occasionnellement", "Jamais"))
table(enq.final$E2_b_re)

#E3_re#
colnames(enq.final)[232]
class(enq.final$E3_re)
table(enq.final$E3_re) ##Déjà ordonnée selon le questionaire

#E3_a_re#
colnames(enq.final)[233]
class(enq.final$E3_a_re)
table(enq.final$E3_a_re) ##Déjà ordonnée selon le questionaire

#E3_b_re#
colnames(enq.final)[234]
class(enq.final$E3_b_re)
table(enq.final$E3_b_re)
enq.final$E3_b_re <- factor(enq.final$E3_b_re, levels = c("Très souvent", "Souvent", "Occasionnellement", "Jamais"))
table(enq.final$E3_b_re)

#E4_re#
colnames(enq.final)[235]
class(enq.final$E4_re)
table(enq.final$E4_re) ##Déjà ordonnée selon le questionaire

#E4_a_re#
colnames(enq.final)[236]
class(enq.final$E4_a_re)
table(enq.final$E4_a_re) ##Déjà ordonnée selon le questionaire

#E4_b_re#
colnames(enq.final)[237]
class(enq.final$E4_b_re)
table(enq.final$E4_b_re)
enq.final$E4_b_re <- factor(enq.final$E4_b_re, levels = c("Très souvent", "Souvent", "Occasionnellement", "Jamais"))
table(enq.final$E4_b_re)

#E5_re#
colnames(enq.final)[238]
class(enq.final$E5_re)
table(enq.final$E5_re) ##Déjà ordonnée selon le questionaire

#E5_a_re#
colnames(enq.final)[239]
class(enq.final$E5_a_re)
table(enq.final$E5_a_re) ##Déjà ordonnée selon le questionaire

#E5_b_re#
colnames(enq.final)[240]
class(enq.final$E5_b_re)
table(enq.final$E5_b_re)
enq.final$E5_b_re <- factor(enq.final$E5_b_re, levels = c("Très souvent", "Souvent", "Occasionnellement", "Jamais"))
table(enq.final$E5_b_re)

#E6_re#
colnames(enq.final)[241]
class(enq.final$E6_re)
table(enq.final$E6_re) ##Déjà ordonnée selon le questionaire

#E6_a_re#
colnames(enq.final)[242]
class(enq.final$E6_a_re)
table(enq.final$E6_a_re) ##Déjà ordonnée selon le questionaire

#E6_b_re#
colnames(enq.final)[243]
class(enq.final$E6_b_re)
table(enq.final$E6_b_re)
enq.final$E6_b_re <- factor(enq.final$E6_b_re, levels = c("Très souvent", "Souvent", "Occasionnellement", "Jamais"))
table(enq.final$E6_b_re)

#E7_re#
colnames(enq.final)[244]
class(enq.final$E7_re)
table(enq.final$E7_re) ##Déjà ordonnée selon le questionaire

#E7_a_re#
colnames(enq.final)[245]
class(enq.final$E7_a_re)
table(enq.final$E7_a_re) ##Déjà ordonnée selon le questionaire

#E7_b_re#
colnames(enq.final)[246]
class(enq.final$E7_b_re)
table(enq.final$E7_b_re)
enq.final$E7_b_re <- factor(enq.final$E7_b_re, levels = c("Très souvent", "Souvent", "Occasionnellement", "Jamais"))
table(enq.final$E7_b_re)

#E8_re#
colnames(enq.final)[247]
class(enq.final$E8_re)
table(enq.final$E8_re) ##Déjà ordonnée selon le questionaire

#E8_a_re#
colnames(enq.final)[248]
class(enq.final$E8_a_re)
table(enq.final$E8_a_re) ##Déjà ordonnée selon le questionaire

#E8_b_re#
colnames(enq.final)[249]
class(enq.final$E8_b_re)
table(enq.final$E8_b_re)
enq.final$E8_b_re <- factor(enq.final$E8_b_re, levels = c("Très souvent", "Souvent", "Occasionnellement", "Jamais"))
table(enq.final$E8_b_re)

##########################################"#
###### Partie 3: Socio-démo/habitudes ######
############################################
#PARTIE SEBASTIEN

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
#H2 "Considérez-vous que la soirée d’hier était une soirée inhabituelle ?"
colnames(enq.final)[116]
attributes(enq.final)$variable.labels[116]
typeof(enq.final$H2)
summary(enq.final$H2)

table(enq.final$H2)
sum(is.na(enq.final$H2))
enq.final$H2_re <- enq.final$H2
enq.final$H2_re<-relevel(enq.final$H2_re,"Oui")
summary(enq.final$H2_re)#Remis dans l'ordre
##•Fait: Remis dans l'ordre
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
#Fait : remis dans l'ordre #
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
#Fait : rien # Comment gérer ces chhoix multiples
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
#Fait : valeurs abjectes supprimées (3 NA en plus) #

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
levels(enq.final$D3_re)
enq.final$D3_re<-enq.final$D3



#D3_other : [Autre] Quel est le diplôme le plus élevé que vous ayez obtenu ?  #
colnames(enq.final)[125]
attributes(enq.final)$variable.labels[125]
summary(enq.final$D3_other)
typeof(enq.final$D3_other)
class(enq.final$D3_other)
table(enq.final$D3_other)
sum(is.na(enq.final$D3_other))

enq.final$D3_other_re [enq.final$D3_other %in% c("14 ans au maroc, ne sait pas quel niveau")]<-"14 ans au Maroc (à reclasser)"
enq.final$D3_other_re [enq.final$D3_other %in% c("à l'étranger jusqu'au lycée")]<-"a reclasser(etranger jusqu'au lycee)"
#A soumettre: pour moi on ne peut rien tirer de "à l'étranger jusqu'au lycée"
enq.final$D3_other_re [enq.final$D3_other %in% c("brevet professionel")]<-"BEPC, brevet"
enq.final$D3_other_re [enq.final$D3_other %in% c("diplome auxiliaire de puericulture", "niveau cap")]<-"CAP, BEP"
enq.final$D3_other_re [enq.final$D3_other %in% c("Diplome d'état")]<-"Diplome d'état (à reclasser)"
#Je comptais le mettre en NA
enq.final$D3_other_re [enq.final$D3_other %in% c("Diplôme de retoucherie")]<-"CAP, BEP"
# si retoucherie signifie retouche j'ai cherché et il suffit d'un CAP ou BEP pour y acceder
enq.final$D3_other_re [enq.final$D3_other %in% c("educatrice spécialisée")]<-"Licence, Bac+3" 
enq.final$D3_other_re [enq.final$D3_other %in% c("Je n'ai pas passé mon certificat d'étude donc je ne sais pas ce que ça vaut maintenant","permis de conduire")]<-"Aucun diplôme"
enq.final$D3_other_re [enq.final$D3_other %in% c("niveau 4")]<-"Baccalauréat, BP"#j'ai vérifié
enq.final$D3_other_re [enq.final$D3_other %in% c("passe le bac")]<-"BEPC, brevet"
enq.final$D3_other_re [enq.final$D3_other %in% c("premier prix du conservatoire de paris (20ans)")]<-"Licence, Bac+3"
enq.final$D3_other_re [enq.final$D3_other %in% c("Thèse")]<-"Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc."
table(enq.final$D3_other_re)# il y en a 12 avec les 2 NA, ce qui est normal



#Lien D3 DE_other
enq.final$D3_re <- ifelse(is.na(enq.final$D3_other_re)==FALSE,ifelse(enq.final$D3_other_re %in% levels(enq.final$D3_re), as.character(enq.final$D3_other_re), "Autre (voir D3_other_re)"),as.character(enq.final$D3_re) )

table(enq.final$D3_re, useNA = "ifany")
enq.final$D3_re <- factor(enq.final$D3_re,levels=c(levels(enq.final[,124])[6],"BEPC, brevet",
                                                   "CAP, BEP","Baccalauréat, BP",
                                                   "Deug, DUT, BTS, diplômes des professions sociales ou de la santé",
                                                   "Licence, Bac+3","Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc.",
                                                   "Aucun diplôme", "Autre (voir D3_other_re)",
                                                   "Refus"))

table(enq.final$D3_re)

#fait: niveaux remis dans l'ordre, rangé les individus qui pouvaient l'être
#a faire: finir de regrouper les autres regroupables (les trois derniers)

enq.final$D3_re <- as.factor(enq.final$D3_re)
typeof(enq.final$D3_re)
class(enq.final$D3_re)
table(enq.final$D3_re)
#remis en facrtor

#D4 : Actuellement, quelle est votre situation ?  # /dans le dictionnaire des variables: "exercez-vous une activité rémunérée?
colnames(enq.final)[126]
attributes(enq.final)$variable.labels[126]
summary(enq.final$D4)
typeof(enq.final$D4)
class(enq.final$D4)
table(enq.final$D4)
##Problème de modalités répétées + revoir dictionnaire des variables (Etudiant(s) absents...)
sum(is.na(enq.final$D4))
levels(enq.final$D4)
#croisement selon ordi/télphone de cette variable
table(enq.final$D4,enq.final$dummy.tel,useNA = "ifany")
#visiblement, j'imagine que la question et les modalités ont été changées en cours d'enquête,
#pendant que nous passions les questionnaires par téléphone, au tout début (donc pas de pb pour le téléphone)
#Le mail qu'Antoine a envoyé aux chargés de Limesurvey atteste de cette version, il mentionnait la répétion de la case retraitée
#En revanche pour les panels nous sommes revenus sur l'ancienne version du questionnaire avec les modalités répétées
#Les modalités concernant les etudiant sont présentes dans les deux modes d'enquêtes donc pas de biais de ce côté là
#La question initiale devait être exercez-vous une activité rémunérée et la finale, Actuellement quelle est votre situation?
#Que faire?
#J'ai pour l'instant décidé de regrouper les modalités retraite et recherche d'emploi (ci-dessous) qui sont répétées
#Le problème des auto-entrepreneurs reste donc en suspension

enq.final$D4_re <- enq.final$D4

#La modalité
enq.final$D4_re [enq.final$D4_re %in% c("En recherche d'emploi",levels(enq.final[,126])[9])]<-"En recherche d'emploi"
enq.final$D4_re [enq.final$D4_re %in% c("Non, à la retraite","A la retraite")]<-"A la retraite"
enq.final$D4_re <- droplevels(enq.final$D4_re)


table(enq.final$D4_re, useNA = "ifany") # on a bien 57 NA

#Fait : regroupé modalités identiques #


#D4_other :  [Autre] Actuellement, quelle est votre situation ?  #
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
enq.final$D4_other_re [enq.final$D4_other %in% c("rentiste")]<-"Rentier ou dentiste"




#Maladie est-ce invalidité? Intérim? Rentiste est-ce rentier?



##Lien D4,D4other
table(enq.final$D4_other_re, useNA = "ifany")

enq.final$D4_re <- ifelse(is.na(enq.final$D4_other_re)==FALSE,ifelse(enq.final$D4_other_re %in% levels(enq.final$D4_re), as.character(enq.final$D4_other_re), "Autre (voir D4_other_re)"),as.character(enq.final$D4_re) )

table(enq.final$D4_re, useNA = "ifany")

enq.final$D4_re <- as.factor(enq.final$D4_re)
#Ci-dessous, remise en ordre des niveaux
enq.final$D4_re <- factor(enq.final$D4_re,levels=c("Emploi à plein temps","Emploi à temps partiel","Au foyer",
                                                   "En recherche d'emploi","Etudiant sans emploi","Etudiant avec emploi","A la retraite","Autre (voir D4_other_re)"))
table(enq.final$D4_re, useNA = "ifany")

typeof(enq.final$D4_re)
class(enq.final$D4_re)
table(enq.final$D4_re)

#Fait : regroupement des modalités identiques, remise dans l'ordre #
#A faire: Discuter du problème des modalités et du changemetn de l'intitulé de la question
#Eventuelle recatégorisation des "autres"
#Eventuellement réordonner selon le futur nouveau dico des variables
#regroupement des autres?
#Quelle est votre profession ou la dernière que vous ayez exercée ? 
colnames(enq.final)[128]
attributes(enq.final)$variable.labels[128]
summary(enq.final$D5)
typeof(enq.final$D5)
class(enq.final$D5)
table(enq.final$D5)
sum(is.na(enq.final$D5))
#Fait rien
#A faire: Voir Antoine et Nadia

#Qui compose votre foyer ? 
colnames(enq.final)[129]
attributes(enq.final)$variable.labels[129]
summary(enq.final$D6)
typeof(enq.final$D6)
class(enq.final$D6)
table(enq.final$D6)
sum(is.na(enq.final$D6))
enq.final$D6_re<-enq.final$D6
levels(enq.final$D6_re)

table(enq.final$D6_re,useNA = "ifany")


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
enq.final$D6_re <- ifelse(is.na(enq.final$D6_other_re)==FALSE,ifelse(enq.final$D6_other_re %in% levels(enq.final$D6_re), as.character(enq.final$D6_other_re), "Autre (voir D6_other_re)"),as.character(enq.final$D6_re) )

table(enq.final$D6_re, useNA = "ifany") #1 "vous uniquement en plus"

enq.final$D6_re <- as.factor(enq.final$D6_re)
typeof(enq.final$D6_re)
class(enq.final$D6_re)
table(enq.final$D6_re)

#remise en ordre
enq.final$D6_re <- factor(enq.final$D6_re,levels=c("Vous uniquement","Vous et un ou plusieurs enfant(s)",
                                                   "Vous et votre conjoint(e) (sans enfant)","Vous et votre conjoint(e), avec un ou plusieurs enfant(s)",
                                                   levels(enq.final[,129])[1],"Autre (voir D6_other_re)" ))
table(enq.final$D6_re, useNA = "ifany")

#Fait: prise en comprte autre, remise dans l'ordre
#a faire: recoder autres?
#Actuellement, quelle est la situation de votre conjoint ? 
colnames(enq.final)[131]
attributes(enq.final)$variable.labels[131]
summary(enq.final$D7)
typeof(enq.final$D7)
class(enq.final$D7)
table(enq.final$D7)
#Visiblement, pas le même pb que précédemment (pas de modalités répétées) 
#mais les modalités du dico des variables sont erronnées (etudiants ommis)
sum(is.na(enq.final$D7))
levels(enq.final$D7)
enq.final$D7_re <-enq.final$D7
table(enq.final$D7_re,useNA="ifany")


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
enq.final$D7_other_re [enq.final$D7_other %in% c("SANS EMPLOI")]<-levels(enq.final[,131])[5]
enq.final$D7_other_re [enq.final$D7_other %in% c("Conger maternité")]<-"congé maternité"
enq.final$D7_other_re [enq.final$D7_other %in% c("etudiante")]<-"Etudiant"
enq.final$D7_other_re [enq.final$D7_other %in% c("Formation")]<-"en formation"
enq.final$D7_other_re [enq.final$D7_other %in% c("intercontrat","interim")]<-"interim"
enq.final$D7_other_re [enq.final$D7_other %in% c("medecin")]<-"Emploi"
table(enq.final$D7_other_re,useNA="ifany")

#Lien D7 D7_other
enq.final$D7_re <- ifelse(is.na(enq.final$D7_other_re)==FALSE,ifelse(enq.final$D7_other_re %in% levels(enq.final$D7_re), as.character(enq.final$D7_other_re), "Autre (voir D7_other_re)"),as.character(enq.final$D7_re) )

table(enq.final$D7_re, useNA = "ifany")

enq.final$D7_re <- as.factor(enq.final$D7_re)
typeof(enq.final$D7_re)
class(enq.final$D7_re)
table(enq.final$D7_re)
enq.final$D7_re <- factor(enq.final$D7_re,levels=c("Emploi à plein temps","Emploi à temps partiel","Au foyer",
                                                   levels(enq.final[,131])[5],"Etudiant sans emploi","Etudiant avec emploi","A la retraite","Autre (voir D7_other_re)"))
table(enq.final$D7_re,useNA = "ifany")

#Fait: prise en compte "autre", remise dans l'ordre
#a faire: recoder autres?

#Quelle est cette profession ?  #NADIA ET ANTOINE?
colnames(enq.final)[133]
attributes(enq.final)$variable.labels[133]
summary(enq.final$D8)
typeof(enq.final$D8)
class(enq.final$D8)
table(enq.final$D8)
sum(is.na(enq.final$D8))
levels(enq.final$D8)

#Quel est le diplôme le plus élevé de votre conjoint ?
colnames(enq.final)[134]
attributes(enq.final)$variable.labels[134]
summary(enq.final$D3_bis)
typeof(enq.final$D3_bis)
class(enq.final$D3_bis)
table(enq.final$D3_bis)
sum(is.na(enq.final$D3_bis))
levels(enq.final$D3_bis)

enq.final$D3_bis_re<-enq.final$D3_bis


table(enq.final$D3_bis_re)




#[Autre] Quel est le diplôme le plus élevé de votre conjoint ?
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
enq.final$D3_bis_other_re [enq.final$D3_bis_other %in% c("Artiste")]<-"Artiste"
enq.final$D3_bis_other_re [enq.final$D3_bis_other %in% c("refus")]<-"Refus"
table(enq.final$D3_bis_other_re)

#Lien D3_bis D3_bis_other
enq.final$D3_bis_re <- ifelse(is.na(enq.final$D3_bis_other_re)==FALSE,ifelse(enq.final$D3_bis_other_re %in% levels(enq.final$D3_bis_re), as.character(enq.final$D3_bis_other_re), "Autre (voir D3_bis_other_re)"),as.character(enq.final$D3_bis_re) )

table(enq.final$D3_bis_re, useNA = "ifany")

enq.final$D3_bis_re <- as.factor(enq.final$D3_bis_re)
typeof(enq.final$D3_bis_re)
class(enq.final$D3_bis_re)
enq.final$D3_bis_re <- factor(enq.final$D3_bis_re,levels=c(levels(enq.final[,134])[6],"BEPC, brevet",
                                                           "CAP, BEP","Baccalauréat, BP",
                                                           "Deug, DUT, BTS, diplômes des professions sociales ou de la santé",
                                                           "Licence, Bac+3","Bac+4 ou plus : Master, maîtrise, DEA, école d'ingénieur, doctorat etc.",
                                                           "Aucun diplôme", "Autre (voir D3_bis_other_re)",
                                                           "Refus"))
table(enq.final$D3_bis_re)





#Combien d’enfants vivent avec vous au moins la moitié du temps?
colnames(enq.final)[136]
attributes(enq.final)$variable.labels[136]
summary(enq.final$D9)
typeof(enq.final$D9)
class(enq.final$D9)
table(enq.final$D9)
sum(is.na(enq.final$D9))
levels(enq.final$D9)
enq.final$D9_re<-relevel(enq.final$D9,"Aucun")
#croisement avoir un enfant et compo foyer=>on voit qu'il n'y a pas de NA chez ceux qui répondes a avoir un enfant
table(enq.final$D6_re,enq.final$D9_re,useNA = "ifany")
#Rien à faire,

#Quel âge a votre enfant ?
colnames(enq.final)[137]
attributes(enq.final)$variable.labels[137]
summary(enq.final$D10_bis)
typeof(enq.final$D10_bis)
class(enq.final$D10_bis)
table(enq.final$D10_bis)#ça me paraît correct, pas de valeurs absurdes
sum(is.na(enq.final$D10_bis))
levels(enq.final$D10_bis)
enq.final$D10_bis_re<-enq.final$D10_bis
#croisement avec nb enfants
table(enq.final$D10_bis_re,enq.final$D9_re,useNA = "ifany")#PAs de pb

#Quel âge a le plus jeune ?
colnames(enq.final)[138]
attributes(enq.final)$variable.labels[138]
summary(enq.final$D10)
typeof(enq.final$D10)
class(enq.final$D10)
table(enq.final$D10)#ça me paraît correct, pas de valeurs absurdes
sum(is.na(enq.final$D10))
levels(enq.final$D10)
enq.final$D10_re<-enq.final$D10

#Quel âge a le plus âgé?
colnames(enq.final)[139]
attributes(enq.final)$variable.labels[139]
summary(enq.final$D11)
typeof(enq.final$D11)
class(enq.final$D11)
table(enq.final$D11)#ça me paraît correct, pas de valeurs absurdes
sum(is.na(enq.final$D11))
levels(enq.final$D11)
enq.final$D11_re<-enq.final$D11

#Quel est le revenu mensuel moyen de votre ménage ? (en revenu net)
colnames(enq.final)[140]
attributes(enq.final)$variable.labels[140]
summary(enq.final$D12)
typeof(enq.final$D12)
class(enq.final$D12)
table(enq.final$D12)
sum(is.na(enq.final$D12))
levels(enq.final$D12)
enq.final$D12_re<-enq.final$D12
enq.final$D12_re <- factor(enq.final$D12_re,levels=c(levels(enq.final[,140])[2],levels(enq.final[,140])[3],
                                                     levels(enq.final[,140])[4],levels(enq.final[,140])[5],
                                                     levels(enq.final[,140])[6],levels(enq.final[,140])[7],
                                                     levels(enq.final[,140])[8],levels(enq.final[,140])[9],
                                                     levels(enq.final[,140])[10],
                                                     levels(enq.final[,140])[1],"Refus","Ne sait pas"))
table(enq.final$D12_re)
#Niveaux reconnus
#Regardez si le signe euro fonctionne chez vous

#D14 c'est les commentaires. Comment les traiter
