setwd("C:/Users/Marie Plessz/Google Drive/Socstat Enquete Quanti/Socstat Enquete Quanti partage/2-TeleRepas/_Data/BDD réconciliée")
enq.final <- read.csv("Etiquettes/enq final.csv", encoding = "UTF-8")
etiquettes <- read.csv("Etiquettes/etiquettes.csv", encoding = "UTF-8")
etiquettes <- as.character(etiquettes$x)
attributes(enq.final)$variable.labels <- etiquettes
attributes(enq.final)$variable.labels[120]
enq.final$X <- NULL

######


```{r}
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
#A faire : transformer en variable quanti pourrait donner une idée du nb moyen de prise alimentaire (borne inférieure). Pas sûr que ça soit à faire cela dit. #
#15 NA - s'en occuper#

```

```{r}
#A2_1_1 "[Prise alimentaire] [Heure] À quelle heure a eu lieu votre prise alimentaire  ?"
colnames(enq.final)[9]
attributes(enq.final)$variable.labels[9]
typeof(enq.final$A2_1_1)
summary(enq.final$A2_1_1)
table(enq.final$A2_1_1)
sum(is.na(enq.final$A2_1_1))
#FAIT/A FAIRE : voir plus bas (j'en fais deux d'un coup) #

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
#(1)id_r = 479 a une réponse aberrante ("12,30-19,00") - C'est un pannel#
#(2)il existe un certain nombre de réponses problématiques à recoder : 05,30 /07,30/6,00/7,00/7,15/7,20/8,00.#
#2-1 - 5h30 : pain grillé (ID_R 499)
#2-2 ID_R 437-439 "UN BOL DE CHOCOLAT DES TARTINES BEURRE UN VERRE DE JUS D ORANGE" & 07h30....PANNEL
#2-3 ID_R 567 MUESLI à 6h00....
#2-3 Que fait-on des 16h à 16H30 ?
#(3) J'ai laissé cette variable en charactère. En quoi faut-il la convertir ? Il est possible de la convertir en objet #time# à l'aide du code as.POSIXct(enq.final$A2_re, format = "%H,%M"), mais "it turns out there's no pure "time" object, so every time must also have a date" donc ça donne forcément une date##
#(4) NA à enquêter (231 ????!!!!!) = même nombre de NA que dans les variables d'origine, c'est étrange - peut être problème d'importation ?#
#(5) Il est sans doute faux de considérer que lorsque qqn dit "19h", il voulait dire "19h,00"
```

```{r}
#A2_bis_1_1 [Heure] À quelle heure a eu lieu votre prise alimentaire principale (celle que vous considérez comme la plus importante) ?""
colnames(enq.final)[11]
attributes(enq.final)$variable.labels[11]
typeof(enq.final$A2_bis_1_1)
table(enq.final$A2_bis_1_1)
sum(is.na(enq.final$A2_bis_1_1))

#"A2_bis_1_2" "[Prise alimentaire] [Minute] À quelle heure a eu lieu votre prise alimentaire principale (celle que vous considérez comme la plus importante) ?"
colnames(enq.final)[12]
attributes(enq.final)$variable.labels[12]
typeof(enq.final$A2_bis_1_2)
table(enq.final$A2_bis_1_2)
sum(is.na(enq.final$A2_bis_1_2))

enq.final$A2_bis_re <- paste(enq.final$A2_bis_1_1, enq.final$A2_bis_1_2, sep=',')
table(enq.final$A2_bis_re)
enq.final$A2_bis_re <- gsub("h",",",enq.final$A2_bis_re, ignore.case =TRUE)
enq.final$A2_bis_re <- gsub(",,",",",enq.final$A2_bis_re, ignore.case =TRUE)
enq.final$A2_bis_re <-ifelse(enq.final$A2_bis_re=="00,0","00,00",ifelse(enq.final$A2_bis_re=="1,0","01,00",ifelse(enq.final$A2_bis_re=="12,0","12,00",ifelse(enq.final$A2_bis_re=="13,0","13,00",ifelse(enq.final$A2_bis_re=="13,5","13,05",ifelse(enq.final$A2_bis_re=="16,0","16,00",ifelse(enq.final$A2_bis_re=="17,0","17,00",ifelse(enq.final$A2_bis_re=="18,0","18,00",ifelse(enq.final$A2_bis_re=="19,0","19,00",ifelse(enq.final$A2_bis_re=="19,30,30","19,30",ifelse(enq.final$A2_bis_re=="1930,30","19,30",ifelse(enq.final$A2_bis_re=="21,0","21,00",ifelse(enq.final$A2_bis_re=="22,0","22,00",ifelse(enq.final$A2_bis_re=="23,0","23,00",ifelse(enq.final$A2_bis_re=="8,0","8,00",ifelse(enq.final$A2_bis_re=="20,0","20,00",enq.final$A2_bis_re))))))))))))))))
table(enq.final$A2_bis_re)

# A FAIRE :
#1. Un certain nombre de valeurs problématiques : 13h, 16h, etc.
#2. Recoder "13,5"/22,9/9,5 ( sans doute 13,05/22,09/09,05) mais à voir
#3. + même pb que pour la question précédente (i.e quel type de variable)
```

```{r}
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

```

```{r}
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

```

```{r}
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

```

```{r}
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

```

```{r}
#A6 "Veuillez lister tout ce que vous avez mangé à cette occasion :"
colnames(enq.final)[16]
attributes(enq.final)$variable.labels[16]
typeof(enq.final$A6)
class(enq.final$A6)
table(enq.final$A6)
sum(is.na(enq.final$A6))

enq.final$A6_re <- enq.final$A6

#FAIT : rien (déjà en caractère)#

```

```{r}
#A7 "Considérez-vous que c'était un repas ?"
colnames(enq.final)[17]
attributes(enq.final)$variable.labels[17]
typeof(enq.final$A7)
class(enq.final$A7_re)
table(enq.final$A7)
sum(is.na(enq.final$A7))

enq.final$A7_re <- enq.final$A7


#FAIT : rien (déjà en factor)#

```

```{r}
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

```

```{r}
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

```

```{r}
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

enq.final$A10_other_re <- enq.final$A10_other

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
## *PROBLEME* : il y a une modalité 7##

```

```{r}
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

```

```{r}
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
enq.final$A11_6_other_re <- ifelse(is.na(enq.final$A11_other_re)==FALSE,
                                   "Oui",enq.final$A11_other_re)
table(enq.final$A11_6_other_re)

#FAIT : créé une dummy A11_6_other pour les réponses autres ; nettoyé les réponses autres dans A11_6_other #
#A faire : 
# *probleme* modalité non pour a11_other_6_re 
#-Rebasculer le " Oui, je regardais la télévision" en oui je regardais un écran
-#Regarder ce qu'il faut rebasculer en créant une dummmy (tout ce qui a trait au son et à la musique notamment)#
  ```


```{r}
#A12 "Quelqu'un était-il avec vous pendant que vous mangiez ? "#
colnames(enq.final)[29]
attributes(enq.final)$variable.labels[29]
typeof(enq.final$A12)
class(enq.final$A12)
table(enq.final$A12)
sum(is.na(enq.final$A12))

enq.final$A12_re <- enq.final$A12

#FAIT : rien#
#A faire : rien ?#

```

```{r}

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


enq.final$A13_5_other_re <- enq.final$A13_other

enq.final$A13_5_other_re <- ifelse(enq.final$A13_5_other_re=="2 Petits enfants"|enq.final$A13_5_other_re=="mon petit fils de 3 ans","Petit enfant",ifelse(enq.final$A13_5_other_re=="aide menagere","Aide Ménagère",ifelse(enq.final$A13_5_other_re=="camarades de fac","Camarades de fac",enq.final$A13_5_other_re)))
table(enq.final$A13_5_other_re)

enq.final$A13_6_re <- ifelse(is.na(enq.final$A13_5_other_re)==FALSE,
                             "Oui",enq.final$A13_5_other_re)

#FAIT : créé une dummy A13_6 pour les réponses autres ; nettoyé les réponses autres dans A13_6_other #
# A FAIRE#
#Recodages
#*probleme* ça nous a encore fait un 4 wtf*
#"petit fils". Très étrange d'avoir mis "famille"" et enfant. Où mettre petit-fils (à la fois "famille" et enfants (mais pas les siens !)) ? Si ce qui nous intéresse, c'est l'âge, alors recoder en enfant ; si c'est la nature du lien, alors recoder en famille.
#présences forcées non prof ("collègues") : colocataire/camarades de fac (non-amis)
#

#

```

```{r}
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

```

```{r}
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

```

```{r}
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

```

```{r}
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
table(enq.final$A17_re)
#A FAIRE : RECODER 0,0 en 12h30 etc.#

#A17_bis_SQ001_SQ001 "[Prise alimentaire] [Heure] \tNous allons maintenant évoquer rapidement vos autres prises alimentaires de la soirée. Commençons par celle que vous avez pris le plus tôt dans la soirée (à partir de 18h).\t A quelle heure a-t-elle eu lieu ?"#

colnames(enq.final)[41]
attributes(enq.final)$variable.labels[41]

enq.final$A17_bis_re <- paste(enq.final$A17_bis_SQ001_SQ001, enq.final$A17_bis_SQ001_SQ002, sep=',')
enq.final$A17_bis_re <- gsub("h","",enq.final$A17_re, ignore.case =TRUE)
enq.final$A17_bis_re <- gsub(",,",",",enq.final$A17_re, ignore.case =TRUE)
table(enq.final$A17_bis_re)
#A FAIRE : RECODER 0,0 en 12h30 etc.#
#*probleme* 6,0**


enq.final$A17_re_synth <- ifelse(enq.final$A17_re!="NA,NA",enq.final$A17_re,ifelse(enq.final$A17_bis_re!="NA,NA",enq.final$A17_bis_re,enq.final$A17_bis_re))
table(enq.final$A17_re_synth)

#FAIT : J'ai créé une variable synthétique A17_re_synth dans laquelle on a l'heure du deuxième repas pour tous ceux qui en ont pris 1#
# *probleme* homogeniser notation variables temps (synth)#

```


```{r}
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

```

```{r}
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

```

```{r}
#A20 "Pouvez-vous me dire où vous étiez à ce moment-là ?"#
colnames(enq.final)[45]
attributes(enq.final)$variable.labels[45]
typeof(enq.final$A20)
class(enq.final$A20)
table(enq.final$A20)
sum(is.na(enq.final$A20))
table(enq.final$A20)
enq.final$A20_re <- as.character(enq.final$A20)

#A21 "Pouvez-vous me dire où vous étiez à ce moment-là ?"#
colnames(enq.final)[46]
attributes(enq.final)$variable.labels[46]
typeof(enq.final$A20_other)
class(enq.final$A20_other)
table(enq.final$A20_other)
sum(is.na(enq.final$A20_other))
table(enq.final$A20_other)
enq.final$A20_other_re <- enq.final$A20_other

enq.final$A20_other_re <- ifelse(enq.final$A20_other_re=="opéra","Opéra",ifelse(enq.final$A20_other_re=="terrasse","Terrasse",ifelse(enq.final$A20_other_re=="théâtre","Théâtre",enq.final$A13_5_other_re)))

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

```

```{r}
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

enq.final$A21_6_re <- ifelse(is.na(enq.final$A21_other_re)==FALSE,"Oui",enq.final$A21_other_re)

#FAIT : CLEANAGE VAR OTHER (AVEC REGROUPEMENT) / + CREATION NVELLE DUMMY#

#A FAIRE : DISCUTER RECODAGE#
#Même prb que précédemment pour musique + Questions des travaux ménagers (nettoyage, ménage, cuisine)#
#2 cents...anti-passivité....Ces gens remangent parce qu'ils font qqchose (activité) et qu'ils sont en contact avec de la nourriture...approche par le corps (activité + proximité avec bouffe comme prédicteur)...mais minoritaire (versus télé)#

```

```{r}
#A22_SQ001_SQ001 [Heure] \tNous allons maintenant parler de votre dernière prise alimentaire de la soirée. S'il s'agit de votre prise principale, parlez-nous de l'avant dernière.\tÀ quelle heure a-t-elle eu lieu ?"#
colnames(enq.final)[53]
attributes(enq.final)$variable.labels[53]
colnames(enq.final)[54]

enq.final$A22_re <- paste(enq.final$A22_SQ001_SQ001, enq.final$A22_SQ001_SQ002, sep=',')
enq.final$A22_re <- gsub("h","",enq.final$A22_re, ignore.case =TRUE)
enq.final$A22_re <- gsub(",,",",",enq.final$A22_re, ignore.case =TRUE)

table(enq.final$A22_re)
enq.final$A22_re <-ifelse(enq.final$A22_re=="0,0","00,00",ifelse(enq.final$A22_re=="00,0","00,00",enq.final$A22_re))
table(enq.final$A22_re)
#FAIT : CREATION D'UNE VAR REGROUPANT H/MIN#
#A FAIRE : REGROUPER LES HEURES 7h30/19h30...Interroger les valeurs aberrantes#
```

```{r}
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
```

```{r}
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

```

```{r}
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

```

```{r}
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
```

```{r}
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

enq.final$A26_7_re <-ifelse(is.na(enq.final$A26_other_re)==FALSE,"Oui",enq.final$A26_other_re)
table(enq.final$A26_7_re)

#FAiT : cREATION d'une dummy supp#

#A faire : checker les recodages#

```