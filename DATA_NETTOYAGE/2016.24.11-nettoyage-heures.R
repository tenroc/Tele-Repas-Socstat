# Code une focntion paste qui prends en compte les NA au lieu de donner un resultat "NAhNA"

paste3 <- function(...,sep=", ") {
  L <- list(...)
  L <- lapply(L,function(x) {x[is.na(x)] <- ""; x})
  ret <-gsub(paste0("(^",sep,"|",sep,"$)"),"",
             gsub(paste0(sep,sep),sep,
                  do.call(paste,c(L,list(sep=sep)))))
  is.na(ret) <- ret==""
  ret
}

## Recodage des niveaux pour la premiere tranche horaire

enq.final$A2_1_1_R <- paste3(as.character(enq.final$A2_1_1),as.character(enq.final$A2_bis_1_1))
enq.final$A2_1_2_R <- paste3(as.character(enq.final$A2_1_2),as.character(enq.final$A2_bis_1_2))

enq.final$A2_1_1_R [enq.final$A2_1_1_R == "18h"] <- "18"
enq.final$A2_1_1_R [enq.final$A2_1_1_R == "13H"] <- "13"
enq.final$A2_1_1_R [enq.final$A2_1_1_R %in% c("19.00","19h")] <- "19"
enq.final$A2_1_2_R [enq.final$A2_1_1_R == "19H00"] <- "00"
enq.final$A2_1_2_R [enq.final$A2_1_1_R %in% c("19h30","1930")] <- "30"
enq.final$A2_1_1_R [enq.final$A2_1_1_R %in% c("19H00","19h30","1930")] <- "19"
enq.final$A2_1_1_R [enq.final$A2_1_1_R == "20h"] <- "20"
enq.final$A2_1_1_R [enq.final$A2_1_1_R == "21h"] <- "21"
enq.final$A2_1_2_R [enq.final$A2_1_1_R == "12h30"] <- "30"
enq.final$A2_1_1_R [enq.final$A2_1_1_R %in% c("12h30","12h")] <- "12"
enq.final$A2_1_2_R [enq.final$A2_1_1_R == ".              8"] <- "8"


enq.final$A2_1_2_R [enq.final$A2_1_1_R == "19h00"] <- "19"
enq.final$A2_1_2_R [enq.final$A2_1_2_R == "19h00"] <- "00"
enq.final$A2_1_2_R [enq.final$A2_1_2_R == "0"] <- "00"
enq.final$A2_1_2_R [enq.final$A2_1_2_R == "40mn"] <- "40"
enq.final$A2_1_2_R [enq.final$A2_1_2_R == "5"] <- "05"
enq.final$A2_1_2_R [enq.final$A2_1_2_R == "8"] <- "08"
enq.final$A2_1_2_R [enq.final$A2_1_2_R == "9"] <- "09"

levels(as.factor(enq.final$A2_1_1_R))
levels(as.factor(enq.final$A2_1_2_R))

## Recodage des niveaux pour la seconde tranche horaire:

enq.final$A17_SQ001_SQ001_R <- paste3(as.character(enq.final$A17_SQ001_SQ001),as.character(enq.final$A17_bis_SQ001_SQ001))
enq.final$A17_SQ001_SQ002_R <- paste3(as.character(enq.final$A17_SQ001_SQ002),as.character(enq.final$A17_bis_SQ001_SQ002))

enq.final$A17_SQ001_SQ001_R [enq.final$A17_SQ001_SQ001_R == "19h"] <- "19"
enq.final$A17_SQ001_SQ001_R [enq.final$A17_SQ001_SQ001_R == "20h"] <- "20"
enq.final$A17_SQ001_SQ001_R [enq.final$A17_SQ001_SQ001_R == "0"] <- "00"
enq.final$A17_SQ001_SQ001_R [enq.final$A17_SQ001_SQ001_R == "21h"] <- "21"
enq.final$A17_SQ001_SQ001_R [enq.final$A17_SQ001_SQ001_R == "22h"] <- "22"

enq.final$A17_SQ001_SQ002_R [enq.final$A17_SQ001_SQ002_R == "0"] <- "00"
enq.final$A17_SQ001_SQ002_R [enq.final$A17_SQ001_SQ002_R == "3O"] <- "30"

levels(as.factor(enq.final$A17_SQ001_SQ001_R))
levels(as.factor(enq.final$A17_SQ001_SQ002_R))


## Recodage des niveaux pour la troisieme tranche horaire

enq.final$A22_SQ001_SQ001_R <- as.character(enq.final$A22_SQ001_SQ001)
enq.final$A22_SQ001_SQ002_R <- as.character(enq.final$A22_SQ001_SQ002)

enq.final$A22_SQ001_SQ001_R [enq.final$A22_SQ001_SQ001_R == "0"] <- "00"
enq.final$A22_SQ001_SQ001_R [enq.final$A22_SQ001_SQ001_R == "20h"] <- "20"
enq.final$A22_SQ001_SQ001_R [enq.final$A22_SQ001_SQ001_R == "22H"] <- "22"
enq.final$A22_SQ001_SQ001_R [enq.final$A22_SQ001_SQ001_R == "23h"] <- "23"

enq.final$A22_SQ001_SQ002_R [enq.final$A22_SQ001_SQ002_R == "0"] <- "00"
enq.final$A22_SQ001_SQ002_R [enq.final$A22_SQ001_SQ002_R == "5"] <- "05"

levels(as.factor(enq.final$A22_SQ001_SQ001_R))
levels(as.factor(enq.final$A22_SQ001_SQ002_R))


# loop flag: si la variable a ete marquee comme etant dans la bonne tanche horaire, mais codee en am/pm, la repasse en format 24h
# Si la variable a été codée comme étant dans la mauvaise tranche horaire (incompréhension de la question),
# tente de repasser la seconde prise déclarée en premiere prise (a débattre, dû au format: prise principale)
# Si la seconde prise est une non-réponse, l'individu est supprimé (a débattre)

# heure3:

for(i in 1:nrow(enq.final)){
  if(is.na(enq.final[i,"flag_n3"])==F){
  if(enq.final[i,"flag_n3"]==1){
    enq.final[i,"A22_SQ001_SQ001_R"] <- as.character(as.numeric(enq.final[i,"A22_SQ001_SQ001_R"]) + 12)
  } else {
    if (enq.final[i,"flag_n3"]==0){
      enq.final[i,"A22_SQ001_SQ001_R"] <- NA
      enq.final[i,"A22_SQ001_SQ002_R"] <- NA
    }
  }
}
}

# heures2:

for(i in 1:nrow(enq.final)){
  if(is.na(enq.final[i,"flag_n2"])==F){
  if(enq.final[i,"flag_n2"]==1){
    enq.final[i,"A17_SQ001_SQ001_R"] <- as.character(as.numeric(enq.final[i,"A17_SQ001_SQ001_R"]) + 12)
  } else {
    if (enq.final[i,"flag_n2"]==0){
      if (is.na(enq.final[i,"A22_SQ001_SQ001_R"]) != T){
        enq.final[i,"A17_SQ001_SQ001_R"] <- enq.final[i,"A22_SQ001_SQ001_R"]
        enq.final[i,"A17_SQ001_SQ002_R"] <- enq.final[i,"A22_SQ001_SQ002_R"]
      }
    }
  }
  }
}

# heures1:

for(i in 1:nrow(enq.final)){
  if(is.na(enq.final[i,"flag_n1"])==F){
  if(enq.final[i,"flag_n1"]==1){
    enq.final[i,"A2_1_1_R"] <- as.character(as.numeric(enq.final[i,"A2_1_1_R"]) + 12)
  } else {
    if (enq.final[i,"flag_n1"]==0){
      if (is.na(enq.final[i,"A2_2_1_R"]) != T){
        enq.final[i,"A2_1_1_R"] <- enq.final[i,"A17_SQ001_SQ001_R"]
        enq.final[i,"A2_1_2_R"] <- enq.final[i,"A17_SQ001_SQ002_R"]
      } else {
        rm (enq.final[i,])
      }
    }
  }
  }
}


# Repasse 24h en 00h

enq.final$A2_1_1_R [enq.final$A2_1_1_R == "24"] <- "00"
enq.final$A17_SQ001_SQ001_R [enq.final$A17_SQ001_SQ001_R == "24"] <- "00"
enq.final$A22_SQ001_SQ001_R [enq.final$A22_SQ001_SQ001_R == "24"] <- "00"

# Colle les morceaux (pour obtenir un format: hh h mm)

enq.final$heure_1 <- paste3(enq.final$A2_1_1_R, enq.final$A2_1_2_R, sep="h")
enq.final$heure_2 <- paste3(enq.final$A17_SQ001_SQ001_R, enq.final$A17_SQ001_SQ002_R, sep="h")
enq.final$heure_3 <- paste3(enq.final$A22_SQ001_SQ001_R, enq.final$A22_SQ001_SQ002_R, sep="h")

# Remise du bon nombre de prises alimentaires:

for(i in 1:nrow(enq.final)){
  if(is.na(enq.final[i,"heure_1"]) != T & is.na(enq.final[i,"heure_2"]) != T & is.na(enq.final[i,"heure_3"]) != T){
    enq.final[i, "A1"] <- "3 fois ou plus"
  } else {
    if(is.na(enq.final[i,"heure_1"]) != T & is.na(enq.final[i,"heure_2"]) != T & is.na(enq.final[i,"heure_3"]) == T){
      enq.final[i, "A1"] <- "2 fois"
    } else {
      if(is.na(enq.final[i,"heure_1"]) != T & is.na(enq.final[i,"heure_2"]) == T & is.na(enq.final[i,"heure_3"]) == T){
        enq.final[i, "A1"] <- "1 fois"
      }
    }
  }
}

