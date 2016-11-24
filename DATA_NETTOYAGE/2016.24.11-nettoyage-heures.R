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

data$A2_1_1_R <- paste3(as.character(data$A2_1_1),as.character(data$A2_bis_1_1))
data$A2_1_2_R <- paste3(as.character(data$A2_1_2),as.character(data$A2_bis_1_2))

data$A2_1_1_R [data$A2_1_1_R == "18h"] <- "18"
data$A2_1_1_R [data$A2_1_1_R == "13H"] <- "13"
data$A2_1_1_R [data$A2_1_1_R %in% c("19.00","19h")] <- "19"
data$A2_1_2_R [data$A2_1_1_R == "19H00"] <- "00"
data$A2_1_2_R [data$A2_1_1_R %in% c("19h30","1930")] <- "30"
data$A2_1_1_R [data$A2_1_1_R %in% c("19H00","19h30","1930")] <- "19"
data$A2_1_1_R [data$A2_1_1_R == "20h"] <- "20"
data$A2_1_1_R [data$A2_1_1_R == "21h"] <- "21"
data$A2_1_2_R [data$A2_1_1_R == "12h30"] <- "30"
data$A2_1_1_R [data$A2_1_1_R %in% c("12h30","12h")] <- "12"
data$A2_1_2_R [data$A2_1_1_R == ".              8"] <- "8"


data$A2_1_2_R [data$A2_1_1_R == "19h00"] <- "19"
data$A2_1_2_R [data$A2_1_2_R == "19h00"] <- "00"
data$A2_1_2_R [data$A2_1_2_R == "0"] <- "00"
data$A2_1_2_R [data$A2_1_2_R == "40mn"] <- "40"
data$A2_1_2_R [data$A2_1_2_R == "5"] <- "05"
data$A2_1_2_R [data$A2_1_2_R == "8"] <- "08"
data$A2_1_2_R [data$A2_1_2_R == "9"] <- "09"

levels(as.factor(data$A2_1_1_R))
levels(as.factor(data$A2_1_2_R))

# loop flag: si la variable a ete marquee comme etant dans la bonne tanche horaire, mais codee en am/pm, la repasse en format 24h
# Si la variable a été codée comme étant dans la mauvaise tranche horaire (incompréhension de la question),
# tente de repasser la seconde prise déclarée en premiere prise (a débattre, dû au format: prise principale)
# Si la seconde prise est une non-réponse, l'individu est supprimé (a débattre)

for(i in 1:nrow(data)){
  if (data[i,17] %in% c("209","253")){
    a[i] <-  data[i,1]
  }
}

for(i in 1:nrow(data)){
  if(data[i,"flag_n"]==1){
    data[i,"A2_1_1_R"] <- as.character(as.numeric(data[i,"A2_1_1_R"]) + 12)
  } else {
    if (data[i,"flag_n"]==0){
      if (is.na(data[i,"A2_2_1_R"]) != T){
        data[i,"A2_1_1_R"] <- data[i,"A17_SQ001_SQ001_R"]
        data[i,"A2_1_2_R"] <- data[i,"A17_SQ001_SQ002_R"]
      } else {
        rm (data[i,])
      }
    }
  }
}

# Repasse 24h en 00h

data$A2_1_1_R [data$A2_1_1_R == "24"] <- "00"

# Colle les morceaux (pour obtenir un format: hh h mm)

data$heure_1 <- paste3(data$A2_1_1_R, data$A2_1_2_R, sep="h")


## Recodage des niveaux pour la seconde tranche horaire:

data$A17_SQ001_SQ001_R <- paste3(as.character(data$A17_SQ001_SQ001),as.character(data$A17_bis_SQ001_SQ001))
data$A17_SQ001_SQ002_R <- paste3(as.character(data$A17_SQ001_SQ002),as.character(data$A17_bis_SQ001_SQ002))

data$A17_SQ001_SQ001_R [data$A17_SQ001_SQ001_R == "19h"] <- "19"
data$A17_SQ001_SQ001_R [data$A17_SQ001_SQ001_R == "20h"] <- "20"
data$A17_SQ001_SQ001_R [data$A17_SQ001_SQ001_R == "0"] <- "00"
data$A17_SQ001_SQ001_R [data$A17_SQ001_SQ001_R == "21h"] <- "21"
data$A17_SQ001_SQ001_R [data$A17_SQ001_SQ001_R == "22h"] <- "22"

data$A17_SQ001_SQ002_R [data$A17_SQ001_SQ002_R == "0"] <- "00"
data$A17_SQ001_SQ002_R [data$A17_SQ001_SQ002_R == "3O"] <- "30"

levels(as.factor(data$A17_SQ001_SQ001_R))
levels(as.factor(data$A17_SQ001_SQ002_R))

# Repasse 24h en 00h

data$A17_SQ001_SQ001_R [data$A17_SQ001_SQ001_R == "24"] <- "00"

# Colle les morceaux (pour obtenir un format: hh h mm)

data$heure_2 <- paste3(data$A17_SQ001_SQ001_R, data$A17_SQ001_SQ002_R, sep="h")

## Recodage des niveaux pour la troisieme tranche horaire

data$A22_SQ001_SQ001_R <- as.character(data$A22_SQ001_SQ001)
data$A22_SQ001_SQ002_R <- as.character(data$A22_SQ001_SQ002)

data$A22_SQ001_SQ001_R [data$A22_SQ001_SQ001_R == "0"] <- "00"
data$A22_SQ001_SQ001_R [data$A22_SQ001_SQ001_R == "20h"] <- "20"
data$A22_SQ001_SQ001_R [data$A22_SQ001_SQ001_R == "22H"] <- "22"
data$A22_SQ001_SQ001_R [data$A22_SQ001_SQ001_R == "23h"] <- "23"

data$A22_SQ001_SQ002_R [data$A22_SQ001_SQ002_R == "0"] <- "00"
data$A22_SQ001_SQ002_R [data$A22_SQ001_SQ002_R == "5"] <- "05"

levels(as.factor(data$A22_SQ001_SQ001_R))
levels(as.factor(data$A22_SQ001_SQ002_R))

# Repasse 24h en 00h

data$A22_SQ001_SQ001_R [data$A22_SQ001_SQ001_R == "24"] <- "00"

# Colle les morceaux (pour obtenir un format: hh h mm)

data$heure_3 <- paste3(data$A22_SQ001_SQ001_R, data$A22_SQ001_SQ002_R, sep="h")