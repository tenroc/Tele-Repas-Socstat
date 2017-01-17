
rm(list=ls())
setwd(chemin)

e<- read.csv2("./DATA_BDD-RECONCILIE/enq final_03.csv",encoding="UTF-8")

#
round(prop.table(table(e[,"age_calage"])),2)
round(prop.table(table(e[,"D13_re"])),2)
round(prop.table(table(e[,"D5_re"])),2)

round(prop.table(table(e[,"age_calage"],e[,"D13_re"])),2)
#POIDS DE CALAGE "à la main"
e$weight<-1
e$weight<-ifelse(e$D13_re=="Homme"&e$age_calage=="18-24",1.7,
          ifelse(e$D13_re=="Homme"&e$age_calage=="25-39",0.7,
          ifelse(e$D13_re=="Homme"&e$age_calage=="40-54",1.3,
          ifelse(e$D13_re=="Homme"&e$age_calage=="55-64",1.0,
          ifelse(e$D13_re=="Homme"&e$age_calage=="65-79",2.6,       
          ifelse(e$D13_re=="Homme"&e$age_calage=="80 et plus",2.7,                 
          ifelse(e$D13_re=="Femme"&e$age_calage=="18-24",1.7,  
          ifelse(e$D13_re=="Femme"&e$age_calage=="25-39",0.9,    
          ifelse(e$D13_re=="Femme"&e$age_calage=="40-54",0.6,
          ifelse(e$D13_re=="Femme"&e$age_calage=="55-64",0.6,  
          ifelse(e$D13_re=="Femme"&e$age_calage=="65-79",1.8,    
          ifelse(e$D13_re=="Femme"&e$age_calage=="80 et plus",2.5,"NA"
          ))))))))))))
                 
table(e$weight)
write.csv2(e,"./DATA_BDD-RECONCILIE/enq final_04.csv",fileEncoding="UTF-8")
e2<- read.csv2("./DATA_BDD-RECONCILIE/enq final_04.csv",encoding="UTF-8")
