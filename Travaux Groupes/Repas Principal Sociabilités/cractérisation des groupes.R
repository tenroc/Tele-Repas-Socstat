rm(list=ls())
setwd(chemin)
enq.final <- read.csv2("./DATA_BDD-RECONCILIE/enq final_08.csv", header=T)
table(enq.final$clust5, useNA = "ifany")

#FONCTION de tableau croisés
tab.crois <-function(colonne,ligne,enligne=T,chi=F){
  tab.f<-table(ligne, colonne)
  tab.m<-addmargins(tab.f)
  if(enligne==T){ 
    tab.t <- prop.table(tab.m[-nrow(tab.m),-ncol(tab.m)],1)
    tab.t <- round(tab.t,4)
    tab.t <- tab.t*100
    tab.e <- round(prop.table(table(colonne)),4)*100
    tab<-rbind(tab.t,Total=tab.e)
    tab<-addmargins(tab,2)
    tab[,ncol(tab)]<-round(tab[,ncol(tab)],0)
    colnames(tab)[ncol(tab)]<-"Total"
    tab <- cbind(tab,"Effectifs"=round(tab.m[,ncol(tab.m)],0))
  }else{
    tab.t <- prop.table(tab.m[-nrow(tab.m),-ncol(tab.m)],2)
    tab.t <- round(tab.t,4)
    tab.t <- tab.t*100
    tab.e <- round(prop.table(table(ligne)),4)*100
    tab<-cbind(tab.t,Total=tab.e)
    tab<-addmargins(tab,1)
    tab[nrow(tab),]<-round(tab[nrow(tab),],0)
    rownames(tab)[nrow(tab)]<-"Total"
    tab <- rbind(tab,"Effectifs"=round(tab.m[nrow(tab.m),],0))
  }
  nom<-paste("tableau croise")
  res<-list()
  if(chi==F)  {
    res <- list(nom,tab)
  }
  else{
    chi.tab <-unlist(chisq.test(tab.f)[c(1,3)])
    res<- list(nom,tab,"chi-deux"=chi.tab)
    return(res)}
}




library(questionr)
t<- tab.crois(enq.final$D13_re,enq.final$clust5,chi=T)
t
copie(t)

t<- tab.crois(enq.final$age_calage,enq.final$clust5, chi=T)
t
copie(t)

t<- tab.crois(enq.final$HA3_re,enq.final$clust5)
t
copie(t)

t<- tab.crois(enq.final$A13_2_re,enq.final$clust5)
t
copie(t)

t<- tab.crois(enq.final$dummy.panel,enq.final$clust5)
t
copie(t)




