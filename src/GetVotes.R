require(RCurl)
require(rvest)
require(dplyr)
require(tidyr)

#GetRecord Function
DB <- function(Y){
  print(Y)
  #Get Candidate Info
  D0 <- read_html(paste("http://sitl.diputados.gob.mx/LXIII_leg/curricula.php?dipt=",Y,sep=""))
  D0 <- D0 %>% html_table(fill=T)
  D0 <- as.data.frame(D0[3])[-1,1:2]
  D0[1,1] <- "Nombre"
  D0 <- D0 %>% mutate(X1=gsub(pattern=":",replacement="",x=X1),X1=gsub(pattern=" ",replacement="",x=X1)) %>%
    spread(X1,X2)
  
  #Get Voting Record
  VotingPeriods <- paste("http://sitl.diputados.gob.mx/LXIII_leg/votaciones_por_pernplxiii.php?iddipt=",Y,"&pert=",c(1,3,5,6,7,9,11),sep="")
  VoteRecord <- function(X){
    T0 <- read_html(X)
    tryCatch({
      T0 <- html_table(x = T0,fill=T)
      T0 <- as.data.frame(T0[3])[-c(1:3),]
      T0 <- T0 %>% mutate(X1=ifelse(nchar(X1)<5,NA,X1)) %>% fill(X1) %>% select(-X3,-X5) %>%
        filter(X1!=X2) 
      names(T0) <- c("Fecha","Titulo","Sentido")
      F0 <- cbind(D0,T0)
      return(F0)
    },error=function(cond) {
      F0 <- data.frame(X1 =NA,X2 =NA,X3 =NA,X4 =NA,X5 =NA,X6=NA,X7 =NA,X8 =NA,X9 =NA)
      names(F0) <- c('Correoelectrónico','Entidad','Nombre','Onomástico','Suplente','Tipodeelección','Fecha','Titulo','Sentido')
      return(F0)
    })
  }
  F0 <- bind_rows(lapply(VotingPeriods,VoteRecord))
  return(F0)
}

#Get complete DB for all 500 deputies
Votes <- bind_rows(lapply(1:500,DB))

#Clean 
Votes <- Votes %>% filter(is.na(Nombre)!=T)

#Export
write.csv("data-out/votesdb.csv",row.names = F)


