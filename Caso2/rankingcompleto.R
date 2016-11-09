setwd("C:/Users/EILEEN/Desktop/Programación Actuarial lll/Programacion_Actuarial_lll_OT16/Caso2")

#Punto 1: Gráfica
outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
ncol(outcome)   #46
names(outcome)

outcome[,11] <- as.numeric(outcome[,11])
hist(outcome[,11])


#Punto 2: Encontrar el mejor hospital en un Estado

mejor <- function(estado,resultado){  
  estado <- as.character(estado)
  resultado <- as.character(resultado)
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  x <- outcome[,7]
  x1 <- factor(x)
  x2 <- attr(x1,"levels")
  x3 <- x2[x2 == estado]
  if(length(x3) == 0){ stop("Estado inválido") }
  
  y <- c("neumonia", "falla", "ataque")
  y1 <- y[y == resultado]
  if(length(y1) == 0){ stop("Resultado inválido") }
  else {  
    if(resultado == "ataque"){ t <- 11 }
    else{ 
      if(resultado == "falla"){ t <- 17 }
      else{ 
        if(resultado == "neumonia"){
          t <- 23
        }
      }
    }
  }
  
  
  z <- split(outcome, outcome$State)
  z1 <- z[[estado]]
  s <- data.frame()
  s <- cbind(z1[,2],z1[,t])
  l <- nrow(s)
  cont<- 1
  d <- which(s[,2] != "Not Available")
  ss<-as.numeric(s[d,2])
  r <- ss[1]
  for(i in 1:length(s[d])){
    if(ss[i] < r){
      r<-ss[i]
    }
  }
  res <- which(as.numeric(s[d,2])==r)
  fn <- s[d,1]
  fn1<-order(fn[res])
  fn2<-fn[res]
  fn2[fn1[1]]
  
}

#Punto 3: Jeraquía de Hospitales por resultado de un Estado

rankhospital <- function(estado, resultado, num = "mejor")  {
 
  estado <- as.character(estado)
  resultado <- as.character(resultado)
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  x <- outcome[,7]
  x1 <- factor(x)
  x2 <- attr(x1,"levels")
  x3 <- x2[x2 == estado]
  if(length(x3) == 0){ stop("Estado inválido") }
  
  y <- c("neumonia", "falla", "ataque")
  y1 <- y[y == resultado]
  if(length(y1) == 0){ stop("Resultado inválido") }
  else {  
    if(resultado == "ataque"){ t <- 11 }
    else{ 
      if(resultado == "falla"){ t <- 17 }
      else{ t <- 23 }
    }
  }

  z <- split(outcome, outcome$State)
  z1 <- z[[estado]]
  s <- data.frame()
  s <- cbind(z1[,2],z1[,t])
  d <- which(s[,2] != "Not Available" )
 
  if(num == "mejor") { num <- 1 }
  if(num == "peor") { num <- length(s[d])}
  else if (num > length(s[d])) { return("NA")}
  #if(is.integer(num)==F ){stop("Solo enteros positivos, mejor y peor")}

  ss<-order(as.numeric(s[d,2]))
  fn <- s[d,1]
  fnt <- s[d,2]
  fns <- fn[ss]
  fn2<-fnt[ss]
  i <- 0
  while (fn2[i+1] != fn2[num]){
    i <- i + 1
  }
  res <- which(fn2==fn2[num])
  fn1<-order(fns[res])
  fns[res[fn1[num-i]]]
  
}
########
# funcion 4
rankingcompleto <- function(resultado, num = "mejor")  {
  
  
  resultado <- as.character(resultado)
  
  outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  
  
  y <- c("neumonia", "falla", "ataque")
  y1 <- y[y == resultado]
  if(length(y1) == 0){ stop("Resultado inválido") }
  else {  
    if(resultado == "ataque"){ t <- 11 }
    else{ 
      if(resultado == "falla"){ t <- 17 }
      else{ t <- 23 }
    }
  }
  
  s <- data.frame()
  s <- cbind(outcome[,2],outcome[,7],outcome[,t])
  
  x <- s[,2]
  x1 <- factor(x)
  x2 <- attr(x1,"levels")
  
  sol <- data.frame(matrix(nrow = length(x2), ncol = 2))
  
 for (i in 1:length(x2)){ 
  dd <- which(s[,2]==x2[i] & s[,3] != "Not Available")
  if(num == "mejor") { num <- 1 }
  if(num == "peor") { num <- length(s[dd])}
  #if(is.integer(num)==F ){stop("Solo enteros positivos, mejor y peor")}
  if(num > length(s[dd])) { 
    sol[i,1] <- "NA"
    sol[i,2] <- x2[i]
  }
  else {
  
  fnt <- s[dd,3]
  fn <- s[dd,1]
  ss<-order(as.numeric(fnt))
  fns <- fn[ss]
  fn2<-fnt[ss]
  res <-  which(fn2==fn2[ss[num]])
 
  if (length(res)!=0){
    
    j <- 0
    while (fn2[j+1] != fn2[num]){
      j <- j + 1
    }
    res <- which(fn2==fn2[num])
    fn1<-order(fns[res])
    
    sol[i,1] <- fns[res[fn1[num-j]]]
    sol[i,2] <- x2[i]
  }
  else{
    sol[i,1] <- "NA"
    sol[i,2] <- x2[i]
  }
  
  }
 }
  
   rownames(sol) <- x2
   colnames(sol) <- c("hospital","state")
   format( sol[,1],justify="left")
   sol
}


