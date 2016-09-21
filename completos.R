setwd("C:/Users/EILEEN/Desktop/Programación Actuarial lll/Programacion_Actuarial_lll_OT16/specdata")


completos <- function(directorio,id=1:332){ 
  
  id2 <- vector("character")
  data <- data.frame(id="",nobs="")
  cont <- 0
  x2 <- ""
  x3 <- ""
  for (i in id){
    
    if (i>=1 && i<10){ id2[i] <- paste("00",i,".csv", sep="") } 
    else { if (i>=10 && i<100){ id2[i] <- paste("0",i,".csv", sep="") } 
      else { id2[i] <- paste(i,".csv", sep="") } }
    
    y <- read.csv(id2[i])
    z <- cbind(y[,2:3])
    
    todos <- complete.cases(z)
    x <- z[todos,]
    x1 <- nrow(x)
    
    if (cont==0){ 
      x2 <- i
      x3 <- x1
      }
    else { if (cont>0){
    x2 <- c(a,i)
    x3 <- c(b,x1)
    }}
    cont <- cont+1
    a <- x2
    b <- x3
   } 
    
  data <- data.frame(id=x2,nobs=x3)
  data
  
}  

completos("specdata", 1:15)