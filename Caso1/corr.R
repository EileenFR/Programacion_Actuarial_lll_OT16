setwd("C:/Users/EILEEN/Desktop/Programación Actuarial lll/Programacion_Actuarial_lll_OT16/Caso1/specdata")


corr <- function(directorio,horizonte=0){
  
  id2 <- vector("character")
  result <- vector("numeric")
  x2 <- vector("numeric")
  x3 <- data.frame()
  
  
  for (i in 1:332){  
  
    if (i>=1 && i<10){ id2[i] <- paste("00",i,".csv", sep="") } 
    else { if (i>=10 && i<100){ id2[i] <- paste("0",i,".csv", sep="") } 
      else { id2[i] <- paste(i,".csv", sep="") } }
    
    
    y <- read.csv(id2[i])
    z <- cbind(y[,2:3])
    
    todos <- complete.cases(z)
    x <- z[todos,]
    x1 <- nrow(x)
    
    if (x1>=horizonte){ x3 <- cor(x) } 
    x2 <- x3[1,2]
    result <- c(result,x2)
    
  }  
  result
}


corr("specdata",150)
