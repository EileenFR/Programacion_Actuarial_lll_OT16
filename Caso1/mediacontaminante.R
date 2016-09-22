setwd("C:/Users/EILEEN/Desktop/Programación Actuarial lll/Programacion_Actuarial_lll_OT16/Caso1/specdata")

mediacontaminante <- function(directorio,contaminante,id=1:332){   
  
  id2 <- vector("character")
  x2 <- vector("numeric")
  
  for (i in id){ 
    
    if (i>=1 && i<10){ id2[i] <- paste("00",i,".csv", sep="") } 
    else { if (i>=10 && i<100){ id2[i] <- paste("0",i,".csv", sep="") } 
      else { id2[i] <- paste(i,".csv", sep="") } }
    
    y <- read.csv(id2[i])
    z <- cbind(y[,contaminante])
    
    x2 <- rbind(x2,z)
    
    medias <- mean(x2,na.rm=TRUE)
  }
  medias

}

  
mediacontaminante("specdata","sulfate",70:72)   



