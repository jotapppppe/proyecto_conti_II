# Función para calcular v

descuento <- function(i, j, n) 1/((1-i)*(1-j))^n

########################################################

#Función para calcular el tpx con matrices

tpxM <- function(x, t, i, j, tabla){
  prob <- 0
  if(x<30){
    if(t<=9){
      prob <- (tabla[[i + 1]][1, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (tabla[[i + 1]][1, j + 1])^(9) * (tabla[[i + 1]][2, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (tabla[[i + 1]][1, j + 1])^(9) * (tabla[[i + 1]][2, j + 1])^(9) *(tabla[[i + 1]][3, j + 1])^(t-20)
    }
    else if (t<=39){
      prob <- (tabla[[i + 1]][1, j + 1])^(9) * (tabla[[i + 1]][2, j + 1])^(9) *(tabla[[i + 1]][3, j + 1])^(9) *(tabla[[i + 1]][4, j + 1])^(t-30)
    }
    else if (t<=49){
      prob <- (tabla[[i + 1]][1, j + 1])^(9) * (tabla[[i + 1]][2, j + 1])^(9) *(tabla[[i + 1]][3, j + 1])^(9) *(tabla[[i + 1]][4, j + 1])^(9) *(tabla[[i + 1]][5, j + 1])^(t-40)
    }
    else if (t<=59){
      prob <- (tabla[[i + 1]][1, j + 1])^(9) * (tabla[[i + 1]][2, j + 1])^(9) *(tabla[[i + 1]][3, j + 1])^(9) *(tabla[[i + 1]][4, j + 1])^(9) *(tabla[[i + 1]][5, j + 1])^(9) *(tabla[[i + 1]][6, j + 1])^(t-50)
    }
  }
  else if(x<40){
    if(t<=9){
      prob <- (tabla[[i + 1]][2, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (tabla[[i + 1]][2, j + 1])^(9) * (tabla[[i + 1]][3, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (tabla[[i + 1]][2, j + 1])^(9) * (tabla[[i + 1]][3, j + 1])^(9) *(tabla[[i + 1]][4, j + 1])^(t-20)
    }
    else if (t<=39){
      prob <- (tabla[[i + 1]][2, j + 1])^(9) * (tabla[[i + 1]][3, j + 1])^(9) *(tabla[[i + 1]][4, j + 1])^(9) *(tabla[[i + 1]][5, j + 1])^(t-30)
    }
    else if (t<=49){
      prob <- (tabla[[i + 1]][2, j + 1])^(9) * (tabla[[i + 1]][3, j + 1])^(9) *(tabla[[i + 1]][4, j + 1])^(9) *(tabla[[i + 1]][5, j + 1])^(9) *(tabla[[i + 1]][6, j + 1])^(t-40)
    }
  }
  else if(x<50){
    if(t<=9){
      prob <- (tabla[[i + 1]][3, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (tabla[[i + 1]][3, j + 1])^(9) * (tabla[[i + 1]][4, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (tabla[[i + 1]][3, j + 1])^(9) * (tabla[[i + 1]][4, j + 1])^(9) *(tabla[[i + 1]][5, j + 1])^(t-20)
    }
    else if (t<=39){
      prob <- (tabla[[i + 1]][3, j + 1])^(9) * (tabla[[i + 1]][4, j + 1])^(9) *(tabla[[i + 1]][5, j + 1])^(9) *(tabla[[i + 1]][6, j + 1])^(t-30)
    }
  }
  else if(x<60){
    if(t<=9){
      prob <- (tabla[[i + 1]][4, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (tabla[[i + 1]][4, j + 1])^(9) * (tabla[[i + 1]][5, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (tabla[[i + 1]][4, j + 1])^(9) * (tabla[[i + 1]][5, j + 1])^(9) *(tabla[[i + 1]][6, j + 1])^(t-20)
    }
  }
  else if(x<70){
    if(t<=9){
      prob <- (tabla[[i + 1]][5, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (tabla[[i + 1]][5, j + 1])^(9) * (tabla[[i + 1]][6, j + 1])^(t-10)
    }
  }
  else if(x<80){
    if(t<=9){
      prob <- (tabla[[i + 1]][6, j + 1])^(t)
    }
 
  }
  return (prob)
}