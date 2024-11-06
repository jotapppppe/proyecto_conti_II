# Función para calcular v

descuento <- function(i, j, n) 1/((1-i)*(1-j))^n

########################################################

#Función para calcular el tpx

tpx <- function(x, t, m, n) {
  
  # Caso base: si t = 1, devolvemos la probabilidad directa de transitar desde m a n con edad x
  if (t == 1) {
    return(tablas_estados[[m + 1]][x - 19, n + 2])
  }
  
  # Caso recursivo: sumamos las probabilidades de transitar de m a un estado intermedio (k) en un paso,
  # luego de k a n en t-1 pasos, incrementando la edad en cada paso.
  probabilidad_total <- 0
  
  # Iterar sobre todos los estados intermedios (k = 0, 1, 2, 3)
  for (k in 0:3) {
    # Probabilidad de ir de m a k en el primer paso, con edad x
    prob_m_a_k <- tablas_estados[[m + 1]][x - 19, k + 2]
    
    # Llamada recursiva para calcular la probabilidad de ir de k a n en t-1 pasos, con edad incrementada en 1
    prob_k_a_n <- tpx(x + 1, t - 1, k, n)
    
    # Sumamos la probabilidad ponderada a la probabilidad total
    probabilidad_total <- probabilidad_total + (prob_m_a_k * prob_k_a_n)
  }
  
  return(probabilidad_total)
}

###########################################################

# tpx con matrices
tpxM <- function(x, t, i, j){
  prob <- 0
  if(x<30){
    if(t<=9){
      prob <- (lista_tablas[[i + 1]][1, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (lista_tablas[[i + 1]][1, j + 1])^(9) * (lista_tablas[[i + 1]][2, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (lista_tablas[[i + 1]][1, j + 1])^(9) * (lista_tablas[[i + 1]][2, j + 1])^(9) *(lista_tablas[[i + 1]][3, j + 1])^(t-20)
    }
    else if (t<=39){
      prob <- (lista_tablas[[i + 1]][1, j + 1])^(9) * (lista_tablas[[i + 1]][2, j + 1])^(9) *(lista_tablas[[i + 1]][3, j + 1])^(9) *(lista_tablas[[i + 1]][4, j + 1])^(t-30)
    }
    else if (t<=49){
      prob <- (lista_tablas[[i + 1]][1, j + 1])^(9) * (lista_tablas[[i + 1]][2, j + 1])^(9) *(lista_tablas[[i + 1]][3, j + 1])^(9) *(lista_tablas[[i + 1]][4, j + 1])^(9) *(lista_tablas[[i + 1]][5, j + 1])^(t-40)
    }
    else if (t<=59){
      prob <- (lista_tablas[[i + 1]][1, j + 1])^(9) * (lista_tablas[[i + 1]][2, j + 1])^(9) *(lista_tablas[[i + 1]][3, j + 1])^(9) *(lista_tablas[[i + 1]][4, j + 1])^(9) *(lista_tablas[[i + 1]][5, j + 1])^(9) *(lista_tablas[[i + 1]][6, j + 1])^(t-50)
    }
  }
  else if(x<40){
    if(t<=9){
      prob <- (lista_tablas[[i + 1]][2, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (lista_tablas[[i + 1]][2, j + 1])^(9) * (lista_tablas[[i + 1]][3, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (lista_tablas[[i + 1]][2, j + 1])^(9) * (lista_tablas[[i + 1]][3, j + 1])^(9) *(lista_tablas[[i + 1]][4, j + 1])^(t-20)
    }
    else if (t<=39){
      prob <- (lista_tablas[[i + 1]][2, j + 1])^(9) * (lista_tablas[[i + 1]][3, j + 1])^(9) *(lista_tablas[[i + 1]][4, j + 1])^(9) *(lista_tablas[[i + 1]][5, j + 1])^(t-30)
    }
    else if (t<=49){
      prob <- (lista_tablas[[i + 1]][2, j + 1])^(9) * (lista_tablas[[i + 1]][3, j + 1])^(9) *(lista_tablas[[i + 1]][4, j + 1])^(9) *(lista_tablas[[i + 1]][5, j + 1])^(9) *(lista_tablas[[i + 1]][6, j + 1])^(t-40)
    }
  }
  else if(x<50){
    if(t<=9){
      prob <- (lista_tablas[[i + 1]][3, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (lista_tablas[[i + 1]][3, j + 1])^(9) * (lista_tablas[[i + 1]][4, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (lista_tablas[[i + 1]][3, j + 1])^(9) * (lista_tablas[[i + 1]][4, j + 1])^(9) *(lista_tablas[[i + 1]][5, j + 1])^(t-20)
    }
    else if (t<=39){
      prob <- (lista_tablas[[i + 1]][3, j + 1])^(9) * (lista_tablas[[i + 1]][4, j + 1])^(9) *(lista_tablas[[i + 1]][5, j + 1])^(9) *(lista_tablas[[i + 1]][6, j + 1])^(t-30)
    }
  }
  else if(x<60){
    if(t<=9){
      prob <- (lista_tablas[[i + 1]][4, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (lista_tablas[[i + 1]][4, j + 1])^(9) * (lista_tablas[[i + 1]][5, j + 1])^(t-10)
    }
    else if (t<=29){
      prob <- (lista_tablas[[i + 1]][4, j + 1])^(9) * (lista_tablas[[i + 1]][5, j + 1])^(9) *(lista_tablas[[i + 1]][6, j + 1])^(t-20)
    }
  }
  else if(x<70){
    if(t<=9){
      prob <- (lista_tablas[[i + 1]][5, j + 1])^(t)
    }
    else if(t<=19){
      prob <- (lista_tablas[[i + 1]][5, j + 1])^(9) * (lista_tablas[[i + 1]][6, j + 1])^(t-10)
    }
  }
  else if(x<80){
    if(t<=9){
      prob <- (lista_tablas[[i + 1]][6, j + 1])^(t)
    }
 
  }
  return (prob)
}