# Función para calcular v

descuento <- function(i, j, n) 1/((1+i)%*% (1+j)) %^% n

########################################################

#Función para calcular el tpx con matrices

tpxM <- function(x, t, i, j, tabla){
  prob <- 0
  mat <- diag(4) 
  for (k in 1:t){
    mat <- mat %*% as.matrix(tabla[[as.integer(substr(as.character(x+k-1), 1, 1)) - 1]])
  }
  prob <- mat[i + 1, j + 1]
  return (prob)
}

tpxM_permanencia <- function(x, t, i, j, tabla){
  prob <- 0
  mat <- diag(4) 
  for (k in 1:t){
    mat <- mat * tabla[[as.integer(substr(as.character(x+k-1), 1, 1)) - 1]]
  }
  prob <- mat[i + 1, j + 1]
  return (prob)
}