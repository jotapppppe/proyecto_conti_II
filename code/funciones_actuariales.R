anualidad_discreta <- function(x, n, i, j, inflacion){
  suma <- 0
  for (k in 1:(n-1)){
    suma <- suma + (descuento(interes, inflacion, k) * tpx(x, k, i, j))
  }
  return (suma)
}

#seguro_discreto <- 