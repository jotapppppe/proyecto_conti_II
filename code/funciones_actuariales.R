anualidad_discreta <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- sum(unlist(sapply(1:(n-1), function(k) descuento(interes, inflacion, k) * tpxM(x, k, i, j, s))))
  return (suma)
}

#seguro_discreto <- 