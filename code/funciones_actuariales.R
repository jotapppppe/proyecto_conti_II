anualidad_discreta <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- 1 + sum(unlist(sapply(1:(n-1), function(k) descuento(interes, inflacion, k) * tpxM(x, k, i, j, s))))
  return (suma)
}

anualidad_discreta_post <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- sum(unlist(sapply(1:(n), function(k) descuento(interes, inflacion, k) * tpxM(x, k, i, j, s))))
  return (suma)
}

