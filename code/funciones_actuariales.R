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

seguro <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- sum(unlist(sapply(1:(n), function(k) descuento(interes, inflacion, k) * tpxM(x, k, i, j, s))))
  return (suma)
}

seguro_permanencia <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- sum(unlist(sapply(1:(n), function(k) descuento(interes, inflacion, k) * tpxM_permanencia(x, k, i, j, s))))
  return (suma)
}


prima <- function(x, inflacion, s){
  n <- 80 - x
  m <- 65 - x
  res <- (5e7 * seguro(x, n, 0, 3, inflacion, s) + 4e7*seguro(x, n, 1, 3, inflacion, s) + 3e7*seguro(x, n, 2, 3, inflacion, s) 
          + 25e5 * anualidad_discreta(x, n, 2, 2, inflacion, s) + 4e4 * (seguro(x, n, 0, 3, inflacion, s) + seguro(x, n, 0, 3, inflacion, s) 
          + seguro(x, n, 2, 3, inflacion, s))) / (anualidad_discreta(x, m, 0, 0, inflacion, s) 
                                                                                            + 0.75 * anualidad_discreta(x, m, 0, 1, inflacion, s)) 
                                                                                            + 0.25 * anualidad_discreta(x, m, 0, 2, inflacion, s) - 0.05
                                                                                            - 0.05 * anualidad_discreta_post(x, n, 0, 0, inflacion, s)
  return (res)
}