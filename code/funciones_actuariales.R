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
  suma <- descuento(interes, inflacion, 1) * tpxM(x, 1, 0, i, s) + sum(unlist(sapply(2:(n), function(k) descuento(interes, inflacion, k) * tpxM(x + 1, k, i, j, s))))
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
  res <- (5e6 * seguro(x, n, 0, 3, inflacion, s) + 4e6*seguro(x, n, 1, 3, inflacion, s) + 3e6*seguro(x, n, 2, 3, inflacion, s) 
          + 25e4 * anualidad_discreta(x, n, 2, 2, inflacion, s) + 4e3 * (seguro(x, n, 0, 3, inflacion, s) + seguro(x, n, 0, 3, inflacion, s) 
          + seguro(x, n, 2, 3, inflacion, s))) / (anualidad_discreta(x, m, 0, 0, inflacion, s) 
                                                                                            + 0.75 * anualidad_discreta(x, m, 0, 1, inflacion, s)) 
                                                                                            + 0.25 * anualidad_discreta(x, m, 0, 2, inflacion, s) - 0.05
                                                                                            - 0.05 * anualidad_discreta_post(x, n, 0, 0, inflacion, s)
  return (res)
}