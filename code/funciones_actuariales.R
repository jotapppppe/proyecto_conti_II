anualidad_discreta <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- 1 + sum(unlist(sapply(1:(n-1), function(k) descuento(interes, inflacion, k) * tpxM(x, k, i, j, s))))
  return (suma)
}

anualidad_discreta_nocero <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- descuento(interes, inflacion, 1) * tpxM(x, 1, 0, i, s) + sum(unlist(sapply(2:(n-1), function(k) descuento(interes, inflacion, k) * tpxM(x + 1, k, i, j, s))))
  return (suma)
}


anualidad_discreta_post <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- sum(unlist(sapply(1:(n), function(k) descuento(interes, inflacion, k) * tpxM(x, k, i, j, s))))
  return (suma)
}

anualidad_discreta_permanencia <- function(x, n, i, j, inflacion, s){
  ifelse(s == "M", s <- lista_tablas_masc, s <- lista_tablas_fem)
  suma <- 1 + sum(unlist(sapply(1:(n-1), function(k) descuento(interes, inflacion, k) * tpxM_permanencia(x, k, i, j, s))))
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

################################################

prima <- function(x, inflacion, s){
  n <- 80 - x
  m <- 65 - x
  res <- (30000000 * seguro(x, n, 0, 3, inflacion, s) + 20000000*seguro(x, n, 1, 3, inflacion, s) + 10000000*seguro(x, n, 2, 3, inflacion, s) 
          + 2500000 * anualidad_discreta_permanencia(x, n, 2, 2, inflacion, s) + 400000 * (seguro(x, n, 0, 3, inflacion, s) + seguro(x, n, 1, 3, inflacion, s) 
          + seguro(x, n, 2, 3, inflacion, s))) / (anualidad_discreta(x, m, 0, 0, inflacion, s) + 0.75 * anualidad_discreta(x, m, 1, 1, inflacion, s) 
                                                                                               + 0.25 * anualidad_discreta(x, m, 2, 2, inflacion, s) - 0.05
                                                                                               - 0.05 * anualidad_discreta_post(x, n, 0, 0, inflacion, s))
  return (res)
}


prima_riesgo <- function(x, inflacion, s){
  n <- 80 - x
  m <- 65 - x
  res <- (30000000 * seguro(x, n, 0, 3, inflacion, s) + 20000000*seguro(x, n, 1, 3, inflacion, s) + 10000000*seguro(x, n, 2, 3, inflacion, s) 
          + 2500000 * anualidad_discreta_permanencia(x, n, 2, 2, inflacion, s)) / (anualidad_discreta(x, m, 0, 0, inflacion, s) + 0.75 * anualidad_discreta(x, m, 1, 1, inflacion, s) 
                                                                                                                                   + 0.25 * anualidad_discreta(x, m, 2, 2, inflacion, s))
  return (res)
}
