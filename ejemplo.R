
.initial_time <- Sys.time() # Cronometrar corrida
# delete all the environment variables
rm(list = ls())
set.seed(123) # Fijar semilla

# Ejemplo ----------------------------------------------------------------
pacman::p_load(readxl, dplyr, stats, forecast, parallel)

# Leer y listar flujos ---------------------------------------------------------------------

bonds <-
  read_excel(
    path = file.path(getwd(), "data.xlsx"),
    sheet = "Datos semanales"
  ) %>% as.data.frame()

rownames(bonds) <- bonds[, 1]
# Eliminar la primera columna del dataframe
bonds <- bonds[, -1]

colnames(bonds) <- c(.25, .5, 1, 2, 3, 5, 7, 10)

last_curve <- tail(bonds, 1)


get_flows <- function(last_curve, bond) {
  semesters <- max(1, 2 * (colnames(last_curve)[bond] %>% as.numeric()))
  rate <- last_curve[bond]
  cupon_semesters <- rep(rate * 100 * 182.5 / 360, semesters) %>% unlist()
  names(cupon_semesters) <- seq(from = 1, to = length(cupon_semesters)) / 2
  cupon_semesters[length(cupon_semesters)] <- tail(cupon_semesters, 1) + 100
  if (colnames(last_curve)[bond] %>% as.numeric() == 0.25) {
    names(cupon_semesters) <- 0.25
    cupon_semesters <- rate * 100 * 91.25 / 360 + 100
  }
  cupon_semesters
}

flows <- sapply(c(1:8), function(x) get_flows(last_curve, x))
names(flows) <- paste("Bono de", colnames(bonds), "años")
# Quitar el elemento 1: 3 meses
flows <-  flows[-1]

# Punto 4 ---------------------------------------------------------------------
diff_bonds <- apply(bonds, 2, diff) # Serie diferenciada
pca_result <- prcomp(diff_bonds, scale. = FALSE, center = FALSE)

# Punto 5 -----------------------------------------------------------------

var_explicada <- pca_result$sdev^2 / sum(pca_result$sdev^2)

# Encontrar el número de componentes que explican más del 90% de la varianza
var_acumulada <- cumsum(var_explicada)
n_componentes <- which.max(var_acumulada > 0.99)

# Seleccionar los primeros n componentes principales
pca_scores <- pca_result$x[, 1:n_componentes]

# Punto 6 -----------------------------------------------------------------

# Ajustar un modelo ARIMA a cada componente principal
arima_models <- lapply(1:n_componentes, function(i) {
  auto.arima(pca_scores[, i])
})

# Simulo
n_semanas <- 4 # Un mes
n_simulaciones <- 10000

# Simular sendas futuras para cada componente principal
simulaciones <- lapply(arima_models, function(model) {
  matrix(replicate(n_simulaciones, simulate(model, nsim = n_semanas)), ncol = n_simulaciones)
})

# Espacio original
# Preparar un array para almacenar los resultados
resultados <- array(dim = c(n_semanas, ncol(bonds), n_simulaciones))
rotation_matrix <- pca_result$rotation[, 1:n_componentes]

# Transformar las simulaciones de vuelta al espacio original
for (i in 1:n_simulaciones) {
  # Combinar las simulaciones de todos los componentes en una matriz
  sim_matrix <- sapply(simulaciones, function(x) x[, i])

  # Transformar de vuelta al espacio original usando la matriz de carga
  resultados_original_space <- rotation_matrix %*% t(sim_matrix)

  # Agregar los resultados al array
  resultados[, , i] <- t(resultados_original_space)
}

resultados <- apply(resultados, c(2, 3), cumsum)

tasas_simuladas <- sweep(
  t(resultados[n_semanas, , ]), # El último
  2,
  as.numeric(tail(bonds, 1)), # El último elemento
  "+"
)

# Interpolar -----------------------------------------------------------------
years <- colnames(bonds) %>% as.numeric() 
months <- years * 12
desired_months <- seq(1, max(months), by = 1)

# function that takes a vector, find the first non NA and return the vector with the NAs replaced by the first non NA
fill_na <- function(x) {
  first_non_na <- which(!is.na(x))[1]
  x[is.na(x)] <- x[first_non_na]
  x
}

curve_0 <- approx(x = months, y = tail(bonds, 1), xout = desired_months)$y %>% fill_na()

curves_1 <- apply(tasas_simuladas, 1, function(x)
 approx(x = months, y = x, xout = desired_months)$y %>% fill_na())


# Valorar -----------------------------------------------------------------

value_flows <- function(flows, curve, move_time = FALSE) {
  if (move_time) {
    flows <- flows[-1] # Quitar mes 1
    flows <- c(flows, 0) # Agregar mes al final
  }

  t <- seq_len(length(curve))
  df <- exp(-curve * t / 12)
  pv <- sum(flows * df)
  return(pv)
}

get_flows <- function(list, years = 10) {
  names <- names(list) %>% as.numeric()
  names <- names * 12 # Convertir a meses

  bond_flows <- rep(0, years * 12)
  bond_flows[names] <- list
  bond_flows
}

pv_0 <- lapply(flows, function(x) {
  bond_flows <- get_flows(list = x)
  pv <- value_flows(bond_flows, curve_0)
  pv
})


pv_1 <- lapply(flows, function(x) {
    bond_flows <- get_flows(list = x)
    pv <- apply(curves_1, 2, function(y) value_flows(bond_flows, curve = y, move_time = TRUE))
    pv
  })

# Resultado final -----------------------------------------------------------------
desired_graph <- 7

changes <- (pv_1[[desired_graph]] / pv_0[[desired_graph]])
graph_name <- paste("Histograma de retornos en un mes - ", names(flows)[desired_graph])

hist(changes,
  col = "skyblue", # Color of the bars
  border = "white", # Color of the border of the bars
  xlim = c(min(changes), max(changes)), # X-axis limits
  main = graph_name, # Main title
  xlab = "Valores", # X-axis label
  ylab = "Frecuencia", # Orientation of axis labels: always parallel to the axis,
  breaks = 100
)

Sys.time() - .initial_time # Reporta cuánto toma corriendo
