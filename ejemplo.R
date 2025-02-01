
.initial_time <- Sys.time() # Cronometrar corrida
# delete all the environment variables
rm(list = ls())
set.seed(123) # Fijar semilla

# Ejemplo ----------------------------------------------------------------
pacman::p_load(readxl, dplyr, stats, forecast, parallel, ggplot2, tidyverse)

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
n_semanas <- 52 # Un año
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
desired_graph <- 3

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

# fanchart -----------------------------------------------------------------

create_fan_chart <- function(history, simulations) {
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  
  # Create a data frame for the historical data
  history_df <- data.frame(
    Period = 1:length(history),
    Value = history
  )
  
  # Ensure simulations is a numeric matrix and convert it to a data frame
  simulations <- as.matrix(simulations)
  simulations_df <- as.data.frame(simulations)
  
  # The forecast periods will start immediately after the historical periods
  start_forecast <- max(history_df$Period) + 1
  simulations_df$Period <- start_forecast:(start_forecast + nrow(simulations_df) - 1)
  
  # Shift the simulations so they are on the same level as history.
  # (Assuming your simulations represent deviations/cumulative changes from 0,
  # we add the last historical value to each simulation.)
  last_value <- tail(history, 1)
  sim_cols <- setdiff(names(simulations_df), "Period")
  simulations_df[sim_cols] <- lapply(simulations_df[sim_cols], function(x) as.numeric(x) + last_value)
  
  # Reshape the simulations from wide to long format
  simulations_long <- simulations_df %>%
    pivot_longer(
      cols = -Period,
      names_to = "Simulation",
      values_to = "Value"
    ) %>%
    mutate(Value = as.numeric(Value))
  
  # Calculate quantiles for each forecast period
  quantiles <- simulations_long %>%
    group_by(Period) %>%
    summarise(
      Q10 = quantile(Value, 0.10, na.rm = TRUE),
      Q25 = quantile(Value, 0.25, na.rm = TRUE),
      Q50 = quantile(Value, 0.50, na.rm = TRUE),
      Q75 = quantile(Value, 0.75, na.rm = TRUE),
      Q90 = quantile(Value, 0.90, na.rm = TRUE)
    )
  
  # Build the fan chart:
  # - First add the forecast fan (ribbons and median forecast line),
  # - Then add the historical line on top.
  ggplot() +
    geom_ribbon(data = quantiles, aes(x = Period, ymin = Q10, ymax = Q90),
                fill = "skyblue", alpha = 0.5) +
    geom_ribbon(data = quantiles, aes(x = Period, ymin = Q25, ymax = Q75),
                fill = "blue", alpha = 0.5) +
    geom_line(data = quantiles, aes(x = Period, y = Q50),
              color = "red", size = 1) +
    geom_line(data = history_df, aes(x = Period, y = Value),
              color = "blue", size = 1) +
    labs(
      title = "Fan Chart nodo de un año",
      x = "Semana",
      y = "Tasa"
    ) +
    theme_minimal()
}


desired_node <- 3 # one year

create_fan_chart(history = bonds[-c(1:400), 3] * 100,
 simulations = resultados[, desired_node, ] * 100)
