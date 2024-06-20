# Ejemplo de vector
vec <- 1:20  # un vector con 20 elementos
m <- 4       # número de filas deseadas
n <- 5       # número de columnas deseadas

# Convertir el vector en una matriz
mat <- matrix(vec, nrow = m, ncol = n, byrow = TRUE)  # 'byrow = TRUE' llena la matriz por filas

# Convertir la matriz en un dataframe
df <- as.data.frame(mat)

# Imprimir el dataframe
print(df)

# Definir la fecha inicial y la fecha final
fecha_inicial <- as.Date("2024-01-01")
fecha_final <- as.Date("2025-01-10")

# Crear un vector de fechas con salto de un día
fechas <- seq.Date(from = fecha_inicial, to = fecha_final, by = "day")

# Mostrar el vector de fechas
print(fechas)


# Definir una función para calcular la diferencia en días entre dos fechas
calcular_dias <- function(fecha_inicio, fecha_fin) {
  # Convertir las entradas en tipo fecha si no lo son
  fecha_inicio <- as.Date(fecha_inicio)
  fecha_fin <- as.Date(fecha_fin)
  
  # Calcular la diferencia en días
  diferencia <- difftime(fecha_fin, fecha_inicio, units = "days")
  
  # Devolver la diferencia como un número entero
  return(as.integer(diferencia))
}

# Ejemplo de uso de la función
fecha_inicio <- "2024-01-01"
fecha_fin <- "2024-01-31"
dias_entre_fechas <- calcular_dias(fecha_inicio, fecha_fin)
print(dias_entre_fechas)

