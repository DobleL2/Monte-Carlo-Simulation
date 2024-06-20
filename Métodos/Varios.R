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