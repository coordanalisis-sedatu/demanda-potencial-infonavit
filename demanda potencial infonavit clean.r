# Instalar y cargar pacman si no está instalado
if(!require('pacman')) install.packages('pacman')
pacman::p_load(tidyverse, stringi, stringr)

#------------------------------------------------------------------------------- # Función para la limpieza de datos
limpiar_datos <- function(data) {
  
  # Reemplazar NA con 0
  data[is.na(data)] <- 0
  
  # Función para capitalizar la primera letra de cada palabra
  capitalizar_palabras <- function(x) {
    stri_trans_totitle(stri_trans_general(x, "Latin-ASCII"))
  }
  
  # Convertir a minúsculas, quitar acentos y capitalizar palabras
  data <- data %>%
    mutate(
      Estado = capitalizar_palabras(tolower(iconv(Estado, from = "latin1", to = "UTF-8"))),
      Municipio = capitalizar_palabras(tolower(iconv(Municipio, from = "latin1", to = "UTF-8")))
    )
  
  # Convertir el objeto 'table' a 'data.frame'
  data <- as.data.frame(data)
  
  return(data)
}

#------------------------------------------------------------------------------- # Función para estandarizar nombres de estados y municipios
estandarizar_nombres <- function(data) {
  data <- data %>%
    mutate(
      Estado = case_when(
        Estado %in% c('Ciudad de Mexico', 'Cd Mexico', 'Cdmx', 'Distrito Federal') ~ 'Ciudad De Mexico',
        Estado %in% c('Edo. De Mexico', 'Estado De Mexico') ~ 'Mexico',
        Estado == 'Veracruz' ~ 'Veracruz De Ignacio De La Llave',
        Estado == 'Baja California Norte' ~ 'Baja California',
        Estado == 'Michoacan' ~ 'Michoacan De Ocampo',
        Estado == 'Coahuila' ~ 'Coahuila De Zaragoza',
        TRUE ~ Estado
      ),
      Municipio = case_when(
        Municipio %in% c('Ciudad de Mexico', 'Cd Mexico', 'Cdmx', 'Distrito Federal') ~ 'Ciudad De Mexico',
        Municipio %in% c('Edo. De Mexico', 'Estado De Mexico') ~ 'Mexico',
        Municipio == 'Veracruz' ~ 'Veracruz De Ignacio De La Llave',
        Municipio == 'Baja California Norte' ~ 'Baja California',
        Municipio == 'Michoacan' ~ 'Michoacan De Ocampo',
        Municipio == 'Coahuila' ~ 'Coahuila De Zaragoza',
        TRUE ~ Municipio
      )
    )
  return(data)
}

# Función para ordenar los datos
ordenar_datos <- function(data, anio_levels, mes_levels) {
  data <- data %>%
    arrange(
      factor(anio, levels = anio_levels),
      factor(mes, levels = mes_levels)
    )
  return(data)
}

#------------------------------------------------------------------------------- # Leer archivos CSV
archivos <- list(
  "C:/Users/quetz/Documents/Sedatu archivos/dp_infonavit/demanda portencial infonavit original  2014-2016.csv",
  "C:/Users/quetz/Documents/Sedatu archivos/dp_infonavit/demanda portencial infonavit original  2017-2023.csv"
)

# Cargar y limpiar los datos
dp_list <- lapply(archivos, function(archivo) {
  data <- read_csv(archivo)
  data <- limpiar_datos(data)
  data <- estandarizar_nombres(data)
  return(data)
})

#------------------------------------------------------------------------------- # Limpieza adicional para dp2
municipios_cdmx <- c("Alvaro Obregon", "Azcapotzalco", "Benito Juarez", "Coyoacan", 
                     "Cuajimalpa De Morelos", "Cuauhtemoc", "Gustavo A. Madero", 
                     "Iztacalco", "Iztapalapa", "La Magdalena Contreras", "Miguel Hidalgo", 
                     "Milpa Alta", "Tlahuac", "Tlalpan", "Venustiano Carranza", "Xochimilco", 
                     "Distrito Federal", "Magdalena Contreras, La")

dp_list[[2]] <- dp_list[[2]] %>%
  mutate(
    Estado = ifelse(Estado == 'Drmvm' & Municipio %in% municipios_cdmx & mes == 12 & anio == 2018, 
                    "Ciudad De Mexico", 
                    ifelse(Estado == 'Drmvm' & mes == 12 & anio == 2018, 
                           "Mexico", 
                           Estado))
  )

# Filtrar las observaciones del mes 12 y año 2018 con los cambios aplicados
consulta_dp2 <- dp_list[[2]] %>%
  filter(mes == 12 & anio == 2018)

# Obtener los estados presentes en las observaciones filtradas
print(table(consulta_dp2$Estado))

#------------------------------------------------------------------------------- # Ordenar los datos
dp_list[[1]] <- ordenar_datos(dp_list[[1]], c(2014, 2015, 2016), c(2, 4, 6, 8, 10, 12))
dp_list[[2]] <- ordenar_datos(dp_list[[2]], c(2017, 2018, 2019, 2020, 2021, 2022, 2023), c(2, 4, 6, 8, 10, 12))

# Comparar frecuencias de estados en ambos dataframes
print(table(dp_list[[1]]$Estado))
print(table(dp_list[[2]]$Estado))

# Ver información de los dataframes
str(dp_list)


#------------------------------------------------------------------------------- Leer catálogo
c <- read_csv("C:/Users/quetz/Documents/Sedatu archivos/catalogo.csv")
c <- as.data.frame(c) # Convertir el objeto 'table' a 'data.frame'
str(c)
  
# Verificar los nombres de las columnas en el dataframe `c`
print(names(c))

# Agregar las columnas clave_ent y clave_mun a dp_list
dp_list <- lapply(dp_list, function(data) {
  data <- data %>%
    left_join(c, by = c("Estado" = "n_entidad", "Municipio" = "n_mun"))
  return(data)
})

# Mostrar la estructura de los dataframes modificados
str(dp_list)



# Ordenar los dataframes en dp_list por anio, mes, clave_ent y luego por clave_mun
dp_list <- lapply(dp_list, function(data) {
  data <- data %>%
    arrange(anio, mes, clave_ent, clave_mun)
  return(data)
})

# Mostrar las primeras 5 filas de cada dataframe en dp_list para verificar
lapply(dp_list, head, 20)



###########3
library(dplyr)

# Función para verificar si hay filas con NA
verificar_na <- function(data) {
  na_rows <- data %>% filter(is.na(clave_ent) | is.na(clave_mun))
  return(na_rows)
}

# Aplicar la función a cada dataframe en dp_list y guardar los resultados
na_results <- lapply(dp_list, verificar_na)

# Mostrar las filas con NA si existen
lapply(na_results, function(df) {
  if (nrow(df) > 0) {
    print(df)
  } else {
    print("No hay filas con NA en este dataframe")
  }
})

dp_list <- as.data.frame(dp_list) # Convertir el objeto 'table' a 'data.frame'


######################
library(readr)

# Nombres de los archivos CSV
nombres_archivos <- c("dp_list_1.csv", "dp_list_2.csv")

# Guardar cada dataframe en dp_list como un archivo CSV
lapply(seq_along(dp_list), function(i) {
  write_csv(dp_list[[i]], nombres_archivos[i])
})

# Confirmar que los archivos se han guardado
print("Archivos CSV guardados:")
print(nombres_archivos)





