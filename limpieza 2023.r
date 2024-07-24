#2023

# Instalar paquetes
install.packages("dplyr")
install.packages("stringr")
install.packages("openxlsx")

# Librerías
library(readr)
library(dplyr)
library(stringr)
library(openxlsx)
library(readxl)
library(tidyr)

#------------------------------------------------------------------------------- Leer el dataframe
# Leer el archivo csv
infonavit <- read_csv("C:/Users/quetz/Documents/Sedatu archivos/dem_pot_INFONAVIT_19-23.csv")
View(infonavit)
str(infonavit)

#------------------------------------------------------------------------------- Ordenar

#Ordenar la tabla infonavit de la siguiente forma: 

#primero por año: 2019, 2020, 2023, 2023, 2023
#mes: 2, 4, 6, 8, 10, 12
#por clave_entidad del 01 al 32
#después por clave_municipio que son letras tipo string: del 001 ...
#y por ultimo por id_salario_infonavit de la siguiente forma: c(23, 24, 25, 26, 27, 9999)

# Ordenar la tabla infonavit
infonavit <- infonavit %>%
  arrange(
    factor(anio, levels = c(2023)),
    factor(mes, levels = c(2, 4, 6, 8, 10, 12)),
    clave_entidad_federativa,
    clave_municipio,
    factor(id_salario_infonavit, levels = c(23, 24, 25, 26, 27, 9999))
  )

# Ver la tabla ordenada
print(infonavit)

#------------------------------------------------------------------------------- Filtrar


#Ahora procedemos a verificar que en la columna id_salario_infonavit tenga las 6 variables (18, 19, 20, 21, 22, 9999) según cada clave_municipio seleccionado 
#por clave_entidad_federativa, mes 2,4,6,8,10,12 y año 2019, en orden 2019, 2020, 2023, 2023, 2023. 
#Poner un TRUE o FALSE cuando no esten completas las 6 variables.


# Filtrar la tabla para el año 2023
infonavit_2023 <- infonavit %>%
  filter(anio == 2023)

# Verificar si cada combinación tiene las 6 variables necesarias en id_salario_infonavit
verification <- infonavit_2023 %>%
  group_by(clave_entidad_federativa, clave_municipio, mes) %>%
  summarise(
    contar = n_distinct(id_salario_infonavit),
    completos = all(c(23, 24, 25, 26, 27, 9999) %in% id_salario_infonavit)
  ) %>%
  ungroup()

# Unir la verificación con la tabla original
infonavit_check <- infonavit_2023 %>%
  left_join(verification, by = c("clave_entidad_federativa", "clave_municipio", "mes"))

# Ver la tabla verificada
print(infonavit_check)


# Ordenar la tabla
infonavit_check <- infonavit_check %>%
  arrange(
    factor(anio, levels = c(2023)),
    factor(mes, levels = c(2, 4, 6, 8, 10, 12)),
    clave_entidad_federativa,
    clave_municipio,
    factor(id_salario_infonavit, levels = c(23, 24, 25, 26, 27, 9999))
  )

# Ver la tabla verificada
print(infonavit_check)

# Guardar el DataFrame edo en un archivo Excel
write.xlsx(infonavit_check, "infonavit_check.xlsx")

# Verificar el resultado guardado
file.exists("infonavit_check.xlsx")

#-------------------------------------------------------------------------------

# Checar el archivo infonavit_check.xlsx y crear un filtro con todos los que tienen false en complete o en count todos los numeros menos el 6

#-------------------------------------------------------------------------------

# Leer el archivo de excel 
infonavit_23_clean <- read_xlsx("C:/Users/quetz/Documents/Sedatu archivos/demanda potencial infonavit/2023.xlsx")
head(infonavit_23_clean)

################################################################################
# Leer el archivo de excel 
infonavit_23_clean <- read_xlsx("C:/Users/quetz/Documents/infonavit_check.xlsx")
head(infonavit_23_clean)
################################################################################

#En la tabla infonavit_23_clean está la columna id_salario_infonavit que tiene las siguientes variables: 
str(infonavit_23_clean)

# Ordenar la tabla
infonavit_23_clean <- infonavit_23_clean %>%
  arrange(
    factor(anio, levels = c(2023)),
    factor(mes, levels = c(2, 4, 6, 8, 10, 12)),
    clave_entidad_federativa,
    clave_municipio,
    factor(id_salario_infonavit, levels = c(23, 24, 25, 26, 27, 9999))
  )

#La tabla ya está ordenada,

#1) Contar la secuencia del id_salario_infonavit según según cada clave_municipio seleccionado, por clave_entidad_federativa, mes ordenado 2,4,6,8,10,12 
#2) Después agregar una fila según se necesiten completar las 6 variables pero dejarlas en blanco.

# Todos los valores posibles de id_salario_infonavit
id_salarios_posibles <- c(23, 24, 25, 26, 27, 9999)

# Crear una combinación completa de las claves y los id_salario_infonavit
combinacion_completa <- infonavit_23_clean %>%
  distinct(clave_entidad_federativa, clave_municipio, anio, mes) %>%
  expand(nesting(clave_entidad_federativa, clave_municipio, anio, mes), id_salario_infonavit = id_salarios_posibles)

# Unir con la tabla original para encontrar las filas faltantes
infonavit_23_completo <- combinacion_completa %>%
  left_join(infonavit_23_clean, by = c("clave_entidad_federativa", "clave_municipio", "anio", "mes", "id_salario_infonavit")) %>%
  mutate(complete = ifelse(!is.na(id), TRUE, FALSE))  # Marcar como completas las filas existentes

# Filtrar solo las filas incompletas para agregarlas al dataframe original
filas_faltantes <- infonavit_23_completo %>%
  filter(!complete) %>%
  select(-complete)  # Quitamos la columna complete para agregar las filas faltantes

# Agregar las filas faltantes a infonavit_23_clean
infonavit_23_clean <- bind_rows(infonavit_23_clean, filas_faltantes)

# Ordenar el dataframe final por las columnas necesarias si es necesario
infonavit_23_clean <- infonavit_23_clean %>%
  arrange(clave_entidad_federativa, clave_municipio, anio, mes, id_salario_infonavit)

# Mostrar las primeras filas del dataframe final
head(infonavit_23_clean)

################################################################################

# Ver la tabla completada y ordenada
print(infonavit_23_clean)

# Guardar el DataFrame edo en un archivo Excel
write.xlsx(infonavit_23_clean, "infonavit_23_clean.xlsx")

# Verificar el resultado guardado
file.exists("infonavit_23_clean.xlsx")