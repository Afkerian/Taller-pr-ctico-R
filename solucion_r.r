# cargar los datos desde un archivo CSV
datos <- read.csv("C:/Users/aleja/Downloads/at_health_facilities.csv")

# ver los primeros registros de los datos
head(datos)

# 1. Cuántos países reportaron los datos?
num_paises <- length(unique(datos$iso3))
cat("Número de países reportando datos: ", num_paises, "\n")

# 2. Cuál es la diferencia entre el año mínimo y año máximo con datos válidos por cada país?
dif_anios <- aggregate(year ~ iso3, data=datos, FUN=function(x) diff(range(x)))
cat("Diferencia de años con datos válidos por país: \n")
print(dif_anios)

# 3. Cuántos países reportaron datos en 3 años o más?
paises_3anios <- aggregate(year ~ iso3, data=datos, FUN=function(x) length(unique(x)))
paises_3anios <- subset(paises_3anios, year >= 3)
num_paises_3anios <- nrow(paises_3anios)
cat("Número de países reportando datos en 3 años o más: ", num_paises_3anios, "\n")

# 4. Qué países reportaron el 100% de incidencia para al menos un año en cualquier grupo de edad?
library(dplyr)

paises_100porc <- datos %>%
  group_by(iso3) %>%
  filter(any(`age.15.17` == 100) | any(`age.20.34` == 100)) %>%
  distinct(iso3)

cat("Países que reportaron el 100% de incidencia para al menos un año en cualquier grupo de edad: \n")
print(paises_100porc$iso3)
