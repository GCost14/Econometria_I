#install.packages("tidyverse") # lo realizo solo una vez
library(tidyverse)

#Importar el archivo Notas.csv
notas <- read_csv2("notas.csv") 

#Observar los nombres de las variables
names(notas)
head(notas) #las primeras 6 líneas
dim(notas) #la dimensión
summary(notas) #un resumen de cada variable

#Relizar un gráfico de dispersión entre dos variables y un histograma de otra.
?plot