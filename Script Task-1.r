#Nombres Alejandra Orellanos, Maria Jose Rivera 
#Codigos   201913577, 201914133


#Secuencia de 1 a 100
num <- 1:100
#Secuencia impares
impares <- seq(1, 99, by=2)

#Extrae las primeras 4 columnas para dejar solo la base de datos
cultivos <- cultivos[4:nrow(cultivos),]

#Establece los nombres de las columnas
colnames(cultivos) <- cultivos[1,]

#Quita los pies de tabla
cultivos <- cultivos[2:351,]



#https://tidyr.tidyverse.org/
install.packages("tidyverse")
library(tidyverse)
library(tidyr)

#Se busca el archivo
cultivosFilename <- file.choose()
#Se lee el archivo de excel
cultivos <- read_excel(cultivosFilename)

#Si hay una cadena con mas de 2 caracteres de longitud, se filtra y se pone en un dataframe
cultivosClean <- as.data.frame(filter(cultivos, nchar(cultivos$CODDEPTO)<=2))

#Se define el tipo de variable de los datos como numericos
cultivosClean[, 5:13] <- as.numeric(unlist(cultivosClean[, 5:13]))

#Selecciona el archivo
filenameGenerales <- file.choose()
filenameOcupados <- file.choose()
#Carga el archivo
CarGenerales <- readRDS(filenameGenerales)
CarOcupados <- readRDS(filenameOcupados)

#Añade una columna de ID que concatena el directorio, la secuencia y el orden
CarGenerales <- cbind(paste(CarGenerales$directorio, CarGenerales$secuencia_p, CarGenerales$orden, sep = ""), CarGenerales)
CarOcupados <- cbind(paste(CarOcupados$directorio, CarOcupados$secuencia_p, CarOcupados$orden, sep = ""), CarOcupados)

#Agrega el nombre a la columna ID
CarGenerales <- CarGenerales %>% rename("ID"= "paste(CarGenerales$directorio, CarGenerales$secuencia_p, CarGenerales$orden, ")
CarOcupados <- CarOcupados %>% rename("ID" = "paste(CarOcupados$directorio, CarOcupados$secuencia_p, CarOcupados$orden, ")

#Se quitan columnas duplicadas
CarOcupados <- CarOcupados[, 1:20]

#Se juntan las bases por el ID
CarTotal <- merge(CarGenerales, CarOcupados, by = c("ID", "ID"), all.x=TRUE)

#Se importa ggplot2 para realizar distintas graficas
library(ggplot2)
#Se importa RColorBrewer para usar paletas de color en los graficos
library(RColorBrewer)

#Por año

#Se realiza un histograma que permita visualizar la distribucion de edad
hist(CarTotal$P6040, xlab= "Edad", breaks = 6, main="Histograma de la distribución de edades", col=c("blue", "purple", "red", "orange", "yellow", "green"))
#Se despliegan estadisticas descriptivas para la edad
summary(CarTotal$P6040)

#Por sexo

#Se realiza un conteo por sexo usando la funcion count del paquete dplyr 
conteoPorSexo <- as.data.frame(CarTotal %>% count(P6020))
#Se realiza un diagrama de barras para visualizar el conteo por sexo
barplot(conteoPorSexo$n, col =c("blue","pink"), xlab= "Sexo", ylab="Conteo", names.arg = c("1", "2"), main = "Conteo por sexo")

#Lugar de trabajo

#Se hace un diagrama de barras para visualizar la informacion de lugar de trabajo
barplot(summary(as.factor(CarTotal$P6880))[1:11], xlab = "Lugar donde realiza principalmente su trabajo", ylab = "Frecuencia", main = "Lugares donde la población ocupada realizan principalmente su trabajo", col = brewer.pal(5, "Set1"))

#Filtrar por ocupado/desocupado

#Se usa dplyr para agrupar y contar por ocupado/desocupado
Oci_count <- CarTotal %>% count(Oci)

#Se hace un diagrama de barras para comparar las personas ocupadas/desocupadas
barplot(Oci_count[,2], col =c("gray","red"), xlab= "Ocupado", ylab="Conteo", names.arg = c("Ocupado", "No ocupado"), main = "Conteo por ocupación/desempleo")

#Nivel de ingresos

#Se define un vector con los ingresos
Ingresos_Raw <- CarTotal$P6500
#Se genera un summary de los ingresos (estadisticas descriptivas)
summary(Ingresos_Raw)
#Se quitan los ingresos superiores a 20 millones de pesos debido a que se consideran outliers
Ingresos_Filtered <- CarTotal%>%filter(!is.na(P6500)&P6500<20000000&P6500>0)

#Se extrae unicamente el vector de ingresos
Ingresos_Filtered <- Ingresos_Filtered$P6500
#Se crea un histograma para ver la distribucion de ingresos
hist(Ingresos_Filtered/1000, main = "Histograma de los ingresos del mes pasado (pre tax, miles de COP, menores a 2M COP)", xlab="Ingresos", col = brewer.pal(5, "Set2"), nclass = 30)
#Se hace un boxplot para visualizar cuartiles y outliers
boxplot(Ingresos_Filtered/1000, main = "Boxplot de los ingresos del mes pasado (pre tax, miles de COP, menores a 2M COP)", xlab="Ingresos")


#Nivel de escolaridad

#Se hace un summary que brinda estadisticas descriptivas de la columna referente al nivel de escolaridad (no es un numero sino un factor)
Escolaridad_count <- summary(as.factor(CarTotal$P6220))
#Se hace un diagrama de barras para poder visualizar facilmente la informacion
barplot(Escolaridad_count, names.arg = c("Primaria", "Básica Secundaria", "Secundaria", "Técnico/Tecnológico", "Profesional o superior", "No estudió"), xlab = "Máximo nivel de escolaridad alcanzado", ylab = "Frecuencia", main ="Nivel de escolaridad más alto alcanzado", col = brewer.pal(5, "Set1"))

#Horas trabajadas semanalmente
#Estadisticas descriptivas de las horas trabajadas semanalmente
summary(CarTotal$P6800)
#Se realiza un histograma para visualizar la distribucion de horas trabajadas a la semana
hist(CarTotal$P6800, main = "Histograma de las horas trabajadas semanalmente", xlab="Horas trabajadas semanalmente", col = brewer.pal(3, "Set3"))

