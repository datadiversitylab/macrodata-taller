# Dia 1, bloque 2. Fundamentos de R

# Asignar. La flecha, no el igual
x <- 5
x

# Vectores
masas <- c(3.2, 5.8, 9.1, 4.4, 20.5)
especies <- c("Amazilia", "Coeligena", "Ensifera", "Chlorostilbon", "Patagona")

length(masas)
class(masas)
class(especies)

# Los vectores se operan completos, sin loops
masas * 2
log(masas)
masas > 5

?log
# Indexar. En R se cuenta desde 1
masas[1]
masas[c(1, 3)]
masas[masas > 5]
especies[masas > 5] ##Esta saca las especies que son verdaderas o que quiere decir cuales son las que si  cumplen la funcion de mayor

# Data frame. Vectores del mismo largo puestos en columnas
colibries <- data.frame(especie = especies, masa = masas, stringsAsFactors = FALSE)
colibries

str(colibries)##que fue lo que leyo 
nrow(colibries)
ncol(colibries)
dim(colibries)[2]
names(colibries)

# Una columna se saca con el signo de pesos
colibries$masa
mean(colibries$masa)##Promedio

# Columna nueva
colibries$log_masa <- log(colibries$masa)
colibries

# Filas que cumplen una condicion##Estas funciones dan el mismo resultado
colibries[colibries$masa > 5, ]
subset(colibries, masa > 5)
?subset

# Ordenar
colibries[order(colibries$masa, decreasing = TRUE),]


# Datos faltantes. Vas a verlos todo el tiempo
con_na <- c(3.2, NA, 9.1)
mean(con_na)
mean(con_na, na.rm = TRUE)
is.na(con_na) ##VERDADERO ES CUANDO SI HAY NA
sum(is.na(con_na))
##Esta seccion nos ayuda a saber si lo vamos a imputar o borrar los datos faltantes, osea los que aparecen como NA


# Funciones
convertir_a_kg <- function(gramos) {
  gramos / 1000
}##La importancia de una funcion radica en que se la puede llamar mas adelante


convertir_a_kg(colibries$masa)
##Tiene argumentos y tiene cuerpo 


# Condicionales
if (mean (colibries$masa) > 5) {
  print("Colibries pesados")
} else {
  print("Colibries livianos")##El promedio de los colibries es pesado 
}

# Loops. Utiles, pero en R casi siempre hay algo mejor
for (i in 1:nrow(colibries)) {
  print(colibries$especie[i])
}

for (i in 1:nrow(colibries)) {
  print(colibries$masa[i])
}


# Esto hace lo mismo sin loop
sapply(colibries, class)
tapply(colibries$masa, colibries$masa > 5, mean)

##


# Graficar
plot(colibries$masa, colibries$log_masa, pch = 16, xlab = "Masa (g)", ylab = "log masa")
hist(colibries$masa)
