# Dia 1, bloque 2. Fundamentos de R

# Asignar. La flecha, no el igual
x <- 5
x

# Vectores
masas <- c(3.2, 5.8, 9.1, 4.4, 20.5)
especies <- c("Amazilia", "Coeligena", "Ensifera", "Chlorostilbon", "Patagona")

length(masas) #longitud de un vector(número de elementos)
class(masas)
class(especies)

# Los vectores se operan completos, sin loops
masas * 2 #se multiplica cada uno de los emlementos por vector
log(masas) #log2 (binario), logb, buscar alternativas de log en la ayuda
masas > 5 

# Indexar. En R se cuenta desde 1
masas[1]
masas[c(1, 3)]
masas[masas > 5]
especies[masas > 5] #combinando, haciendo subsets

# Data frame. Vectores del mismo largo puestos en columnas
colibries <- data.frame(especie = especies, masa = masas, stringsAsFactors = FALSE)
colibries
?data.frame

str(colibries) #como es la estructura?
nrow(colibries) 
dim(colibries) #número de filas y columnas
ncol(colibries)
names(colibries)

# Una columna se saca con el signo de pesos
colibries$masa
mean(colibries$masa)

# Columna nueva
colibries$log_masa <- log(colibries$masa)
colibries

# Filas que cumplen una condicion
colibries[colibries$masa > 5, ]
subset(colibries, masa > 5) #por que usar subset? funcion implicita.

# Ordenar
colibries[order(colibries$masa), ] #cuales son los argumentos de order?


# Datos faltantes. Vas a verlos todo el tiempo
con_na <- c(3.2, NA, 9.1)
#con_na <- c(3.2, "NA", 9.1)
mean(con_na)
mean(con_na, na.rm = TRUE) #REMOVER NA
is.na(con_na)
sum(is.na(con_na))

# Funciones
convertir_a_kg <- function(gramos) {
  gramos / 1000
}

convertir_a_kg(colibries$masa)

# Condicionales
if (mean(colibries$masa) > 5) {
  print("Colibries pesados")
} else {
  print("Colibries livianos")
}

#que pasa si quito el promedio--> sale error
if (colibries$masa) > 5) {
  print("Colibries pesados")
} else {
  print("Colibries livianos")
}

# Loops. Utiles, pero en R casi siempre hay algo mejor
for (i in 1:nrow(colibries)) {
  print(colibries$especie[i])
}
###################################
#TAREA:combinar for con condicional
#Para cada especie categorizarla en pesado o liviano
#con if/else y forr loop

# Esto hace lo mismo sin loop
sapply(colibries, class) 
tapply(colibries$masa, colibries$masa > 5, mean) #vector de peso, promedio false=liviano

# Graficar
plot(colibries$masa, colibries$log_masa, pch = 16, xlab = "Masa (g)", ylab = "log masa")
hist(colibries$masa)

dev.off() 
?dev.off

