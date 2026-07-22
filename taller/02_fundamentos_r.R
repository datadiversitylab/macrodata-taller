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

# Indexar. En R se cuenta desde 1
masas[1]
masas[c(1, 3)]
masas[masas > 5]
especies[masas > 5]

# Data frame. Vectores del mismo largo puestos en columnas
colibries <- data.frame(especie = especies, masa = masas, stringsAsFactors = FALSE)
colibries

str(colibries)
nrow(colibries)
names(colibries)

# Una columna se saca con el signo de pesos
colibries$masa
mean(colibries$masa)

# Columna nueva
colibries$log_masa <- log(colibries$masa)
colibries

# Filas que cumplen una condicion
colibries[colibries$masa > 5, ]
subset(colibries, masa > 5)

# Ordenar
colibries[order(colibries$masa), ]

# Datos faltantes. Vas a verlos todo el tiempo
con_na <- c(3.2, NA, 9.1)
mean(con_na)
mean(con_na, na.rm = TRUE)
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

# Loops. Utiles, pero en R casi siempre hay algo mejor
for (i in 1:nrow(colibries)) {
  print(colibries$Species[i])
}

# Esto hace lo mismo sin loop
sapply(colibries, class)
tapply(colibries$masa, colibries$masa > 5, mean)

# Graficar
plot(colibries$masa, colibries$log_masa, pch = 16, xlab = "Masa (g)", ylab = "log masa")
hist(colibries$masa)
