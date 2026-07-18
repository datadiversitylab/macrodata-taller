# Dia 1, bloque 3. Datos reales con BirdBase

# check.names = FALSE conserva los nombres originales, que tienen espacios
datos <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Mirar antes de tocar. Siempre
dim(datos)
str(datos[, 1:10])
names(datos)

# Solo colibries
colibries <- datos[datos$Family == "Trochilidae", ]
nrow(colibries)

# Nos quedamos con lo que vamos a usar y le ponemos nombres manejables
colibries <- data.frame(
  especie = colibries$"Scientific Name",
  masa = colibries$"Average Mass",
  masa_min_macho = colibries$"Male MinMass",
  masa_max_macho = colibries$"Male MaxMass",
  x_min = colibries$"Xmin",
  norm_min = colibries$"NormMin",
  norm_max = colibries$"NormMax",
  x_max = colibries$"Xmax",
  stringsAsFactors = FALSE
)

str(colibries)

# Aqui esta la trampa. La altitud no siempre es un numero
head(sort(unique(colibries$norm_min)))
table(colibries$norm_min[is.na(suppressWarnings(as.numeric(colibries$norm_min)))])

# L es tierras bajas, F es piedemonte, M es montano
convertir_altitud <- function(x) {
  x <- trimws(as.character(x))
  equivalencias <- c(L = 250, F = 750, M = 1500)
  numerico <- suppressWarnings(as.numeric(x))
  letra <- x %in% names(equivalencias)
  numerico[letra] <- equivalencias[x[letra]]
  numerico
}

colibries$norm_min <- convertir_altitud(colibries$norm_min)
colibries$norm_max <- convertir_altitud(colibries$norm_max)

# Convertir letras a numeros es una decision, no un paso obligatorio
colibries$altitud <- (colibries$norm_min + colibries$norm_max) / 2

# Cuanto falta y donde
sum(is.na(colibries$masa))
sum(is.na(colibries$altitud))
colSums(is.na(colibries))

# Explorar
summary(colibries$masa)
summary(colibries$altitud)

hist(colibries$masa, breaks = 30, main = "Masa", xlab = "Gramos")
hist(log(colibries$masa), breaks = 30, main = "log masa", xlab = "log gramos")

plot(colibries$altitud, log(colibries$masa), pch = 16, col = rgb(0, 0, 0, 0.4),
     xlab = "Altitud (m)", ylab = "log masa (g)")

# Una regresion normal. Ignora la filogenia, y por eso esta mal
summary(lm(log(masa) ~ altitud, data = colibries))

# Manana vemos por que esta mal y como se corrige
completos <- colibries[!is.na(colibries$masa) & !is.na(colibries$altitud), ]
nrow(completos)

write.csv(completos, "datos/colibries_limpio.csv", row.names = FALSE)
