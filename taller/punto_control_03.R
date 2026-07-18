# Punto de control despues del bloque 3. Datos limpios
# Cargalo si te atrasaste y sigue con el resto del grupo

datos <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)
colibries <- datos[datos$Family == "Trochilidae", ]

colibries <- data.frame(
  especie = colibries$"Scientific Name",
  masa = colibries$"Average Mass",
  norm_min = colibries$"NormMin",
  norm_max = colibries$"NormMax",
  stringsAsFactors = FALSE
)

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
colibries$altitud <- (colibries$norm_min + colibries$norm_max) / 2

completos <- colibries[!is.na(colibries$masa) & !is.na(colibries$altitud), ]
write.csv(completos, "datos/colibries_limpio.csv", row.names = FALSE)

cat("Listo. Tienes", nrow(completos), "especies en datos/colibries_limpio.csv\n")
