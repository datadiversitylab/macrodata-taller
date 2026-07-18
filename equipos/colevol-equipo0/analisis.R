# Equipo 0. Ejemplo de referencia para los talleres
# Pregunta: la altitud predice la masa corporal en Trochilidae
# Correr desde la raiz del repositorio con una sesion limpia de R

library(ape)
library(nlme)

equipo <- "colevol-equipo0"
salida <- file.path("equipos", equipo)

# 1. Datos de rasgos ------------------------------------------------------

colibries <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Renombramos las columnas que vamos a usar para no arrastrar espacios
colibries <- data.frame(
  especie = colibries$`AviList v1 2025`,
  masa = colibries$"Average Mass",
  norm_min = colibries$"NormMin",
  norm_max = colibries$"NormMax",
  stringsAsFactors = FALSE
)

# 2. Altitud --------------------------------------------------------------

# BirdBase codifica algunos limites como letras en lugar de numeros
# L es tierras bajas, F es piedemonte, M es montano
# Decidimos convertirlas al punto medio de cada intervalo en vez de descartarlas
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

# Resumimos la altitud como el punto medio del rango normal
colibries$altitud <- (colibries$norm_min + colibries$norm_max) / 2

# 3. Limpieza -------------------------------------------------------------

completos <- !is.na(colibries$masa) & !is.na(colibries$altitud)
colibries <- colibries[completos, ]

# Log de la masa porque la distribucion es fuertemente sesgada
colibries$log_masa <- log(colibries$masa)

# Altitud en kilometros para que el coeficiente sea legible
colibries$altitud_km <- colibries$altitud / 1000

cat("Especies con datos completos:", nrow(colibries), "\n")

# 4. Arbol ----------------------------------------------------------------

# Usamos el arbol de Jetz et al. 2012, especifico para colibries
arbol <- read.tree("datos/arboles/Jetz_ericson.tre")[[1]]

# Los nombres del arbol usan guion bajo, los de BirdBase usan espacio
colibries$especie_arbol <- gsub(" ", "_", colibries$especie)

en_ambos <- intersect(arbol$tip.label, colibries$especie_arbol)
cat("Especies en el arbol y en la matriz:", length(en_ambos), "\n")

# Guardamos los nombres que no empataron para revisarlos a mano
no_empataron <- setdiff(colibries$especie_arbol, arbol$tip.label)
write.csv(data.frame(especie = no_empataron), file.path(salida, "nombres_sin_empatar.csv"), row.names = FALSE)

arbol <- drop.tip(arbol, setdiff(arbol$tip.label, en_ambos))
colibries <- colibries[colibries$especie_arbol %in% en_ambos, ]
rownames(colibries) <- colibries$especie_arbol
colibries <- colibries[arbol$tip.label, ]

# Resolvemos politomias al azar y damos longitud minima a las ramas de cero
if (!is.binary(arbol)) arbol <- multi2di(arbol)
arbol$edge.length[arbol$edge.length == 0] <- 1e-8

# 5. Modelo ---------------------------------------------------------------

# PGLS con lambda de Pagel estimada por maxima verosimilitud
modelo <- gls(
  log_masa ~ altitud_km,
  correlation = corPagel(1, phy = arbol, form = ~especie_arbol),
  data = colibries,
  method = "ML"
)

resumen <- summary(modelo)
coeficientes <- resumen$tTable
intervalo <- confint(modelo)

print(resumen)

# 6. Resultados -----------------------------------------------------------

resultados <- data.frame(
  equipo = equipo,
  respuesta = "log(masa corporal)",
  predictor = "altitud, punto medio del rango normal, en km",
  estimado = coeficientes["altitud_km", "Value"],
  error_estandar = coeficientes["altitud_km", "Std.Error"],
  ic_inferior = intervalo["altitud_km", 1],
  ic_superior = intervalo["altitud_km", 2],
  valor_p = coeficientes["altitud_km", "p-value"],
  n = nrow(colibries),
  modelo = "PGLS, lambda de Pagel estimada",
  lambda = as.numeric(coef(modelo$modelStruct$corStruct, unconstrained = FALSE)),
  arbol = "McGuire et al. 2014",
  stringsAsFactors = FALSE
)

write.csv(resultados, file.path(salida, "resultados.csv"), row.names = FALSE)

# 7. Figura ---------------------------------------------------------------

png(file.path(salida, "figura.png"), width = 1400, height = 1200, res = 200)
plot(colibries$altitud_km, colibries$log_masa,
     pch = 16, col = rgb(0, 0, 0, 0.5),
     xlab = "Altitud (km)", ylab = "log masa (g)",
     main = "Trochilidae, equipo 0")
abline(coef(modelo), lwd = 2)
dev.off()

# 8. Sesion ---------------------------------------------------------------

writeLines(capture.output(sessionInfo()), file.path(salida, "sessionInfo.txt"))
