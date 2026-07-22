# Dia 2, bloque 3. Tu proyecto
# Copia este archivo a equipos/tu-ciudad-tu-equipo/analisis.R y trabaja alli
# Cada DECISION es una fila de decisiones.csv. Escribela cuando la tomes

library(ape)
library(nlme)

# Ciudades: bogota, barranquilla, pasto, quibdo

equipo <- "bogota-equipo03" # Tienes que modificar la ciudad y el numero
salida <- file.path("equipos", equipo)

datos <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)
colibries03 <- datos[datos$`Family IOC 15.1` == "Trochilidae", ]

# Nos quedamos con lo que vamos a usar y le ponemos nombres manejables
colibries03 <- data.frame(
  especie = colibries03$`Latin (BirdLife > IOC > Clements>AviList)`,
  masa = colibries03$"Average Mass",
  norm_min = colibries03$"NormMin",
  norm_max = colibries03$"NormMax",
  stringsAsFactors = FALSE
)

str(colibries03)


# --- DECISION 1. Que columna de masa -----------------------------------------
# Average Mass, solo machos, solo hembras, punto medio de minimo y maximo

masa <- colibries03$"Average Mass"


# --- DECISION 2. Transformar o no --------------------------------------------
# Sin transformar, logaritmo, raiz cuadrada

log_masa <- log(masa)


# --- DECISION 3. Como resumir la altitud -------------------------------------
# NormMin, NormMax, punto medio del rango normal, punto medio de Xmin y Xmax

colibries03$altitud_promedio <- (colibries03$norm_min + colibries03$norm_max) / 2

# --- DECISION 4. Especies con L, F o M ---------------------------------------
# Descartar, convertir al punto medio, imputar, tratar como categorica

convertir_altitud <- function(x) {
  x <- trimws(as.character(x))
  equivalencias <- c(L = 250, F = 750, M = 1500)
  numerico <- suppressWarnings(as.numeric(x))
  letra <- x %in% names(equivalencias)
  numerico[letra] <- equivalencias[x[letra]]
  numerico
}

norm_min <- convertir_altitud(colibries$"NormMin")
norm_max <- convertir_altitud(colibries$"NormMax")
altitud <- (norm_min + norm_max) / 2

colibries$"Scientific Name" <- colibries$`Latin (BirdLife > IOC > Clements>AviList)`

tabla <- data.frame(
  especie = colibries$`Scientific Name`,
  log_masa = log_masa,
  altitud_km = altitud / 1000,
  stringsAsFactors = FALSE
)


# --- DECISION 5. Que arbol ---------------------------------------------------
# McGuire et al. 2014, rtrees con Jetz et al. 2012, megatree de McTavish 2025
# Y si usas uno solo o una muestra de la posterior

arbol <- read.tree("datos/arboles/McTavish.tre")


# --- DECISION 7. Como empatar los nombres ------------------------------------
# Exacto, sinonimia manual con una lista taxonomica, busqueda difusa

tabla$especie_arbol <- gsub(" ", "_", tabla$especie)
en_ambos <- intersect(arbol$tip.label, tabla$especie_arbol)

no_empataron <- setdiff(tabla$especie_arbol, arbol$tip.label)
write.csv(data.frame(especie = no_empataron), file.path(salida, "nombres_sin_empatar.csv"), row.names = FALSE)


# --- DECISION 8. Que especies excluir ----------------------------------------
# Casos completos, imputacion por genero, imputacion filogenetica

tabla <- tabla[!is.na(tabla$log_masa) & !is.na(tabla$altitud_km), ]
tabla <- tabla[tabla$especie_arbol %in% en_ambos, ]

arbol <- drop.tip(arbol, setdiff(arbol$tip.label, tabla$especie_arbol))
rownames(tabla) <- tabla$especie_arbol
tabla <- tabla[arbol$tip.label, ]

# Sin este TRUE nada de lo que sigue sirve
all(rownames(tabla) == arbol$tip.label)


# --- DECISION 6. Politomias y ramas de longitud cero -------------------------
# Resolver al azar, colapsar, dejarlas, reemplazar los ceros

if (!is.binary(arbol)) arbol <- multi2di(arbol)
arbol$edge.length[arbol$edge.length == 0] <- 1e-8


# --- DECISION 9. Que modelo --------------------------------------------------
# corBrownian, corPagel, corMartins, modelo mixto. Y si incluyes error de medicion

modelo <- gls(log_masa ~ altitud_km,
              correlation = corPagel(1, phy = arbol, form = ~especie_arbol),
              data = tabla, method = "ML")

summary(modelo)


# --- Guardar -----------------------------------------------------------------

coeficientes <- summary(modelo)$tTable
intervalo <- confint(modelo)

resultados <- data.frame(
  equipo = equipo,
  respuesta = "log(masa corporal)",
  predictor = "DESCRIBE AQUI COMO RESUMISTE LA ALTITUD",
  estimado = coeficientes["altitud_km", "Value"],
  error_estandar = coeficientes["altitud_km", "Std.Error"],
  ic_inferior = intervalo["altitud_km", 1],
  ic_superior = intervalo["altitud_km", 2],
  valor_p = coeficientes["altitud_km", "p-value"],
  n = nrow(tabla),
  modelo = "DESCRIBE AQUI EL MODELO QUE AJUSTASTE",
  arbol = "DESCRIBE AQUI QUE ARBOL USASTE",
  stringsAsFactors = FALSE
)

write.csv(resultados, file.path(salida, "resultados.csv"), row.names = FALSE)

png(file.path(salida, "figura.png"), width = 1400, height = 1200, res = 200)
plot(tabla$altitud_km, tabla$log_masa, pch = 16, col = rgb(0, 0, 0, 0.5),
     xlab = "Altitud (km)", ylab = "log masa (g)", main = equipo)
abline(coef(modelo), lwd = 2)
dev.off()

writeLines(capture.output(sessionInfo()), file.path(salida, "sessionInfo.txt"))

# Guarda este archivo como analisis.R tu carpeta de trabajo. Este es el path
file.path(salida, "analisis.r")

# Ahora copiamos el template de decisiones.csv a tu capeta. No olvides llenarlo
file.path(salida, "decisiones.csv")


