# Dia 2, bloque 3. Tu proyecto
# Copia este archivo a equipos/tu-ciudad-tu-equipo/analisis.R y trabaja alli
# Cada DECISION es una fila de decisiones.csv. Escribela cuando la tomes

library(ape)
library(nlme)

# Ciudades: bogota, barranquilla, pasto, quibdo

equipo <- "bogota-equipo02" # Tienes que modificar la ciudad y el numero
salida <- file.path("equipos", equipo)

datos <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)
colibries <- datos[datos$`Family IOC 15.1` == "Trochilidae", ]


# --- DECISION 1. Que columna de masa -----------------------------------------
# Average Mass, mayor representatividad

masa <- colibries$"Average Mass"


# --- DECISION 2. Transformar o no --------------------------------------------
# logaritmo

#verficar distribución de masa y transformación con log para normalizar
hist(colibries$`Average Mass`)
log_masa <- log(masa)
hist(log_masa)


# --- DECISION 3. Como resumir la altitud -------------------------------------
# Elevational Range (diferencia entre NormMax y NormMin), para evitar medidas alejadas de la media.



# --- DECISION 4. Especies con L, F o M ---------------------------------------
# Convertir al punto medio

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
  altitud = altitud,
  stringsAsFactors = FALSE
)


# --- DECISION 5. Que arbol ---------------------------------------------------
# McGuire et al. 2014, rtrees con Jetz et al. 2012, megatree de McTavish 2025
# Y si usas uno solo o una muestra de la posterior

arbol <- read.tree("datos/arboles/McTavish.tre")
plot(arbol,  type = "fan", cex = 0.3, no.margin = TRUE)


# --- DECISION 7. Como empatar los nombres ------------------------------------
# Exacto, sinonimia manual con una lista taxonomica, busqueda difusa

tabla$especie_arbol <- gsub(" ", "_", tabla$especie)
en_ambos <- intersect(arbol$tip.label, tabla$especie_arbol)

no_empataron <- setdiff(tabla$especie_arbol, arbol$tip.label)
write.csv(data.frame(especie = no_empataron), file.path(salida, "nombres_sin_empatar.csv"), row.names = FALSE)

#arbol vs database
#no_empataron <- setdiff(arbol$tip.label, tabla$especie_arbol)

# --- DECISION 8. Que especies excluir ----------------------------------------
# Casos completos

tabla <- tabla[!is.na(tabla$log_masa) & !is.na(tabla$altitud), ]
tabla <- tabla[tabla$especie_arbol %in% en_ambos, ]

arbol <- drop.tip(arbol, setdiff(arbol$tip.label, tabla$especie_arbol))
rownames(tabla) <- tabla$especie_arbol
tabla <- tabla[arbol$tip.label, ]

# Sin este TRUE nada de lo que sigue sirve
all(rownames(tabla) == arbol$tip.label)


# --- DECISION 6. Politomias y ramas de longitud cero -------------------------
# Resolver al azar, colapsar, dejarlas, reemplazar los ceros

#if (!is.binary(arbol)) arbol <- multi2di(arbol)
#arbol$edge.length[arbol$edge.length == 0] <- 1e-8
#No hay politomias o ramas de longitud 0

# --- DECISION 9. Que modelo --------------------------------------------------
# corBrownian, corPagel, corMartins, modelo mixto. Y si incluyes error de medicion

modelo_pagel <- gls(log_masa ~ altitud,
              correlation = corPagel(1, phy = arbol, form = ~especie_arbol),
              data = tabla, method = "ML")

summary(modelo_pagel)

modelo_browniano <- gls(log_masa ~ altitud,
                    correlation = corBrownian(1, phy = arbol, form = ~especie_arbol),
                    data = tabla, method = "ML")

summary(modelo_browniano)

# --- Guardar -----------------------------------------------------------------

coeficientes <- summary(modelo_pagel)$tTable
intervalo <- confint(modelo_pagel)

resultados <- data.frame(
  equipo = equipo,
  respuesta = "log(masa corporal)",
  predictor = "Rango medio altitud",
  estimado = coeficientes["altitud", "Value"],
  error_estandar = coeficientes["altitud", "Std.Error"],
  ic_inferior = intervalo["altitud", 1],
  ic_superior = intervalo["altitud", 2],
  valor_p = coeficientes["altitud", "p-value"],
  n = nrow(tabla),
  modelo = "Pagel",
  arbol = "McTavish",
  stringsAsFactors = FALSE
)

write.csv(resultados, file.path(salida, "resultados.csv"), row.names = FALSE)

png(file.path(salida, "figura.png"), width = 1400, height = 1200, res = 200)
plot(tabla$altitud, tabla$log_masa, pch = 16, col = rgb(0, 0, 0, 0.5),
     xlab = "Altitud (m)", ylab = "log masa (g)", main = equipo)
abline(coef(modelo_pagel), lwd = 2)
dev.off()

writeLines(capture.output(sessionInfo()), file.path(salida, "sessionInfo.txt"))

# Guarda este archivo como analisis.R tu carpeta de trabajo. Este es el path
file.path(salida, "analisis.r")

# Ahora copiamos el template de decisiones.csv a tu capeta. No olvides llenarlo
file.path(salida, "decisiones.csv")


