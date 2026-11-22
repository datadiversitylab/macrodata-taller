# Dia 2, bloque 3. Tu proyecto
# Copia este archivo a equipos/tu-ciudad-tu-equipo/analisis.R y trabaja alli
# Cada DECISION es una fila de decisiones.csv. Escribela cuando la tomes

# Nombre 1: Sonia Mahecha Vahos
# Nombre 2: Weider Jobani Diaz Briceno

library(ape)
library(nlme)

# Ciudades: bogota, barranquilla, pasto, quibdo

equipo <- "pasto_equipo01" # Tienes que modificar la ciudad y el numero
salida <- file.path("equipos", equipo)

datos <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)
colibries <- datos[datos$`Family IOC 15.1` == "Trochilidae", ]


# --- DECISION 1. Que columna de masa -----------------------------------------
# Average Mass, solo machos, solo hembras, punto medio de minimo y maximo

masa <- colibries$"Average Mass"
sum(is.na(colibries$"Female MinMass"))
sum(is.na(colibries$"Male MinMass"))
sum(is.na(colibries$"Unsexed MinMass"))
sum(is.na(colibries$"Average Mass"))

spp_av <- c()
for(i in seq_along(colibries[,1])){
  spp_av[i] <- mean(c(colibries$`Male MinMass`[i], colibries$`Female MinMass`[i], colibries$`Unsexed MinMass`[i]), na.rm = TRUE)
}


# --- DECISION 2. Transformar o no --------------------------------------------
# Sin transformar, logaritmo, raiz cuadrada

log_masa <- log(spp_av)


# --- DECISION 3. Como resumir la altitud -------------------------------------
# NormMin, NormMax, punto medio del rango normal, punto medio de Xmin y Xmax

norm_min = as.numeric(colibries$NormMin)
norm_max = as.numeric(colibries$NormMax)

spp_al <- c()
for(i in seq_along(norm_min)){
  spp_al[i] <- mean(c(norm_min[i], norm_max[i]), na.rm = TRUE)
}

altitud <- spp_al

sum(is.na(spp_al))
summary(spp_al)

# --- DECISION 4. Especies con L, F o M --------NO LO USAMOS------------------
# Descartar, convertir al punto medio, imputar, tratar como categorica

#convertir_altitud <- function(x) {
  #x <- trimws(as.character(x))
 # equivalencias <- c(L = 250, F = 750, M = 1500)
  #numerico <- suppressWarnings(as.numeric(x))
  #letra <- x %in% names(equivalencias)
  #numerico[letra] <- equivalencias[x[letra]]
  #numerico
#}

##norm_min <- convertir_altitud(colibries$"NormMin")
##norm_max <- convertir_altitud(colibries$"NormMax")
##altitud <- (norm_min + norm_max) / 2##

colibries$"Scientific Name" <- colibries$`AviList v1 2025`

tabla <- data.frame(
  especie = colibries$`Scientific Name`,
  log_masa = log_masa,
  altitud_km = altitud / 1000,
  stringsAsFactors = FALSE
)

#Eliminar especies NA#

# 1. Revisar la cantidad de registros iniciales
nrow(tabla)

# 2. Contar los valores NA reales
sum(is.na(tabla$especie))

# 3. Contar los valores "NA" escritos como texto
sum(tabla$especie == "NA", na.rm = TRUE)

# 4. Eliminar las filas con NA reales y "NA" como texto
tabla2 <- tabla[
  !is.na(tabla$especie) &
    trimws(tabla$especie) != "NA" &
    trimws(tabla$especie) != "",
]

# 5. Revisar cuántos registros quedan
nrow(tabla2)

# 6. Comprobar que ya no existan NA
sum(is.na(tabla2$especie))


# --- DECISION 5. Que arbol ---------------------------------------------------
# McGuire et al. 2014, rtrees con Jetz et al. 2012, megatree de McTavish 2025
# Y si usas uno solo o una muestra de la posterior

arbol <- read.tree("datos/arboles/McGuire.tre")


# --- DECISION 7. Como empatar los nombres ------------------------------------
# Exacto, sinonimia manual con una lista taxonomica, busqueda difusa

tabla2$especie_arbol <- gsub(" ", "_", tabla2$especie)
en_ambos <- intersect(arbol$tip.label, tabla2$especie_arbol)

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


## --- Guardar -----------------------------------------------------------------

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
file.path(salida, "analisis.R")

# Ahora copiamos el template de decisiones.csv a tu capeta. No olvides llenarlo
file.path(salida, "decisiones.csv")

# Copia a tu path y modifica el README.md
file.path(salida, "README.md")
file.path("equipos/bogota-equipo00/README.md")


