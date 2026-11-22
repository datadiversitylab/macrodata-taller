# Dia 2, bloque 3. Tu proyecto
# Copia este archivo a equipos/tu-ciudad-tu-equipo/analisis.R y trabaja alli
# Cada DECISION es una fila de decisiones.csv. Escribela cuando la tomes

# Nombre 1: [Gabriel Borja-Calonge]
# Nombre 2: [Michael S. Guerrero-Suarez]

library(ape)
library(nlme)
library(MASS)

# Ciudades: bogota, barranquilla, pasto, quibdo

equipo <- "pasto-equipo08" # Tienes que modificar la ciudad y el numero
salida <- file.path("equipos", equipo)

datos <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)
colibries <- datos[datos$`Family IOC 15.1` == "Trochilidae", ]

head(datos)

# --- DECISION 1. Que columna de masa -----------------------------------------
# Average Mass, solo machos, solo hembras, punto medio de minimo y maximo


masa <- colibries$"Average Mass"

#spp_av <- c()
#for(i in seq_along(colibries[,1])){
 # spp_av[i] <- mean(c(colibries$`Male MinMass`[i], colibries$`Female MinMass`[i]), na.rm = TRUE)
#}
#spp_av


# --- DECISION 2. Transformar o no --------------------------------------------
# Sin transformar, logaritmo, raiz cuadrada

log_masa <- log(masa)
sqrt_masa <- sqrt(masa)

var(log_masa, na.rm = T)
var(log_masa, na.rm = T)

### Se realiza una prueba antes de tomar la desición

masa <- na.omit(colibries$`Average Mass`)

# 2. Prueba de Box-Cox (evalúa la potencia exacta que normaliza la variable)
bc <- boxcox(lm(masa ~ 1), lambda = seq(-2, 2, by = 0.05), plotit = TRUE)
lambda_optimo <- bc$x[which.max(bc$y)]

lambda_optimo

# 3. Prueba de normalidad de Shapiro-Wilk para cada transformación
# (Si tienes más de 5000 datos, usa ks.test en su lugar)
sw_orig <- shapiro.test(masa)$statistic
sw_sqrt <- shapiro.test(sqrt(masa))$statistic
sw_log  <- shapiro.test(log(masa))$statistic

# 4. Tabla resumen de decisión
resultados <- data.frame(
  Transformacion = c("Sin transformar (lambda = 1)", "Raíz cuadrada (lambda = 0.5)", "Logaritmo (lambda = 0)"),
  Estadistico_W  = round(c(sw_orig, sw_sqrt, sw_log), 4),
  Distancia_a_Lambda = round(abs(c(1, 0.5, 0) - lambda_optimo), 3)
)

cat("Lambda óptimo de Box-Cox:", round(lambda_optimo, 3), "\n\n")
print(resultados)

#Considerando el valor óptimo de la prueba de BoxCox, el cual está más cercano a 0 
# se determina trabajar con la transformación basada en logaritmo. 
log_masa <- log(masa)

# --- DECISION 3. Como resumir la altitud -------------------------------------
# NormMin, NormMax, punto medio del rango normal, punto medio de Xmin y Xmax


# --- DECISION 4. Especies con L, F o M ---------------------------------------
# Descartar, convertir al punto medio, imputar, tratar como categorica

# La altitud puede que no siempre sea un numero
head(sort(unique(colibries$`Elevational Range`)))
#table(colibries$Xmin[is.na(suppressWarnings(as.numeric(colibries$Xmin)))])

# L es tierras bajas, F es piedemonte, M es montano
convertir_altitud <- function(x) {
  x <- trimws(as.character(x))
  equivalencias <- c(L = 250, F = 750, M = 1500)
  numerico <- suppressWarnings(as.numeric(x))
  letra <- x %in% names(equivalencias)
  numerico[letra] <- equivalencias[x[letra]]
  numerico
}


# Convertir letras a numeros 
colibries$altitud <- (colibries$`Elevational Range` + colibries$`Elevational Range`) / 2

convertir_altitud <- function(x) {
  x <- trimws(as.character(x))
  equivalencias <- c(L = 250, F = 750, M = 1500)
  numerico <- suppressWarnings(as.numeric(x))
  letra <- x %in% names(equivalencias)
  numerico[letra] <- equivalencias[x[letra]]
  numerico
}

ampL <- convertir_altitud(colibries$`Elevational Range`)
#x_max <- convertir_altitud(colibries$"Xmax")
#altitud <- (x_min + x_max) / 2
altitud <- (ampL)/2

colibries$"Scientific Name" <- colibries$`Latin (BirdLife > IOC > Clements>AviList)`

tabla <- data.frame(
  especie = colibries$`Scientific Name`,
  log_masa = log_masa,
  altitud_km = altitud / 1000,
  stringsAsFactors = FALSE
)

tabla

# --- DECISION 5. Que arbol ---------------------------------------------------
# McGuire et al. 2014, rtrees con Jetz et al. 2012, megatree de McTavish 2025
# Y si usas uno solo o una muestra de la posterior

arbol <- read.tree("datos/arboles/McTavish.tre")


# --- DECISIÓN 6. Politomías y ramas de longitud 0 



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
file.path(salida, "analisis.R")

# Ahora copiamos el template de decisiones.csv a tu capeta. No olvides llenarlo
file.path(salida, "decisiones.csv")

# Copia a tu path y modifica el README.md
file.path(salida, "README.md")
file.path("equipos/bogota-equipo00/README.md")


