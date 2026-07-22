# Dia 2, bloque 3. Tu proyecto
# Copia este archivo a equipos/tu-ciudad-tu-equipo/analisis.R y trabaja alli
# Cada DECISION es una fila de decisiones.csv. Escribela cuando la tomes

#### Proyecto final - Curso MacroData (COLEVOL 2026) #####
# Zabala-Cruz M. C.() 1 & Quintero J.J.2
# 1 Universidad Industrial de Santander
# 2 Universidad del Quindio

# --- Sobre el proyecto -----------------------------------------
# Especificaciones del equipo #
sessionInfo()

# Rama creada para el proyecto #
salida <- "equipos/bogota-equipo11"
dir.create(salida, recursive = TRUE, showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), file.path(salida, "sessionInfo.txt"))

equipo <- "bogota-equipo11" 
salida <- file.path("equipos", equipo)

# Retribucion de datos del repositorio
datos <- read.csv("datos/birdbase/birdbase.csv", check.names = FALSE, stringsAsFactors = FALSE)

# Listado de los datos disponibles
list.files("datos", recursive = TRUE) #Recursive muestra los datos NO modificables

#Librerias usadas
library(ape)
library(nlme)


#Filtrado de datos de Trochilidae (Colibries)
colibries <- datos[datos$`Family IOC 15.1` == "Trochilidae", ]


machos$na.rm=TRUE
# --- DECISION 1. Que columna de masa -----------------------------------------
# Se uso la columna de Male MaxMax, es decir que solo se tomaron en cuenta los 
# valores extremos en masa de los machos ♂ 
masa <- colibries$`Male MaxMass`
masa
str(masa)
# Contamos cuantos datos faltantes hay [195 de 366 datos faltan]
sum(is.na(masa))

fixedtabla <- na.omit(tabla[,])# nos quedan 55 de 366 datos sin NA de m y h max

# --- DECISION 2. Transformar o no --------------------------------------------
# Sin transformar, logaritmo, raiz cuadrada
shapiro.test(fixedtabla$masa)
shapiro.test(fixedtabla$altitud_km) # solo este cumple la normalidad
#entonces normalizamos la masa con Log
fixmass <- log(fixedtabla$masa)


# --- DECISION 3. Como resumir la altitud -------------------------------------
# NormMin, NormMax, punto medio del rango normal, punto medio de Xmin y Xmax
# Se selecciono Xmax y se transformo en km

altitud <-  colibries$Xmax

colibries$"Scientific Name" <- colibries$`Latin (BirdLife > IOC > Clements>AviList)`

tabla <- data.frame(
  especie = colibries$`Scientific Name`,
  masa = masa,
  altitud_km = altitud / 1000,
  stringsAsFactors = FALSE
)

# --- DECISION 4. Especies con L, F o M ---------------------------------------
# Descartar, convertir al punto medio, imputar, tratar como categorica
# La elevacion se mantuvo como un dato continuo, por ende no se categorizo


#Eliminar los NA de Altitud y de Masa
sum(is.na(masa))
sum(is.na(altitud))

tabla[!is.na(tabla$masa),] & tabla[!is.na(tabla$altitud_km),]


colibries[!is.na(colibries$altitud),]

# --- DECISION 5. Que arbol ---------------------------------------------------
# Se eligio el megatree de McTavish 2025
arbol <- read.tree("datos/arboles/McTavish.tre")
plot(arbol, cex = 0.5)
arbol                   # Nuestro arbol esta:
is.binary(arbol)         # Binario
is.rooted(arbol)         # Enraizado
is.ultrametric(arbol)    # No ultrametrico

distancias <- cophenetic(arbol)

fixedtabla$especie <- gsub(" ", "_", arbol$tip.label)


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


