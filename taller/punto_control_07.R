# Punto de control despues del bloque 7. Modelo ajustado
# Cargalo si te atrasaste y sigue con el resto del grupo

library(ape)
library(nlme)

if (!file.exists("datos/colibries_listo.RData")) source("taller/puntos_control/punto_control_05.R")

load("datos/colibries_listo.RData")

datos$log_masa <- log(datos$masa)
datos$altitud_km <- datos$altitud / 1000
datos$especie_arbol <- rownames(datos)

if (!is.binary(arbol)) arbol <- multi2di(arbol)
arbol$edge.length[arbol$edge.length == 0] <- 1e-8

modelo <- gls(log_masa ~ altitud_km,
              correlation = corPagel(1, phy = arbol, form = ~especie_arbol),
              data = datos, method = "ML")

print(summary(modelo))

cat("Listo. Tienes el objeto modelo en tu sesion\n")
