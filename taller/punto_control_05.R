# Punto de control despues del bloque 5. Arbol y datos emparejados
# Cargalo si te atrasaste y sigue con el resto del grupo

library(ape)

if (!file.exists("datos/colibries_limpio.csv")) source("taller/puntos_control/punto_control_03.R")

arbol <- read.tree("datos/arboles/McTavish.tre")
datos <- read.csv("datos/colibries_limpio.csv", stringsAsFactors = FALSE)

datos$especie_arbol <- gsub(" ", "_", datos$especie)
en_ambos <- intersect(arbol$tip.label, datos$especie_arbol)

arbol <- drop.tip(arbol, setdiff(arbol$tip.label, en_ambos))
datos <- datos[datos$especie_arbol %in% en_ambos, ]

rownames(datos) <- datos$especie_arbol
datos <- datos[arbol$tip.label, ]

stopifnot(all(rownames(datos) == arbol$tip.label))

save(arbol, datos, file = "datos/colibries_listo.RData")

cat("Listo.", Ntip(arbol), "especies en el arbol y en la matriz\n")
