# Dia 1, bloque 5. Filogenias

library(ape)

arbol <- read.tree("datos/arboles/McTavish.tre")

# Un objeto phylo es una lista con cuatro partes
class(arbol)
names(arbol)

Ntip(arbol)
Nnode(arbol)
head(arbol$tip.label)
head(arbol$edge)
head(arbol$edge.length)

# Graficar
plot(arbol, cex = 0.3, no.margin = TRUE)
plot(ladderize(arbol), cex = 0.3, no.margin = TRUE)
plot(arbol, type = "fan", cex = 0.25, no.margin = TRUE)

axisPhylo()

# Revisar el arbol antes de confiar en el
is.rooted(arbol)
is.binary(arbol)
is.ultrametric(arbol)
any(arbol$edge.length == 0)

# Politomias. Varias funciones exigen un arbol binario
is.binary(arbol)
arbol_resuelto <- multi2di(arbol)
is.binary(arbol_resuelto)

# Quitar y conservar especies
sin_patagona <- drop.tip(arbol, "Patagona_gigas")
Ntip(sin_patagona)

# Distancias entre especies segun el arbol
distancias <- cophenetic(arbol)
distancias[1:4, 1:4]

# Un subarbol
un_genero <- arbol$tip.label[grepl("^Coeligena", arbol$tip.label)]
plot(keep.tip(arbol, un_genero), no.margin = TRUE)


# --- Empatar arbol y datos ---------------------------------------------------

datos <- read.csv("datos/colibries_limpio.csv", stringsAsFactors = FALSE)

# El arbol usa guion bajo, la matriz usa espacio
head(arbol$tip.label)
head(datos$especie)

datos$especie_arbol <- gsub(" ", "_", datos$especie)

en_ambos <- intersect(arbol$tip.label, datos$especie_arbol)
length(en_ambos)

# Los que no empataron hay que mirar porque, no ignorarlos
setdiff(datos$especie_arbol, arbol$tip.label)
setdiff(arbol$tip.label, datos$especie_arbol)

arbol <- drop.tip(arbol, setdiff(arbol$tip.label, en_ambos))
datos <- datos[datos$especie_arbol %in% en_ambos, ]

# Uno de los pasos mas importantes del taller
rownames(datos) <- datos$especie_arbol
datos <- datos[arbol$tip.label, ]

# Si esto no da TRUE, todo lo que sigue esta mal y R no te va a avisar
all(rownames(datos) == arbol$tip.label)

geiger::name.check(arbol, datos)

# Tambien pudimos haber usado treedata() en geiger :) revisalo

save(arbol, datos, file = "datos/colibries_listo.RData")
