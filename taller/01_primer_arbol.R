# Dia 1, bloque 1. Tu primer arbol de colibries
# Corre este script completo. Todavia no importa entender cada linea

library(ape)

arbol <- read.tree("datos/arboles/McTavish.tre")

arbol

plot(arbol, cex = 0.3, no.margin = TRUE)

# En abanico se ven mejor las especies
plot(arbol, type = "fan", cex = 0.25, no.margin = TRUE)

# Eso que acabas de graficar es la historia evolutiva de los colibries
Ntip(arbol)

# Y estas son cinco de sus especies
head(arbol$tip.label, 5)

# El resto de los dos dias es aprender a trabajar con esto
