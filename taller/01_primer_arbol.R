# Dia 1, bloque 1. Tu primer arbol de colibries
# Corre este script completo. Todavia no importa entender cada linea

library(ape)

arbol <- read.tree("datos/arboles/McTavish.tre") #dirección relativa al repositorio
#Las direcciones relativas se priorizan, las absolutas mejor no usarlas.
#asigno los datos del árbol de McTavish dentro del objeto arbol
arbol
arbol$tip.label #$extraer del arbol

plot(arbol, cex = 0.3, no.margin = TRUE)

# En abanico se ven mejor las especies
plot(arbol, type = "fan", cex = 0.25, no.margin = TRUE)
?plot.phylo()
#Ayuda para ingresar a cada una de las funciones y argumentos

# Eso que acabas de graficar es la historia evolutiva de los colibries
Ntip(arbol)

# Y estas son cinco de sus especies
head(arbol$tip.label, 5)
?head

tail(arbol$tip.label)

# El resto de los dos dias es aprender a trabajar con esto

#Vectores
#Para sacar el primer elemento del vector=[1]
#Esto es un vector pues tiene direccionalidad
arbol$tip.label[1]
arbol$tip.label[20]
arbol$tip.label[1:10]

#dataframes: es un compendio de vectores
#Pasar a fundamentos de r 02
