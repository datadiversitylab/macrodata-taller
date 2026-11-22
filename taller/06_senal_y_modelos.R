# Dia 2, bloque 1. Señal filogenetica y modelos de evolucion


# lambda es para caracteres continuos- no hay modelo
#k de blomerg discretos (cuanto se parecen las especies emparentadas)-si hay modelo 
#el arbol debe tener tiempo calibrado y los terminales son especies 
#el arbol puede ser morfologia, evidencia total, molecular etc 
#en ornstein- unlenbeck tiene un optimo (selección estabilizadora) mientras que lambda no 
# early burst  cambios fuertes y luego éstasis (estabilizador en los cambios de los caracteres)
# ci = BIC o BIC o CIC 

library(ape)
library(geiger)
library(phytools)

load("datos/colibries_listo.RData")

all(rownames(datos) == arbol$tip.label)

datos$log_masa <- log(datos$masa)

# phylosig necesita un vector con nombres, no una columna suelta
masa <- setNames(datos$log_masa, rownames(datos))
altitud <- setNames(datos$altitud, rownames(datos))


# --- Señal filogenetica ------------------------------------------------------

# Cuanto se parecen las especies emparentadas
phylosig(arbol, masa, method = "lambda", test = TRUE)
phylosig(arbol, masa, method = "K", test = TRUE)

phylosig(arbol, altitud, method = "lambda", test = TRUE)

# Lambda va de 0 a 1. Uno es movimiento browniano puro, cero es ninguna señal

# Verlo ayuda mas que el numero
contMap(arbol, masa, fsize = 0.3)


# --- Modelos de evolucion de caracteres continuos ----------------------------

# Movimiento browniano. Deriva al azar
bm <- fitContinuous(arbol, masa, model = "BM")

# Ornstein-Uhlenbeck. Hay un optimo que atrae al rasgo
ou <- fitContinuous(arbol, masa, model = "OU")

# Early burst. Los cambios grandes ocurrieron temprano en la radiacion
eb <- fitContinuous(arbol, masa, model = "EB")

bm$opt$aicc
ou$opt$aicc
eb$opt$aicc

comparacion <- data.frame(
  modelo = c("BM", "OU", "EB"),
  aicc = c(bm$opt$aicc, ou$opt$aicc, eb$opt$aicc)
)
comparacion[order(comparacion$aicc), ]

# Diferencias de AICc menores a 2 no distinguen entre modelos


# --- Reconstruccion del estado ancestral -------------------------------------

ancestral <- ace(masa, arbol, type = "continuous")
head(ancestral$ace)

plot(arbol, cex = 0.25, no.margin = TRUE)
nodelabels(round(ancestral$ace, 1), cex = 0.3, frame = "none")


# --- Contrastes independientes -----------------------------------------------

# La forma mas intuitiva de ver que hace la correccion filogenetica
cm <- pic(masa, multi2di(arbol))
ca <- pic(altitud, multi2di(arbol))

plot(ca, cm, pch = 16, xlab = "Contrastes de altitud", ylab = "Contrastes de log masa")
abline(h = 0, v = 0, col = "gray70")

# La regresion sobre contrastes va sin intercepto
summary(lm(cm ~ ca - 1))
regresion <- lm(cm ~ ca - 1)
r <- plot(regresion) 



library(ggplot2)

ggplot(datos) +
 aes(x = altitud, y = masa) +
 geom_smooth(method = "lm", se = TRUE, colour = "#112446") +
 geom_point(colour = "#112446") +
 scale_y_continuous(trans = "log") +
 labs(title = "regresión") +
 theme_minimal()

