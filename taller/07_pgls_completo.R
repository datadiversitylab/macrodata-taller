# Dia 2, bloque 2. PGLS de principio a fin
# Este es el ejemplo guiado. En la tarde haces el tuyo con otras decisiones

library(ape)
library(nlme)

load("datos/colibries_listo.RData")

datos$log_masa <- log(datos$masa)
datos$altitud_km <- datos$altitud / 1000

all(rownames(datos) == arbol$tip.label)


# --- Por que no basta con lm -------------------------------------------------

# Una regresion normal asume que las especies son independientes
normal <- lm(log_masa ~ altitud_km, data = datos)
summary(normal)

# Los residuales tienen senal filogenetica, y eso es justo lo que no puede pasar
phytools::phylosig(arbol, setNames(residuals(normal), rownames(datos)),
                   method = "lambda", test = TRUE)


# --- El arbol tiene que estar listo ------------------------------------------

if (!is.binary(arbol)) arbol <- multi2di(arbol)
arbol$edge.length[arbol$edge.length == 0] <- 1e-8


# --- PGLS --------------------------------------------------------------------

datos$especie_arbol <- rownames(datos)

# Browniano. Asume lambda igual a uno
pgls_bm <- gls(log_masa ~ altitud_km,
               correlation = corBrownian(phy = arbol, form = ~especie_arbol),
               data = datos, method = "ML")

# Lambda de Pagel. Estima cuanta senal hay en los residuales
pgls_lambda <- gls(log_masa ~ altitud_km,
                   correlation = corPagel(1, phy = arbol, form = ~especie_arbol),
                   data = datos, method = "ML")

summary(pgls_lambda)
confint(pgls_lambda)

# Lambda estimada
coef(pgls_lambda$modelStruct$corStruct, unconstrained = FALSE)

# Comparar las tres versiones. Fijate en como cambia el valor p
AIC(normal, pgls_bm, pgls_lambda)

summary(normal)$coefficients["altitud_km", ]
summary(pgls_bm)$tTable["altitud_km", ]
summary(pgls_lambda)$tTable["altitud_km", ]


# --- Revisar el ajuste -------------------------------------------------------

res <- residuals(pgls_lambda, type = "normalized")
qqnorm(res); qqline(res)
plot(fitted(pgls_lambda), res, pch = 16, col = rgb(0, 0, 0, 0.4))
abline(h = 0, col = "gray50")


# --- Graficar el resultado ---------------------------------------------------

plot(datos$altitud_km, datos$log_masa, pch = 16, col = rgb(0, 0, 0, 0.4),
     xlab = "Altitud (km)", ylab = "log masa (g)")
abline(coef(normal), lty = 2, col = "gray50")
abline(coef(pgls_lambda), lwd = 2)
legend("topleft", c("lm", "PGLS"), lty = c(2, 1), lwd = c(1, 2), bty = "n")

# Esa distancia entre las dos lineas es el efecto de la filogenia
