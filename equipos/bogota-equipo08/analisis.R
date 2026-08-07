# Dia 2, bloque 3. Proyecto de equipo
# Pregunta: la altitud predice la masa corporal en Trochilidae
# Correr desde la raiz del repositorio con una sesion limpia de R

library(msm)
library(ape)
library(nlme)

equipo <- "bogota-equipo08"
salida <- file.path("equipos", equipo)

# Fijamos la semilla al inicio, antes de cualquier paso aleatorio (imputacion
# de masa con rtnorm y resolución de politomias con multi2di más adelante) para
# garantizar la reproducibilidad de este script
set.seed(8)

# 1. Datos de rasgos ------------------------------------------------------

datos <- read.csv("datos/birdbase/data.csv", check.names = FALSE, stringsAsFactors = FALSE)

# La columna "Xmax" es la ultima columna con datos utiles del archivo;
# ubicamos su indice y cortamos todo lo que viene despues
idx_xmax <- which(names(datos) == "Xmax")

if (length(idx_xmax) != 1) {
  stop("No se encontro (o se encontro más de una vez) la columna 'Xmax'; ",
       "revisa el archivo de datos antes de continuar.")
}

datos <- datos[, 1:idx_xmax]

# Nos quedamos solo con la familia Trochilidae (en este caso, todos los
# registros del archivo pertenecen a esta familia)
colibries <- datos[datos$`Family IOC 15,1` == "Trochilidae", ]

# Reservamos columnas para la masa imputada. Solo "rtnorm_masa" se usa en el
# analisis actual; rtnorm_masa2..10 quedan disponibles por si más adelante
# decidimos propagar la incertidumbre de la imputacion repitiendo el analisis
# para cada uno de los 10 sorteos
colibries$rtnorm_masa <- NA
colibries$rtnorm_masa2 <- NA
colibries$rtnorm_masa3 <- NA
colibries$rtnorm_masa4 <- NA
colibries$rtnorm_masa5 <- NA
colibries$rtnorm_masa6 <- NA
colibries$rtnorm_masa7 <- NA
colibries$rtnorm_masa8 <- NA
colibries$rtnorm_masa9 <- NA
colibries$rtnorm_masa10 <- NA

# 2. Imputacion de masa (Decision 1) ---------------------------------------

# Imputamos la masa muestreando de una normal truncada entre la masa minima y
# maxima reportada para hembras. Generamos 10 sorteos para dejar la puerta
# abierta a propagar esta incertidumbre más adelante.
colibries$rtnorm_masa <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa2 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa3 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa4 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa5 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa6 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa7 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa8 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa9 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)
colibries$rtnorm_masa10 <- rtnorm(nrow(colibries), mean = 0, sd = 1, lower = colibries$`Female MinMass`, upper = colibries$`Female MaxMass`)

masa <- colibries$rtnorm_masa

# 3. Transformacion de la masa (Decision 2) --------------------------------

# Transformamos la masa con raiz cubica ya que esta transformacion mantiene 
# la relacion conocida entre masa y volumen corporal en aves
raiz3_masa <- (masa)^(1/3)

# 4. Como resumir la altitud (Decision 3) ----------------------------------

# Usamos la altitud normal maxima (NormMax) porque en aves pequeñas como los
# colibries la densidad del aire es determinante para el costo energetico del
# vuelo
altitud <- colibries$NormMax

# 5. Especies con codigos L, F o M (Decision 4) ----------------------------

# NormMax no presentaba letras que requirieran conversion, así que no fue 
# necesario un paso de limpieza adicional aquí

colibries$"Scientific Name" <- colibries$`Latin (BirdLife > IOC > Clements>AviList)`

tabla <- data.frame(
  especie = colibries$`Scientific Name`,
  masa = raiz3_masa,
  altitud_norm_max = altitud / 1000, # convertida a km
  stringsAsFactors = FALSE
)

# 6. Arbol (Decision 5) -----------------------------------------------------

# Usamos una muestra de 100 arboles especificos para nuestras especies,
# descargados desde BirdTree (Jetz et al., 2012)
arbol <- read.nexus("datos/arboles/output.nex")
class(arbol)       # "multiPhylo": una LISTA de 100 arboles, no un arbol individual
length(arbol)      # 100

# Verificamos que los 100 arboles comparten el mismo set de especies 
mismo_set_inicial <- all(sapply(arbol, function(x) setequal(x$tip.label, arbol[[1]]$tip.label)))
mismo_set_inicial  # deberia ser TRUE

# 7. Emparejamiento de nombres (Decision 7) --------------------------------

# Empatamos el arbol y la tabla de datos por nombre exacto de especie
tabla$especie_arbol <- gsub(" ", "_", tabla$especie)

# Como los 100 arboles comparten el mismo set de especies (verificado arriba),
# usamos arbol[[1]] como referencia unica y consistente para el resto del script
en_ambos <- intersect(arbol[[1]]$tip.label, tabla$especie_arbol)
no_empataron <- setdiff(tabla$especie_arbol, arbol[[1]]$tip.label)
write.csv(data.frame(especie = no_empataron), file.path(salida, "nombres_sin_empatar.csv"), row.names = FALSE)

# 8. Especies excluidas (Decision 8) ---------------------------------------

# Los arboles recuperados solo contenian 261 especies de las que hay en
# nuestros datos; nos quedamos con la interseccion
tabla <- tabla[!is.na(tabla$masa) & !is.na(tabla$altitud_norm_max), ]
tabla <- tabla[tabla$especie_arbol %in% en_ambos, ]
rownames(tabla) <- tabla$especie_arbol

tips_a_quitar <- setdiff(arbol[[1]]$tip.label, tabla$especie_arbol)
arbol <- lapply(arbol, function(x) drop.tip(x, tips_a_quitar))
class(arbol) <- "multiPhylo"

# Revisamos, arbol por arbol, si el conjunto de puntas coincide exactamente
# con las especies que quedan en la tabla, sin importar el orden. mismo_set_final
# guarda un unico valor logico: TRUE solo si los 100 arboles pasan la prueba
mismo_set_final <- all(sapply(arbol, function(x) setequal(x$tip.label, tabla$especie_arbol)))
mismo_set_final # sin este TRUE nada de lo que sigue sirve

# Además verificamos que no hayan datos vaciós para la masa ni para la altitud
sum(is.na(tabla$masa))
sum(is.na(tabla$altitud_norm_max))

# 9. Politomias y ramas de longitud cero (Decision 6) ----------------------

# Al tener un objeto multiPhylo (100 arboles), aplicamos el reemplazo de
# longitudes arbol por arbol con lapply y restauramos despues la clase
# multiPhylo
arbol <- lapply(arbol, function(x) {
  x$edge.length[x$edge.length == 0] <- 1e-8
  x
})
class(arbol) <- "multiPhylo"

# Verificamos con sapply que no queden longitudes en cero y que la minima sea
# mayor que 0 en los 100 arboles
sapply(arbol, function(x) c(quedan_ceros = any(x$edge.length == 0), min_long = min(x$edge.length)))

# Repetimos la verificacion de politomias y longitudes cero inmediatamente
# antes de ajustar los modelos, para garantizar que ningun paso anterior
# haya reintroducido politomias o ceros
if (!all(sapply(arbol, is.binary))) {
  arbol <- lapply(arbol, multi2di)
  class(arbol) <- "multiPhylo"
}

arbol <- lapply(arbol, function(x) {
  x$edge.length[x$edge.length == 0] <- 1e-8
  x
})
class(arbol) <- "multiPhylo"

any(sapply(arbol, function(x) any(x$edge.length == 0)))  # debe dar FALSE
all(sapply(arbol, is.binary))                             # debe dar TRUE

# 10. Modelo (Decision 9) ---------------------------------------------------

# Comparamos un modelo Browniano (corBrownian) contra un modelo OU (corMartins)
# en cada uno de los 100 arboles, sin error de medicion explicito. En cada
# arbol nos quedamos con el modelo de menor AIC y al final reportamos cuantos
# de los 100 "prefirieron" cada uno
resultados_100 <- vector("list", length(arbol))

# El parametro alpha de corMartins representa la fuerza de atraccion hacia un
# optimo evolutivo, y la escala apropiada para buscarlo varia de un arbol a
# otro: las longitudes de rama van de ~0.0001 a ~27 entre los 100 arboles de
# la muestra. Por eso probamos varios valores iniciales de alpha, de menor a
# mayor, y nos quedamos con el primer ajuste que converge para cada arbol. Si
# ninguno converge, la funcion devuelve NULL y ese arbol usa Browniano como
# respaldo 
ajustar_ou <- function(arbol_i, tabla_i, valores_iniciales = c(1, 0.1, 0.01, 0.001, 5, 10)) {
  for (v in valores_iniciales) {
    modelo <- tryCatch(
      gls(masa ~ altitud_norm_max,
          correlation = corMartins(v, phy = arbol_i, form = ~especie_arbol),
          data = tabla_i, method = "ML"),
      error = function(e) NULL
    )
    if (!is.null(modelo)) return(modelo)
  }
  NULL
}

# Ajustamos cada modelo con su propio tryCatch en vez de uno compartido:
# corMartins es numericamente inestable y puede fallar por completo en un
# arbol donde el Browniano si ajusta bien. Con un tryCatch compartido se
# perdería el arbol entero (incluido el Browniano) cuando fallaba el OU. Ahora,
# si corMartins falla para un arbol, ese arbol se queda con el resultado del
# modelo Browniano (marcado como tal); solo se omite el arbol si fallan los
# dos modelos
for (i in seq_along(arbol)) {
  arbol_i <- arbol[[i]]
  tabla_i <- tabla[arbol_i$tip.label, ]  # reordena la tabla al orden de puntas de este arbol
  
  modelo_browniano <- tryCatch(
    gls(masa ~ altitud_norm_max,
        correlation = corBrownian(1, phy = arbol_i, form = ~especie_arbol),
        data = tabla_i, method = "ML"),
    error = function(e) {
      message("Arbol ", i, ": Browniano no ajusto (", conditionMessage(e), ").")
      NULL
    }
  )
  
  modelo_ou <- tryCatch(
    ajustar_ou(arbol_i, tabla_i),
    error = function(e) {
      message("Arbol ", i, ": OU no ajusto (", conditionMessage(e), ").")
      NULL
    }
  )
  if (is.null(modelo_ou)) {
    message("Arbol ", i, ": OU no ajusto con ningun valor inicial probado.")
  }
  
  if (is.null(modelo_browniano) && is.null(modelo_ou)) {
    # Fallaron los dos modelos: no hay nada que reportar para este arbol
    message("Arbol ", i, ": fallaron ambos modelos; se omite por completo.")
    resultados_100[[i]] <- NULL
    next
  } else if (!is.null(modelo_browniano) && !is.null(modelo_ou)) {
    # Caso normal: comparamos los dos por AIC
    aic_browniano <- AIC(modelo_browniano)
    aic_ou        <- AIC(modelo_ou)
    gana_ou       <- aic_ou < aic_browniano
    modelo_i      <- if (gana_ou) modelo_ou else modelo_browniano
    etiqueta      <- if (gana_ou) "OU (corMartins)" else "Browniano (corBrownian)"
  } else if (!is.null(modelo_browniano)) {
    # Solo ajusto el Browniano: lo usamos y dejamos constancia de que OU fallo
    modelo_i      <- modelo_browniano
    aic_browniano <- AIC(modelo_browniano)
    aic_ou        <- NA
    etiqueta      <- "Browniano (corBrownian) -- OU no convergio"
  } else {
    # Solo ajusto el OU: lo usamos y dejamos constancia de que Browniano fallo
    modelo_i      <- modelo_ou
    aic_ou        <- AIC(modelo_ou)
    aic_browniano <- NA
    etiqueta      <- "OU (corMartins) -- Browniano no convergio"
  }
  
  coef_i <- summary(modelo_i)$tTable
  ic_i   <- confint(modelo_i)
  
  resultados_100[[i]] <- data.frame(
    arbol_id       = names(arbol)[i],
    modelo_ganador = etiqueta,
    aic_browniano  = aic_browniano,
    aic_ou         = aic_ou,
    intercepto     = coef_i["(Intercept)", "Value"],   # se guarda para graficar la linea despues
    estimado       = coef_i["altitud_norm_max", "Value"],
    error_estandar = coef_i["altitud_norm_max", "Std.Error"],
    ic_inferior    = ic_i["altitud_norm_max", 1],
    ic_superior    = ic_i["altitud_norm_max", 2],
    valor_p        = coef_i["altitud_norm_max", "p-value"]
  )
}

# OU convergio en 91/100 arboles; en los 9 restantes (ids: 11, 29, 39, 52,
# 64, 86, 91, 92, 94) no convergio con ninguno de los valores iniciales de alpha
# probados, y se uso Browniano como respaldo

# Esta linea se corre una sola vez, justo despues del loop: nunca más de una
# vez, porque se sobrescribe y se daña por completo
resultados_100 <- do.call(rbind, resultados_100)

# Cuantos de los 100 arboles prefirieron cada modelo
table(resultados_100$modelo_ganador)

# 11. Resultados ------------------------------------------------------------

# Resumimos el efecto de altitud promediando estimado, error estandar, IC y
# valor p entre los arboles que logramos modelar. Promediar valores p es una
# simplificacion (no es estadisticamente riguroso combinar p-values así), pero
# lo dejamos como un resumen descriptivo

n_arboles_modelados <- nrow(resultados_100)
n_ou_convergio       <- sum(!is.na(resultados_100$aic_ou))
n_ou_gano            <- sum(resultados_100$modelo_ganador == "OU (corMartins)")
n_solo_browniano     <- sum(resultados_100$modelo_ganador == "Browniano (corBrownian) -- OU no convergio")

resultados <- data.frame(
  equipo = equipo,
  respuesta = "raiz cubica de la masa corporal (g^(1/3))",
  predictor = "Altitud normal maxima",
  estimado = mean(resultados_100$estimado),
  error_estandar = mean(resultados_100$error_estandar),
  ic_inferior = mean(resultados_100$ic_inferior),
  ic_superior = mean(resultados_100$ic_superior),
  valor_p = mean(resultados_100$valor_p),
  n = nrow(tabla),
  modelo = paste0(
    "gls con seleccion por AIC entre Browniano y OU (corMartins) cuando ambos ",
    "convergieron, sin error de medicion; de ", n_arboles_modelados,
    " arboles de BirdTree modelados (de 100 intentados), OU convergio en ",
    n_ou_convergio, "/", n_arboles_modelados, " y gano por AIC en ", n_ou_gano,
    " de esos; en los otros ", n_solo_browniano,
    " arboles OU no convergio (ni con los valores iniciales de alpha probados) ",
    "y se uso Browniano como respaldo"
  ),
  arbol = "muestra de 100 arboles de BirdTree",
  stringsAsFactors = FALSE
)
write.csv(resultados, file.path(salida, "resultados.csv"), row.names = FALSE)
write.csv(resultados_100, file.path(salida, "resultados_100_arboles.csv"), row.names = FALSE)  # detalle arbol por arbol

# 12. Figura ------------------------------------------------------------------

# La linea de ajuste usa el intercepto y la pendiente promediados entre los
# arboles que logramos modelar. 

# Agregamos un intervalo de confianza (95%) para la linea de tendencia. 
# Probamos tres formas y nos quedamos con la tercera:
#   1) Intercepto fijo (promedio) y solo la pendiente variando entre
#      ic_inferior/ic_superior
#   2) Dos arboles "extremos" (percentil 2.5/97.5 de pendiente) dibujados
#      como rectas completas
#   3) Banda analitica basada en la formula estandar de la varianza de una
#      recta de regresion: Var(y_pred(x)) = Var(a) + x^2*Var(b) + 2x*Cov(a,b).
#      Normalmente esta formula usa la varianza/covarianza del intercepto (a)
#      y la pendiente (b) de un solo modelo; aqui usamos en su lugar la
#      varianza/covarianza de "intercepto" y "estimado" ENTRE LOS ARBOLES
#      (resultados_100), porque lo que queremos representar es precisamente
#      la incertidumbre filogenetica (cuanto cambia la relacion segun el
#      arbol), no el error de muestreo de un unico ajuste. Esto da, de forma
#      correcta, una banda curva (más angosta cerca del centro de los datos,
#      más ancha en los extremos)
var_intercepto   <- var(resultados_100$intercepto)
var_pendiente    <- var(resultados_100$estimado)
cov_int_pend     <- cov(resultados_100$intercepto, resultados_100$estimado)

alt_grid   <- seq(min(tabla$altitud_norm_max), max(tabla$altitud_norm_max), length.out = 100)
pred_media <- mean(resultados_100$intercepto) + mean(resultados_100$estimado) * alt_grid
pred_se    <- sqrt(var_intercepto + (alt_grid^2) * var_pendiente + 2 * alt_grid * cov_int_pend)
pred_baja  <- pred_media - 1.96 * pred_se
pred_alta  <- pred_media + 1.96 * pred_se

png(file.path(salida, "figura.png"), width = 1400, height = 1200, res = 200)
par(mar = c(5, 4, 4, 2) + 0.1)

plot(tabla$altitud_norm_max, tabla$masa, pch = 16, col = rgb(0, 0, 0, 0.5),
     xlab = "Altitud (km)", ylab = "Masa corporal, raiz cubica (g^(1/3))", main = equipo)

lines(alt_grid, pred_media, lwd = 2)
lines(alt_grid, pred_baja, lwd = 1, lty = 2, col = "grey40")
lines(alt_grid, pred_alta, lwd = 1, lty = 2, col = "grey40")

legend("topleft", legend = c("Pendiente promedio", "IC 95% (entre árboles)"),
       lty = c(1, 2), lwd = c(2, 1), col = c("black", "grey40"),
       bty = "n", cex = 0.8)

dev.off()

# 13. Sesion --------------------------------------------------------------------

writeLines(capture.output(sessionInfo()), file.path(salida, "sessionInfo.txt"))

# Guardamos este archivo como analisis.R en nuestra carpeta de trabajo
message("Recuerda guardar este archivo como: ", file.path(salida, "analisis.r"))

# Copiamos el template de decisiones.csv a nuestra carpeta -- no olvidamos llenarlo
message("Recuerda llenar y copiar: ", file.path(salida, "decisiones.csv"))
