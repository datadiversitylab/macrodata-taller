# Dia 2, bloque 3. Tu proyecto
# Copia este archivo a equipos/tu-ciudad-tu-equipo/analisis.R y trabaja alli
# Cada DECISION es una fila de decisiones.csv. Escribela cuando la tomes

library(msm)
library(ape)
library(nlme)

# Ciudades: bogota, barranquilla, pasto, quibdo
equipo <- "bogota-equipo08" # Tienes que modificar la ciudad y el numero
salida <- file.path("equipos", equipo)

datos <- read.csv("datos/birdbase/data.csv", check.names = FALSE, stringsAsFactors = FALSE)

idx_xmax <- which(names(datos) == "Xmax")

# Debido a que la columna "Xmax" era la ultima columna con datos, buscamos que número de 
# columna representa y cortamos el resto
if (length(idx_xmax) != 1) {
  stop("No se encontro (o se encontro mas de una vez) la columna 'Xmax'; ",
       "revisa el archivo de datos antes de continuar.")
}

datos <- datos[, 1:idx_xmax]

# Pasamos ahora a seleccionar los datos pertenecientes solamente a la familia "Throchilidae"
# (En este caso, todos los datos pertenecen a esta familia)
colibries <- datos[datos$`Family IOC 15,1` == "Trochilidae", ]

# Antes de continuar, debido a la decision tomada para seleccionar una masa,
# es necesario agregar estas columnas a colibries; mas adelante explicamos
# el por que.
# NOTA: solo "rtnorm_masa" se usa en el analisis actual. Las columnas
# rtnorm_masa2..10 quedan disponibles por si mas adelante como equipo decidimos
# propagar la incertidumbre de la imputacion de masa (por ejemplo, repitiendo
# todo el analisis para cada uno de los 10 sorteos, igual que lo hicimos con los
# 100 arboles).
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

# --- DECISION 1. Que columna de masa -----------------------------------------
# EXPLICACIÓN DE LA DECISION

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

# --- DECISION 2. Transformar o no --------------------------------------------
# En este caso optamos por transformar nuestros datos de masa generados anteriormente
# con una raíz cubica. Está comprobado que en las aves transformar la masa a partir de una
# raíz cubica mantiene la relación que esta tiene con el volumen del individuo.

raiz3_masa <- (masa)^(1/3)

# --- DECISION 3. Como resumir la altitud -------------------------------------
# En este caso decidimos utilizar la altura normal maxima debido a que se ha demostrado que
# en las aves pequeñas como los colibries, la densidad del aire es importante a la hora
# de optimizar el uso de energía que estos invierten en el vuelo, a medida que más suben en
# altitud más costoso se vuelve volar en terminos de consumo de energía 

altitud <- colibries$NormMax

# --- DECISION 4. Especies con L, F o M ---------------------------------------
# NormMax no presentaba letras

colibries$"Scientific Name" <- colibries$`Latin (BirdLife > IOC > Clements>AviList)`

tabla <- data.frame(
  especie = colibries$`Scientific Name`,
  masa = raiz3_masa,
  altitud_norm_max = altitud / 1000, #convertida a km
  stringsAsFactors = FALSE
)

# --- DECISION 5. Que arbol ---------------------------------------------------
# 100 arboles especificos para nuestras especies descargados a partir de la
# herramienta BirdTree

arbol <- read.nexus("datos/arboles/output1.nex")
class(arbol)       # "multiPhylo" porque es una LISTA de 100 arboles, no un arbol individual
length(arbol)      # 100

# Verificar que los 100 comparten el mismo set de especies (deberian, al venir de
# la misma consulta a BirdTree). Si esto da FALSE, nada de lo que sigue aplica
# igual para los 100 y hay que investigar antes de continuar.

mismo_set_inicial <- all(sapply(arbol, function(x) setequal(x$tip.label, arbol[[1]]$tip.label)))
mismo_set_inicial  # deberia ser TRUE

# --- DECISION 7. Como empatar los nombres ------------------------------------
# En este caso usamos los nombres exactos para empatar el arbol y los datos

tabla$especie_arbol <- gsub(" ", "_", tabla$especie)
# Como los 100 arboles comparten el mismo set de especies (verificado arriba), 
# fijamos arbol[[1]] como referencia unica y consistente para el resto del script
en_ambos <- intersect(arbol[[1]]$tip.label, tabla$especie_arbol)
no_empataron <- setdiff(tabla$especie_arbol, arbol[[1]]$tip.label)
write.csv(data.frame(especie = no_empataron), file.path(salida, "nombres_sin_empatar.csv"), row.names = FALSE)

# --- DECISION 8. Que especies excluir ----------------------------------------
# Los arboles que recuperamos solo tenían 261 especies de las que encontramos en nuestros
# datos

tabla <- tabla[!is.na(tabla$masa) & !is.na(tabla$altitud_norm_max), ]
tabla <- tabla[tabla$especie_arbol %in% en_ambos, ]
rownames(tabla) <- tabla$especie_arbol

tips_a_quitar <- setdiff(arbol[[1]]$tip.label, tabla$especie_arbol)
arbol <- lapply(arbol, function(x) drop.tip(x, tips_a_quitar))
class(arbol) <- "multiPhylo"

# Esta línea revisa, árbol por árbol, si el conjunto de puntas coincide 
# exactamente con las especies que quedan en la tabla, sin importar el orden en que aparezcan. 
# mismo_set_final guarda un solo valor lógico: TRUE solo si los 100 árboles pasan esa 
# comprobación.

mismo_set_final <- all(sapply(arbol, function(x) setequal(x$tip.label, tabla$especie_arbol)))
# Sin este TRUE nada de lo que sigue sirve
mismo_set_final
sum(is.na(tabla$masa)) # Verificamos que no hayan datos vacíos
sum(is.na(tabla$altitud_norm_max))

# --- DECISION 6. Politomias y ramas de longitud cero -------------------------
# Tenemos un objeto multiPhylo por haber usado los 100 arboles, entonces debemos
# aplicar el reemplazo de las longitudes arbol por arbol y luego restaurar la 
# calse del arbol al final porque lapply devuelve simpre un objeto de clase lista

arbol <- lapply(arbol, function(x) {
  x$edge.length[x$edge.length == 0] <- 1e-8
  x
})

class(arbol) <- "multiPhylo"

# Para verificar que esto resultó bien usamos sapply, con dos parametros "quedan ceros" y
# "min_long", todos los "min_long" deben ser mayores que 0 y los resultados de "quedan_ceros",
# son 0 (representando FALSE) y otro valor distinto a 0 (representando TRUE):
sapply(arbol, function(x) c(quedan_ceros = any(x$edge.length == 0), min_long = min(x$edge.length)))

# --- DECISION 9. Que modelo --------------------------------------------------
# corBrownian, corPagel, corMartins, modelo mixto. Y si incluyes error de medicion
#

#Verif
# Se aplica AQUI, inmediatamente antes de ajustar modelos, para garantizar que
# cualquier drop.tip() posterior no haya reintroducido politomias o ceros
# CORRECCION: is.binary(arbol) se llamaba directamente sobre el objeto
# multiPhylo. Igual que con drop.tip(), esto depende de que ape tenga un
# metodo para multiPhylo; para ser consistentes con el resto del script (y
# evitar el error si esa version de ape no lo soporta) se recorre arbol por
# arbol con sapply/lapply.
if (!all(sapply(arbol, is.binary))) {
  arbol <- lapply(arbol, multi2di)
  class(arbol) <- "multiPhylo"
}

arbol <- lapply(arbol, function(x) {
  x$edge.length[x$edge.length == 0] <- 1e-8
  x
})
class(arbol) <- "multiPhylo"

# Verificacion final antes del loop
any(sapply(arbol, function(x) any(x$edge.length == 0)))  # debe dar FALSE
all(sapply(arbol, is.binary))                             # debe dar TRUE

# DECISION: se comparan Browniano (corBrownian) y OU (corMartins) en cada uno
# de los 100 arboles, sin error de medicion explicito. El modelo con menor AIC en cada 
# arbol es el que se usa para ese arbol; al final se reporta cuantos de los 100 prefirieron 
# cada uno.
resultados_100 <- vector("list", length(arbol))

# CORRECCION: corMartins(1, ...) usa un unico valor inicial (alpha = 1) para
# el parametro de atraccion del modelo OU. Con arboles cuyas longitudes de
# rama van de ~0.0001 a ~27 (rangos muy distintos entre arboles), ese valor
# inicial no siempre es una buena escala de partida: el optimizador de gls()
# puede proponer, en algun paso de la busqueda, un alpha que vuelve la matriz
# de correlacion Inf/NaN, y el ajuste truena con
# "NA/NaN/Inf en llamada a una funcion externa".
# Esta funcion intenta varios valores iniciales de alpha, de menor a mayor,
# y se queda con el primero que SI logra ajustar. Si ninguno funciona,
# devuelve NULL (y el tryCatch de mas abajo hace el fallback a Browniano).
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

# CORRECCION: antes habia un unico tryCatch envolviendo AMBOS modelos
# (Browniano y OU). El problema es que corMartins (el modelo OU) es
# numericamente inestable -- el parametro alpha a veces "se va" a un valor
# extremo durante la optimizacion y produce el error de C
# "NA/NaN/Inf en llamada a una funcion externa". Cuando eso pasaba, el
# tryCatch descartaba TODO el arbol, incluido el modelo Browniano, que
# normalmente si ajustaba bien. Eso hacia perder ~90 de los 100 arboles.
# Ahora cada modelo tiene su PROPIO tryCatch: si corMartins falla para un
# arbol, ese arbol se queda con el resultado del modelo Browniano (marcado
# como tal), en vez de perderse por completo. Solo se omite el arbol si
# fallan los DOS modelos.

for (i in seq_along(arbol)) {
  arbol_i <- arbol[[i]]
  tabla_i <- tabla[arbol_i$tip.label, ]  # reordena la tabla al orden de puntas de ESTE arbol
  
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
    # Los dos modelos fallaron para este arbol: no hay nada que reportar
    message("Arbol ", i, ": fallaron ambos modelos; se omite por completo.")
    resultados_100[[i]] <- NULL
    next
  } else if (!is.null(modelo_browniano) && !is.null(modelo_ou)) {
    # Caso normal: se comparan los dos por AIC, como en la DECISION 9
    aic_browniano <- AIC(modelo_browniano)
    aic_ou        <- AIC(modelo_ou)
    gana_ou       <- aic_ou < aic_browniano
    modelo_i      <- if (gana_ou) modelo_ou else modelo_browniano
    etiqueta      <- if (gana_ou) "OU (corMartins)" else "Browniano (corBrownian)"
  } else if (!is.null(modelo_browniano)) {
    # Solo el Browniano ajusto: se usa ese y se deja constancia de que OU fallo
    modelo_i      <- modelo_browniano
    aic_browniano <- AIC(modelo_browniano)
    aic_ou        <- NA
    etiqueta      <- "Browniano (corBrownian) -- OU no convergio"
  } else {
    # Solo el OU ajusto: se usa ese y se deja constancia de que Browniano fallo
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

# OU convergió en 91/100 árboles; en los 9 restantes (ids: 5, 11, 22, 39, 41, 46, 52, 86, 92) 
# no convergió con ninguno de los valores iniciales de alpha probados, y se usó 
# Browniano como respaldo."

# Esta linea se corre UNA SOLA VEZ, justo despues del loop, nunca mas de una vez porque
# se sobre escribe y se daña completamente
resultados_100 <- do.call(rbind, resultados_100)

# Cuantos de los 100 arboles prefirieron cada modelo
table(resultados_100$modelo_ganador)

# --- Guardar -----------------------------------------------------------------
# Resumimos el efecto de altitud_km promediando el estimado, error estandar,
# IC y valor p entre los arboles que logramos modelar. El promedio de
# valores p es una simplificacion (no es estadisticamente riguroso combinar
# p-values asi), pero lo dejamos como resumen descriptivo, explicito en
# decisiones.csv

# Ajustamos Browniano y OU (corMartins) en los 100 arboles y comparamos por
# AIC cuando los dos convergen (ver DECISION 9). El modelo OU no converge en
# 9 de los 100 arboles (ids: 5, 11, 22, 39, 41, 46, 52, 86, 92) aunque
# probamos varios valores iniciales de alpha (funcion ajustar_ou); en esos 9
# arboles usamos Browniano como respaldo. Por eso calculamos por separado
# cuantos arboles modelamos en total, cuantos de esos convergen con OU
# (columna aic_ou no es NA), y de esos, cuantos gana OU por AIC. Dejamos esta
# distincion documentada en decisiones.csv.

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

# --- Figura --------------------------------------------------------------
# La linea de ajuste usa el intercepto y la pendiente promediados entre los
# arboles que logramos modelar (ya no existe un unico "modelo" del cual sacar
# coef()).
# Graficamos tabla$altitud_norm_max y tabla$masa que son las columnas reales que tenemos en
# "tabla".
png(file.path(salida, "figura.png"), width = 1400, height = 1200, res = 200)
plot(tabla$altitud_norm_max, tabla$masa, pch = 16, col = rgb(0, 0, 0, 0.5),
     xlab = "Altitud (km)", ylab = "Masa corporal, raiz cubica (g^(1/3))", main = equipo)
abline(a = mean(resultados_100$intercepto), b = mean(resultados_100$estimado), lwd = 2)
dev.off()

writeLines(capture.output(sessionInfo()), file.path(salida, "sessionInfo.txt"))

# Guardamos este archivo como analisis.R en nuestra carpeta de trabajo
message("Recuerda guardar este archivo como: ", file.path(salida, "analisis.r"))

# Copiamos el template de decisiones.csv a nuestra carpeta -- no olvidamos llenarlo
message("Recuerda llenar y copiar: ", file.path(salida, "decisiones.csv"))
