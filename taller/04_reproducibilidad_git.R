# Dia 1, bloque 4. Reproducibilidad, Git y GitHub

# Regla uno. Nunca uses setwd ni rutas absolutas
getwd()

# Mal. Solo funciona en tu computador
# datos <- read.csv("/Users/tunombre/Escritorio/birdbase.csv")

# Bien. Funciona en cualquier computador que tenga el repositorio
datos <- read.csv("datos/colibries_limpio.csv", stringsAsFactors = FALSE)

# Regla dos. Los datos crudos no se modifican nunca
list.files("datos", recursive = TRUE)

# Regla tres. Todo lo que hiciste a mano se olvida, lo que esta en el script no
# Si tuviste que abrir Excel para arreglar algo, ese arreglo no es reproducible

# Regla cuatro. Registra tu entorno al final de cada script
sessionInfo()

# Prueba de fuego. Reinicia R con Ctrl+Shift+F10 y corre tu script completo
# Si no corre, no es reproducible, aunque funcione en tu sesion actual


# --- Git ---------------------------------------------------------------------

# Una sola vez en tu vida, desde la terminal
# git config --global user.name "Tu Nombre"
# git config --global user.email "tu@correo.com"

# GitHub no acepta contrasena desde la terminal. Necesitas un token
# install.packages(c("usethis", "gitcreds"))

# Abre GitHub para que generes el token. Copialo antes de cerrar la pagina
# usethis::create_github_token()

# Pega el token aqui
# gitcreds::gitcreds_set()

# El ciclo completo, desde la terminal de RStudio, pestaña Terminal
# git clone [URL]
# git checkout -b mi-ciudad-equipoNN
# git add .
# git commit -m "primer analisis"
# git push --set-upstream origin mi-ciudad-equipoNN

# Y despues el pull request desde la pagina de GitHub

# git status te dice donde estas en ese momento Usalo entre cada paso

# Trabaja solo dentro de tu carpeta y los conflictos son casi imposibles
salida <- "equipos/mi-ciudad-equipoNN"
dir.create(salida, recursive = TRUE, showWarnings = FALSE)
writeLines(capture.output(sessionInfo()), file.path(salida, "sessionInfo.txt"))

