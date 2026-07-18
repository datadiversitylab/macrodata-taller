# Dia 2, bloque 4. Entrega

equipo <- "ciudad-equipoNN"
salida <- file.path("equipos", equipo)

# Antes de entregar, reinicia R con Ctrl+Shift+F10 y corre tu analisis completo
source(file.path(salida, "analisis.R"))

# Revisa que tu entrega este completa
source("scripts/validar_entrega.R")
validar_entrega(salida)

# Corregir lo que salga y volver a validar hasta que diga "Entrega completa"

# La prueba dura. Correr en una sesion limpia, como lo va a hacer otra persona
validar_entrega(salida, correr_script = TRUE)

list.files(salida)


# --- Subir -------------------------------------------------------------------

# Desde la pestana Terminal de RStudio
# git status
# git add equipos/ciudad-equipoNN
# git commit -m "analisis del equipo NN"
# git push --set-upstream origin ciudad-equipoNN

# Y despues el pull request desde la pagina de GitHub


# --- Licencia y citacion -----------------------------------------------------

# El repositorio esta bajo CC BY 4.0 para los datos y MIT para el codigo
# Eso significa que cualquiera puede usarlo citando la fuente

# Si usas estos datos por fuera del taller, cita las fuentes originales
# BirdBase, Jetz et al. 2012, McGuire et al. 2014, McTavish et al. 2025

# El referente metodologico del estudio
# Gould et al. 2025, BMC Biology 23:35, doi 10.1186/s12915-024-02101-x


# --- Lo que sigue ------------------------------------------------------------

# Tienes cuatro semanas si no alcanzaste a terminar hoy
# Los tres requisitos para ser autor estan en el README del repositorio
# El grupo de WhatsApp queda abierto. Uselo
