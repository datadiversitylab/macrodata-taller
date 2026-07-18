# Validacion de entregas
# Revisa que la carpeta de un equipo se pueda usar en la sintesis
# Uso: source("scripts/validar_entrega.R"); validar_entrega("equipos/ciudad-equipo0")
# Los comentarios van sin tildes para evitar problemas de codificacion entre sistemas

columnas_decisiones <- c("equipo", "decision", "opcion", "alternativas", "justificacion", "momento")

columnas_resultados <- c("equipo", "respuesta", "predictor", "estimado", "error_estandar",
                         "ic_inferior", "ic_superior", "valor_p", "n", "modelo")

validar_entrega <- function(carpeta, correr_script = FALSE) {

  problemas <- character(0)
  anotar <- function(...) problemas <<- c(problemas, paste0(...))

  equipo <- basename(carpeta)

  if (!dir.exists(carpeta)) {
    cat("La carpeta", carpeta, "no existe\n")
    return(invisible(FALSE))
  }

  # Archivos obligatorios
  obligatorios <- c("analisis.R", "decisiones.csv", "resultados.csv", "README.md")
  faltantes <- obligatorios[!file.exists(file.path(carpeta, obligatorios))]
  if (length(faltantes) > 0) anotar("Faltan archivos: ", paste(faltantes, collapse = ", "))

  # Decisiones
  ruta_decisiones <- file.path(carpeta, "decisiones.csv")
  if (file.exists(ruta_decisiones)) {
    decisiones <- read.csv(ruta_decisiones, stringsAsFactors = FALSE)

    ausentes <- setdiff(columnas_decisiones, names(decisiones))
    if (length(ausentes) > 0) anotar("decisiones.csv, columnas ausentes: ", paste(ausentes, collapse = ", "))

    if (nrow(decisiones) == 0) {
      anotar("decisiones.csv esta vacio")
    } else if (length(ausentes) == 0) {

      if (nrow(decisiones) < 5) anotar("decisiones.csv tiene solo ", nrow(decisiones), " filas, esperamos al menos cinco")

      vacia <- function(x) is.na(x) | trimws(x) == ""

      sin_justificacion <- which(vacia(decisiones$justificacion))
      if (length(sin_justificacion) > 0) {
        anotar("Filas sin justificacion: ", paste(sin_justificacion, collapse = ", "))
      }

      cortas <- which(!vacia(decisiones$justificacion) & nchar(decisiones$justificacion) < 40)
      if (length(cortas) > 0) {
        anotar("Justificaciones demasiado cortas en las filas: ", paste(cortas, collapse = ", "))
      }

      sin_alternativas <- which(vacia(decisiones$alternativas))
      if (length(sin_alternativas) > 0) {
        anotar("Filas sin alternativas consideradas: ", paste(sin_alternativas, collapse = ", "))
      }

      momento <- tolower(trimws(decisiones$momento))
      momento <- gsub("\u00e9", "e", momento)
      invalidos <- which(!momento %in% c("antes", "despues"))
      if (length(invalidos) > 0) {
        anotar("La columna momento solo acepta antes o despues, revisar filas: ", paste(invalidos, collapse = ", "))
      }

      if (any(decisiones$equipo != equipo)) {
        anotar("La columna equipo de decisiones.csv no coincide con el nombre de la carpeta")
      }
    }
  }

  # Resultados
  ruta_resultados <- file.path(carpeta, "resultados.csv")
  if (file.exists(ruta_resultados)) {
    resultados <- read.csv(ruta_resultados, stringsAsFactors = FALSE)

    ausentes <- setdiff(columnas_resultados, names(resultados))
    if (length(ausentes) > 0) anotar("resultados.csv, columnas ausentes: ", paste(ausentes, collapse = ", "))

    if (nrow(resultados) != 1) {
      anotar("resultados.csv debe tener exactamente una fila, tiene ", nrow(resultados))
    } else if (length(ausentes) == 0) {

      numericas <- c("estimado", "error_estandar", "ic_inferior", "ic_superior", "valor_p", "n")
      no_numericas <- numericas[!sapply(resultados[numericas], is.numeric)]
      if (length(no_numericas) > 0) {
        anotar("Columnas que deberian ser numericas: ", paste(no_numericas, collapse = ", "))
      } else {

        if (is.na(resultados$estimado)) anotar("El estimado es NA")
        if (!is.na(resultados$error_estandar) && resultados$error_estandar <= 0) anotar("El error estandar no es positivo")
        if (!is.na(resultados$valor_p) && (resultados$valor_p < 0 | resultados$valor_p > 1)) anotar("El valor p esta fuera de cero a uno")
        if (!is.na(resultados$n) && resultados$n < 30) anotar("La muestra final es de ", resultados$n, " especies, conviene revisar la limpieza")

        limites <- c(resultados$ic_inferior, resultados$ic_superior)
        if (!any(is.na(limites))) {
          if (resultados$ic_inferior > resultados$ic_superior) anotar("Los limites del intervalo estan invertidos")
          if (!is.na(resultados$estimado) &&
              (resultados$estimado < resultados$ic_inferior | resultados$estimado > resultados$ic_superior)) {
            anotar("El estimado queda por fuera de su propio intervalo de confianza")
          }
        }
      }

      if (resultados$equipo != equipo) {
        anotar("La columna equipo de resultados.csv no coincide con el nombre de la carpeta")
      }
      if (trimws(resultados$modelo) == "") anotar("Falta describir el modelo ajustado")
    }
  }

  # Script
  ruta_script <- file.path(carpeta, "analisis.R")
  if (file.exists(ruta_script)) {
    codigo <- readLines(ruta_script, warn = FALSE)
    activo <- codigo[!grepl("^\\s*#", codigo)]

    if (any(grepl("setwd\\(", activo))) anotar("El script usa setwd, las rutas deben ser relativas a la raiz del repositorio")
    if (any(grepl("[\"']([A-Za-z]:|/Users/|/home/|/mnt/)", activo))) anotar("El script tiene rutas absolutas")
    if (!any(grepl("sessionInfo", activo))) anotar("El script no registra sessionInfo")
    if (!any(grepl("resultados.csv", activo))) anotar("El script no escribe resultados.csv")
  }

  # README
  ruta_readme <- file.path(carpeta, "README.md")
  if (file.exists(ruta_readme)) {
    readme <- paste(readLines(ruta_readme, warn = FALSE), collapse = " ")
    if (nchar(readme) < 400) anotar("El README es demasiado corto")
    if (grepl("Aqui van los nombres|Esta carpeta es el ejemplo", readme)) {
      anotar("El README todavia tiene el texto de la plantilla")
    }
  }

  # Correr el script en una sesion limpia, opcional porque tarda
  if (correr_script && file.exists(ruta_script)) {
    salida <- system2("Rscript", c("--vanilla", shQuote(ruta_script)), stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(salida, "status")) && attr(salida, "status") != 0) {
      anotar("El script fallo al correr en una sesion limpia")
      writeLines(salida, file.path(carpeta, "error_validacion.txt"))
    }
  }

  # Informe
  cat("\n", equipo, "\n", sep = "")
  if (length(problemas) == 0) {
    cat("  Entrega completa\n")
  } else {
    cat("  ", length(problemas), " problemas\n", sep = "")
    cat(paste0("  - ", problemas, collapse = "\n"), "\n")
  }

  invisible(length(problemas) == 0)
}

# Revisa todas las entregas de una vez
validar_todo <- function(raiz = "equipos", correr_script = FALSE) {
  carpetas <- list.dirs(raiz, recursive = FALSE)
  resultado <- sapply(carpetas, validar_entrega, correr_script = correr_script)
  cat("\n", sum(resultado), " de ", length(resultado), " entregas completas\n", sep = "")
  invisible(data.frame(equipo = basename(carpetas), completa = resultado, row.names = NULL))
}
