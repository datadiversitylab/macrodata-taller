# Prueba de instalacion
# Corre este script completo antes del taller
# Si algo falla, escribenos y lo resolvemos antes de empezar

cat("\n=== Prueba de instalacion ===\n\n")

problemas <- character(0)

# 1. Version de R
version_r <- getRversion()
cat("Version de R:", as.character(version_r), "\n")
if (version_r < "4.2.0") {
  problemas <- c(problemas, "Tu version de R es vieja, instala una mas reciente desde cran.r-project.org")
}

# 2. Paquetes basicos
necesarios <- c("ape", "nlme", "phytools", "geiger")
faltan <- necesarios[!necesarios %in% rownames(installed.packages())]

if (length(faltan) > 0) {
  problemas <- c(problemas, paste("Faltan paquetes:", paste(faltan, collapse = ", ")))
  cat("Paquetes faltantes:", paste(faltan, collapse = ", "), "\n")
} else {
  cat("Paquetes: todos instalados\n")
}

# 3. Prueba de uso real
if (length(faltan) == 0) {
  ok <- tryCatch({
    library(ape)
    arbol <- rtree(20)
    datos <- data.frame(x = rnorm(20), y = rnorm(20), row.names = arbol$tip.label)
    modelo <- nlme::gls(y ~ x, correlation = ape::corBrownian(phy = arbol, form = ~1), data = datos)
    plot(arbol, main = "Si ves este arbol, todo funciona")
    TRUE
  }, error = function(e) {
    problemas <<- c(problemas, paste("Error al correr el ejemplo:", conditionMessage(e)))
    FALSE
  })
  if (ok) cat("Ejemplo de analisis: corre sin errores\n")
}

# 4. Permisos de escritura
archivo <- file.path(tempdir(), "prueba.csv")
escritura <- tryCatch({
  write.csv(data.frame(a = 1), archivo, row.names = FALSE)
  file.remove(archivo)
  TRUE
}, error = function(e) FALSE)

if (escritura) {
  cat("Escritura de archivos: correcta\n")
} else {
  problemas <- c(problemas, "R no puede escribir archivos en tu computador, revisa los permisos")
}

# 5. Git
repositorio <- "https://github.com/datadiversitylab/macrodata-analisis.git"

git <- tryCatch(system2("git", "--version", stdout = TRUE, stderr = TRUE), error = function(e) NULL)

if (is.null(git) || !any(grepl("git version", git))) {
  problemas <- c(problemas, "Git no esta instalado, descargalo de https://git-scm.com/downloads")
} else {
  cat("Git:", git[1], "\n")

  usuario <- system2("git", c("config", "--global", "user.name"), stdout = TRUE, stderr = FALSE)
  correo <- system2("git", c("config", "--global", "user.email"), stdout = TRUE, stderr = FALSE)

  if (length(usuario) == 0 || length(correo) == 0) {
    problemas <- c(problemas, "Falta configurar Git, corre en la terminal: git config --global user.name \"Tu Nombre\" y git config --global user.email \"tu@correo.com\"")
  } else {
    cat("Git configurado como:", usuario, "<", correo, ">\n")
  }

  # Prueba de clonado en una carpeta temporal
  destino <- file.path(tempdir(), "prueba_clon")
  unlink(destino, recursive = TRUE)
  clon <- suppressWarnings(system2("git", c("clone", "--depth", "1", repositorio, shQuote(destino)),
                                   stdout = TRUE, stderr = TRUE))

  if (dir.exists(file.path(destino, ".git"))) {
    cat("Clonado del repositorio: correcto\n")
    unlink(destino, recursive = TRUE)
  } else {
    problemas <- c(problemas, "No se pudo clonar el repositorio del taller, revisa tu conexion o firewall")
  }
}

# 6. Informe
cat("\n")
if (length(problemas) == 0) {
  cat("Todo listo. Nos vemos en el taller.\n\n")
  cat("Copia y pega estas dos lineas en el mensaje de confirmacion:\n")
  cat(as.character(version_r), "|", Sys.info()[["sysname"]], "\n")
} else {
  cat("Hay", length(problemas), "cosas por resolver:\n")
  cat(paste0("  - ", problemas, collapse = "\n"), "\n\n")
  cat("Escribenos con este mensaje completo y lo arreglamos antes del taller.\n")
}

cat("\n")
