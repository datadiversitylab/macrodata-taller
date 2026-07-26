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

# Busca el ejecutable de Git de una forma que funciona igual en Windows, Mac y Linux
git_bin <- Sys.which("git")

# Corre git y devuelve el codigo de salida real, no solo el texto
correr_git <- function(args) {
  salida <- suppressWarnings(system2(git_bin, args, stdout = TRUE, stderr = TRUE))
  estado <- attr(salida, "status")
  if (is.null(estado)) estado <- 0
  list(estado = estado, salida = paste(salida, collapse = " "))
}

if (!nzchar(git_bin)) {
  problemas <- c(problemas, "Git no esta instalado o no esta en el PATH, descargalo de https://git-scm.com/downloads y reinicia el computador")
} else {
  
  version_git <- correr_git("--version")
  cat("Git:", trimws(version_git$salida), "\n")
  
  # git config devuelve codigo 1 y salida vacia cuando no esta configurado
  nombre <- correr_git(c("config", "--global", "user.name"))
  correo <- correr_git(c("config", "--global", "user.email"))
  
  tiene_nombre <- nombre$estado == 0 && nzchar(trimws(nombre$salida))
  tiene_correo <- correo$estado == 0 && nzchar(trimws(correo$salida))
  
  if (!tiene_nombre || !tiene_correo) {
    problemas <- c(problemas, "Falta configurar Git, corre en R: usethis::use_git_config(user.name = \"Tu nombre\", user.email = \"tu@correo.com\")")
  } else {
    cat("Git configurado como:", trimws(nombre$salida), "<", trimws(correo$salida), ">\n")
  }
  
  # Prueba de clonado en una carpeta temporal. Pasamos la ruta sin comillas, system2 se encarga
  destino <- file.path(tempdir(), "prueba_clon")
  unlink(destino, recursive = TRUE, force = TRUE)
  
  clon <- correr_git(c("clone", "--depth", "1", repositorio, destino))
  
  if (clon$estado == 0 && dir.exists(file.path(destino, ".git"))) {
    cat("Clonado del repositorio: correcto\n")
    unlink(destino, recursive = TRUE, force = TRUE)
  } else {
    problemas <- c(problemas, "No se pudo clonar el repositorio del taller, revisa tu conexion o tu firewall")
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
