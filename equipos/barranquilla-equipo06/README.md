# Equipo barranquilla-equipo06

## Quiénes somos
Fernanda Romero Forero, Universidad de Caldas, maria.531812461@caldas.edu.co
Jefferson Flechas Oviedo, Universidad de los llanos, jaflechas@unillanos.edu.co

## Qué hicimos

Trabajamos con la masa minima de solo hembras de BirdBase y con altitudinal maxima normal. En la seleccionada no encontramos especies cuyo límite altitudinal viene codificado , por lo tanto este dato no fue codificado. Ajustamos un PGLS con lambda de Pagel estimada sobre el árbol de McTavish et al. (2025).
Los Na, fueron eliminados y nos ajustamos aun modelo PGLs con movimiento browniniano.

## Archivos

- `analisis.R`: el análisis completo. Corre de principio a fin desde la raíz del repositorio en una sesión limpia.
- `decisiones.csv`: una fila por decisión analítica.
- `resultados.csv`: una fila con el estimado del efecto.
- `nombres_sin_empatar.csv`: especies de BirdBase que no encontramos en el árbol.
- `figura.png`
- `sessionInfo.txt`

## Cómo correrlo

Desde la raíz del repositorio:

```r
source("equipos/barranquilla-equipo06/analisis.R")
```

Necesita `ape` y `nlme`, y los datos en `datos/birdbase/` y `datos/arboles/`.

## Lo que nos quedó pendiente

Ajustamos un solo árbol, así que la incertidumbre filogenética no está representada. Resolvimos las politomías una sola vez al azar sin verificar si otra resolución cambia el resultado. No incorporamos error de medición intraespecífico.

