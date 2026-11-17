# Equipo 7

## Quiénes somos

Jair David Barrera Liñan, Universidad del Atlántico, jairdbarrera@mail.uniatlantico.edu.co  
Juan Diego Silva Arboleda, j.diegosilva2002@gmail.com

## Qué hicimos

Trabajamos con la masa promedio de BirdBase y con el punto medio del rango altitudinal normal. Las especies cuyo límite altitudinal viene codificado como letra las convertimos al punto medio del intervalo correspondiente en lugar de descartarlas, porque descartarlas eliminaba especies de tierras bajas de manera desbalanceada. Ajustamos un PGLS con lambda de Pagel estimada sobre el árbol de McGuire et al. (2014).

Dos o tres párrafos como este son suficientes. Lo detallado va en `decisiones.csv`.

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
source("equipos/ciudad-equipo0/analisis.R")
```

Necesita `ape` y `nlme`, y los datos en `datos/birdbase/` y `datos/arboles/`.

## Lo que nos quedó pendiente

Ajustamos un solo árbol, así que la incertidumbre filogenética no está representada. Resolvimos las politomías una sola vez al azar sin verificar si otra resolución cambia el resultado. No incorporamos error de medición intraespecífico.

Esta sección importa. Escribe lo que no alcanzaste a hacer y por qué, sin adornarlo.
