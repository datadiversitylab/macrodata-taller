# Los scripts del taller

Un script por bloque, en el orden en que los vamos a correr. Todos se corren desde la raíz del repositorio, no desde esta carpeta.

## Día 1

| Script | Bloque |
| --- | --- |
| `01_primer_arbol.R` | 9:20. Tu primer árbol de colibríes en pantalla |
| `02_fundamentos_r.R` | 9:45. Fundamentos de R |
| `03_datos_birdbase.R` | 11:00. Datos reales con BirdBase |
| `04_reproducibilidad_git.R` | 13:30. Reproducibilidad, Git y GitHub |
| `05_filogenias.R` | 15:00. Filogenias y cómo empatar árbol con datos |

## Día 2

| Script | Bloque |
| --- | --- |
| `06_senal_y_modelos.R` | 9:15. Señal filogenética y modelos de evolución |
| `07_pgls_completo.R` | 11:00. PGLS de principio a fin |
| `08_proyecto_plantilla.R` | 13:30. La plantilla de tu proyecto |
| `09_entrega.R` | 15:00. Validación y entrega |

## Puntos de control

Si te atrasaste, carga el punto de control del bloque anterior y sigue con el resto del grupo. Cada uno deja tu sesión en el estado en que debería estar al terminar ese bloque.

```r
source("taller/puntos_control/punto_control_03.R")
source("taller/puntos_control/punto_control_05.R")
source("taller/puntos_control/punto_control_07.R")
```

Los puntos de control se llaman entre sí, así que puedes cargar directamente el 05 sin haber corrido el 03.

## Lo que produce cada script

`03` escribe `datos/colibries_limpio.csv`. `05` escribe `datos/colibries_listo.RData` con el árbol y la matriz ya empatados y en el mismo orden. `08` escribe todo dentro de la carpeta de tu equipo.

Los datos crudos en `datos/birdbase/` y `datos/arboles/` no se modifican nunca.

## Si algo no corre

Reinicia R con Ctrl+Shift+F10 y vuelve a correr el script desde arriba. La mitad de los problemas son objetos viejos que quedaron en la sesión.

Si sigue sin correr, hoja de color sobre la tapa del computador.
