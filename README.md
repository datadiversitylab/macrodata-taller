# Colibríes, altitud y masa corporal: un estudio de múltiples analistas

Este repositorio reúne el trabajo de los talleres de métodos comparativos filogenéticos y ciencia de datos en `MacroData`. Todos los equipos responden la misma pregunta con los mismos datos, pero cada uno decide cómo analizarlos. Lo que nos interesa medir no es la respuesta de un equipo en particular, sino cuánto se mueve la conclusión según las decisiones analíticas que cada quien tomó por su cuenta.

## La pregunta

¿La altitud predice la masa corporal en Trochilidae?

## Los datos

**Rasgos.** BIRDBASE (Scientific Data, 2025): https://springernature.figshare.com/articles/dataset/BIRDBASE_A_Global_Database_of_Avian_Biogeography_Conservation_Ecology_and_Life_History_Traits/27051040?file=55634729

De ahí usamos dos bloques de columnas.

Masa corporal, en gramos: `Male MinMass`, `Male MaxMass`, `Female MinMass`, `Female MaxMass`, `Unsexed MinMass`, `Unsexed MaxMass` y `Average Mass`. La última es el promedio entre machos, hembras e individuos sin sexar.

Altitud, en metros sobre el nivel del mar: `Xmin` (altitud más baja registrada), `NormMin` (límite bajo normal), `NormMax` (límite alto normal), `Xmax` (altitud más alta registrada) y `Elevational Range` (diferencia entre `NormMax` y `NormMin`).

Cuidado con las columnas de altitud. Para algunas especies el límite no es un número sino una letra: `L` para tierras bajas (0 a 500 m), `F` para piedemonte (501 a 1000 m) y `M` para montano (más de 1000 m). Cuando un límite es número y el otro es letra, `Elevational Range` queda como `NA`. Estas especies no se pueden ignorar en silencio. Lo que cada equipo haga con ellas es una de las decisiones que vamos a registrar.

**Árboles.** Hay tres fuentes disponibles y cada equipo escoge una, o varias:

- `rtrees` en R, que da acceso a la distribución posterior de Jetz et al. (2012). Ojo con las especies sin secuencia en su nombre fueron imputadas con taxonómia.
- Megatree de McTavish et al. (2025), construido sobre el Open Tree of Life.
- El árbol de McGuire et al. (2014), específico para colibríes: https://github.com/bw4sz/FutureAnalog/blob/53b12a2bd0c4befabd7b2106a0044cf116871f23/InputData/hum294.tre

Los tres usan taxonomías distintas y ninguno coincide exactamente con la de BIRDBASE. Emparejar los nombres es parte del trabajo, no un paso previo al trabajo.

## Lo que decide cada equipo

Nada de esto viene resuelto. Estas son algunas de las decisiones que esperamos que difieran entre equipos y son, en buena medida, el objeto del estudio:

- Qué columna de masa usar, y si transformarla.
- Cómo resumir la altitud en una sola variable: mínimo, máximo, punto medio, amplitud, o alguna combinación.
- Qué hacer con las especies codificadas como `L`, `F` o `M`.
- Qué árbol usar, y si usar un árbol o una muestra de la posterior.
- Cómo tratar politomías y ramas de longitud cero.
- Qué especies excluir, si alguna, y por qué.
- Qué modelo ajustar: PGLS con movimiento browniano, con lambda de Pagel, con Ornstein-Uhlenbeck, un modelo mixto filogenético, o cualquier otro que se pueda justificar.

No hay respuesta correcta en esta lista. Hay respuestas justificables, y queremos la justificación escrita.

## Estructura del repositorio

```
datos/            # Datos crudos. No se modifican nunca
  birdbase/
  arboles/
equipos/
  ciudad-equipo01/
    analisis.R
    decisiones.csv
    resultados.csv
    README.md
  ciudad-equipo02/
    ...
scripts/          # Funciones compartidas y puntos de control del taller
sintesis/         # Análisis conjunto de todos los equipos
manuscrito/
```

Cada equipo trabaja únicamente dentro de su propia carpeta.

## Cómo contribuir

1. Clona el repositorio y crea una rama con el nombre de tu equipo.
2. Trabaja dentro de `equipos/tu-ciudad-tu-equipo/`. *No modifiques nada más*
3. Escribe el análisis en `analisis.R`. Debe correr de principio a fin en una sesión limpia de R, leyendo desde `datos/` y escribiendo sus salidas en tu carpeta. Si no corre en la máquina de otra persona, no lo podemos usar.
4. Llena `decisiones.csv` a medida que avanzas, no al final.
5. Guarda los resultados en `resultados.csv` con el formato descrito abajo.
6. Escribe un `README.md` corto en tu carpeta: quiénes son, de dónde vienen, y en dos o tres frases qué enfoque tomaron.
7. Abre un pull request.

Un par de reglas prácticas. El código en R, en base R hasta donde sea razonable, sin dependencias que no sean necesarias. Comentarios en español. Nada de rutas absolutas: usa rutas relativas a la raíz del repositorio. Y registra la versión de R y de los paquetes con `sessionInfo()` al final del script.

## El formulario de decisiones

`decisiones.csv` es la mitad de los datos del estudio. Tiene una fila por decisión y estas columnas:

| Columna | Contenido |
| --- | --- |
| `equipo` | Identificador del equipo |
| `decision` | Qué se decidió, en pocas palabras |
| `opcion` | La opción escogida |
| `alternativas` | Las opciones que se consideraron y se descartaron |
| `justificacion` | Por qué. Texto libre, dos o tres frases |
| `momento` | Antes o después de ver los resultados |

La última columna importa. Queremos saber cuáles decisiones se tomaron a ciegas y cuáles se tomaron después de mirar el resultado.

`resultados.csv` es más corto: el estimado del efecto de la altitud sobre la masa, su error estándar, el intervalo de confianza, el valor p, el tamaño de muestra final y el modelo ajustado.

## Autoría

Es autor quien cumpla las tres condiciones:

1. Entregar un análisis completo con su script y su formulario de decisiones.
2. Revisar y aprobar el borrador final del manuscrito.
3. Responder a la solicitud de aprobación dentro del plazo que se anuncie.

Quien cumpla algunas pero no todas queda en los agradecimientos. El orden de autoría es alfabético con una nota en el manuscrito explicando el criterio.

