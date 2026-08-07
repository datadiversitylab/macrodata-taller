# bogota-equipo08

Juan José Lemos Lucumí (Univalle), Guerly León Castillo (Universidad Nacional de Colombia, sede Bogotá) y Raúl Sedano (Univalle). Los tres tomamos el curso en Bogotá.

## Requisitos para reproducir

Antes de correr `analisis.R`, agrega el archivo `output.nex` (los 100 árboles de BirdTree usados) a `datos/arboles/`. Sin ese archivo el script no corre.

## Enfoque

Analizamos la relación entre la masa corporal de colibríes (transformada con raíz cúbica) y la altitud normal máxima a la que habitan, mediante modelos GLS filogenéticos. Para cada uno de los 100 árboles de BirdTree (Jetz et al. 2012) descargados para nuestras especies, ajustamos un modelo Browniano y uno Ornstein-Uhlenbeck y seleccionamos el de menor AIC, con el fin de propagar la incertidumbre filogenética en la estimación del efecto en lugar de depender de un único árbol.
