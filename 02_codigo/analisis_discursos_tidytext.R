### Cargar y limpiar datos ----
# source("02_codigo/cargar_limpiar_datos.R") # Descomentar esta línea de código y ejecutarla sólo si antes no ejecutaste el archivo cargar_limpiar_datos.R

### Generar data frame con TODAS las palabras pronuncidas por AMLO en cada discurso ----
bd_palabras_todas <- 
  discursos_amlo %>% 
  unnest_tokens(word, texto)