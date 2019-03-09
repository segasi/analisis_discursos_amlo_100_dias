### Cargar y limpiar datos ----
# source("02_codigo/cargar_limpiar_datos.R") # Descomentar esta línea de código y ejecutarla sólo si antes no ejecutaste el archivo cargar_limpiar_datos.R

### Generar data frame con TODAS las palabras pronuncidas por AMLO en cada discurso ----
bd_palabras_todas <- 
  discursos_amlo %>% 
  unnest_tokens(word, texto)

### Contar el número total de palabras pronunciadas por AMLO por discurso, incluyendo stopwords ----
bd_palabras_todas %>% 
  group_by(discurso_id) %>% 
  summarise(num_palabras = n()) %>% 
  ungroup() %>% 
  mutate(total_palabras = sum(num_palabras)) %>% 
  arrange(-num_palabras)

### Generar lista de stopwords en español ----
# Código adaptado de aquí: http://jvera.rbind.io/post/2017/10/16/spanish-stopwords-for-tidytext-package/

# Definir stopwords ----
word <-  c(tm::stopwords("spanish"))
custom_stop_words <- tibble(word,
                            lexicon = "custom")

custom_stop_words %>% arrange(word) %>%  print(n = nrow(.))