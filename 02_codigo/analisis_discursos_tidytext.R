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


### Generar data frame con las palabras (unigramas) pronuncidas en cada discurso, después de eliminar stopwords ----
bd_palabras <- 
  discursos_amlo %>% 
  unnest_tokens(word, texto) %>% 
  anti_join(custom_stop_words) # Eliminar stopwords 

### Generar data frame con las dos palabras (bigramas) pronuncidas en cada discurso, después de eliminar stopwords ----
bd_bigramas <- 
  discursos_amlo %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>% 
  separate(bigrama, c("palabra_1", "palabra_2"), sep = " ", remove = F) %>% 
  filter(!palabra_1 %in% custom_stop_words$word) %>%  # Eliminar stopwords
  filter(!palabra_2 %in% custom_stop_words$word) 

### Generar data frame con las tres palabras (trigramas) pronuncidas en cada discurso, después de eliminar stopwords ----
bd_trigramas <- 
  discursos_amlo %>%
  unnest_tokens(trigrama, texto, token = "ngrams", n = 3) %>% 
  separate(trigrama, c("palabra_1", "palabra_2", "palabra_3"), sep = " ", remove = FALSE) %>%
  filter(!palabra_1 %in% custom_stop_words$word) %>% # Eliminar stopwords
  filter(!palabra_2 %in% custom_stop_words$word) %>% 
  filter(!palabra_3 %in% custom_stop_words$word)

### Generar data frame con las cuatro palabras (cuatrigramas) pronuncidas en cada discurso, después de eliminar stopwords ----
bd_cuatrigramas <- 
  discursos_amlo %>%
  unnest_tokens(cuatrigrama, texto, token = "ngrams", n = 4) %>% 
  separate(cuatrigrama, c("palabra_1", "palabra_2", "palabra_3", "palabra_4"), sep = " ", remove = FALSE) %>%
  filter(!palabra_1 %in% custom_stop_words$word) %>% # Eliminar stopwords
  filter(!palabra_2 %in% custom_stop_words$word) %>% 
  filter(!palabra_3 %in% custom_stop_words$word) %>% 
  filter(!palabra_4 %in% custom_stop_words$word)