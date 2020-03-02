# Descargar (si hace falta) tidyverse
# install.packages("tidyverse")

# Cargar en memoria tidyverse
library(tidyverse)

# Creo una lista con todos los ficheros CSV que hay en el directorio input
lista_ficheros <- list.files(path = "input/",
                             pattern = "csv",
                             full.names = TRUE)

# Aplico la funcion read_csv (de tidyverse) a todos los ficheros
# mediante la funcion ldply (de plyr) y los junto en el mismo tibble (conjunto de datos)
resultados <- plyr::ldply(lista_ficheros, read_csv)

# Si no quiero que me salgan todos los mensajes de aviso cada vez que abre un fichero,
# usaría esta versión que elimina los mensajes de aviso
# resultados <- suppressMessages(plyr::ldply(lista_ficheros, read_csv))

# Elimino aquellos ensayos en los que los tiempos de reaccion son superiores a 4
resultados <- filter(resultados, rt < 4)

# Esto seria lo mismo pero usando los pipes de tidyverse
resultados <- resultados %>%
  filter(rt < 4)

# Saco estadisticos descriptivos de los primeros 9 ensayos del participante 2
resultados %>%
  filter(participante == 2) %>%
  filter(ensayo < 10) %>%
  summary()

# Creo una nueva variable en la que almaceno la longitud de la palabra estimulo
resultados <- resultados %>%
  mutate(longitud = nchar(palabra))

# Calculo la media, desviacion estandar y mediana de los tiempos de reaccion
# para cada una de las longitudes de palabra
resultados %>%
  group_by(longitud) %>%
  summarise(media = mean(rt),
            desviacion = sd(rt),
            mediana = median(rt))

# Histograma de todos los tiempos de reaccion con r base
hist(resultados$rt, breaks = 1000)

# Histograma de los tiempos de reaccion con ggplot
resultados %>% # Parte del data frame de datos
  ggplot(aes(rt)) + # Queremos representar los tiempos de reaccion
  geom_histogram(bins = 1000) + # Una un histograma con 1000 bins (divisiones)
  labs(title = "Distribución de los RTs", # Incorpora las etiquetas correctas de eje
       x = "Tiempos de reacción (s)",
       y = "Frecuencia") +
  theme_light() # Cambia el tema  que se va a emplear (aspectos esteticos)

# Representa las medias de los participantes para cada una de las longitudes
resultados %>% # Partimos de nuestra data.frame
  group_by(participante, longitud) %>% # Agrupamos por participantes y longitudes
  summarise(media_rt = mean(rt)) %>% # Calculamos la media del rt para cada participante y longitud
  ggplot(aes(x = longitud, y = media_rt, # Asignamos variables a ejes y el color a la longitud
             color = longitud)) +
  geom_boxplot(aes(group = longitud), color = "red") + # Dibujamos boxplots para cada longitud
  geom_jitter(height = 0, alpha = 0.1) + # Usamos puntos con jitter horizontal y transparencia
  labs(title = "Distribución de los RTs", # Incorpora las etiquetas correctas de eje
       x = "Longitud de la palabra en caracteres",
       y = "Tiempos de reacción medios (s)") +
  guides(color=FALSE) # Eliminar la leyenda de color

# Salvamos el ultimo grafico que hemos creado
ggsave(filename = "grafico_jitter.pdf")

# Grafico de las medias de los tiempos de reaccion de los 50 primeros
# participantes con las medias de las medias y barras de error 
# con el error estandar de la media
resultados %>%
  filter(participante <= 50) %>% # Seleccionamos los primeros 50 participantes
  group_by(participante, longitud) %>% # Agrupamos por participante y longitud
  summarise(media_rt = mean(rt)) %>% # Calculamos la media para cada participante en cada longitud
  ggplot(aes(x = longitud, y = media_rt, # Asignamos variables a ejes y el color a la longitud
             color = longitud)) +
  geom_jitter(height = 0, alpha = 0.1) + # Dibujamos el jitter, las barras de error y el punto
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.5, color = "dark green") +
  stat_summary(fun.y = mean, geom = "point", color = "dark green") + # Usamos puntos con jitter horizontal y transparencia
  labs(title = "Distribución de los RTs", # Incorpora las etiquetas correctas de eje
       x = "Longitud de la palabra en caracteres",
       y = "Tiempos de reacción medios (s)") +
  guides(color=FALSE) # Eliminar la leyenda de color

  
# Pasar resultados de un formato largo a un formato ancho (SPSS) de los tiempos de reacción
# de los ensayos 1 a 10 de los participantes 1 a 5
resultados_largo <- resultados %>%
  filter(ensayo <= 10, participante <= 5) %>% # Filtra ensayos y participantes
  select(rt, participante, ensayo) %>% # Seleccionamos solamente las variables que nos interesan
  pivot_wider(names_from = ensayo, 
              names_prefix = "ensayo_",
              # El numero de ensayo
              values_from = rt)

# Salvar en formato csv los resultados largos para abrir con excel o spss
write_csv(resultados_largo, 'output_spss.csv')
