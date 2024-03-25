# Procesamiento de data------
## Matías Deneken.

# Cargar paquetes ---

pacman::p_load(dplyr, 
               sjmisc, 
               tidyverse, 
               sjlabelled, 
               ggplot2,
               readxl)

# Cargar BBDD----

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

encuesta_turista <- readRDS("bbdd/encuesta_turista.rds")

str(encuesta_turista)


genero <-encuesta_turista %>%
  group_by(genero) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


promedio_por_genero <- encuesta_turista %>%
  group_by(genero) %>%
  summarize(promedio_edad_por_genero = mean(edad, na.rm = TRUE))


porcentaje_pueblo_indigena <- encuesta_turista %>%
  group_by(pueblo_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

educacion <- encuesta_turista %>%
  group_by(educacion) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


internet <- encuesta_turista %>%
  group_by(conexion_internet) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


comuna <- encuesta_turista %>%
  group_by(comuna_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

comuna_otra_pegada <- encuesta_turista %>%
  mutate(comuna_emprendimiento = ifelse(comuna_emprendimiento == "Otra", comuna_emprendimiento_otra, comuna_emprendimiento)) %>% 
  group_by(comuna_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

zona_emprendimiento <- encuesta_turista %>%
  group_by(zona_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

sociedad_emprendimiento <- encuesta_turista %>%
  group_by(sociedad_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


ano_inicio <- encuesta_turista %>%
  group_by(ano_inicio) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


turismo_principal <- encuesta_turista %>%
  group_by(turismo_principal) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


turismo_principal_pueblo <- encuesta_turista %>%
  filter(turismo_principal == "Sí") %>%
  group_by(pueblo_indigena) %>%
  summarize(total_si = n()) %>%
  left_join(encuesta_turista %>% group_by(pueblo_indigena) %>% summarize(total_respuestas = n()), by = "pueblo_indigena") %>%
  mutate(porcentaje_si = (total_si / total_respuestas) * 100)


temporada_abierto <- encuesta_turista %>%
  #mutate(temporada_abierto = str_replace(temporada_abierto, "Temporada Alta \\(Diciembre-Marzo\\),", "")) %>%
  group_by(temporada_abierto) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


temporada_abierto <- encuesta_turista %>%
  separate_rows(temporada_abierto, sep = ", ") %>% #Aqui tuve que separarlas por la coma.
  filter(temporada_abierto != "sin parar") %>% 
  group_by(temporada_abierto) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


tramite_formalizacion <- encuesta_turista %>%
  separate_rows(tramite_formalizacion, sep = ", ") %>%
  group_by(tramite_formalizacion) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)



tramite_formalizacion_pueblos <- encuesta_turista %>%
  separate_rows(tramite_formalizacion, sep = ", ") %>%
  group_by(tramite_formalizacion, pueblo_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


inicio_actividades <- encuesta_turista %>%
  group_by(inicio_actividades) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


encuesta_turista$justificacion_no_inicio

justificacion_no_inicio <- encuesta_turista %>%
  filter(!grepl("agua potable|etc\\.\\)", justificacion_no_inicio)) %>% 
  separate_rows(justificacion_no_inicio, sep = ", ") %>%
  group_by(justificacion_no_inicio) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

tipos_sociedad <- encuesta_turista %>%
  separate_rows(tipos_sociedad, sep = ", ") %>%
  group_by(tipos_sociedad) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

sello_indigena <- encuesta_turista %>%
  group_by(sello_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

espacio_desarrollo <- encuesta_turista %>%
  group_by(espacio_desarrollo) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

instalaciones_turisticas <- encuesta_turista %>%
  separate_rows(instalaciones_turisticas, sep = ", ") %>%
  group_by(instalaciones_turisticas ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


herramienta_difusion <- encuesta_turista %>%
  separate_rows(herramienta_difusion, sep = ", ") %>%
  group_by(herramienta_difusion) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


idioma <- encuesta_turista %>%
  separate_rows(idioma, sep = ", ") %>%
  group_by(idioma) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

organizacion_no_indigena <- encuesta_turista %>%
  #separate_rows(idioma, sep = ", ") %>%
  group_by(organizaciones_no_indigenas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


servicios_turisticos <- encuesta_turista %>%
  separate_rows(servicios_turisticos, sep = ", ") %>%
  filter(!grepl("No responde|agua potable|etc\\.\\)", servicios_turisticos)) %>%
  filter(!grepl("cabalgatas|charlas culturales|pesca|termas|(Por ejemplo: caminatas)", servicios_turisticos)) %>% 
  group_by(servicios_turisticos) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


discapacidad <- encuesta_turista %>%
  group_by(discapacidad) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


comida <- encuesta_turista %>%
  group_by(adaptabilidad_comida) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)






# Obtener una lista de todos los objetos en el entorno de trabajo
objetos <- ls()

# Mostrar la lista de objetos
print(objetos)




# Agregar cada objeto a una hoja diferente -----

wb <- createWorkbook()


addWorksheet(wb, "EncuestaTurista")
writeData(wb, sheet = "EncuestaTurista", genero)

addWorksheet(wb, "PromedioPorGenero")
writeData(wb, sheet = "PromedioPorGenero", promedio_por_genero)

addWorksheet(wb, "PorcentajePuebloIndigena")
writeData(wb, sheet = "PorcentajePuebloIndigena", porcentaje_pueblo_indigena)


saveWorkbook(wb, file.path("bbdd", "resultados_encuesta2.xlsx"))

## Obtener una lista de todos los objetos en el entorno de trabajo
objetos <- ls()

# Crear un libro de Excel para todos los objetos
wb <- createWorkbook()

# Agregar cada objeto a una hoja diferente en el libro de Excel
for (objeto in objetos) {
  addWorksheet(wb, objeto)
  writeData(wb, sheet = objeto, get(objeto))
}

# Guardar el libro de Excel
saveWorkbook(wb, file.path("bbdd", "resultados_encuesta2.xlsx"))
