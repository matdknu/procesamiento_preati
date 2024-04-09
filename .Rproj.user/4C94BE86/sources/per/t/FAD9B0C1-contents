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
  #mutate(comuna_emprendimiento = ifelse(comuna_emprendimiento == "Otra", comuna_emprendimiento_otra, comuna_emprendimiento)) %>% 
  group_by(comuna_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

zona_emprendimiento <- encuesta_turista %>%
  group_by(zona_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); zona_emprendimiento

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
  mutate(porcentaje = (total / sum(total)) * 100); turismo_principal


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
  mutate(porcentaje = (total / sum(total)) * 100); tramite_formalizacion



tramite_formalizacion_pueblos <- encuesta_turista %>%
  separate_rows(tramite_formalizacion, sep = ", ") %>%
  group_by(tramite_formalizacion, pueblo_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


inicio_actividades <- encuesta_turista %>%
  group_by(inicio_actividades) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); inicio_actividades


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
  group_by(instalaciones_turisticas, ID ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); instalaciones_turisticas


herramienta_difusion <- encuesta_turista %>%
  separate_rows(herramienta_difusion, sep = ", ") %>%
  group_by(herramienta_difusion, ID) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); herramienta_difusion


idioma <- encuesta_turista %>%
  separate_rows(idioma, sep = ", ") %>%
  group_by(idioma) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

organizacion_indigena <- encuesta_turista %>%
  #separate_rows(idioma, sep = ", ") %>%
  group_by(organizaciones_indigenas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); organizacion_indigena


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


servicios_recreativos <- encuesta_turista %>%
  separate_rows(servicios_recreativos, sep = ", ") %>%
  group_by(servicios_recreativos) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); servicios_recreativos




comida <- encuesta_turista %>%
  group_by(adaptabilidad_comida) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

admision_mascotas <- encuesta_turista %>%
  group_by(admision_mascotas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); admision_mascotas

n_turistas_alta <- encuesta_turista %>%
  group_by(n_turistas_alta) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); n_turistas_alta


encuesta_turista %>% 
  group_by(invita_actividad) %>% 
  select(invita_actividad) %>% 
  separate_rows(invita_actividad, sep = ",") %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


n_turistas_suma <- encuesta_turista %>%
  mutate(n_turistas_alta = as.numeric(n_turistas_alta)) %>%
  group_by(comuna_emprendimiento) %>%
  summarize(total = sum(n_turistas_alta, na.rm = TRUE))

n_turistas_suma_pueblo <- encuesta_turista %>%
  mutate(n_turistas_alta = as.numeric(n_turistas_alta)) %>%
  group_by(pueblo_indigena) %>%
  summarize(total = sum(n_turistas_alta, na.rm = TRUE))


gasto_turista <- encuesta_turista %>%
  group_by(gasto_turista) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); gasto_turista

gasto_turista_comuna <- encuesta_turista %>%
  filter(comuna_emprendimiento %in% c("Rapa Nui", "Nueva Imperial")) %>% 
  group_by(gasto_turista, comuna_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); gasto_turista


actividad_complementaria <- encuesta_turista %>%
 separate_rows(actividad_complementaria, sep = ", ") %>%
  group_by(actividad_complementaria) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


n_personas_emprendimiento_alta <- encuesta_turista %>%
  #separate_rows(actividad_complementaria, sep = ", ") %>%
  group_by(n_personas_emprendimiento_alta) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


n_personas_emprendimiento_baja <- encuesta_turista %>%
  #separate_rows(actividad_complementaria, sep = ", ") %>%
  group_by(n_personas_emprendimiento_baja) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


financiamiento <- encuesta_turista %>%
  separate_rows(financiamiento, sep = ", ") %>%
  group_by(financiamiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


financiamiento_institucion <- encuesta_turista %>%
  separate_rows(financiamiento_institucion , sep = ", ") %>%
  group_by(financiamiento_institucion ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

compra_comunidad <- encuesta_turista %>%
  group_by(compra_comunidad) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

relaciones_externas <- encuesta_turista %>%
  group_by(relaciones_externas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


vinculacion_otros_actores <- encuesta_turista %>%
  group_by(vinculacion_otros_actores) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

artesania_local <- encuesta_turista %>%
  group_by(artesania_local) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

resguardo_patrimonial <- encuesta_turista %>%
  group_by(resguardo_patrimonial) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)





informacion_ancestral <- encuesta_turista %>%
  group_by(informacion_ancestral) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

informacion_ancestral_pueblo <- encuesta_turista %>%
  group_by(informacion_ancestral, pueblo_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); informacion_ancestral_pueblo


normas_comportamiento_personas  <- encuesta_turista %>%
  group_by(normas_comportamiento_personas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); normas_comportamiento_personas

normas_comportamiento_servicios <- encuesta_turista %>%
  group_by(normas_comportamiento_servicios ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); normas_comportamiento_personas


gestion_basura  <- encuesta_turista %>%
  separate_rows(gestion_basura , sep = ", ") %>%
  group_by(gestion_basura) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); gestion_basura 


eficiencia_energetica  <- encuesta_turista %>%
  separate_rows(eficiencia_energetica , sep = ", ") %>%
  group_by(eficiencia_energetica ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); eficiencia_energetica 

disponibilidad_agua  <- encuesta_turista %>%
  #separate_rows(disponibilidad_agua , sep = ", ") %>%
  group_by(disponibilidad_agua ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); disponibilidad_agua


recuperacion_ambiental  <- encuesta_turista %>%
  group_by(recuperacion_ambiental) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); recuperacion_ambiental


capacidad_de_carga  <- encuesta_turista %>%
  group_by(capacidad_de_carga) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); capacidad_de_carga 

capacitaciones_recibidas <- encuesta_turista %>% select(capacitaciones_recibidas) %>% 
  separate_rows(capacitaciones_recibidas , sep = ", ") %>% 
  group_by(capacitaciones_recibidas) %>% 
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

capacitaciones_necesitadas <- encuesta_turista %>% select(capacitaciones_necesitadas) %>% 
  separate_rows(capacitaciones_necesitadas , sep = ", ") %>% 
  group_by(capacitaciones_necesitadas) %>% 
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); capacitaciones_necesitadas


turismo_afectado  <- encuesta_turista %>%
  group_by(turismo_afectado ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); turismo_afectado 


turismo_afectado  <- encuesta_turista %>%
  group_by(turismo_afectado ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); turismo_afectado

## Crisis-----

crisis_desarrollo  <- encuesta_turista %>%
  group_by(crisis_desarrollo ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_desarrollo


crisis_tipo_evento  <- encuesta_turista %>%
  separate_rows(crisis_tipo_evento , sep = ", ") %>%
  group_by(crisis_tipo_evento ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_tipo_evento


crisis_tipo_evento  <- encuesta_turista %>%
  separate_rows(crisis_tipo_evento , sep = ", ") %>%
  group_by(crisis_tipo_evento ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_tipo_evento

crisis_recuperacion  <- encuesta_turista %>%
  group_by(crisis_recuperacion ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_recuperacion

crisis_protocolo_emergencia  <- encuesta_turista %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_protocolo_emergencia ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_protocolo_emergencia


crisis_zona_riesgo  <- encuesta_turista %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_zona_riesgo ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_zona_riesgo


crisis_zona_segura  <- encuesta_turista %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_zona_segura  ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_zona_segura 

crisis_protocolo_sanitario  <- encuesta_turista %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_protocolo_sanitario) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_protocolo_sanitario

crisis_primeros_auxilios <- encuesta_turista %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_primeros_auxilios) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_primeros_auxilios

library(openxlsx)


# Obtener una lista de los nombres de los objetos en el entorno
objetos <- ls()

# Crear un nuevo libro de Excel
wb <- createWorkbook()

# Iterar sobre cada objeto y guardarlo en una hoja separada del libro de Excel
for (objeto in objetos) {
  # Obtener el objeto del entorno
  obj <- get(objeto)
  
  # Crear una nueva hoja en el libro de Excel y escribir el objeto en ella
  addWorksheet(wb, sheetName = objeto)
  writeData(wb, sheet = objeto, x = obj)
}

# Guardar el libro de Excel
saveWorkbook(wb, "descriptivos_encuesta33.xlsx")
