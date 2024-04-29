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

getwd()

bbdd <- readRDS("bbdd/encuesta_emprendedor_v2.rds")
#bbdd <- readRDS("bbdd/encuesta_emprendedor.rds")

str(bbdd)


genero <-bbdd %>%
  group_by(a1_genero) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


promedio_por_genero <- bbdd %>%
  group_by(a1_genero) %>%
  summarize(promedio_edad_por_genero = mean(a2_edad, na.rm = TRUE))



porcentaje_pueblo_indigena <- bbdd %>%
  group_by(a3_pueblo_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)



educacion <- bbdd %>%
  group_by(a4_educacion) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

internet <- bbdd %>%
  group_by(a5_conexion_internet) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

bbdd$a6otra_comuna_emprendimiento_otra

comuna <- bbdd %>%
  group_by(a6_comuna_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

comuna_otra_pegada <- bbdd %>%
  #mutate(comuna_emprendimiento = ifelse(comuna_emprendimiento == "Otra", comuna_emprendimiento_otra, comuna_emprendimiento)) %>% 
  group_by(a6otra_comuna_emprendimiento_otra) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

zona_emprendimiento <- bbdd %>%
  group_by(a7_zona_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); zona_emprendimiento



sociedad_emprendimiento <- bbdd %>%
  group_by(b1_sociedad_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

bbdd$b3_ano_inicio

ano_inicio <- bbdd %>%
  group_by(b3_ano_inicio) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

bbdd$a3_pueblo_indigena

turismo_principal <- bbdd %>%
  group_by(b4_turismo_principal) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); turismo_principal


turismo_principal_pueblo <- bbdd %>%
  filter(b4_turismo_principal == "Sí") %>%
  group_by(a3_pueblo_indigena) %>%
  summarize(total_si = n())# %>%
  #left_join(bbdd %>% group_by(a3_pueblo_indigena) %>% summarize(total_respuestas = n()), by = "pueblo_indigena") %>%
  #mutate(porcentaje_si = (total_si / total_respuestas) * 100)



#temporada_abierto <- bbdd %>%
#  mutate(b5_temporada_abierto = str_replace(b5_temporada_abierto, "Temporada Alta \\(Diciembre-Marzo\\),", "")) %>%
#  group_by(b5_temporada_abierto) %>%
#  summarize(total = n()) %>%
#  mutate(porcentaje = (total / sum(total)) * 100)
#

temporada_abierto <- bbdd %>%
  separate_rows(b5_temporada_abierto, sep = ", ") %>% #Aqui tuve que separarlas por la coma.
  filter(b5_temporada_abierto != "sin parar") %>% 
  group_by(b5_temporada_abierto) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)



tramite_formalizacion <- bbdd %>%
  separate_rows(b6_tramite_formalizacion, sep = ", ") %>%
  group_by(b6_tramite_formalizacion) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); tramite_formalizacion


bbdd$abastecimiento_agua

abastecimiento_agua <- bbdd %>%
  separate_rows(abastecimiento_agua, sep = ", ") %>%
  group_by(abastecimiento_agua) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); abastecimiento_agua


tramite_formalizacion_pueblos <- bbdd %>%
  separate_rows(b6_tramite_formalizacion, sep = ", ") %>%
  group_by(b6_tramite_formalizacion, a3_pueblo_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

inicio_actividades <- bbdd %>%
  group_by(b7_inicio_actividades) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); inicio_actividades


bbdd$abastecimiento_agua

justificacion_no_inicio <- bbdd %>%
#  filter(!grepl("agua potable|etc\\.\\)", b10_justificacion_no_inicio)) %>% 
  separate_rows(b10_justificacion_no_inicio, sep = ", ") %>%
  group_by(b10_justificacion_no_inicio) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


tipos_sociedad <- bbdd %>%
  separate_rows(b8_tipos_sociedad, sep = ", ") %>%
  group_by(b8_tipos_sociedad) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


sello_indigena <- bbdd %>%
  group_by(b11_sello_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

espacio_desarrollo <- bbdd %>%
  group_by(b12_espacio_desarrollo) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

bbdd$

instalaciones_turisticas <- bbdd %>%
  separate_rows(b13_instalaciones_turisticas, sep = ", ") %>%
  group_by(b13_instalaciones_turisticas ) %>%
  summarize(total = n()) # %>%
 # mutate(porcentaje = (total / sum(total)) * 100); instalaciones_turisticas


herramienta_difusion <- bbdd %>%
  separate_rows(b14_herramienta_difusion, sep = ", ") %>%
  group_by(b14_herramienta_difusion) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

idioma <- bbdd %>%
  separate_rows(b15_idioma, sep = ", ") %>%
  group_by(b15_idioma) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)



organizacion_indigena <- bbdd %>%
  separate_rows(b16_organizaciones_indigenas, sep = ", ") %>%
  group_by(b16_organizaciones_indigenas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); organizacion_indigena


organizacion_no_indigena <- bbdd %>%
  #separate_rows(idioma, sep = ", ") %>%
  group_by(b17_organizaciones_no_indigenas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

bbdd$


servicios_turisticos <- bbdd %>%
  separate_rows(b19_servicios_turisticos, sep = ", ") %>%
  group_by(b19_servicios_turisticos, ID) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


discapacidad <- bbdd %>%
  group_by(discapacidad) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

servicios_recreativos <- bbdd %>%
  separate_rows(b20_servicios_recreativos, sep = ", ") %>%
  group_by(b20_servicios_recreativos) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); servicios_recreativos


categoria_sernatur <- bbdd %>%
  separate_rows(b9_categoria_sernatur, sep = ", ") %>%
  group_by(b9_categoria_sernatur) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); categoria_sernatur


comida <- bbdd %>%
  group_by(adaptabilidad_comida) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

admision_mascotas <- bbdd %>%
  group_by(admision_mascotas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); admision_mascotas

n_turistas_alta <- bbdd %>%
  group_by(n_turistas_alta) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); n_turistas_alta


invita_actividad <- bbdd %>% 
  select(invita_actividad) %>% 
  separate_rows(invita_actividad, sep = ",") %>%
  group_by(invita_actividad) %>% 
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


n_turistas_suma <- bbdd %>%
  mutate(n_turistas_alta = as.numeric(n_turistas_alta)) %>%
  group_by(a6_comuna_emprendimiento) %>%
  summarize(total = sum(n_turistas_alta, na.rm = TRUE))

n_turistas_suma_pueblo <- bbdd %>%
  mutate(n_turistas_alta = as.numeric(n_turistas_alta)) %>%
  group_by(a3_pueblo_indigena) %>%
  summarize(total = sum(n_turistas_alta, na.rm = TRUE))


gasto_turista <- bbdd %>%
  group_by(gasto_turista) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); gasto_turista



gasto_turista_comuna <- bbdd %>%
 # filter(a6_comuna_emprendimiento %in% c("Rapa Nui", "Nueva Imperial")) %>% 
  group_by(gasto_turista, a6_comuna_emprendimiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); gasto_turista_comuna


actividad_complementaria <- bbdd %>%
 separate_rows(actividad_complementaria, sep = ", ") %>%
  group_by(actividad_complementaria) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


n_personas_emprendimiento_alta <- bbdd %>%
  #separate_rows(actividad_complementaria, sep = ", ") %>%
  group_by(n_personas_emprendimiento_alta) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


n_personas_emprendimiento_baja <- bbdd %>%
  #separate_rows(actividad_complementaria, sep = ", ") %>%
  group_by(n_personas_emprendimiento_baja) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

n_personas_emprendimiento_familia <- bbdd %>%
  #separate_rows(actividad_complementaria, sep = ", ") %>%
  group_by(n_personas_emprendimiento_familia) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


financiamiento <- bbdd %>%
  separate_rows(financiamiento, sep = ", ") %>%
  group_by(financiamiento) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


financiamiento_institucion <- bbdd %>%
  separate_rows(financiamiento_institucion , sep = ", ") %>%
  group_by(financiamiento_institucion ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

compra_comunidad <- bbdd %>%
  group_by(compra_comunidad) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

relaciones_externas <- bbdd %>%
  group_by(relaciones_externas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)


vinculacion_otros_actores <- bbdd %>%
  group_by(vinculacion_otros_actores) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

artesania_local <- bbdd %>%
  group_by(artesania_local) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

resguardo_patrimonial <- bbdd %>%
  group_by(resguardo_patrimonial) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)



informacion_ancestral <- bbdd %>%
  group_by(informacion_ancestral) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

informacion_ancestral_pueblo <- bbdd %>%
  group_by(informacion_ancestral, a3_pueblo_indigena) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); informacion_ancestral_pueblo


normas_comportamiento_personas  <- bbdd %>%
  group_by(normas_comportamiento_personas) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); normas_comportamiento_personas

normas_comportamiento_servicios <- bbdd %>%
  group_by(normas_comportamiento_servicios ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); normas_comportamiento_personas


gestion_basura  <- bbdd %>%
  separate_rows(gestion_basura , sep = ", ") %>%
  group_by(gestion_basura) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); gestion_basura 


eficiencia_energetica  <- bbdd %>%
  separate_rows(eficiencia_energetica , sep = ", ") %>%
  group_by(eficiencia_energetica ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); eficiencia_energetica 

disponibilidad_agua  <- bbdd %>%
  #separate_rows(disponibilidad_agua , sep = ", ") %>%
  group_by(disponibilidad_agua ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); disponibilidad_agua


recuperacion_ambiental  <- bbdd %>%
  group_by(recuperacion_ambiental) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); recuperacion_ambiental


capacidad_de_carga  <- bbdd %>%
  group_by(capacidad_de_carga) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); capacidad_de_carga 

capacitaciones_recibidas <- bbdd %>% select(capacitaciones_recibidas) %>% 
  separate_rows(capacitaciones_recibidas , sep = ", ") %>% 
  group_by(capacitaciones_recibidas) %>% 
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100)

capacitaciones_necesitadas <- bbdd %>% select(capacitaciones_necesitadas) %>% 
  separate_rows(capacitaciones_necesitadas , sep = ", ") %>% 
  group_by(capacitaciones_necesitadas) %>% 
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); capacitaciones_necesitadas


turismo_afectado  <- bbdd %>%
  group_by(turismo_afectado ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); turismo_afectado 


turismo_afectado  <- bbdd %>%
  group_by(turismo_afectado ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); turismo_afectado

## Crisis-----

crisis_desarrollo  <- bbdd %>%
  group_by(crisis_desarrollo ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_desarrollo


crisis_tipo_evento  <- bbdd %>%
  separate_rows(crisis_tipo_evento , sep = ", ") %>%
  group_by(crisis_tipo_evento ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_tipo_evento


crisis_medios_comunicacion  <- bbdd %>%
  separate_rows(crisis_medios_comunicacion , sep = ", ") %>%
  group_by(crisis_medios_comunicacion ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_medios_comunicacion

encuesta_emprendedor$crisis_actores

crisis_actores  <- bbdd %>%
  separate_rows(crisis_actores , sep = ", ") %>%
  group_by(crisis_actores) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_actores

crisis_recuperacion  <- bbdd %>%
  group_by(crisis_recuperacion ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_recuperacion

crisis_protocolo_emergencia  <- bbdd %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_protocolo_emergencia ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_protocolo_emergencia


crisis_zona_riesgo  <- bbdd %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_zona_riesgo ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_zona_riesgo


crisis_zona_segura  <- bbdd %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_zona_segura  ) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_zona_segura 

crisis_protocolo_sanitario  <- bbdd %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_protocolo_sanitario) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_protocolo_sanitario

crisis_primeros_auxilios <- bbdd %>%
  #separate_rows(crisis_protocolo_emergencia , sep = ", ") %>%
  group_by(crisis_primeros_auxilios) %>%
  summarize(total = n()) %>%
  mutate(porcentaje = (total / sum(total)) * 100); crisis_primeros_auxilios

bbdd$agu


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
