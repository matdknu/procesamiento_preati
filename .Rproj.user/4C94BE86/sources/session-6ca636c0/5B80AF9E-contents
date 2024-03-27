# Procesamiento de data------
## Matías Deneken.

# Cargar paquetes ---

pacman::p_load(dplyr, 
               sjmisc, 
               tidyverse, 
               sjlabelled, 
               ggplot2,
               readxl, 
               openxlsx)

# Cargar BBDD----

rm(list=ls())       # borrar todos los objetos en el espacio de trabajo
options(scipen=999) # valores sin notación científica

bbdd <- read_excel("bbdd/bbdd_emprendedor.xlsx")

## Limpieza de datos ----
### Reemplazo de NA----

encuesta_turista <- bbdd %>% mutate_all(funs(ifelse(is.na(.), "No responde", .)))

### Cambio de nombre de variables ----

# Renombrar todas las variables
names(encuesta_turista) <- c("genero", "edad", "pueblo_indigena", "educacion", "conexion_internet",
                 "comuna_emprendimiento", "comuna_emprendimiento_otra", "zona_emprendimiento",
                 "sociedad_emprendimiento", "sociedad_emprendimiento_otra", "ano_inicio",
                 "turismo_principal", "temporada_abierto", "tramite_formalizacion",
                 "inicio_actividades", "tipos_sociedad", "tipos_sociedad_otras",
                 "categoria_sernatur", "justificacion_no_inicio", "sello_indigena",
                 "espacio_desarrollo", "instalaciones_turisticas", "instalaciones_turisticas_otro",
                 "herramienta_difusion", "herramienta_difusion_otra", "idioma", "idioma_otra",
                 "organizaciones_indigenas", "organizaciones_no_indigenas", "organizaciones_no_indigenas_cual",
                 "servicios_turisticos", "servicios_turisticos_otro", "servicios_recreativos",
                 "servicios_recreativos_otro", "discapacidad", "adaptabilidad_comida",
                 "admision_mascotas", "n_turistas_alta", "gasto_turista", "ingreso_otra",
                 "actividad_complementaria", "actividad_complementaria_otra", "n_personas_emprendimiento_alta",
                 "n_personas_emprendimiento_baja", "n_personas_emprendimiento_familia", "financiamiento",
                 "financiamiento_especifique", "financiamiento_institucion", "financiamiento_institucion_especifique",
                 "invita_actividad", "invita_actividad_otra", "compra_comunidad", "relaciones_externas",
                 "vinculacion_otros_actores", "artesania_local", "resguardo_patrimonial", "informacion_ancestral",
                 "normas_comportamiento_personas", "normas_comportamiento_servicios", "gestion_basura",
                 "gestion_basura_otro", "eficiencia_energetica", "eficiencia_energetica_cual",
                 "disponibilidad_agua", "abastecimiento_agua", "recuperacion_ambiental", "capacidad_de_carga",
                 "turismo_afectado", "capacitaciones_recibidas", "capacitaciones_necesitadas", "venta_total",
                 "crisis_desarrollo", "crisis_tipo_evento", "crisis_tipo_evento_otro", "crisis_desercion_laboral",
                 "crisis_recuperacion", "crisis_medios_comunicacion", "crisis_medios_comunicacion_otros",
                 "crisis_actores", "crisis_actores_cual", "crisis_protocolo_emergencia", "crisis_zona_riesgo",
                 "crisis_zona_segura", "crisis_protocolo_sanitario", "crisis_primeros_auxilios")

# Verificar los nuevos nombres de las variables
names(encuesta_turista)

# guardar base nueva ----

# Modificación variable

encuesta_turista <- encuesta_turista %>% mutate(edad = as.numeric(if_else(edad == "NA", NA_character_, edad))) %>% 
  mutate(comuna_emprendimiento = ifelse(comuna_emprendimiento == "Otra", comuna_emprendimiento_otra, comuna_emprendimiento)) 


# Guardar BBDD-----

saveRDS(encuesta_turista, "bbdd/encuesta_turista.rds")
