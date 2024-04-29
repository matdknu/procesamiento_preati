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
bbdd <- read_excel("bbdd/bbdd_emprendedor_v2.xlsx")


ls <- variable.names(bbdd)


## Limpieza de datos ----
### Reemplazo de NA----

encuesta_emprendedor <- bbdd %>% mutate_all(funs(ifelse(is.na(.), "No responde", .))) 
encuesta_emprendedor$ID <- seq_along(bbdd[,1])

### Cambio de nombre de variables ----

# Renombrar todas las variables
names(encuesta_emprendedor) <- c("a1_genero", 
                                 "a2_edad",
                                 "a3_pueblo_indigena",
                                 "a4_educacion", 
                                 "a5_conexion_internet",
                                 "a6_comuna_emprendimiento", 
                                 "a6otra_comuna_emprendimiento_otra",
                                 "a7_zona_emprendimiento",
                                "b1_sociedad_emprendimiento",
                                "b2_sociedad_emprendimiento_otra",
                                "b3_ano_inicio",
                                "b4_turismo_principal",
                                "b5_temporada_abierto",
                                "b6_tramite_formalizacion",
                                "b7_inicio_actividades",
                                "b8_tipos_sociedad", 
                                "b8otra_tipos_sociedad_otras",
                                "b9_categoria_sernatur", 
                                "b10_justificacion_no_inicio", "b11_sello_indigena",
                                "b12_espacio_desarrollo", "b13_instalaciones_turisticas", "b13otro_instalaciones_turisticas_otro",
                                "b14_herramienta_difusion", "b14otra_herramienta_difusion_otra", "b15_idioma", "b15otra_idioma_otra",
                                "b16_organizaciones_indigenas", "b17_organizaciones_no_indigenas", "b18_organizaciones_no_indigenas_cual",
                                "b19_servicios_turisticos", "b19otro_servicios_turisticos_otro", "b20_servicios_recreativos",
                                "servicios_recreativos_otro", "discapacidad", "adaptabilidad_comida",
                                "admision_mascotas", "n_turistas_alta", "gasto_turista", "ingreso_otra",
                                "actividad_complementaria", "actividad_complementaria_otra", "n_personas_emprendimiento_alta",
                                "n_personas_emprendimiento_baja", "n_personas_emprendimiento_familia", "financiamiento",
                                "financiamiento_especifique", "financiamiento_institucion", "financiamiento_institucion_especifique",
                                "invita_actividad", 
                                "invita_actividad_otra",
                                "compra_comunidad",
                                "relaciones_externas",
                                "vinculacion_otros_actores",
                                "artesania_local",
                                "resguardo_patrimonial", 
                                "informacion_ancestral",
                                "normas_comportamiento_personas",
                                "normas_comportamiento_servicios", 
                                "gestion_basura",
                                "gestion_basura_otro", 
                                "eficiencia_energetica", 
                                "eficiencia_energetica_cual",
                                "disponibilidad_agua",
                                "abastecimiento_agua",
                                "recuperacion_ambiental", "capacidad_de_carga",
                                "turismo_afectado",
                                "capacitaciones_recibidas",
                                "capacitaciones_necesitadas",
                                "venta_total",
                                "crisis_desarrollo", 
                                "crisis_tipo_evento", 
                                "crisis_tipo_evento_otro",
                                "crisis_desercion_laboral",
                                "crisis_recuperacion",
                                "crisis_medios_comunicacion",
                                "crisis_medios_comunicacion_otros",
                               "crisis_actores",
                               "crisis_actores_cual", 
                               "crisis_protocolo_emergencia",
                               "crisis_zona_riesgo",
                                "crisis_zona_segura", 
                               "crisis_protocolo_sanitario", 
                               "crisis_primeros_auxilios", 
                               "ID")

# Verificar los nuevos nombres de las variables
names(encuesta_emprendedor)

# guardar base nueva ----

# Modificación variable

encuesta_emprendedor <- encuesta_emprendedor %>% mutate(a2_edad = as.numeric(if_else(a2_edad == "NA", NA_character_, a2_edad))) %>% 
  mutate(a6_comuna_emprendimiento = ifelse(a6_comuna_emprendimiento== "Otra", a6_comuna_emprendimiento, a6_comuna_emprendimiento)) 



# Guardar BBDD-----

saveRDS(encuesta_emprendedor, "bbdd/encuesta_emprendedor_v2.rds")
write.csv(encuesta_emprendedor, "bbdd/encuesta_emprendedor_v2.csv2")
