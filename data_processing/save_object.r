# Lista de objetos que deseas escribir en hojas de Excel
objetos <- c("actividad_complementaria", "admision_mascotas", "ano_inicio", "artesania_local", 
             "capacidad_de_carga", "comida", "compra_comunidad", "comuna", 
             "comuna_otra_pegada", "discapacidad", "disponibilidad_agua", 
             "educacion", "eficiencia_energetica", "encuesta_turista", 
             "espacio_desarrollo", "financiamiento", "financiamiento_institucion", 
             "gasto_turista", "gasto_turista_comuna", "genero", "gestion_basura", 
             "herramienta_difusion", "idioma", "informacion_ancestral", 
             "informacion_ancestral_pueblo", "inicio_actividades", 
             "instalaciones_turisticas", "internet", "justificacion_no_inicio", 
             "n_personas_emprendimiento_alta", "n_personas_emprendimiento_baja", 
             "n_turistas_alta", "n_turistas_suma", "n_turistas_suma_pueblo", 
             "normas_comportamiento_personas", "normas_comportamiento_servicios", 
             "objeto", "organizacion_no_indigena", "porcentaje_pueblo_indigena", 
             "promedio_por_genero", "recuperacion_ambiental", "relaciones_externas", 
             "resguardo_patrimonial", "sello_indigena", "servicios_turisticos", 
             "sociedad_emprendimiento", "temporada_abierto", "tipos_sociedad", 
             "tramite_formalizacion", "tramite_formalizacion_pueblos", 
             "turismo_afectado", "turismo_principal", "turismo_principal_pueblo", 
             "vinculacion_otros_actores", "zona_emprendimiento")

# Iterar sobre cada objeto y agregarlo a una hoja en el libro de Excel
for (objeto in objetos) {
  if (exists(objeto)) {  # Verificar si el objeto existe en el entorno de trabajo
    addWorksheet(wb, objeto)  # Agregar una nueva hoja con el nombre del objeto
    writeData(wb, sheet = objeto, get(objeto))  # Escribir los datos en la hoja
  } else {
    warning(paste("El objeto", objeto, "no existe en el entorno de trabajo. No se pudo escribir en una hoja."))
  }
}

# Guardar el libro de Excel en un archivo
saveWorkbook(wb, "resultados_varios.xlsx")
