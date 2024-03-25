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

bbdd <- read_excel("bbdd/bbdd_emprendedor.xlsx")

## Limpieza de datos ----
### Reemplazo de NA----

bbdd <- bbdd %>% mutate_all(funs(ifelse(is.na(.), "No responde", .)))

### Cambio de nombre de variables ----








