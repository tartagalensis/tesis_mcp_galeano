# Carga de las bibliotecas necesarias para el análisis de datos
library(tidyverse)
library(janitor)


# Lectura de datos de elecciones de 2011 a 2021 y visualización inicial de los datos
elecciones_11_21 <- read_csv("DipNacPBAxcircuito_2011-21.csv") %>% 
  print()


# Procesamiento de los datos electorales de 2023
# Filtrado para obtener solo votos positivos para el distrito específico y cargo de "DIPUTADO NACIONAL"
# Ajuste y formateo de los códigos de provincia y departamento
# Agrupación de datos por provincia, departamento, circuito, y agrupación política
# Suma de votos por agrupación y eliminación de duplicados
votos_positivos_23 <- read_csv("data_electoral/2023_Generales/ResultadoElectorales_2023_Generales.csv")  %>% 
  filter(distrito_id == 2) %>% 
  filter(votos_tipo == "POSITIVO") %>% 
  filter(cargo_nombre == "DIPUTADO NACIONAL") %>% 
  mutate(codprov = str_pad(distrito_id,2,side="left",pad = "0"),
         coddepto = str_pad(seccion_id,3,side="left",pad = "0")) %>% 
  select(codprov, coddepto, circuito = circuito_id, mesa_id,  
         agrupacion_id, agrupacion_nombre, votos_cantidad) %>%
  group_by(codprov, coddepto, circuito, agrupacion_id, agrupacion_nombre) %>% 
  mutate(votos_cantidad = sum(votos_cantidad, na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(codprov, coddepto, circuito, agrupacion_nombre, agrupacion_id, votos_cantidad) %>% 
  unique() %>% 
  print()


# Proceso similar para obtener datos sobre electores y votos en blanco para 2023
# Se agrupan los datos por codificaciones geográficas y se suman los votos en blanco y el total de electores

electores_2023 <- read_csv("data_electoral/2023_Generales/ResultadoElectorales_2023_Generales.csv")  %>% 
  filter(distrito_id == 2) %>% 
  filter(votos_tipo == "EN BLANCO") %>% 
  filter(cargo_nombre == "DIPUTADO NACIONAL")%>% 
  mutate(codprov = str_pad(distrito_id,2,side="left",pad = "0"),
         coddepto = str_pad(seccion_id,3,side="left",pad = "0")) %>% 
  select(codprov, coddepto, circuito = circuito_id,  mesa_electores, votos_cantidad) %>% 
  group_by(codprov, coddepto, circuito) %>% 
  mutate(electores = sum(mesa_electores,na.rm=TRUE),
         blancos = sum(votos_cantidad,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(codprov, coddepto, circuito, electores, blancos) %>% 
  unique() %>% 
  print()

# Proceso similar para datos de votos nulos en 2023
# Se suman los votos nulos y el total de electores por las mismas codificaciones geográficas

nulos_2023 <- read_csv("data_electoral/2023_Generales/ResultadoElectorales_2023_Generales.csv")  %>% 
  filter(distrito_id == 2) %>% 
  filter(votos_tipo == "NULO") %>% 
  filter(cargo_nombre == "DIPUTADO NACIONAL") %>% 
  mutate(codprov = str_pad(distrito_id,2,side="left",pad = "0"),
         coddepto = str_pad(seccion_id,3,side="left",pad = "0")) %>% 
  select(codprov, coddepto, circuito = circuito_id,  mesa_electores, votos_cantidad) %>% 
  group_by(codprov, coddepto, circuito) %>% 
  mutate(electores = sum(mesa_electores,na.rm=TRUE),
         nulos = sum(votos_cantidad,na.rm=TRUE)) %>% 
  ungroup() %>% 
  select(codprov, coddepto, circuito, electores, nulos) %>% 
  unique() %>% 
  print()

# Carga y procesamiento de metadatos relacionados con las divisiones geográficas
# Selección y transformación de columnas relevantes y filtrado para la provincia de Buenos Aires

metadata_pba <- read_csv("equivalencias_dineINDEC.csv") %>% 
  select(codprov, coddepto, codprov_censo, coddepto_censo, nomdepto_censo, name_prov) %>% 
  mutate(DPTO = paste0(codprov_censo ,coddepto_censo)) %>% 
  filter(name_prov == "BUENOS AIRES") %>% 
  print()

# Combinación de los distintos conjuntos de datos para formar una base completa para la elección de 2023
# Se incluyen datos de votos positivos, electores, blancos, nulos y metadatos
# Se realiza un join con los metadatos y se seleccionan y transforman las columnas finales

eleccion_2023 <- votos_positivos_23 %>% 
  left_join(electores_2023) %>% 
  left_join(nulos_2023) %>% 
  mutate(id_eleccion = "dip_gral2023") %>% 
  left_join(metadata_pba) %>% 
  select(id_eleccion, name_prov, codprov, codprov_censo, nomdepto = nomdepto_censo, coddepto, coddepto_censo, DPTO, circuito,
         electores, blancos, nulos, nombre_frente = agrupacion_nombre, id_fuerza_politica = agrupacion_id, votos_partido_politico = votos_cantidad) %>%
  mutate(id_fuerza_politica = as.character(id_fuerza_politica)) %>% 
  print()

# Creación de una base de datos consolidada que incluye elecciones de 2011-2023
# Se combinan los datos históricos con los de la elección de 2023

base_madre <- elecciones_11_21 %>% 
  bind_rows(eleccion_2023) %>% 
  print()

# Exportación de la base de datos consolidada a un nuevo archivo CSV

base_madre %>% write_csv("DipNacPBAxcircuito_2011-23.csv")
