
library(tidyverse)
library(janitor)

base_elecciones <- read_csv("DipNacPBAxcircuito_2011-23.csv") %>% 
  filter(id_eleccion %in% c("dip_gral2015","dip_gral2017","dip_gral2019","dip_gral2021","dip_gral2023")) %>% 
  filter(electores > 3000) %>% 
  print()


votos_positivos <- base_elecciones %>% 
  group_by(DPTO, circuito) %>% summarise(votos_positivos =  sum(votos_partido_politico))

base_elecciones %>% 
  left_join(votos_positivos) %>% 
  mutate(porcentaje = round(votos_partido_politico / votos_positivos,4)*100) %>% 
  select(id_eleccion, nombre_frente, votos_partido_politico, votos_positivos, porcentaje) %>% 
  group_by(id_eleccion, nombre_frente) %>%
  summarise(media = mean(porcentaje, na.rm = TRUE),
            mediana = median(porcentaje, na.rm = TRUE)) %>% 
  unique() %>% 
  view()




