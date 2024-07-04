library(tidyverse)


#Genero Dataset eleccion 2023 ####
votos_positivos <- read_csv("data_electoral/2023_Generales/ResultadoElectorales_2023_Generales.csv")  %>% 
  filter(votos_tipo == "POSITIVO") %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3 ) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores, agrupacion_id, votos_cantidad) %>% 
  pivot_wider(names_from = agrupacion_id,values_from = votos_cantidad) %>% 
  janitor::clean_names() %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id, mesa_electores, 
         gral_uxp = x20134, gral_jxc = x20132, gral_lla = x20135, gral_fit = x20136)

votos_blancos <- read_csv("data_electoral/2023_Generales/ResultadoElectorales_2023_Generales.csv")  %>% 
  filter(votos_tipo == "EN BLANCO") %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3 ) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores,  gral_blancos = votos_cantidad) %>% 
  print()


grales_2023 <- votos_positivos %>% left_join(votos_blancos) %>% 
  print()

grales_2023 %>% write_csv("grales_dipnac_pba_2023.csv")



#Genero Dataset eleccion 2021 ####
votos_positivos <- read_csv("data_electoral/crudos_dine/2021/GRALES/ResultadosElectorales.csv",
         locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "POSITIVO") %>% 
  janitor::clean_names() %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores, agrupacion_id, votos_cantidad) %>% 
  pivot_wider(names_from = agrupacion_id,values_from = votos_cantidad) %>% 
  janitor::clean_names() %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id, mesa_electores, 
         gral_fdt = x507, gral_jxc = x506, gral_lla = x503, gral_fit = x504, gral_vcv = x508, gral_valores = x501) %>% 
  print()

votos_blancos <- read_csv("data_electoral/crudos_dine/2021/GRALES/ResultadosElectorales.csv",
                          locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "BLANCOS") %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3 ) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores,  gral_blancos = votos_cantidad) %>% 
  print()

grales_2021 <- votos_positivos %>% left_join(votos_blancos) %>% 
  print()


grales_2021 %>% write_csv("grales_dipnac_pba_2021.csv")



#Genero Dataset eleccion 2019 ####
votos_positivos <- read_csv("data_electoral/crudos_dine/2019/GRALES/ResultadosElectorales.csv",
         locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "POSITIVO") %>% 
  janitor::clean_names() %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores, agrupacion_id, votos_cantidad) %>% 
  pivot_wider(names_from = agrupacion_id,values_from = votos_cantidad) %>% 
  janitor::clean_names() %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id, mesa_electores, 
         gral_fdt = x02_503, gral_jxc = x135, gral_cf = x137, gral_fit = x133) %>% 
  print()

votos_blancos <- read_csv("data_electoral/crudos_dine/2019/GRALES/ResultadosElectorales.csv",
                          locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "EN BLANCO") %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3 ) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores,  gral_blancos = votos_cantidad) %>% 
  print()

grales_2019 <- votos_positivos %>% left_join(votos_blancos) %>% 
  print()


grales_2019 %>% write_csv("grales_dipnac_pba_2019.csv")



#Genero Dataset eleccion 2017 ####
votos_positivos <- read_csv("data_electoral/crudos_dine/2017/GRALES/ResultadosElectorales.csv",
                            locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "POSITIVO") %>% 
  janitor::clean_names() %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores, agrupacion_id, votos_cantidad) %>% 
  pivot_wider(names_from = agrupacion_id,values_from = votos_cantidad) %>% 
  janitor::clean_names() %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id, mesa_electores, 
         gral_uc = x574, gral_jxc = x518, gral_massa = x511, gral_fit = x503, gral_fuj = x546) %>% 
  print()

votos_blancos <- read_csv("data_electoral/crudos_dine/2017/GRALES/ResultadosElectorales.csv",
                          locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "EN BLANCO") %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3 ) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores,  gral_blancos = votos_cantidad) %>% 
  print()

grales_2017 <- votos_positivos %>% left_join(votos_blancos) %>% 
  print()


grales_2017 %>% write_csv("grales_dipnac_pba_2017.csv")

#Genero Dataset eleccion 2015 ####

votos_positivos <- read_csv("data_electoral/crudos_dine/2015/GRALES/ResultadosElectorales.csv",
                            locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "POSITIVO") %>% 
  janitor::clean_names() %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores, agrupacion_id, votos_cantidad) %>% 
  pivot_wider(names_from = agrupacion_id,values_from = votos_cantidad) %>% 
  janitor::clean_names() %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id, mesa_electores, 
         gral_fpv = x131, gral_jxc = x145, gral_massa = x138, gral_fit = x137, gral_progre = x132) %>% 
  print()

votos_blancos <- read_csv("data_electoral/crudos_dine/2015/GRALES/ResultadosElectorales.csv",
                          locale=locale(encoding="latin1"))  %>% 
  filter(votos_tipo == "EN BLANCO") %>% 
  filter(distrito_id == 2) %>% 
  filter(cargo_id == 3 ) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id , mesa_electores,  gral_blancos = votos_cantidad) %>% 
  print()


grales_2015 <- votos_positivos %>% left_join(votos_blancos) %>% 
  print()


grales_2015 %>% write_csv("grales_dipnac_pba_2015.csv")



#Generacion Datasets Inferencia Ecológica ####

#15-17
x15 <- grales_2015 %>%   
  mutate(distrito_id = str_pad(distrito_id,2,side="left",pad = "0"),
         seccion_id = str_pad(seccion_id,3,side="left",pad = "0"),
         circuito_id = str_pad(circuito_id,5,side="left",pad = "0"),
         mesa_id = str_pad(mesa_id,5,side="left",pad = "0")) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id,
         ele_15 = mesa_electores,
         fpv_15 = gral_fpv, 
         progre_15 = gral_progre, 
         fit_15 = gral_fit,
         jxc_15 = gral_jxc,
         massa_15 = gral_massa,
         blancos_15 = gral_blancos) %>% 
  print()



x17 <- grales_2017 %>% 
  mutate(distrito_id = str_pad(distrito_id,2,side="left",pad = "0"),
         seccion_id = str_pad(seccion_id,3,side="left",pad = "0"),
         circuito_id = str_pad(circuito_id,5,side="left",pad = "0"),
         mesa_id = str_pad(mesa_id,5,side="left",pad = "0"))


base_1517 <-  x15 %>% left_join(x17) %>% drop_na() %>% 
  select(-mesa_electores) %>% 
  rowwise() %>%
  mutate(votos_15 = sum(across(fpv_15:blancos_15),na.rm = T),
         votos_17 = sum(across(gral_uc:gral_blancos),na.rm = T)) %>% 
  filter(votos_15 > 200) %>% 
  filter(votos_17 > 200) %>% 
  print()


base_1517 %>% write_csv("base_infeco_1517.csv")


#17-19


x19 <- grales_2019 %>%   
  mutate(distrito_id = str_pad(distrito_id,2,side="left",pad = "0"),
         seccion_id = str_pad(seccion_id,3,side="left",pad = "0"),
         circuito_id = str_trunc(circuito_id,5,side="left", "0"),
         mesa_id = str_pad(mesa_id,5,side="left",pad = "0")) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id,
         ele_19 = mesa_electores,
         fdt_19 = gral_fdt, 
         fit_19 = gral_fit,
         jxc_19 = gral_jxc,
         cf_19 = gral_cf,
         blancos_19 = gral_blancos) %>% 
  print()


base_1719 <- x17 %>% left_join(x19) %>% drop_na() %>% 
  select(-mesa_electores) %>% 
  mutate(votos_19 = sum(across(fdt_19:blancos_19),na.rm = T),
         votos_17 = sum(across(gral_uc:gral_blancos),na.rm = T)) %>% 
  filter(votos_19 > 200) %>% 
  filter(votos_17 > 200) %>% 
  print()

base_1719 %>% write_csv("base_infeco_1719.csv")

#19-21
x21 <- grales_2021  %>% 
  mutate(distrito_id = str_pad(distrito_id,2,side="left",pad = "0"),
         seccion_id = str_pad(seccion_id,3,side="left",pad = "0"),
         circuito_id = str_trunc(circuito_id,5,side="left", "0"),
         mesa_id = str_pad(mesa_id,5,side="left",pad = "0")) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id,
         ele_21 = mesa_electores,
         uxp_21 = gral_fdt, 
         fit_21 = gral_fit,
         jxc_21 = gral_jxc,
         lla_21 = gral_lla,
         valores_21 = gral_valores,
         vcv_21 = gral_vcv,
         blancos_21 = gral_blancos) %>% 
  print()



base_1921 <- x19 %>% left_join(x21) %>% drop_na() %>% 
  select(-ele_19) %>% 
  mutate(votos_19 = sum(across(fdt_19:blancos_19),na.rm = T),
         votos_21 = sum(across(uxp_21:blancos_21),na.rm = T)) %>% 
  filter(votos_19 > 200) %>% 
  filter(votos_21 > 200) %>% 
  print()


base_1921 %>% write_csv("base_infeco_1921.csv")


#21-23
x23 <- grales_2023 %>% 
  mutate(distrito_id = str_pad(distrito_id,2,side="left",pad = "0"),
         seccion_id = str_pad(seccion_id,3,side="left",pad = "0"),
         circuito_id = str_trunc(circuito_id,5,side="left", "0"),
         mesa_id = str_pad(mesa_id,5,side="left",pad = "0")) %>% 
  select(distrito_id, seccion_id, circuito_id, mesa_id,
         ele_23 = mesa_electores,
         uxp_23 = gral_uxp, 
         fit_23 = gral_fit,
         jxc_23 = gral_jxc,
         lla_23 = gral_lla,
         blancos_23 = gral_blancos) %>% 
  print()


base_2123 <- x21 %>% left_join(x23) %>% drop_na() %>% 
  select(-ele_21) %>% 
  mutate(votos_21 = sum(across(uxp_21:blancos_21),na.rm = T),
         votos_23 = sum(across(uxp_23:blancos_23),na.rm = T)) %>% 
  filter(votos_21 > 200) %>% 
  filter(votos_23 > 200) %>% 
  print()


base_2123 %>% write_csv("base_infeco_2123.csv")

