setwd("~")
rm(list=ls())


##################################
# Procesar Datos SNSP            #
#     nivel estatal              #
# Aritzy Sanchez                 #
# 05/12/2018                     #
##################################

library(pacman)
p_load(foreign, readxl, tidyverse, plyr, dplyr, tidyr, reshape,scales, zoo)

############# Datos víctimas ########

setwd("/Users/aritzy/Documents/Rizika/Víctimas")

library(readxl)
datos <- read_excel("Estatal-Victimas-2015-2020_ago2020.xlsx")

datos_hd <- datos %>% 
  filter(`Subtipo de delito` == "Homicidio doloso")

meses <- colnames(datos_hd)[10:21]


#### Datos nacionales 

datos_nac <- datos_hd %>% 
  group_by(Año) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)

datos_nac <- datos_nac %>% 
  gather(mes, victimas, Enero:Diciembre, factor_key = TRUE)

datos_nac <- datos_nac %>% 
  filter(victimas != 0)

datos_nac$mes <- tolower(datos_nac$mes)

datos_nac <- datos_nac %>% 
  mutate(fecha = paste0(Año, "-", mes))

library(lubridate)

Sys.setlocale("LC_TIME", "es_ES.UTF-8")

datos_nac$fecha <- as.yearmon((datos_nac$fecha),"%Y-%b")

datos_victimas <- datos_nac %>% 
  select(1,2,4,3)

names(datos_victimas)[2] <- "victimas_hd_nacional"


#### Gráfica por mes año

graf <- ggplot(datos_nac, aes(fecha, victimas)) +
  geom_line(color = "#0A2240", size = 1.5)+
  geom_point(color = "#0A2240", size = 1.5) +
  geom_smooth(method = "loess", color = "#005E6E")+
  scale_x_yearmon(breaks = datos_nac$fecha, lab = format(datos_nac$fecha, ifelse(cycle(datos_nac$fecha) == 1, "%b\n%Y", "%b")))+
  geom_text(data= datos_nac,
            aes(x = fecha, y = victimas,label= victimas), vjust=-1, size = 2.5) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(
    x= "Mes",
    y = "Total de víctimas") 

#### Promedios

datos_prom <- datos %>% 
  filter(`Subtipo de delito` == "Homicidio doloso")

meses <- colnames(datos_prom)[10:21]

datos_prom <- datos_prom %>% 
  group_by(Año) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)

datos_prom <- aggregate(victimas ~ Año, data= datos_hd, mean)


##### datos para catografía estatal

datos_estatal <- datos_hd %>% 
  group_by(Año, Entidad) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)

datos_estatal <- datos_estatal %>% 
  gather(mes, victimas, Enero:Diciembre, factor_key = TRUE)

datos_estatal$mes <- tolower(datos_estatal$mes)

datos_estatal_2 <- datos_estatal[datos_estatal$Año== "2019" & datos_estatal$mes == "septiembre" | datos_estatal$Año== "2019" & datos_estatal$mes == "octubre" | datos_estatal$Año== "2019" & datos_estatal$mes == "noviembre" | datos_estatal$Año== "2019" & datos_estatal$mes == "diciembre" | datos_estatal$Año== "2020",]

datos_estatal_1 <- aggregate(victimas ~ Año + Entidad, sum, data = datos_estatal)

datos_estatal_1 <- datos_estatal_1 %>% 
  filter(Año < 2020)

datos_estatal_1 <- datos_estatal_1 %>% 
  spread(Año, victimas) %>% 
  group_by(Entidad)

datos_estatal_2 <- aggregate(victimas ~ Entidad, sum, data = datos_estatal_2)

names(datos_estatal_2)[2] <- "2020"

datos_estatal_fin <- left_join(datos_estatal_1, datos_estatal_2, by = "Entidad")

datos_estatal_fin <- datos_estatal_fin %>% 
  mutate(NUM_EDO = ifelse(Entidad == "Aguascalientes", "01",
                          ifelse(Entidad == "Baja California", "02", 
                                 ifelse(Entidad == "Baja California Sur", "03",
                                        ifelse(Entidad == "Campeche", "04",
                                               ifelse( Entidad == "Chiapas", "05", 
                                                       ifelse(Entidad == "Chihuahua", "06",
                                                              ifelse(Entidad == "Coahuila de Zaragoza", "07",
                                                                     ifelse(Entidad == "Colima", "08",
                                                                            ifelse(Entidad == "Ciudad de México", "09",
                                                                                   ifelse(Entidad == "Durango", "10",
                                                                                          ifelse(Entidad == "Guanajuato", "11", 
                                                                                                 ifelse(Entidad == "Guerrero", "12", 
                                                                                                        ifelse(Entidad == "Hidalgo", "13", 
                                                                                                               ifelse(Entidad == "Jalisco", "14",
                                                                                                                      ifelse(Entidad == "México", "15",
                                                                                                                             ifelse(Entidad == "Michoacán de Ocampo", "16",
                                                                                                                                    ifelse(Entidad == "Morelos", "17",
                                                                                                                                           ifelse(Entidad == "Nayarit", "18", 
                                                                                                                                                  ifelse(Entidad == "Nuevo León", "19",
                                                                                                                                                         ifelse(Entidad == "Oaxaca", "20",
                                                                                                                                                                ifelse(Entidad == "Puebla", "21",
                                                                                                                                                                       ifelse(Entidad == "Querétaro", "22",
                                                                                                                                                                              ifelse(Entidad == "Quintana Roo", "23",
                                                                                                                                                                                     ifelse(Entidad == "San Luis Potosí", "24",
                                                                                                                                                                                            ifelse(Entidad == "Sinaloa", "25",
                                                                                                                                                                                                   ifelse(Entidad == "Sonora", "26",
                                                                                                                                                                                                          ifelse(Entidad == "Tabasco", "27",
                                                                                                                                                                                                                 ifelse(Entidad == "Tamaulipas", "28", 
                                                                                                                                                                                                                        ifelse(Entidad == "Tlaxcala", "29", 
                                                                                                                                                                                                                               ifelse(Entidad == "Veracruz de Ignacio de la Llave", "30", 
                                                                                                                                                                                                                                      ifelse(Entidad == "Yucatán", "31", "32"))))))))))))))))))))))
                                                                                   ))))))))))



###Sacamos tasas (PENDIENTE DE SUBIR POBLACIÓN)

datos_pob <- read_excel("~/Rizika/pob-geo/pob_estatal_2020.xlsx")

datos_estatal_fin <- left_join(datos_estatal_fin, datos_pob, by = "Entidad")

datos_estatal_fin <- datos_estatal_fin %>% 
  select(3, 1, 2, 5)

datos_estatal <- datos_estatal %>% 
  mutate(tasa_victimas = round(((victimas/PoblaciónDin)*100000),2))

##### Sacar jenks

p_load(BAMMtools)

jenks_victimas <- getJenksBreaks(datos_estatal$tasa_victimas, 6)

as.data.frame(jenks_victimas)

###### Cartografía###
p_load(rgdal)

cartografia_estatal <- readOGR(dsn="/Users/HP/Documents/Rizika/pob-geo/estados_mex", layer= "destdv250k_2gw")

carto_anual <- merge(cartografia_estatal, datos_estatal, by.x="NUM_EDO", by.y="NUM_EDO")

carto_anual_victimas <- writeOGR(carto_anual, "carto_anual_victimas.GeoJSON", layer = "carto_anual", driver="GeoJSON")


###### Datos fuero federal #####

datos_fuero <- read_excel("~/Documents/Rizika/Fuero federal/INCIDENCIA DEL FUERO FEDERAL 2012 - 2020.xlsx")

lista_fuero <- datos_fuero %>% 
  select(5,6) %>% 
  group_by(CONCEPTO, TIPO) %>% 
  count()
