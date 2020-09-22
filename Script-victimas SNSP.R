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

setwd("/Users/HP/Documents/Rizika/Víctimas")

library(readxl)

datos <- read_excel("~/Rizika/Víctimas/Estatal-Víctimas-2015-2020_ago2020.xlsx")

datos_hd <- datos %>% 
  filter(`Subtipo de delito` == "Homicidio doloso")

meses <- colnames(datos_hd)[10:21]

### datos nacionales 

datos_hd <- datos_hd %>% 
  group_by(Año) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)

datos_hd <- datos_hd %>% 
  gather(mes, victimas, Enero:Diciembre, factor_key = TRUE)

datos_hd <- datos_hd %>% 
  filter(victimas != 0)

datos_hd$mes <- tolower(datos_hd$mes)

datos_hd <- datos_hd %>% 
  mutate(fecha = paste0(Año, "-", mes))

library(lubridate)

datos_hd$fecha <- as.yearmon((datos_hd$fecha),"%Y-%b")

graf <- ggplot(datos_hd, aes(fecha, victimas)) +
  geom_line()+
  geom_point()+
  scale_x_yearmon(breaks = datos_hd$fecha, lab = format(datos_hd$fecha, ifelse(cycle(datos_hd$fecha) == 1, "%b\n%Y", "%b")))+
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs( 
    title = "Total de víctimas de homicidio doloso",
    x= "mes",
    y = "Total de víctimas") 


##### datos para catografía estatal

datos_estatal <- datos_hd %>% 
  group_by(Año, Entidad) %>% 
  summarise_at(meses[1:12], sum, na.rm = TRUE)

datos_estatal <- datos_estatal %>% 
  gather(mes, victimas, Enero:Diciembre, factor_key = TRUE)

datos_estatal$mes <- tolower(datos_estatal$mes)


datos_estatal <- datos_estatal[datos_estatal$Año== "2019" & datos_estatal$mes == "septiembre" | datos_estatal$Año== "2019" & datos_estatal$mes == "octubre" | datos_estatal$Año== "2019" & datos_estatal$mes == "noviembre" | datos_estatal$Año== "2019" & datos_estatal$mes == "diciembre" | datos_estatal$Año== "2020",]

datos_estatal <- aggregate(victimas ~ Entidad, sum, data = datos_estatal)

datos_estatal <- datos_estatal %>% 
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



###Sacamos tasas

datos_pob <- read_excel("~/Rizika/pob-geo/pob_estatal_2020.xlsx")

datos_estatal <- left_join(datos_estatal, datos_pob, by = "Entidad")

datos_estatal <- datos_estatal %>% 
  select(3, 1, 2, 5)

datos_estatal <- datos_estatal %>% 
  mutate(tasa_victimas = round(((victimas/PoblaciónDin)*100000),2))

##### Sacar jenks

p_load(BAMMtools)

jenks_victimas <- getJenksBreaks(datos_estatal$tasa_victimas, 6)

as.data.frame(jenks_victimas)
###### Cartografía#####
p_load(rgdal)

cartografia_estatal <- readOGR(dsn="/Users/HP/Documents/Rizika/pob-geo/estados_mex", layer= "destdv250k_2gw")

carto_anual <- merge(cartografia_estatal, datos_estatal, by.x="NUM_EDO", by.y="NUM_EDO")

carto_anual_victimas <- writeOGR(carto_anual, "carto_anual_victimas.GeoJSON", layer = "carto_anual", driver="GeoJSON")
