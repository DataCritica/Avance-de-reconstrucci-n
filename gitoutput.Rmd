---
title: Informe sobre estado de la reconstrucción y acciones requeridas en localidades
  prioritarias por entidad
  
output: 
  html_document:
    keep_md: true
  always_allow_html: yes
  rmarkdown : github_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(rgdal)
library(leaflet)
library(raster)
library(here)
library(ggplot2)
library(openxlsx)
library(ggiraph)
library(knitr)


```

# Equipo de Transición. Comisión de Reconstrucción
####Diagnóstico estatal de vivienda en localidades prioritarias basado en muestreo de Agosto de 2018

*Introducción  
*Estimadores de daño por entidad  
*Estimadores de avance en reconstrucción por entidad  
*Estimadores de modos de reconstrucción por entidad  
*Estimadores de atención deficiente a damnificados  
*Conclusiones  

##Introducción

El presente informe se encuentra alineado con los principios 8 y 9 del Programa de Reconstrucción: 

* Mayor y mejor organización
* Total transparencia.

A través de un despliegue de encuestadoras y encuestadores en campo, y por medio de la tecnología abierta de Kobo Toolbox, sobre la que ocurrió capacitación previa impartida por un colectivo de la sociedad civil al equipo de transición, y del equipo de transición a representantes locales la última semana de agosto de 2018, se llevó a cabo el muestreo en un total de 118 localidades prioritarias distribuidas en 114 municipios de las entidades más afectadas: Chiapas, Oaxaca, Puebla, Morelos, Estado de México y Guerrero. Fueron seleccionadas para tal efecto considerando el número de daños totales, parciales, así como la población afectada y el índice de marginación municipal (disponible a 2015). Las viviendas fueron elegidas usando el procedimiento de muestreo aleatorio simple con un nivel de confianza de 95% y un margen de error de +/- 5%.

Este informe presenta los resultados de las variables estimadas y en la última parte un diagnóstico de avance en la reconstrucción y número de acciones requeridas en el total de localidades prioritarias por entidad. En cada secció se presenta un resumen, además de un mapa donde es posible consultar el detalle de cada entidad, los porcentajes respecto del total de viviendas encuestadas.

A continuación puedes conocer todos los municipios donde se realizó el levantamiento, y el número de encuestas levantadas en cada uno de ellos. Un listado se encuentra como apéndice de este documento.


###Da clic para conocer el número de encuestas realizadas por municipio 
```{r, include = F}

indicadores_ok <- read.xlsx("mapa_prioritarios.xlsx")
contornos_mun <- shapefile("municipios_muestreo.shp")
options("scipen"=100, "digits"=15)

```

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#fb6a4a",
                      "#de2d26",
                      "#a50f15"), 
                    bins = c(1,50, 167))



popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indicadores_ok$NOM_ENT ,         #column containing the district names
                 "<br>Municipio: ", 
                 indicadores_ok$NOM_MUN  #column that contains the relative amount data
                 ,"<br>Número de encuestas: ", 
                 indicadores_ok$encuestas      #column that contains the absolute amount data
)

leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos_mun, 
              fillColor = ~palette(indicadores_ok$encuestas ),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Encuestas") %>%
  
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#fb6a4a",
                       "#de2d26",
                       "#a50f15"),
            labels = c('1',"",'167'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Municipios prioritarios, <br> número de encuestas") %>%  ## title of the legend
  addLayersControl(
    baseGroups = c("Encuestas"
    ),
    options = layersControlOptions(collapsed = TRUE)) ## we want our control to be seen right away

```


Dado el levantamiento de datos para localidades prioritarias, los intervalos de confianza para cada entidad son los siguientes:

Chiapas: +/-5.04%
Oaxaca: +/-5.12%
Puebla: +/- 5%
Edomex: +/- 5.37%
Morelos: +/- 6.47%
Guerrero: +/- 4.96

Lo anterior en consideración de que para cada entidad, el número total de viviendas dañadas N para el muestreo fue el siguiente:

Chiapas: 39063
Oaxaca: 35818
Puebla: 21706
Edomex: 3985
Morelos: 3480
Guerrero: 2858

Esto quiere decir que todos los porcentajes desglosados en este informe corresponden a este número de viviendas dañadas para cada entidad, con el margen de error ya mencionado. Es indispensable mencionar que el muestreo permite obtener el número de acciones totales y parciales para cada entidad **solamente para las localidades prioritarias** ya que ninguna otra localidad puede ser medida o estimada con este procedimiento.

##Estimadores de daño por entidad

###Daño autoestimado

La entidad con mayor porcentaje de daño total autoestimado, es decir, daño total percibido así por las familias damnificadas, es Oaxaca. En ella el total de viviendas en localidades prioritarias con daño total es el 35% respecto del total de viviendas dañadas en estos sitios. Le sigue Puebla, México, Morelos, Chiapas y Guerrero.

La que mayor porcentaje de daño parcial reporta respecto del total de daños en todas las entidades es también Oaxaca, seguida del Estado de México, Puebla, Guerrero, Morelos y Chiapas, en ese orden.

###Da clic para ver el detalle de los porcentajes de daño por entidad, selecciona entre daño total y parcial dando clic en el rombo

```{r, include = F}
indica_ok <- read_csv("entidades_mapa_ok.csv")
contornos <- shapefile("contornos_muestreo.shp")
options("scipen"=100, "digits"=15)

```


```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(49, 52, 55, 58, 60, 65, 70, 75, 80))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(21, 25 , 30, 32, 35, 40, 45, 47, 51))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "<br>Porcentaje de daño total: ", 
                 indica_ok$pc_daño_total  #column that contains the relative amount data
                 ,"<br>Porcentaje de daño parcial: ", 
                 indica_ok$pc_daño_parcial       #column that contains the absolute amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$pc_daño_total ),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Daño total")%>%  
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  
  ## for the second layer we mix things up a little bit, so you'll see the difference in the map!
  addPolygons(data = contornos, 
              fillColor = ~palette2(indica_ok$pc_daño_parcial), 
              fillOpacity = 0.2, 
              color = "white", 
              weight = 1.0, 
              popup = popup2, 
              group="Daño parcial")%>%
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'100%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Porcentaje por tipo <br>  de daño autoestimado") %>%  ## title of the legend
  addLayersControl(
    baseGroups = c("<span style='color: #7f0000; font-size: 11pt'><strong>Daño total</strong></span>", ## group 1
                   "Daño parcial" ## group 2
    ),
    options = layersControlOptions(collapsed = TRUE)) ## we want our control to be seen right away
```


###Porcentaje de Avance nulo y avance medio

En cada entidad hay entre 18% y 26% de viviendas donde no se ha avanzado en absoluto en la reconstrucción. 

Se observa que las entidades con mayor porcentaje de avance nulo son Oaxaca, Morelos y Chiapas, seguidos por Puebla, Guerrero y México, en ese orden. No obstante, a más de un año de los sismos estos porcentajes pueden considerarse elevados. 

En cuanto al avance medio en la reconstrucción, el mayor porcentaje, 43%, lo tiene el Estado de México, seguido de Chiapas, Morelos, Puebla, Oaxaca, y el menor porcentaje de avance medio, Guerrero.


###Da clic para conocer los porcentajes por entidad federativa, selecciona entre tipo de avance dando clic en el rombo.

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(18, 19, 20, 21, 22, 23, 24, 25, 26))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(24, 26 , 28, 30, 32, 34, 36, 38, 43))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "<br>Porcentaje de avance nulo: ", 
                 indica_ok$pc_avancenulo  #column that contains the relative amount data
                 ,"<br>Porcentaje de avance medio: ", 
                 indica_ok$pc_avancemedio       #column that contains the absolute amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$pc_avancenulo ),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Avance Nulo")%>%  
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  
  ## for the second layer we mix things up a little bit, so you'll see the difference in the map!
  addPolygons(data = contornos, 
              fillColor = ~palette2(indica_ok$pc_avancemedio), 
              fillOpacity = 0.2, 
              color = "white", 
              weight = 1.0, 
              popup = popup2, 
              group="Avance Medio")%>%
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'40%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Porcentaje de la entidad
            <br> por tipo de avance") %>%  ## title of the legend
  addLayersControl(
    baseGroups = c("<span style='color: #7f0000; font-size: 11pt'><strong>Avance Nulo</strong></span>", ## group 1
                   "Avance Medio" ## group 2
    ),
    options = layersControlOptions(collapsed = TRUE)) ## we want our control to be seen right away
```


###Porcentaje de familias que concluyeron su vivienda, por entidad.

Se observa que las localidades prioritarias que van a la cabeza en viviendas concluidas están en Guerrero, seguida de Morelos, Puebla, México y Chiapas, cuyas localidades están en último lugar.


###Da clic en el mapa para ver los porcentajes de conclusión de cada entidad

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(14, 19, 20, 21, 22, 23, 24, 25, 33))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(24, 26 , 28, 30, 32, 34, 36, 38, 43))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "Porcentaje que concluyó <br> la reconstrucción: ", 
                 indica_ok$pc_concluyo  #column that contains the relative amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$pc_concluyo ),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Concluyeron la reconstrucción")%>%  
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'100%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Porcentaje de familias <br> damnificadas que reconstruyeron") 
```

##Estimadores de modos de reconstrucción

###Recursos de reconstrucción 

Las localidades prioritarias construyen reuniendo recursos propios con los entregados por gobierno. Un porcentaje más bajo recurre a créditos. La entidad que más usa recursos propios para la reconstrucción es Guerrero, seguida de México, Puebla, Oaxaca, Chiapas y Morelos. 
La entidad que más recurre a créditos es Morelos, seguida de Chiapas, Oaxaca, Puebla, México y Guerrero, en ese orden.

###Da clic para conocer los porcentajes por entidad federativa

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(67, 70, 72, 74, 76, 78, 80, 84, 85))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(27, 30 , 40, 50, 60, 65, 70, 75, 80))

palette3 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(15, 18 , 20, 24, 25, 26, 27, 30, 32))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "<br>Porcentaje de uso de recursos propios: ", 
                 indica_ok$pc_rec_propio  #column that contains the relative amount data
                 ,"<br>Porcentaje de uso de recursos gubernamentales: ", 
                 indica_ok$pc_usan_rec_gob      #column that contains the absolute amount data
                 ,"<br>Porcentaje de uso de créditos: ", 
                 indica_ok$pc_rec_credito      #column that contains the absolute amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$pc_rec_propio ),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Recursos propios")%>%  
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  
  ## for the second layer we mix things up a little bit, so you'll see the difference in the map!
  addPolygons(data = contornos, 
              fillColor = ~palette2(indica_ok$pc_usan_rec_gob), 
              fillOpacity = 0.2, 
              color = "white", 
              weight = 1.0, 
              popup = popup2, 
              group="Recursos públicos")%>%
    
  addPolygons(data = contornos, 
              fillColor = ~palette3(indica_ok$pc_rec_credito), 
              fillOpacity = 0.2, 
              color = "white", 
              weight = 1.0, 
              popup = popup2, 
              group="Crédito")%>%
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'80%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Porcentaje de la entidad <br> por tipo de recurso utilizado") %>% 
  addLayersControl(
    baseGroups = c("<span style='color: #7f0000; font-size: 11pt'><strong>Recursos propios</strong></span>", ## group 1
                   "Recursos públicos", "Crédito" ## group 2
    ),
    options = layersControlOptions(collapsed = TRUE)) ## we want our control to be seen right away
```

###Modos de reconstrucción 

Las localidades prioritarias construyen contratando mano de obra. Los mayores porcentajes de esta práctica se encuentran en Oaxaca, seguido de México, Chiapas, Guerrero, Puebla y Morelos.
La autoconstrucción familiar y vecinal alcanza el 27% de los modos de reconstrucción en Chiapas, y por debajo de esta cifra, en menor medida construyen así en localidades prioritarias de Morelos, Oaxaca, México, Guerrero y Puebla. Otros modos de reconstrucción, como constructoras u organizaciones civiles, no alcanzan más que el 10% en Puebla, y por debajo de esta entidad se encuentran localidades de Guerrero, Morelos, México, Chiapas y Oaxaca.

###Da clic para conocer los porcentajes por entidad federativa

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(67, 68, 69, 70, 71, 72, 73, 74, 75))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(21, 22 , 23, 24, 25, 26, 26.5, 27, 28))

palette3 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(0.8, 1, 2, 3, 5, 6, 7, 8, 10))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "<br>Porcentaje de contratación de mano de obra: ", 
                 indica_ok$pc_manodeobra  #column that contains the relative amount data
                 ,"<br>Porcentaje de recurrencia de autoconstrucción : ", 
                 indica_ok$pc_autoconstr      #column that contains the absolute amount data
                 ,"<br>Otros modos de reconstrucción: ", 
                 indica_ok$pc_otromodo_recons      #column that contains the absolute amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$pc_manodeobra ),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Mano de obra")%>%  
  addPolygons(data = contornos, 
              fillColor = ~palette3(indica_ok$pc_otromodo_recons), 
              fillOpacity = 0.2, 
              color = "white", 
              weight = 1.0, 
              popup = popup2, 
              group="Otros modos")%>%
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  
  ## for the second layer we mix things up a little bit, so you'll see the difference in the map!
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'80%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Porcentaje de la entidad <br> por modo de reconstrucción")%>%
  addLayersControl(
    baseGroups = c("<span style='color: #7f0000; font-size: 11pt'><strong>Recursos propios</strong></span>", ## group 1
                   "Recursos públicos", "Crédito" ## group 2
    ),
    options = layersControlOptions(collapsed = TRUE)) ## we want our control to be seen right away
```

##Estimadores de atención deficiente

###Porcentaje de familias a las que no se ha despositado recursos completos.

El mayor porcentaje de depósitos gubarnamentales incompletos está en el Estado de Chiapas, con un porcentaje, respecto del total de viviendas dañadas, de 68%. Le siguen Morelos, Guerrero, Estado de México, Puebla y Oaxaca, en ese orden. 

###Da clic en el mapa para ver los porcentajes de conclusión de cada entidad

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(19, 35, 40, 45, 50, 55, 60, 65, 69))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(24, 26 , 28, 30, 32, 34, 36, 38, 43))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "<br>Porcentaje que no ha recibido recursos completos: ", 
                 indica_ok$pc_monto_insatis  #column that contains the relative amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$pc_monto_insatis),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Insatisfacción")%>%  
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'100%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Porcentaje de familias damnificadas <br> que no recibieron recursos completos") 
```

##Porcentaje de familias a las que no se entregaron tarjetas.

A pesar de haber sido censadas y haber recibido folio. Chiapas tiene el más alto porcentaje de no entregas reportadas en localidades prioritarias, 31%. Le siguen Morelos, Puebla, Guerrero, Oaxaca y Estado de México, en ese orden. 

###Da clic en el mapa para ver los porcentajes de no entrega de tarjetas en cada entidad

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(8, 10, 12, 14, 16, 20, 24, 28, 31))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                     bins = c(24, 26 , 28, 30, 32, 34, 36, 38, 43))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "<br>Porcentaje que no ha recibido recursos completos: ", 
                 indica_ok$pc_norecibiotarjeta  #column that contains the relative amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$pc_norecibiotarjeta),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="No recibió tarjeta")%>%  
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'100%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Porcentaje de familias damnificadas <br> que no recibieron tarjetas Bansefi") 
```

##Conclusiones

### Número de acciones de vivienda en localidades prioritarias, por entidad

De acuerdo con el nivel de daño autoestimado por las familias, aunado al nivel de avance declarado en la reconstrucción, es posible calcular el nivel de atención requerido por las localidades prioritarias, por entidad, traducido en número de acciones de tipo total y parcial necesarias, por entidad. Se presenta en la visualización el margen superior, considerando que el número real podría ser hasta 10% menor. 

A las viviendas con daño reportado como total y avance reportado como nulo o bajo se les asignó una acción requerida total. A las viviendas con daño reportado como parcial y avance bajo o nulo se les asigño acción parcial. A las viviendas con daño total y avance medio también se les asignó acción parcial, y también a las viviendas con daño parcial y avance medio.

La entidad que mayor número de acciones de reconstrucción total requeridas en localidades prioritarias es Chiapas, con  18303 acciones de este tipo; Oaxaca, con 14,147; Puebla, con 10,513; Estado de México con 2,139; Morelos, con 1508 y Guerrero, con 958.

En cuanto a las acciones parciales, también las localidades prioritarias de Chiapas son las que se requieren en mayor número: 8, 482; Chiapas, 3460; Oaxaca, con 7,725; Puebla, 3933; Estado de México, 791; Morelos, 565 y Guerrero, 526.


```{r, include = F}
indica_ok <- read_csv("entidades_mapa.csv")
contornos <- shapefile("contornos_muestreo.shp")
options("scipen"=100, "digits"=15)

```
Media de acciones totales (debe considerarse que se pueden elevar o reducir en función de los márgenes de error de cada entidad)

Acciones de reconstrucción total: 47, 570 
Acciones de reconstrucción parcial: 22, 024

```{r, echo=FALSE, warning = FALSE}

palette <- colorBin(c("#f7fcf0",
                      "#e0f3db",
                      "#ccebc5",
                      "#a8ddb5",
                      "#7bccc4",
                      "#4eb3d3",
                      "#2b8cbe",
                      "#0868ac",
                      "#084081"), 
                    bins = c(0, 1500, 3000, 10000, 12000, 13000, 15000, 16500, 18304))


palette2 <- colorBin(c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"), 
                      bins = c(0, 526, 700, 3000, 5000, 6000, 7000, 7500, 8483))

popup2 <- paste0("Ficha técnica",
                 "<br>Entidad: ",             
                 indica_ok$NOM_ENT ,         #column containing the district names
                 "<br>Acciones de reconstrucción total: ", 
                 indica_ok$numero_acciontotal  #column that contains the relative amount data
                 ,"<br>Acciones de reconstrucción parcial: ", 
                 indica_ok$numero_accionparcial       #column that contains the absolute amount data
)


leaflet() %>% 
  addProviderTiles("Esri.WorldGrayCanvas",
                   options = tileOptions(minZoom=0, maxZoom=22)) %>% #"freeze" the mapwindow to max and min zoomlevel
  
  
  addPolygons(data = contornos, 
              fillColor = ~palette(indica_ok$numero_acciontotal ),  ## we want the polygon filled with 
              ## one of the palette-colors
              ## according to the value in student1$Anteil
              fillOpacity = 0.6,         ## how transparent do you want the polygon to be?
              color = "darkgrey",       ## color of borders between districts
              weight = 1.5,            ## width of borders
              popup = popup2,         ## which popup?
              group="Acciones totales")%>%  
  ## which group?
  ## the group's name has to be the same as later in "baseGroups", where we define 
  ## the groups for the Layerscontrol. Because for this layer I wanted a specific 
  ## color and size, the group name includes some font arguments.  
  
  ## for the second layer we mix things up a little bit, so you'll see the difference in the map!
  addPolygons(data = contornos, 
              fillColor = ~palette2(indica_ok$numero_accionparcial), 
              fillOpacity = 0.2, 
              color = "white", 
              weight = 1.0, 
              popup = popup2, 
              group="Acciones parciales")%>%
  addLegend(position = 'topright', ## choose bottomleft, bottomright, topleft or topright
            colors = c("#f7fcf0",
                       "#e0f3db",
                       "#ccebc5",
                       "#a8ddb5",
                       "#7bccc4",
                       "#4eb3d3",
                       "#2b8cbe",
                       "#0868ac",
                       "#084081"),
            labels = c('0%',"","","","","","", "",'100%'),  ## legend labels (only min and max)
            opacity = 0.3,      ##transparency again
            title = "Número de acciones por entidad, <br> por tipo de acción") %>%  ## title of the legend
  addLayersControl(
    baseGroups = c("<span style='color: #7f0000; font-size: 11pt'><strong>Acción Total</strong></span>", ## group 1
                   "Acción Parcial" ## group 2
    ),
    options = layersControlOptions(collapsed = TRUE)) ## we want our control to be seen right away
```

###Tabla de muestreo. Para obtener cifras absolutas considera cada porcentaje respecto de la poblacion de viviendas afectadas indicada en las primeras variables.


```{r, include = F}

tabla <- read.xlsx("tabla_final_muestreo.xlsx")

```

```{r, echo=FALSE, warning = FALSE}

kable(tabla, format = "markdown")

```



###Lista de municipios con localidades prioritarias que formaron parte del muestreo


```{r, include = F}

localidades_prio <- read.xlsx("listadomun.xlsx")

```

```{r, echo=FALSE, warning = FALSE}

kable(localidades_prio[,1:2], format = "markdown")

```


