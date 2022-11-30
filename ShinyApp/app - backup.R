library(shiny)
library(tidyverse)
library(readxl)
library(lubridate)
library(forcats)
library(ggplot2)
library(here)
library(plotly)
library(ggpmisc)
library(DT)

#1-CARGAR DATOS
datos <- read_excel(here("Datos","emisivo.xlsx"))

#2-SELECCIONAR FILAS
datos <- datos %>% 
  filter(Pais=="Uruguay",year(FechaSalida)!=2016, GastoTotal<30000)

#3-MODIFICAR ELEMENTOS
datos<- datos %>% 
  mutate(Integrantes_grupo=cut(Estadia,breaks=c(0,quantile(datos$Estadia,probs = c(0.25,0.5,0.75,1)))),
         Días_estadía=cut(Gente,breaks=c(0,2,4,6,8,15)),
         Estudio=case_when(Estudio == "Primaria incompleta" ~ "Sin primaria",
                          Estudio %in% c("Primaria completa",  "Secundaria incompleta") ~ "Primaria",
                          Estudio %in% c("Secundaria completa", "Terciaria incompleta") ~ "Secundaria",
                          Estudio == "Terciaria completa" ~ "Terciaria",
                               TRUE ~ "Otros/Sin datos") ,
         TipoOcupación=case_when(IdOcupacion %in% 1:5 ~ "Inactivo",
                                 IdOcupacion==8 ~ "Desocupado",
                                 IdOcupacion %in% 11:21 ~ "Ocupado",
                                 IdOcupacion %in%  c(0,9) | c(22,99)    ~ "Otros/Sin datos"),
         Destino=case_when(Destino %in% c("Argentina","Brasil", "Chile", "Paraguay", "Resto Sud America") ~ "América del Sur",
                           Destino == "Africa" ~ "África",
                           Destino %in% c("Asia del Este y Pacifico","Asia Meridional","Oriente Medio") ~ "Asia",
                           Destino == "Centro y Norte America" ~ "América Central y Norteamérica",
                           Destino %in% c("Otros","Sin Datos") ~ "Otros/Sin datos",
                           Destino == "Europa" ~ "Europa"),
         Alojamiento=case_when(Alojamiento %in% c("Appart Hotel","Hotel", "Hostel") ~ "Hotel/Hostel",
                                   Alojamiento == c("Cabañas, Bungalows", "Vivienda Arrendada") ~ "Alquiler",
                                   Alojamiento == "Vivienda Propia" ~ "Vivienda propia",
                                   Alojamiento == "Camping" ~ "Camping",
                                   Alojamiento == "Barco, Yate, Crucero" ~ "Yate/Crucero",
                                   Alojamiento == "Vivienda Familiares / Amigos" ~ "Familiares/Amigos",
                                   TRUE ~ "Otros"),
         Motivo=case_when(Motivo == "Compras" ~ "Compras",
                                     Motivo == "Deportivo" ~ "Deportes",
                                     Motivo == "Estudios" ~ "Estudios",
                                     Motivo %in% c("Negocios, Profesion", "Trabajo remunerado Destino") ~ "Trabajo",
                                     Motivo == "Ocio, Recreo, Vacaciones" ~ "Vacaciones",
                                     Motivo == "Tratamiento Salud" ~ "Salud",
                                     Motivo == "Visita familiares / amigos" ~ "Visita",
                                     TRUE ~ "Otros")
         )
#4-SELECCIONAR COLUMNAS
datos <- datos %>% 
  select("Transporte Internacional de Salida", 
         "FechaSalida","Pais","Motivo","Ocupacion",
         "Estudio","Destino","Alojamiento","Gente",
         "Estadia","GastoTotal","GastoAlojamiento",
         "GastoAlimentacion","GastoTransporteInternac",
         "GatoTransporteLocal","GastoCultural","GastoTours",
         "GastoCompras","GastoResto","TipoOcupación","Integrantes_grupo","Días_estadía")

names(datos) <- c("Transp_int_salida","Fecha_salida","Pais",
                  "Motivo","Ocupacion","Nivel educativo máximo","Destino",
                  "Alojamiento","Gente","Estadia","Gasto_total",
                  "Gasto_alojamiento","Gasto_alimentacion","Gasto_transp_int",
                  "Gasto_transp_local","Gasto_cultural","Gasto_tours",
                  "Gasto_compras","Gasto_resto","Tipo de ocupación","Integrantes por grupo","Días de estadía")


datos <- datos %>% 
  mutate(gasto_persona_diario=Gasto_total/(Estadia*Gente),
         alojamiento_persona_diario=Gasto_alojamiento/(Estadia*Gente),
         alimentacion_persona_diario=Gasto_alimentacion/(Estadia*Gente),
         transint_persona_diario=Gasto_transp_int/(Estadia*Gente),
         transloc_persona_diario=Gasto_transp_local/(Estadia*Gente),
         cultural_persona_diario=Gasto_cultural/(Estadia*Gente),
         tours_persona_diario=Gasto_tours/(Estadia*Gente),
         compras_persona_diario=Gasto_compras/(Estadia*Gente),
         resto_persona_diario=Gasto_resto/(Estadia*Gente))




#5-DESESTACIONAR LA SERIE (Extraemos la tendencia de las series)
desest <- datos %>% 
  mutate(salida_anio_mes=round_date(Fecha_salida,"month")) %>% 
  group_by(salida_anio_mes) %>% 
  summarise("Alojamiento"=mean(alojamiento_persona_diario),
            "Alimentacion"=mean(alimentacion_persona_diario),
            "Transporte internacional"=mean(transint_persona_diario),
            "Transporte local"=mean(transloc_persona_diario),
            "Cultural"=mean(cultural_persona_diario),
            "Tours"=mean(tours_persona_diario),
            "Compras"=mean(compras_persona_diario),
            "Resto"=mean(resto_persona_diario)) %>% 
  arrange(salida_anio_mes)

alojamiento <- ts(desest$Alojamiento,start=c(2017,1),end = c(2020,3),frequency = 12)
alojamiento <- alojamiento %>% 
  stl("periodic")
alojamiento <- as.data.frame(alojamiento$time.series)
alojamiento <- alojamiento %>% 
  select(trend) %>% 
  rename("alojamiento_tend"="trend")

alimentacion <- ts(desest$Alimentacion,start=c(2017,1),end = c(2020,3),frequency = 12)
alimentacion <- alimentacion %>% 
  stl("periodic")
alimentacion <- as.data.frame(alimentacion$time.series)
alimentacion <- alimentacion %>% 
  select(trend) %>% 
  rename("alimentacion_tend"="trend")

trans_int <- ts(desest$`Transporte internacional`,start=c(2017,1),end = c(2020,3),frequency = 12)
trans_int <- trans_int %>% 
  stl("periodic")
trans_int <- as.data.frame(trans_int$time.series)
trans_int <- trans_int %>% 
  select(trend) %>% 
  rename("trans_int_tend"="trend")

trans_loc <- ts(desest$`Transporte local`,start=c(2017,1),end = c(2020,3),frequency = 12)
trans_loc <- trans_loc %>% 
  stl("periodic")
trans_loc <- as.data.frame(trans_loc$time.series)
trans_loc <- trans_loc %>% 
  select(trend) %>% 
  rename("trans_loc_tend"="trend")

cultural <- ts(desest$Cultural,start=c(2017,1),end = c(2020,3),frequency = 12)
cultural <- cultural %>% 
  stl("periodic")
cultural <- as.data.frame(cultural$time.series)
cultural <- cultural %>% 
  select(trend) %>% 
  rename("cultural_tend"="trend")

tours <- ts(desest$Tours,start=c(2017,1),end = c(2020,3),frequency = 12)
tours <- tours %>% 
  stl("periodic")
tours <- as.data.frame(tours$time.series)
tours <- tours %>% 
  select(trend) %>% 
  rename("tours_tend"="trend")

compras <- ts(desest$Compras,start=c(2017,1),end = c(2020,3),frequency = 12)
compras <- compras %>% 
  stl("periodic")
compras <- as.data.frame(compras$time.series)
compras <- compras %>% 
  select(trend) %>% 
  rename("compras_tend"="trend")

resto <- ts(desest$Resto,start=c(2017,1),end = c(2020,3),frequency = 12)
resto <- resto %>% 
  stl("periodic")
resto <- as.data.frame(resto$time.series)
resto <- resto %>% 
  select(trend) %>% 
  rename("resto_tend"="trend")

desest <- cbind(desest,alojamiento,alimentacion,trans_int,trans_loc,cultural,tours,compras,resto)


#6-SHINY
ui <- fluidPage(
  titlePanel("Turismo Emisivo"),
  mainPanel( tabsetPanel((navbarMenu("Componentes del turismo",
                                     tabPanel("Evolución de la cantidad de salidas",h2("Evolución de la cantidad de salidas", align = "center"),  plotOutput("pestaña1.1"  , brush = "plot1_brush"), DTOutput("data"),
                                              sidebarPanel(titlePanel("Filtros"), dateRangeInput('date_range1.1', 'Periodo', "2017-01-01", "2020-03-14"  ), style = "position:fixed; width:300px; top:0; bottom:450px; right:0" ) ),
                                     tabPanel("Evolución del gasto en salidas",h2("Evolución del gasto en salidas", align = "center"),plotlyOutput("prestaña1.2"),   
                                                sidebarPanel(titlePanel("Filtros"), dateRangeInput('date_range1.2', 'Periodo', "2017-01-01", "2020-03-14"  ),  selectInput("Var1","Tipo de gasto", c("Gasto medio (en USD corrientes)","Gasto diario por persona (en USD corrientes)")), style = "position:fixed; width:300px; top:0; bottom:450px; right:0" )))),
                         tabPanel(("Características del turismo"), h2("Distribución del gasto según características del turismo", align = "center"),plotOutput("pestaña2.1"),dataTableOutput("pestaña2.2"),  
                                  sidebarPanel( titlePanel("Filtros"),dateRangeInput('date_range2', 'Periodo', "2017-01-01", "2020-03-14"  ),
                                                selectInput("Var2","Características del turismo", c("Alojamiento","Destino","Días de estadía","Integrantes por grupo","Motivo", "Nivel educativo máximo", "Tipo de ocupación")),style = " position:fixed; width:300px; top:0; bottom:450px; right:0"
                                  )),
                         tabPanel(("Serie de tiempo"), h2("Serie desestacionalizada", align = "center"),plotlyOutput("pestaña3.1"),
                                  sidebarPanel(titlePanel("Filtros"), dateRangeInput('date_range3', 'Periodo', "2017-01-01", "2020-03-14"  ), style = "position:fixed; width:300px; top:0; bottom:450px; right:0" ) )
                         
                         
                         )))


server <- function(input, output){
  
  output$pestaña1.1 <- renderPlot({
    ggplot(data=datos %>%   
             filter(
               Fecha_salida > input$date_range1.1[1],
               Fecha_salida < input$date_range1.1[2])%>%
             count(Periodo=as.Date(round_date(Fecha_salida,"month"))) )+
      geom_line(aes(x=Periodo,y=n)) +
      labs(x="Fecha de salida",
           y="Cantidad de salidas")   })
    
    output$data <- renderDT({
      brushedPoints(datos %>% count(Periodo=as.Date(round_date(Fecha_salida,"month")) )  ,  input$plot1_brush)

    
  })
  
    


  output$prestaña1.2 <- renderPlotly({
    if (input$Var1 == "Gasto medio (en USD corrientes)") {
      gg <- datos %>%   
        filter(
          Fecha_salida > input$date_range1.2[1],
          Fecha_salida < input$date_range1.2[2]) %>% 
        mutate(salida_anio_mes=round_date(Fecha_salida,"month")) %>% 
        group_by(salida_anio_mes) %>% 
        summarise("Alojamiento"=mean(Gasto_alojamiento),
                  "Alimentacion"=mean(Gasto_alimentacion),
                  "Transporte internacional"=mean(Gasto_transp_int),
                  "Transporte local"=mean(Gasto_transp_local),
                  "Cultural"=mean(Gasto_cultural),
                  "Tours"=mean(Gasto_tours),
                  "Compras"=mean(Gasto_compras),
                  "Resto"=mean(Gasto_resto)) %>% 
        pivot_longer(cols = Alojamiento:Resto,
                     names_to = "tipo",values_to = "gasto_medio") %>% 
        ggplot() +
        geom_line(aes(x=salida_anio_mes,y=gasto_medio,color=tipo)) +
        labs(x="Fecha de salida",
             y="Gasto medio (en USD corrientes)",
             color="Concepto")
      print(gg)
    }else{
      gg <- datos %>% 
        mutate(salida_anio_mes=round_date(Fecha_salida,"month")) %>% 
        group_by(salida_anio_mes) %>%   
        filter(
          Fecha_salida > input$date_range1.2[1],
          Fecha_salida < input$date_range1.2[2]) %>% 
        summarise("Alojamiento"=mean(alojamiento_persona_diario),
                  "Alimentacion"=mean(alimentacion_persona_diario),
                  "Transporte internacional"=mean(transint_persona_diario),
                  "Transporte local"=mean(transloc_persona_diario),
                  "Cultural"=mean(cultural_persona_diario),
                  "Tours"=mean(tours_persona_diario),
                  "Compras"=mean(compras_persona_diario),
                  "Resto"=mean(resto_persona_diario)) %>% 
        pivot_longer(cols = Alojamiento:Resto,
                     names_to = "tipo",values_to = "gasto_medio") %>% 
        ggplot() +
        geom_line(aes(x=salida_anio_mes,y=gasto_medio,color=tipo)) +
        labs(x="Fecha de salida",
             y="Gasto diario por persona (en USD corrientes)",
             color="Concepto")
      
      print(gg)
    } 
  })

  
  

  output$pestaña2.1 <- renderPlot({
    datos %>% 
      mutate(salida_anio_mes=round_date(Fecha_salida,"month")) %>% 
      group_by(salida_anio_mes) %>%   
      filter(
        Fecha_salida > input$date_range2[1],
        Fecha_salida < input$date_range2[2], gasto_persona_diario  <4000) %>% 
          ggplot() +
      geom_boxplot(aes(x=reorder(.data[[input$Var2]],gasto_persona_diario),y=gasto_persona_diario,fill=.data[[input$Var2]])) +
      theme(legend.position ="none") +
      labs(x="", y="Gasto diario por persona (en USD corrientes)") +
      theme(axis.text.x = element_text(angle = 20))
  })
  
  

  
  
output$pestaña2.2 <- renderDataTable(expr= datos %>% 
                                       group_by(.data[[input$Var2]]) %>% 
                                       summarise(min=round(min(gasto_persona_diario),2),
                                                 Q1=round(quantile(gasto_persona_diario,probs=0.25),2),
                                                 mediana=round(median(gasto_persona_diario),2),
                                                 media=round(mean(gasto_persona_diario),2),
                                                 Q3=round(quantile(gasto_persona_diario,probs=0.75),2),
                                                 max=round(max(gasto_persona_diario),2)) %>% 
                                      arrange(desc(media)))
  
 
  output$pestaña3.1 <- renderPlotly({
    ggplot(data=desest %>% 
             filter(
               salida_anio_mes > input$date_range3[1],
               salida_anio_mes < input$date_range3[2]) %>%
             pivot_longer(cols = alojamiento_tend:resto_tend,
                          names_to = "tipo",values_to = "gasto_tend") %>% 
             mutate(tipo=recode(tipo,"alimentacion_tend"="Alimentación",
                                "alojamiento_tend"="Alojamiento",
                                "compras_tend"="Compras",
                                "cultural_tend"="Cultural",
                                "resto_tend"="Resto",
                                "tours_tend"="Tours",
                                "trans_int_tend"="Transporte internacional",
                                "trans_loc_tend"="Transporte local"))) +
      geom_line(aes(x=salida_anio_mes,y=gasto_tend,color=tipo)) +
      labs(x="Fecha de salida",
           y="Tendencia del gasto medio (en USD corrientes)",
           color="Concepto")
  })
  

}
shinyApp(ui = ui, server = server)


