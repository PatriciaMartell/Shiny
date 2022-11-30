#1-PACKAGES
library(shiny)
library(shinydashboard)
library(here)
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(plotly)
library(DT)


#2-CARGAR DATOS
datos <- read_excel(here("Datos","emisivo.xlsx"))

#3-TRATAMIENTO DE DATOS
datos<- datos %>% 
  mutate(Estudio=case_when(Estudio == "Primaria incompleta" ~ "Sin primaria",
                           Estudio %in% c("Primaria completa",  "Secundaria incompleta") ~ "Primaria",
                           Estudio %in% c("Secundaria completa", "Terciaria incompleta") ~ "Secundaria",
                           Estudio == "Terciaria completa" ~ "Terciaria",
                           TRUE ~ "Otros/Sin datos") ,
         Destino=case_when(Destino %in% c("Argentina","Brasil", "Chile", "Paraguay", "Resto Sud America") ~ "América del Sur",
                           Destino %in% c("Asia del Este y Pacifico","Asia Meridional","Oriente Medio") ~ "Asia",
                           Destino == "Centro y Norte America" ~ "América Central y Norteamérica",
                           Destino == "Africa" ~ "África",
                           Destino == "Europa" ~ "Europa",
                           Destino %in% c("Otros","Sin Datos") ~ "Otros/Sin datos"),
         Alojamiento=case_when(Alojamiento %in% c("Appart Hotel","Hotel", "Hostel") ~ "Hotel/Hostel",
                           Alojamiento %in% c("Cabañas, Bungalows", "Vivienda Arrendada") ~ "Alquiler",
                           Alojamiento == "Vivienda Propia" ~ "Vivienda propia",
                           Alojamiento == "Camping" ~ "Camping",
                           Alojamiento == "Barco, Yate, Crucero" ~ "Yate/Crucero",
                           Alojamiento == "Vivienda Familiares / Amigos" ~ "Familiares/Amigos",
                           TRUE ~ "Otros/Sin datos"),
         Motivo=case_when(Motivo == "Compras" ~ "Compras",
                           Motivo == "Deportivo" ~ "Deportes",
                           Motivo == "Estudios" ~ "Estudios",
                           Motivo %in% c("Negocios, Profesion", "Trabajo remunerado Destino") ~ "Trabajo",
                           Motivo == "Ocio, Recreo, Vacaciones" ~ "Vacaciones",
                           Motivo == "Tratamiento Salud" ~ "Salud",
                           Motivo == "Visita familiares / amigos" ~ "Visita",
                           TRUE ~ "Otros/Sin datos")  )

datos <- datos %>%  select("FechaSalida","Motivo","Estudio","Destino","Alojamiento","Gente", "Estadia","GastoTotal","GastoAlojamiento", "GastoAlimentacion","GastoTransporteInternac", "GatoTransporteLocal","GastoTours","GastoCompras","GastoResto")
names(datos) <- c("Fecha_salida","Motivo","Nivel educativo máximo","Destino","Alojamiento","Gente","Estadia","Gasto_total","Gasto_alojamiento","Gasto_alimentacion","Gasto_transp_int","Gasto_transp_local","Gasto_tours","Gasto_compras","Gasto_resto")
datos2 <- datos %>%  mutate(Fecha_salida=as.Date(as.character(as.POSIXct(Fecha_salida))))

datos <- datos %>% 
  mutate(gasto_persona_diario=Gasto_total/(Estadia*Gente),
         alojamiento_persona_diario=Gasto_alojamiento/(Estadia*Gente),
         alimentacion_persona_diario=Gasto_alimentacion/(Estadia*Gente),
         transint_persona_diario=Gasto_transp_int/(Estadia*Gente),
         transloc_persona_diario=Gasto_transp_local/(Estadia*Gente),
         tours_persona_diario=Gasto_tours/(Estadia*Gente),
         compras_persona_diario=Gasto_compras/(Estadia*Gente))


#4-SHINY
ui<-dashboardPage(skin= "blue",
                  dashboardHeader(title="PROYECTO",
                                  dropdownMenu(type="messages",
                                               messageItem(from="Paty","Bienvenidos")),
                                  dropdownMenu(type="tasks",
                                               taskItem(value=75,text="Avance del dashboard", color="blue"),
                                               taskItem(value=45,text="Avance del proyecto",color="red"))),
                  dashboardSidebar(sidebarMenu(
                                menuItem("Datos", tabName = "tab"),
                                menuItem("Turismo emisivo",
                                         menuSubItem(tabName= "tab1", "Cantidad de salidas", icon = shiny::icon("star")),
                                         menuSubItem(tabName= "tab2", "Gasto en salidas"),
                                         menuSubItem(tabName= "tab3", "Distribución del gasto")))),
                  dashboardBody( tabItems(
                    tabItem(tabName="tab", 
                            DT::dataTableOutput("tab")),
                    tabItem(tabName="tab1", 
                            fluidRow( column(width=10,
                                                     valueBox("",dateRangeInput('vperiodo1', 'PERÍODO', "2017-01-01", "2020-03-14"),color="blue"),
                                                     box(title="Evolución de la cantidad de salidas", plotOutput("plot1"  , brush = "plot1_brush"), DTOutput("table1"), width=12, status="primary", solidHeader=TRUE)))),
                    tabItem(tabName="tab2", 
                            fluidRow( column(width=10,
                                                     valueBox("",dateRangeInput('vperiodo2', 'PERÍODO', "2017-01-01", "2020-03-14"), color="blue"),
                                                     valueBox("",selectInput("vgasto2","TIPO DE GASTO",   c("Gasto medio","Gasto diario por persona")),color="blue"),
                                                     box(title="Evolución del gasto en salidas", plotlyOutput("plot2"),width=12,status="primary", solidHeader=TRUE)))),
                    tabItem(tabName="tab3", 
                            fluidRow( column(width=10,
                                                     valueBox("",dateRangeInput('vperiodo3', 'PERÍODO', "2017-01-01", "2020-03-14"),color="blue"),
                                                     valueBox("",selectInput("vgasto3","COMPOSICIÓN DEL TURISIMO", c("Alojamiento","Destino","Motivo", "Nivel educativo máximo")),color="blue"),
                                                     box(title="Distribución del gasto en salidas", plotOutput("plot3"), dataTableOutput("table3"),width=12, status="primary", solidHeader=TRUE)))))))
 
server <- function(input, output){

##tab## 
  output$tab<-DT::renderDT(datos2)
  
##tab1##  
  output$plot1 <- renderPlot({
        ggplot(datos %>%   
             filter(Fecha_salida >= input$vperiodo1[1], Fecha_salida < input$vperiodo1[2])%>%
             count(Periodo=as.Date(round_date(as.Date(as.character(as.POSIXct(Fecha_salida))),"month"))) )+
        geom_line(aes(x=Periodo,y=n)) +
        labs(x="Fecha de salida",  y="Cantidad de salidas")   })
  
  output$table1 <- renderDT({
        brushedPoints(datos %>% count(Periodo=as.Date(round_date(as.Date(as.character(as.POSIXct(Fecha_salida))),"month")) )  ,  input$plot1_brush)})
  
##tab2##  
  output$plot2 <- renderPlotly({
    if (input$vgasto2 == "Gasto medio") {
        datos %>%   
        filter(Fecha_salida > input$vperiodo2[1], Fecha_salida < input$vperiodo2[2]) %>% 
        mutate(salida_anio_mes=round_date(Fecha_salida,"month")) %>% 
        group_by(salida_anio_mes) %>% 
        summarise("Alojamiento"=mean(Gasto_alojamiento),
                  "Alimentacion"=mean(Gasto_alimentacion),
                  "Transporte internacional"=mean(Gasto_transp_int),
                  "Transporte local"=mean(Gasto_transp_local),
                  "Tours"=mean(Gasto_tours),
                  "Compras"=mean(Gasto_compras)) %>% 
        pivot_longer(cols = Alojamiento:Compras,  names_to = "tipo",values_to = "gasto_medio") %>% 
        ggplot() +
        geom_line(aes(x=salida_anio_mes,y=gasto_medio,color=tipo)) +
        labs(x="Fecha de salida", y="Gasto medio", color="Concepto")

    }else{
        datos %>% 
        filter( Fecha_salida > input$vperiodo2[1],  Fecha_salida < input$vperiodo2[2]) %>%
        mutate(salida_anio_mes=round_date(Fecha_salida,"month")) %>% 
        group_by(salida_anio_mes) %>%   
        summarise("Alojamiento"=mean(alojamiento_persona_diario),
                  "Alimentacion"=mean(alimentacion_persona_diario),
                  "Transporte internacional"=mean(transint_persona_diario),
                  "Transporte local"=mean(transloc_persona_diario),
                  "Tours"=mean(tours_persona_diario),
                  "Compras"=mean(compras_persona_diario)) %>% 
        pivot_longer(cols = Alojamiento:Compras,     names_to = "tipo",values_to = "gasto_medio") %>% 
        ggplot() +
        geom_line(aes(x=salida_anio_mes,y=gasto_medio,color=tipo)) +
        labs(x="Fecha de salida", y="Gasto diario por persona",   color="Concepto")}})
  
##tab3##  
  output$plot3 <- renderPlot({
        datos %>% 
        filter( Fecha_salida > input$vperiodo3[1], Fecha_salida < input$vperiodo3[2]) %>%   
        ggplot() +
        geom_boxplot(aes(x=reorder(.data[[input$vgasto3]],gasto_persona_diario),y=gasto_persona_diario,fill=.data[[input$vgasto3]])) +
        theme(legend.position ="none",axis.text.x = element_text(angle = 20)) +
        labs(x="", y="Gasto diario por persona")})
  
  
  output$table3 <- renderDT(
         datos %>% 
         filter( Fecha_salida > input$vperiodo3[1], Fecha_salida < input$vperiodo3[2]) %>% 
         group_by(.data[[input$vgasto3]]) %>% 
         summarise(min=round(min(gasto_persona_diario),2),
                   Q1=round(quantile(gasto_persona_diario,probs=0.25),2),
                   mediana=round(median(gasto_persona_diario),2),
                   media=round(mean(gasto_persona_diario),2),
                   Q3=round(quantile(gasto_persona_diario,probs=0.75),2),
                   max=round(max(gasto_persona_diario),2)) %>% 
         arrange(desc(media)))}


shinyApp(ui = ui, server = server)


#https://patriciamartell.shinyapps.io/ShinyApp/#