library(tidyverse)
library(readxl)

receptivo <- read_excel("receptivo.xlsx")
names(receptivo)

datos2 <- receptivo %>% select("Lugar Ingreso", "Transporte Internacional de Ingreso", 
                               "FechaIngreso", "FechaEgreso", "Pais", "Residencia", "Motivo", "Ocupacion",
                               "Estudio","Localidad", "Departamento","Otro Departamento", "Otra Localidad", "Alojamiento", 
                               "TransporteLocal", "Lugar Egreso","Transporte Internacional de Egreso","Destino","Estadia", "Gente", 
                               "GastoTotal", "GastoAlojamiento", "GastoAlimentacion","GastoTransporte",
                               "GastoCultural", "GastoTours", "GastoCompras", "GastoOtros", "Coef", "CoefTot")


emisivo <- read_excel("emisivo.xlsx")
summary(emisivo)
names(emisivo)

datos <- emisivo %>% select("Lugar Salida", "Transporte Internacional de Salida", 
                            "FechaSalida", "FechaEntrada", "Pais", "Departamento", "Motivo", "Ocupacion",
                            "Estudio", "Destino", "Alojamiento", "Lugar Ingreso", "Transporte Internacional de Ingreso",
                            "Trasporte Local", "Gente", "Estadia", "GastoTotal","GastoAlojamiento", "GastoAlimentacion", "GastoTransporteInternac",
                            "GatoTransporteLocal", "GastoCultural", "GastoTours", "GastoCompras", "GastoResto", "Coef", "CoefTot")


names(datos) <-c("Lugar_Salida","Transp_int_salida", "Fecha_salida","Fecha_entrada","Pais", "Dpto","Motivo", "Ocupacion","Estudio", "Destino",
                "Alojamiento", "Lugar_ingreso", "Transp_int_ingreso", "Transp_local", "Gente", "Estadia", "Gasto_total", "Gasto_alojamiento", 
                "Gasto_alimentacion","Gasto_transp_int", "Gasto_transp_local","Gasto_cultural","Gasto_tours", "Gasto_compras",
                "Gasto_resto", "Coef","Coef_total")

datos <-datos %>% mutate(across(where(is.character), as.factor))

# Dejar los datos donde Pais == Uruguay (Análisis de los viajeros urugayos)

datos <- datos %>% filter(Pais=="Uruguay")

datos %>% group_by(Destino) %>% summarise(gastos=sum(Gasto_total), cantidad_obs=n())

datos %>% group_by(Transp_int_salida) %>% summarise(cantidad_obs=n())

datos %>% group_by(Motivo) %>% summarise(cantidad_obs=n())

datos %>% group_by(Alojamiento) %>% summarise(gastos = sum(Gasto_alojamiento), cantidad_obs=n())


# Seleccionar que tipo de gasto queremos estudiar 

datos %>% group_by(Destino) %>% summarise(Estadia=mean(Estadia), 
                                          Gastos_totales = mean(Gasto_total), 
                                          Gastos_alojamiento = mean(Gasto_alojamiento), 
                                          Gastos_alimentacion= mean(Gasto_alimentacion),
                                          Gastos_transporte_local = mean(Gasto_transp_local),
                                          Gastos_transporte_int = mean(Gasto_transp_int),
                                          Gastos_Cultural = mean(Gasto_cultural),
                                          Gastos_tours = mean(Gasto_tours),
                                          Gastos_compras=mean(Gasto_compras),
                                          Gastos_otros = mean(Gasto_resto),
                                          cantidad_obs=n())

