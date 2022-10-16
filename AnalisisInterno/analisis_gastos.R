### Análisis del gasto de turistas uruguayos en el Exterior ###

### Evolución del gasto medio
library(lubridate)
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  mutate(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  group_by(salida_anio_mes) %>% 
  summarise(gasto_medio=mean(GastoTotal)) %>% 
  ggplot() +
  geom_line(aes(x=salida_anio_mes,y=gasto_medio)) +
  labs(x="Fecha",
       y="Gasto medio total en dólares americanos corrientes") 

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  mutate(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  group_by(salida_anio_mes) %>% 
  summarise(gasto_alojamiento=mean(GastoAlojamiento),
            gasto_alimentacion=mean(GastoAlimentacion),
            gasto_transporte=mean(GastoTransporteInternac),
            gasto_tours=mean(GastoTours),
            gasto_compras=mean(GastoCompras),
            gasto_otros=mean(GastoResto)) %>% 
  pivot_longer(cols = gasto_alojamiento:gasto_otros,
               names_to = "tipo",values_to = "gasto_medio") %>% 
  ggplot() +
  geom_line(aes(x=salida_anio_mes,y=gasto_medio,color=tipo))

### Evolución del gasto mediano

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  mutate(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  group_by(salida_anio_mes) %>% 
  summarise(gasto_alojamiento=median(GastoAlojamiento),
            gasto_alimentacion=median(GastoAlimentacion),
            gasto_transporte=median(GastoTransporteInternac),
            gasto_tours=median(GastoTours),
            gasto_compras=median(GastoCompras),
            gasto_otros=median(GastoResto)) %>% 
  pivot_longer(cols = gasto_alojamiento:gasto_otros,
               names_to = "tipo",values_to = "gasto_mediano") %>% 
  ggplot() +
  geom_line(aes(x=salida_anio_mes,y=gasto_mediano,color=tipo))

### Hay un ponto raro, nos quedamos con los datos de 2017 en adelante

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  mutate(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  filter(salida_anio_mes>="2017-01-01") %>% 
  group_by(salida_anio_mes) %>% 
  summarise("Alojamiento"=mean(GastoAlojamiento),
            "Alimentación"=mean(GastoAlimentacion),
            "Transporte"=mean(GastoTransporteInternac),
            "Tours"=mean(GastoTours),
            "Compras"=mean(GastoCompras),
            "Otros"=mean(GastoResto)) %>% 
  pivot_longer(cols = "Alojamiento":"Otros",
               names_to = "tipo",values_to = "gasto_medio") %>% 
  ggplot() +
  geom_line(aes(x=salida_anio_mes,y=gasto_medio,color=tipo)) +
  labs(color="Tipo",
       x="Fecha",
       y="Gasto medio en dólares americanos corrientes")

### Gastos por destino ###

# Gasto total

# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Destino) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Destino) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Destino,-media),y=media)) +
  labs(x="País de destino",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Destino,GastoTotal),y=GastoTotal)) +
  labs(x="Destino",
       y="Gasto total en dólares corrientes")

### Gastos por nivel educativo ###

# Gasto total

# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Estudio,GastoTotal),y=GastoTotal)) +
  labs(x="Nivel educativo",
       y="Gasto total en dólares corrientes")

### Gastos por ocupación ###

# Gasto total

# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Ocupacion) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Ocupacion) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Ocupacion,-media),y=media)) +
  labs(x="Ocupación",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Ocupacion,GastoTotal),y=GastoTotal)) +
  labs(x="Ocupacion",
       y="Gasto total en dólares corrientes")

### Gastos por ocupación - recodificado ###

# Reclasificamos categorías

# Obtenemos los códigos (no están guardados como niveles de factor)
class(emisivo$IdOcupacion)
table(emisivo$Ocupacion,emisivo$IdOcupacion)

# O = "Sin datos"
# 1 = "Jubilado, Pensionista"
# 2 = "Ama de casa"
# 3 = "Estudiante"
# 4 = "Rentista"
# 5 = "Otra situacion Inactividad"
# 8 = "Desocupado"
# 9 = "Desconocido / Sin Datos"
# 11 = "Patron, Com, Ind, Prod Agrop"
# 12 = "Director, gerente"
# 13 = "Prof, Tecnico, Docente, Artista"
# 14 = "Jefe, Capataz, Encargado"
# 15 = "Obrero esp, Conductor, Artesano"
# 16 = "Empl. Adm, Cajero, Vendedor"
# 17 = "Mozo, Portero, Serv Dom, Otros Serv"
# 18 = "Trabajador Agro, Pesca"
# 19 = "Trabajador sin especializacion"
# 20 = "Deportista, Entrenador, Juez Dep"
# 21 = "Militar, policia, Aduanero, Insp, bombero, Marinero"
# 22 = "Religioso"
# 99 = "Otros"

emisivo <- emisivo %>% 
  mutate(Ocupacion_rec=case_when(IdOcupacion %in% 1:5 ~ "Inactivo",
                                 IdOcupacion==8 ~ "Desocupado",
                                 IdOcupacion %in% 11:21 ~ "Ocupado",
                                 IdOcupacion %in% c(22,99) ~ "Otros",
                                 IdOcupacion %in% c(0,9) ~ "Sin datos"))
  
# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Ocupacion_rec) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Ocupacion_rec) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Ocupacion_rec,-media),y=media)) +
  labs(x="Ocupación",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Ocupacion_rec,GastoTotal),y=GastoTotal)) +
  labs(x="Ocupacion",
       y="Gasto total en dólares corrientes")

### Gastos por departamento ###

# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Departamento) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Departamento) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Departamento,-media),y=media)) +
  labs(x="Departamento de residencia",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Departamento,GastoTotal),y=GastoTotal)) +
  labs(x="Departamento de residencia",
       y="Gasto total en dólares corrientes")

### Gastos por región - departamento recodificado ###

# Reclasificamos categorías
emisivo <- emisivo %>% 
  mutate(Region=case_when(Departamento=="Montevideo" ~ "Montevideo",
                          TRUE ~ "Interior"))

# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Region) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Region) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Region,-media),y=media)) +
  labs(x="Región de residencia",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Region,GastoTotal),y=GastoTotal)) +
  labs(x="Región de residencia",
       y="Gasto total en dólares corrientes")

### Gastos por transporte de salida ###

# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(`Transporte Internacional de Salida`) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(`Transporte Internacional de Salida`) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(`Transporte Internacional de Salida`,-media),y=media)) +
  labs(x="Medio de transporte de salida",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(`Transporte Internacional de Salida`,GastoTotal),y=GastoTotal)) +
  labs(x="Medio de transporte de salida",
       y="Gasto total en dólares corrientes")

### Gastos por tipo de alojamiento ###


# Tabla
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(`Transporte Internacional de Salida`) %>% 
  summarise(min=min(GastoTotal),
            Q1=quantile(GastoTotal,probs=0.25),
            mediana=median(GastoTotal),
            media=mean(GastoTotal),
            Q3=quantile(GastoTotal,probs=0.75),
            max=max(GastoTotal)) %>% 
  arrange(desc(mediana))

# Gráfico de barras
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Alojamiento) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Alojamiento,-media),y=media)) +
  labs(x="Tipo de alojamiento",
       y="Gasto medio total en dólares corrientes")

# Diagrama de caja
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_boxplot(aes(x=reorder(Alojamiento,GastoTotal),y=GastoTotal)) +
  labs(x="Tipo de alojamiento",
       y="Gasto total en dólares corrientes")

### Desagregación del gasto ###
