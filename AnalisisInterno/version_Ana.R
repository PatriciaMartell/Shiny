library(tidyverse)
library(lubridate)
library(forcats)

emisivo %>% 
  group_by(Pais) %>% 
  summarise(n=n())

min(emisivo$FechaSalida)
max(emisivo$FechaSalida)

# Evolución del turismo emisivo entre diciembre de 2016 y marzo de 2020
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  count(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  ggplot()+
  geom_line(aes(x=salida_anio_mes,y=n)) +
  labs(x="Fecha",
       y="Cantidad de salidas")

# Evolución por motivo
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  mutate(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  group_by(salida_anio_mes,Motivo) %>% 
  summarise(n=n()) %>% 
  ggplot()+
  geom_line(aes(x=salida_anio_mes,y=n,group=Motivo,color=Motivo)) +
  labs(x="Fecha",
       y="Cantidad de salidas")

# Evolución por departamento
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  mutate(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  group_by(salida_anio_mes,Departamento) %>% 
  summarise(n=n()) %>% 
  ggplot()+
  geom_line(aes(x=salida_anio_mes,y=n,group=Departamento)) +
  labs(x="Fecha",
       y="Cantidad de salidas") +
  facet_wrap(~Departamento)

# Evolución en el tiempo por destino
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  mutate(salida_anio_mes=round_date(FechaSalida,"month")) %>% 
  group_by(salida_anio_mes,Destino) %>% 
  summarise(n=n()) %>% 
  ggplot()+
  geom_line(aes(x=salida_anio_mes,y=n,group=Destino)) +
  labs(x="Fecha",
       y="Cantidad de salidas") +
  facet_wrap(~Destino)

# Turismo por destino y nivel educativo
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_bar(aes(x=fct_infreq(Estudio),fill=fct_infreq(Destino))) +
  labs(x="Nivel educativo",
       y="Cantidad")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  ggplot() +
  geom_bar(aes(x=reorder(Estudio,Destino=="Argentina"),fill=Destino),position="fill") +
  labs(x="Nivel educativo",
       y="Proporción")

# Relación entre estadía y gasto total
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
ggplot() +
  geom_point(aes(x=Estadia,y=GastoTotal,color=Estudio),alpha=0.5) +
  labs(x="Días de estadía",
       y="Gasto Total")

# Relación entre alojamiento y gasto en alojamiento
emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Alojamiento) %>% 
  summarise(media=mean(GastoAlojamiento)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Alojamiento,-media),y=media)) +
  labs(x="Tipo de alojamiento",
       y="Gasto medio en alojamiento en dólares corrientes")

# Relación entre nivel educativo y gastos

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoTotal)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio total en dólares corrientes")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoAlimentacion)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio en alimentación en dólares corrientes")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoTransporteInternac)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio en transporte internacional en dólares corrientes")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoCultural)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio en actividades culturales en dólares corrientes")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoTours)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio en tours en dólares corrientes")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoCompras)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio en compras en dólares corrientes")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoResto)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio por otros conceptos en dólares corrientes")

emisivo %>% 
  filter(Pais=="Uruguay") %>% 
  group_by(Estudio) %>% 
  summarise(media=mean(GastoAlojamiento)) %>% 
  ggplot() +
  geom_col(aes(x=reorder(Estudio,-media),y=media)) +
  labs(x="Nivel educativo",
       y="Gasto medio en alojamiento en dólares corrientes")

# Composición del gasto por nivel educativo

# Relación entre motivo y gastos

# Relación entre departamento y gastos

