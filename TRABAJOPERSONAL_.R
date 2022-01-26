install.packages("nycflights13")
library(nycflights13)
library(lubridate)
library(tidyverse)
lineas <- nycflights13::airlines
aeropuertos <- nycflights13::airports
vuelos <- nycflights13::flights
aviones <- nycflights13::planes
clima <- nycflights13::weather

retraso <- vuelos[which(vuelos$arr_delay > 60),]

dim(retraso)
print("Hay 27789 vuelos que llegaron por lo menos 1h despues de su hora de llegada estimada")

vuelos_sf <- vuelos[vuelos$dest == "SFO" | vuelos$dest == "OAK",]
vuelos_sf
dim(vuelos_sf)
print("Hay 13643 vuelos que volaron hacia San Francisco")

vuelos_op <- vuelos[vuelos$carrier == "UA" | vuelos$carrier == "AA",]
dim(vuelos_op)
print("Encontramos 91.394 vuelos operados por United American y American Airlines")


vuelos_mes <- vuelos[vuelos$month == "4" | vuelos$month == "5" | vuelos$month == "6",]
dim(vuelos_mes)

print("Aparecen 85.369 vuelos entre estos meses")

vuelos_hora <- vuelos[which(vuelos$arr_delay > 60 & vuelos$dep_delay < 60),]
dim(vuelos_hora)
print("4956 vuelos llegaron mas de una hora tarde pero salieron con menos de una hora de retraso")

vuelos_hora2 <- vuelos[which(vuelos$dep_delay > 60 & vuelos$arr_delay < 30),]
dim(vuelos_hora2)
print(" 181 vuelos salieron con más de una hora de retraso pero consiguieron
llegar con menos de 30 minutos de retraso")

vuelo_salida <- vuelos[which(vuelos$hour >=0 & vuelos$hour <=7),]
dim(vuelo_salida)
print("salen entre medianoche y las 7 de la mañana (vuelos
                                                    nocturnos")

vuelo_desc <- vuelos[is.na(vuelos$dep_time),]
dim(vuelo_desc)

dato_des <- vuelos[is.na(vuelos$vuelos),]

apply(X = is.na(vuelos), MARGIN = 2, FUN = sum)

arrange(vuelos, desc(dep_delay))

arrange(vuelos, dep_delay)

vuelos$horasalida <-(vuelos$dep_time %/% 100 * 60) + (vuelos$dep_time %% 100)
vuelos$horasalest <-(vuelos$sched_dep_time %/% 100 * 60) + (vuelos$sched_dep_time %% 100)
vuelos&horasalida
arrange(vuelos, desc(distance))


arrange(vuelos, distance)

vuelos$salientes <-(vuelos$dep_time %/% 100 * 60) + (vuelos$dep_time %% 100)
vuelos$horasalientest <-(vuelos$sched_dep_time %/% 100 * 60) + (vuelos$sched_dep_time %% 100)
vuelos$salientes
vuelos$salientes

vueloscancelados <-  vuelos %>%
  mutate(cancelado = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(num_cancelado = sum(cancelado), num_vuelo = n(),)

ggplot(vueloscancelados) +
  geom_line(aes(x = num_vuelo, y = num_cancelado, col=num_vuelo, main = "Patrón de cancelación")) 

proptardanz_cancel <- 
  vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(year, month, day) %>%
  summarise(prop_cancelados = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()

proptardanz_cancel
ggplot(proptardanz_cancel) +
  geom_line(aes(x = med_dep_delay, y = prop_cancelados, col=prop_cancelados))


proptard_canc_aeropuerto <- 
  vuelos %>%
  mutate(cancelados = (is.na(tailnum))) %>%
  group_by(origin, dest) %>%
  summarise(prop_cancelados = mean(cancelados),med_dep_delay = mean(dep_delay, na.rm = TRUE),med_arr_delay = mean(arr_delay, na.rm = TRUE)) %>% ungroup()

ggplot(proptard_canc_aeropuerto) +
  geom_line(aes(x = med_dep_delay, y = prop_cancelados, col= prop_cancelados))

vuelos %>%
  group_by(carrier) %>%
  summarise(arr_delay = mean(arr_delay, na.rm = TRUE)) %>%
  arrange(desc(arr_delay))
?carrier

vuelos %>%
  group_by(hour) %>%
  summarise(dep_delay = mean(dep_delay, na.rm = TRUE)) %>%
  arrange(dep_delay)

make_dtime <- function(year, month, day, time) {
  make_datetime(year, month, day, time %/% 100, time %% 100)
}
vuelos_dt <- vuelos %>% 
  filter(!is.na(dep_time), !is.na(arr_time)) %>% 
  mutate(
    dep_time = make_dtime(year, month, day, dep_time),
    arr_time = make_dtime(year, month, day, arr_time),
    sched_dep_time = make_dtime(year, month, day, sched_dep_time),
    sched_arr_time = make_dtime(year, month, day, sched_arr_time)
  ) %>% 
  select(origin, dest, ends_with("delay"), ends_with("time"))
vuelos_dt %>%
  mutate(dow = wday(sched_dep_time)) %>%
  group_by(dow) %>%
  summarise(
    dep_delay = mean(dep_delay),
    arr_delay = mean(arr_delay, na.rm = TRUE)
  ) %>%
  print(n = Inf)

retraso_totvuelos <- vuelos %>%
  filter(arr_delay > 0) %>%
  group_by(dest) %>%
  summarise(arr_delay = sum(arr_delay))
retraso_totvuelos
