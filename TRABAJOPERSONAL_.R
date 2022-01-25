install.packages("nycflights13")
library(nycflights13)
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



