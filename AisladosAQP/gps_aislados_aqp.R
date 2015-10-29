## Codigo para la generacion de aislados 
## obtenidos por laboratorio en la ciudad de Arequipa

library(zoom)
## Primero abrimos los archivos correspondientes a los
## distritos que se tiene referencia de encontrarse 
## aislados (ubicacion en el phpmyadmin)
gps<-read.csv("AREQUIPA_GPS_GOOGLE.csv")
##
## Segundo abrimos la base de los aislados actualizada
## por laboraratorio hasta el momento 
## https://docs.google.com/spreadsheets/d/1xdFcH2SHyVGA9Uepzk3mJ1GMwmZfPr8ZF5WFFCmXs5o/edit#gid=0

ais<-read.csv("Base_Aislados_Arequipa_27oct2015.csv")

## hacemos el merge para juntar las dos bases
gpsais<-merge(gps,ais,by.x="UNICODE",by.y"unicode",all.x=T)

## enseguida ploteamos el mapa de los gps
with(gpsais,plot(gpsais$LONGITUDE,gpsais$LATITUDE,asp=1,pch=".",main="AISLADOS AREQUIPA",xlab="Longitude",ylab="Latitude"))

## ploteamos todos los puntos de los aislados
lines(gpsais$LONGITUDE,gpsais$LATITUDE,cex=sqrt(gpsais$No >0), asp=1,type="p",col="blue",pch=19)

## para poder realizar el zoom y poder salvar
## las imagenes realizamos un zoom
## zm()

## despues grabamos con
## dev.print(device=pdf,"nombre de archivo.pdf")

## Javier
