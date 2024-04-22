getwd()
setwd("D:/Diego/OneDrive - Universidad Nacional de Colombia/maestria_big_data/clases/estadistica_avanzada/actividad_1/datos_rstudio/PRSA_Data_20130301-20170228")
# Lectura de los datos de calidad del aire de la estación Wanliu:
wanliu <- read.csv("PRSA_Data_Wanliu_20130301-20170228_mod.csv")

# Eliminar los datos NA de las variables year y PM2.5
wanliu_clean <- na.omit(wanliu[, c("year", "PM2.5")])

# Convertir la variable year a numérico
wanliu_clean$year <- as.numeric(wanliu_clean$year)

# Convertir la base de datos en un objeto de series de tiempo
wanliu_ts <- ts(wanliu_clean$PM2.5, start = c(2013, 1), frequency = 8000)
summary(wanliu_ts)
plot(wanliu_ts)

# Descomposición STL
stl_wanliu <- stl(wanliu_ts, 'periodic')
summary(stl_wanliu)
plot(stl_wanliu)
seasonal_component <- stl_wanliu$time.series[, "seasonal"]
plot(seasonal_component, main = "Componente estacional", ylab = "PM2.5", xlab = "Tiempo")

# Crear conjunto de datos de entrenamiento y prueba
train_end <- as.Date("2017-01-01") - 1
cTrain <- wanliu_clean$year <= 2016
train <- ts(subset(wanliu_ts, subset = cTrain), start = c(2013, 1), frequency = 8000)
test <- ts(subset(wanliu_ts, subset = !cTrain), start = c(2017, 1), frequency = 365)

# Pronóstico utilizando el modelo de suavizado exponencial
library(forecast)
sExpMod <- ses(train, h = 8000)
plot(sExpMod)
lines(test, col = 2)
summary(sExpMod)

# Pronóstico utilizando el modelo de Holt
holtMod <- holt(train, h = 8000)
plot(holtMod)
lines(test, col = 2)
summary(holtMod)

# Pronóstico utilizando el modelo de Holt-Winters
holtWinMod <- hw(train, h = 8000)
plot(holtWinMod)
lines(test, col = 2)
summary(holtWinMod)

# Pronóstico utilizando el modelo de Holt-Winters multiplicativo
holtWinMultMod <- hw(train, seasonal = 'multiplicative', h = 8000)
plot(holtWinMultMod)
lines(test, col = 2)
summary(holtWinMultMod)

# Seleccionar el mejor modelo de pronóstico utilizando auto.arima
autoMod <- auto.arima(train)
pr <- forecast(autoMod, h = 8000)
plot(pr)
lines(test, col = 2)
summary(autoMod)
