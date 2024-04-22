getwd()
setwd("D:/Diego/OneDrive - Universidad Nacional de Colombia/maestria_big_data/clases/estadistica_avanzada/actividad_1/datos_rstudio/PRSA_Data_20130301-20170228")
# Lectura de los datos de calidad del aire de la estación Wanliu:
anm <- read.csv("ANM_Producci_n_Nacional_de_Minerales_y_Contraprestaciones_Econ_micas_Trimestral_oro.csv")

# Eliminar los datos NA de las variables Año Producción y Cantidad Producción
anm_clean <- na.omit(anm[, c("Año.Produccion", "Cantidad.Producción")])

#separamos test/train 80%
## Si fijamos la semilla (set seed) luego nos sale siempre la misma muestra aleatoria
set.seed(5)
#seleccionamos los indices para entrenar
train_ind <- sample(seq_len(nrow(anm_clean)), size =floor(0.8*nrow(anm_clean)))

#separamos test/train sets
train <- anm_clean[train_ind, ]
test <- anm_clean[-train_ind, ]

# Estimamos el modelo
names(anm_clean)
lm.fit=lm(Cantidad.Producción~Año.Produccion,data=train) 
# Sacamos por pantalla el modelo
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

# Graficamos el modelo
plot(train$Año.Produccion,train$Cantidad.Producción)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
points(test$Año.Produccion, test$Cantidad.Producción,pch=3, col=3)
##
plot(lm.fit)

predTest<-predict(lm.fit, newdata = test)
plot(test$Año.Produccion, test$Cantidad.Producción)
points(test$Año.Produccion, predTest, col=2,pch=19)

tab=data.frame(Año.Produccion=c(2022,2025,2030))
pred<-predict(lm.fit, newdata = tab)

##prediciones con un intervalo de confianza 

predConfiTest<-predict(lm.fit, newdata = test, interval = 'prediction')
View(predConfiTest)
plot(test$Año.Produccion, test$Cantidad.Producción, pch=19)
#en la primera columna (fit) tenemos el ajuste
lines(test$Año.Produccion, predConfiTest[,1], lwd=3,col="red")
#en la segunda y tercera los limites inferior y superior del intervalo
lines(test$Año.Produccion, predConfiTest[,2], lwd=3,col="green")
lines(test$Año.Produccion, predConfiTest[,3], lwd=3,col="green")

cuad.fit<-lm(Cantidad.Producción~poly(Año.Produccion,2),data=train)


plot(train$Año.Produccion, train$Cantidad.Producción)
points(train$Año.Produccion, cuad.fit$fitted.values, lw=3,col=2)
predTestcuad<-predict(cuad.fit, newdata = test)
points(test$Año.Produccion, test$Cantidad.Producción, col=3, pch=4)

summary(cuad.fit)
#que salgan dos filas y dos columnas de gráficos
par(mfrow=c(2,2))
plot(cuad.fit)
#que salga un solo gráfico otra vez
par(mfrow=c(1,1))
