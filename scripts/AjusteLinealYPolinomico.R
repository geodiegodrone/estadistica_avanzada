getwd()
setwd('D:/Diego/OneDrive - Universidad Nacional de Colombia/maestria_big_data/clases/estadistica_avanzada/scripts')
#carga de datos
sangre<-read.csv('datosDonacionesCorea.csv')
portu<-read.csv('estudiantesPortugues.csv')
plot(sangre)
plot(portu)
#solo si no está instalado:
#install.packages("corrplot")
library(corrplot)
corrplot(cor(sangre[,c("mesesDesde","nDonaciones","vDonado", "mesesDonante")]))
corrplot(cor(portu[,c('edad','mediaAnterior','nota3')]))

#separamos test/train 80%
## Si fijamos la semilla (set seed) luego nos sale siempre la misma muestra aleatoria
set.seed(5)
#seleccionamos los indices para entrenar
train_ind <- sample(seq_len(nrow(portu)), size =floor(0.8*nrow(portu)))

#separamos test/train sets
train <- portu[train_ind, ]
test <- portu[-train_ind, ]

# Estimamos el modelo
names(portu)
lm.fit=lm(nota3~mediaAnterior,data=train) 
# Sacamos por pantalla el modelo
summary(lm.fit)
names(lm.fit)
coef(lm.fit)




# Graficamos el modelo
plot(train$mediaAnterior,train$nota3)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
points(test$mediaAnterior, test$nota3,pch=3, col=3)
##
plot(lm.fit)

predTest<-predict(lm.fit, newdata = test)
plot(test$mediaAnterior, test$nota3)
points(test$mediaAnterior, predTest, col=2,pch=19)

tab=data.frame(mediaAnterior=c(12,3,37))
pred<-predict(lm.fit, newdata = tab)

##prediciones con un intervalo de confianza 

predConfiTest<-predict(lm.fit, newdata = test, interval = 'prediction')
View(predConfiTest)
plot(test$mediaAnterior, test$nota3, pch=19)
#en la primera columna (fit) tenemos el ajuste
lines(test$mediaAnterior, predConfiTest[,1], lwd=3,col="red")
#en la segunda y tercera los limites inferior y superior del intervalo
lines(test$mediaAnterior, predConfiTest[,2], lwd=3,col="green")
lines(test$mediaAnterior, predConfiTest[,3], lwd=3,col="green")

cuad.fit<-lm(nota3~poly(mediaAnterior,2),data=train)


plot(train$mediaAnterior, train$nota3)
points(train$mediaAnterior, cuad.fit$fitted.values, lw=3,col=2)
predTestcuad<-predict(cuad.fit, newdata = test)
points(test$mediaAnterior, test$nota3, col=3, pch=4)

summary(cuad.fit)
#que salgan dos filas y dos columnas de gráficos
par(mfrow=c(2,2))
plot(cuad.fit)
#que salga un solo gráfico otra vez
par(mfrow=c(1,1))

