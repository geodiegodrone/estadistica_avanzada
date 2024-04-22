getwd()
setwd("D:/Diego/OneDrive - Universidad Nacional de Colombia/maestria_big_data/clases/estadistica_avanzada/actividad_1/datos_rstudio/PRSA_Data_20130301-20170228")
# Lectura de los datos de calidad del aire de la estación Wanliu:
wanliu <- read.csv("PRSA_Data_Wanliu_20130301-20170228_mod.csv")


# Eliminar los datos NA de las variables PM2.5, SO2 y TEMP
wanliu_clean <- na.omit(wanliu[, c("PM2.5", "SO2", "TEMP", "lluvia")])

#separamos test/train 80%
## Si fijamos la semilla (set seed) luego nos sale siempre la misma muestra aleatoria
set.seed(5)
#seleccionamos los indices para entrenar
train_ind <- sample(seq_len(nrow(wanliu_clean)), size =floor(0.8*nrow(wanliu_clean)))

#separamos test/train sets
train <- wanliu_clean[train_ind, ]
test <- wanliu_clean[-train_ind, ]

# Estimamos el modelo
names(wanliu_clean)
lm.fit=lm(PM2.5~SO2,data=train) 
# Sacamos por pantalla el modelo
summary(lm.fit)
names(lm.fit)
coef(lm.fit)

# Graficamos el modelo
plot(train$SO2,train$PM2.5)
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
points(test$SO2, test$PM2.5,pch=3, col=3)
##
plot(lm.fit)

predTest<-predict(lm.fit, newdata = test)
plot(test$SO2, test$PM2.5)
points(test$SO2, predTest, col=2,pch=19)

tab=data.frame(SO2=c(500,1,1200))
pred<-predict(lm.fit, newdata = tab)

##prediciones con un intervalo de confianza 

predConfiTest<-predict(lm.fit, newdata = test, interval = 'prediction')
View(predConfiTest)
plot(test$SO2, test$PM2.5, pch=19)
#en la primera columna (fit) tenemos el ajuste
lines(test$SO2, predConfiTest[,1], lwd=3,col="red")
#en la segunda y tercera los limites inferior y superior del intervalo
lines(test$SO2, predConfiTest[,2], lwd=3,col="green")
lines(test$SO2, predConfiTest[,3], lwd=3,col="green")

cuad.fit<-lm(PM2.5~poly(SO2,2),data=train)


plot(train$SO2, train$PM2.5)
points(train$SO2, cuad.fit$fitted.values, lw=3,col=2)
predTestcuad<-predict(cuad.fit, newdata = test)
points(test$SO2, test$PM2.5, col=3, pch=4)

summary(cuad.fit)
#que salgan dos filas y dos columnas de gráficos
par(mfrow=c(2,2))
plot(cuad.fit)
#que salga un solo gráfico otra vez
par(mfrow=c(1,1))


#Regresión Multilineal

#separamos test/train 80%
## Si fijamos la semilla (set seed) luego nos sale siempre la misma muestra aleatoria
set.seed(5)
#seleccionamos los indices para entrenar
train_ind <- sample(seq_len(nrow(wanliu_clean)), size =floor(0.8*nrow(wanliu_clean)))

#separamos test/train sets
train <- wanliu_clean[train_ind, ]
test <- wanliu_clean[-train_ind, ]

library(corrplot)
corrplot(cor(train))
multiAll<-lm(SO2~., data = train)

summary(multiAll)
plot(train$AT, train$SO2)
points(train$AT, multiAll$fitted.values, col=2, pch=19)
predAll<-predict(multiAll, test)

plot(test$SO2, predAll)
abline(a=0, b=1, col=2, lw=3)

par(mfrow=c(2,2))
plot(multiAll)
par(mfrow=c(1,1))
library(car)
vif(multiAll)

#tratamos de obtener un modelo más parsimonioso usando el AIC
# en distintos modelos con diferente numero de predictores
#install.packages('MASS')
library(MASS)
stepModel <- stepAIC(multiAll, direction = "both",,trace=1)
summary(stepModel)
plot(stepModel)
vif(stepModel)
predStep=predict(stepModel,test)

plot(test$SO2, predAll)
points(test$SO2, predStep, col=3,pch=6)
abline(a=0, b=1, col=2, lw=3)

multi2<-lm(SO2~PM2.5+TEMP, data = train)
summary(multi2)
predMulti2<-predict(multi2, test)
plot(test$SO2, predAll)
points(test$SO2, predStep, col=3,pch=6)
points(test$SO2, predMulti2, col=4,pch=4)
abline(a=0, b=1, col=2, lw=3)


stepModel2 <- step(multi2, direction = "both", trace = 1)
summary(stepModel2)
par(mfrow=c(2,2))
plot(stepModel2)
par(mfrow=c(1,1))
vif(stepModel2)
predStep2<-predict(stepModel2, test)

plot(test$SO2, predAll)
points(test$SO2, predStep, col=3,pch=6)
points(test$SO2, predMulti2, col=4,pch=4)
points(test$SO2, predStep2, col=5,pch=5)
abline(a=0, b=1, col=2, lw=3)

multiFN<-lm(SO2~PM2.5+TEMP, data = train)
summary(multiFN)
predFN=predict(multiFN, test)
plot(test$SO2, predAll)
points(test$SO2, predFN, col=3,pch=4)
abline(a=0, b=1, col=2, lw=3)


#Regresión logística
set.seed(5)

# selección de índices para entrenar
train_ind <- sample(seq_len(nrow(wanliu_clean)), size = floor(0.8*nrow(wanliu_clean)))

# separación de conjuntos de entrenamiento y prueba
train <- wanliu_clean[train_ind, ]
test <- wanliu_clean[-train_ind, ]

# creación de variable binaria para la variable lluvia
train$lluvia_bin <- ifelse(train$lluvia == "True", 1, 0)
sum(is.na(train$lluvia_bin))

# ajuste del modelo de regresión logística
logist <- glm(lluvia_bin ~ SO2, data = train, family = binomial)
summary(logist)

# gráfico de dispersión de los datos de entrenamiento y las predicciones del modelo
plot(train$SO2, train$lluvia_bin)
points(train$SO2, logist$fitted.values, col = 2, pch = 20)
abline(lm(lluvia_bin ~ SO2, data=train), col="blue")

# predicción de la variable binaria para los datos de prueba
test$lluvia_bin <- ifelse(test$lluvia == "True", 1, 0)
pred <- predict(logist, newdata = test, type = 'response')
pred_bin <- round(pred)

# tabla de contingencia entre las predicciones y los valores reales
table(pred_bin, test$lluvia_bin)

library(rsq)
pR2(logist)

install.packages("pROC")
library(pROC)
roc_obj <- roc(train$lluvia_bin, logist$fitted.values)
plot(roc_obj)
auc(roc_obj)
library(boot)
cv_logist <- cv.glm(train, logist, K = 5, seed = 123)
cv_logist$delta















