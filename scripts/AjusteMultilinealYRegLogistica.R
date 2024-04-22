turbina<-read.csv('Turbina2011.csv')
#separamos test/train 80%
## Si fijamos la semilla (set seed) luego nos sale siempre la misma muestra aleatoria
set.seed(5)
#seleccionamos los indices para entrenar
train_ind <- sample(seq_len(nrow(turbina)), size =floor(0.8*nrow(turbina)))

#separamos test/train sets
train <- turbina[train_ind, ]
test <- turbina[-train_ind, ]

library(corrplot)
corrplot(cor(train))
multiAll<-lm(CO~., data = train)

summary(multiAll)
plot(train$AT, train$CO)
points(train$AT, multiAll$fitted.values, col=2, pch=19)
predAll<-predict(multiAll, test)

plot(test$CO, predAll)
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

plot(test$CO, predAll)
points(test$CO, predStep, col=3,pch=6)
abline(a=0, b=1, col=2, lw=3)

multi2<-lm(CO~AT+AP+AH+TIT+TAT+TEY+NOX, data = train)
summary(multi2)
predMulti2<-predict(multi2, test)
plot(test$CO, predAll)
points(test$CO, predStep, col=3,pch=6)
points(test$CO, predMulti2, col=4,pch=4)
abline(a=0, b=1, col=2, lw=3)


stepModel2 <- step(multi2, direction = "both", trace = 1)
summary(stepModel2)
par(mfrow=c(2,2))
plot(stepModel2)
par(mfrow=c(1,1))
vif(stepModel2)
predStep2<-predict(stepModel2, test)

plot(test$CO, predAll)
points(test$CO, predStep, col=3,pch=6)
points(test$CO, predMulti2, col=4,pch=4)
points(test$CO, predStep2, col=5,pch=5)
abline(a=0, b=1, col=2, lw=3)

multiFN<-lm(CO~AT+AP+AH+TIT+TAT, data = train)
summary(multiFN)
predFN=predict(multiFN, test)
plot(test$CO, predAll)
points(test$CO, predFN, col=3,pch=4)
abline(a=0, b=1, col=2, lw=3)





###Regresion logistica

set.seed(5)
#seleccionamos los indices para entrenar
train_indB <- sample(seq_len(nrow(Boston)), size =floor(0.8*nrow(Boston)))

#separamos test/train sets
trainB <- Boston[train_indB, ]
testB <- Boston[-train_indB, ]


###Regresion logistica
sum(Boston$chas>0)/nrow(Boston)
#creo una variable aleatoria binaria
sum(!Boston$rm>6)/nrow(Boston)
sum(!trainB$rm>6)/nrow(trainB)

trainB$manyRooms<-trainB$rm>6
testB$manyRooms<-testB$rm>6

sum(!trainB$manyRooms)

logist <- glm(manyRooms~medv, data = trainB, family = binomial)
plot(trainB$medv, trainB$manyRooms)
points(trainB$medv, logist$fitted.values, col=2, pch=20)

lin<-lm(manyRooms~medv, data=trainB)
points(trainB$medv, lin$fitted.values, col=3, pch=20)
pred<-predict(logist, newdata = testB, type = 'response')
predBin<-pred>0.5
plot(predBin, testB$manyRooms)
table(predBin-testB$manyRooms)


logist2 <- glm(chas~medv, data = trainB, family = binomial)
plot(trainB$medv, trainB$chas)
points(trainB$medv, logist2$fitted.values, col=2, pch=20)


