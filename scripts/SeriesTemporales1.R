#datos de ejemplo: nottem, tempe in nothingham
#install.packages('datasets')
library(datasets)
decAdd<-decompose(nottem, type='add')
plot(decAdd)
plot(decAdd$figure)
summary(decAdd)
decMult<-decompose(nottem, type='mult')
summary(decMult)
plot(decMult)

stlPeriod=stl(nottem, 'periodic')
plot(stlPeriod)
stl13=stl(nottem,13)
plot(stl13)

stl7=stl(nottem,7)
plot(stl7)

####Predicción
#install.packages('forecast')
library(forecast)
##separamos dataset train y test
cTrain<- 1:240<=192
train<-ts(subset(nottem, subset = cTrain), start = c(1920,1), frequency = 12)
test<-ts(subset(nottem, subset = !cTrain), start = c(1936,1), frequency = 12)

sExpMOd<-ses(train, h=48)
plot(sExpMOd)
lines(test, col=2)
summary(sExpMOd)
errTestsExp=sExpMOd$mean-test
plot(errTestsExp)

holt<-holt(train, h=48)
plot(holt)
lines(test, col=2)
summary(holt)
errTestsHolt=holt$mean-test

holtWin<-hw(train, h=48)
plot(holtWin)
lines(test, col=2)
summary(holtWin)
errTestsHoltWin=holtWin$mean-test

holtWinMult<-hw(train, seasonal = 'multiplicative', h=48)
plot(holtWinMult)
lines(test, col=2)
summary(holtWinMult)
errTestsHoltWinMult=holtWinMult$mean-test

plot(errTestsHoltWin)
lines(errTestsHoltWinMult, col=2)
