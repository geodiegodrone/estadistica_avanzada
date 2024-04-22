library(datasets)
#install.packages('forecast')
library(forecast)
##separamos dataset train y test
cTrain<- 1:240<=192
train<-ts(subset(nottem, subset = cTrain), start = c(1920,1), frequency = 12)
test<-ts(subset(nottem, subset = !cTrain), start = c(1936,1), frequency = 12)

#recordemos cómo era la serie: descomposición STL
stlPeriod=stl(nottem, 'periodic')
plot(stlPeriod)
#el mod. suavizado Holt-winters era el mejor.
holtWin<-hw(train, h=48)
plot(holtWin)
lines(test, col=2)
summary(holtWin)
##autocorrelación
#efectos los distintos conjuntos

par(mfrow=c(1,1))
dTrendLIn <- ts(data=0.5*(1:100))
plot(dTrendLIn)
acf(dTrendLIn)
pacf(dTrendLIn)
dSeas <- ts(data=sin(0.5*(1:100)))
plot(dSeas)
acf(dSeas)
pacf(dSeas)
dRand <- ts(data=rnorm(100,mean = 0,sd=1))
plot(dRand)
acf(dRand)
pacf(dRand)

stot<-dTrendLIn+10*dSeas+dRand
plot(stot)
acf(stot)
pacf(stot)

#evaluemos el remainder de la descomposición STL
remDesc=stlPeriod$time.series[,'remainder']
acf(remDesc)
pacf(remDesc)



acf(nottem)
pacf(nottem)

dnot<-diff(nottem)
plot(dnot)
acf(dnot)
pacf(dnot)

d12<-diff(train, lag=12)
plot(d12)
acf(d12)
pacf(d12)

d12_2<-diff(d12)
plot(d12_2)
acf(d12_2)
pacf(d12_2)

ari1_111<-arima(train,order=c(1,0,0), seasonal = c(1,1,1))

autoMod<-auto.arima(train)

pr<-predict(ari1_111, n.ahead = 48)

plot(holtWin, xlim=c(1935, 1940))
lines(test)
lines(pr$pred, col=2)

errTestsHoltWin=holtWin$mean-test
errTestAri=pr$pred-test

plot(errTestsHoltWin)
lines(errTestAri, col=2)

sum(errTestsHoltWin^2)
sum(errTestAri^2)



