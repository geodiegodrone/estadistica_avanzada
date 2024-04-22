portu<-read.csv('estudiantesPortugues.csv')
plot(portu)
#separamos test/train 80%
## Si fijamos la semilla (set seed) luego nos sale siempre la misma muestra aleatoria
set.seed(5)
#seleccionamos los indices para entrenar
train_ind <- sample(seq_len(nrow(portu)), size =floor(0.8*nrow(portu)))

#separamos test/train sets
train <- portu[train_ind, ]
test <- portu[-train_ind, ]
#promediados locales
#es algo más complejo que una media directamente
#supersmoother
#escoge entre 3 'span' o tamaños de ventana (ver doc)
#la suavidad se escogería con 'bass', el por defecto es 0
sSmoo<-supsmu(train$mediaAnterior, train$nota3)
plot(train$mediaAnterior, train$nota3)
#lines(train$mediaAnterior, train$nota3)
#lines, funciona bien xq el eje x está ordenado, si no puede hacer cosas extrañas
lines(sSmoo, col=2, lw=2)
lines(supsmu(train$mediaAnterior, train$nota3, bass=5), col=3, lw=2)
lines(supsmu(train$mediaAnterior, train$nota3, bass=10), col=4, lw=2)

##
##regresión: se hace un ajuste en cada entorno
ls0<-loess(nota3~mediaAnterior, data=train, degree = 0)#ver documentación, usar con cuidado
ls1<-loess(nota3~mediaAnterior, data=train, degree = 1)
ls2<-loess(nota3~mediaAnterior, data=train, degree = 2)

#al usar el train set, que no está ordenado, uso puntos
plot(train$mediaAnterior, train$nota3)
points(train$mediaAnterior, ls0$fitted, pch=19, col=2)
points(train$mediaAnterior, ls1$fitted, pch=19, col=3)
points(train$mediaAnterior, ls2$fitted, pch=19, col=4)

#genero un vector ordenado y hago predicción
mediaAnteriorSim<-seq(from=0, to=40, length.out=200)
p0<-predict(ls0, data.frame(mediaAnterior=mediaAnteriorSim))
p1<-predict(ls1, data.frame(mediaAnterior=mediaAnteriorSim))
p2<-predict(ls2, data.frame(mediaAnterior=mediaAnteriorSim))
plot(train$mediaAnterior, train$nota3, xlim=c(0,40))
lines(mediaAnteriorSim, p0, col=2, lw=2)
lines(mediaAnteriorSim, p1, col=3, lw=2)
lines(mediaAnteriorSim, p2, col=4, lw=2)

#para extrapolar
ls0<-loess(nota3~mediaAnterior, data=train, degree = 0, control = loess.control(surface = "direct"))#ver documentación, usar con cuidado
ls1<-loess(nota3~mediaAnterior, data=train, degree = 1, control = loess.control(surface = "direct"))
ls2<-loess(nota3~mediaAnterior, data=train, degree = 2, control = loess.control(surface = "direct"))
p0<-predict(ls0, data.frame(mediaAnterior=mediaAnteriorSim))
p1<-predict(ls1, data.frame(mediaAnterior=mediaAnteriorSim))
p2<-predict(ls2, data.frame(mediaAnterior=mediaAnteriorSim))
#ahora dibujo con límites distintos para que quepa la extrapolación
plot(train$mediaAnterior, train$nota3, xlim = c(0,40), ylim = c(0,60))
lines(mediaAnteriorSim, p0, col=2, lw=2)
lines(mediaAnteriorSim, p1, col=3, lw=2)
lines(mediaAnteriorSim, p2, col=4, lw=2)

#función lowess
lw<-lowess(train$mediaAnterior, train$nota3)
plot(train$mediaAnterior, train$nota3)
lines(lw, col=2)

lw2<-lowess(train$mediaAnterior, train$nota3, f=1/3)
lines(lw2, col=3)

#kernels
kBox<-ksmooth(train$mediaAnterior, train$nota3, kernel='box')
kNor<-ksmooth(train$mediaAnterior, train$nota3, kernel='normal')
plot(train$mediaAnterior, train$nota3)
lines(kBox, col=2)
lines(kNor, col=3)
#kernel con ancho de banda mayor
lines(ksmooth(train$mediaAnterior, train$nota3, kernel='normal', bandwidth = 2), col=4)

#splines
#elección automática de los nodos (siempre son cúbicos, de grado 3)
spli<-smooth.spline(train$mediaAnterior, train$nota3)
plot(train$mediaAnterior, train$nota3)
lines(spli$x, spli$y, col=2)
#fuerzo el número de nodos
spli10<-smooth.spline(train$mediaAnterior, train$nota3, nknots = 10)
lines(spli10$x, spli10$y, col=3)

##aproximación de densidades
hist(train$nota3, n=20)
densnota3<-density(train$nota3)
plot(densnota3)

