## R

# Expresion
1 + 3

# Asignacion
a <- c(1,2,3,4,5,8)
a
(b <- c(3,6,10))

# Combinaciones
ab <- c(a,4,5,6,b)
ab

# Operaciones aritmeticas basicas
v3 <- c(-3:3)
sqrt(v3)
abs(v3)
exp(v3)
log(v3)
factorial(v3)


# Operaciones 
v4 <- c(-2,3,5,0,0,3)
v5 <- c(20,14,51,76,21,30)
length(v4)
max(v4)
min(v4)
sum(v4)
prod(v4)
v4*v5
sum(v4*v5)

# Secuencias
v6 <- c(1,2,3)
rep(v6,2)
rep(v6,each=2)
rep(v6,each=2,times=2)
rep(v6,c(2,3,4))
1:10
seq(1,10)
10:1
seq(10,1)
seq(4,length=8)
seq(0,40,by=5)
seq(0,1,length=4)
seq(from=2,to=8,by=3)
rev(1:5)
2*1:4


# Secuencias
a <- c(-2,-1,0,1,2)
a > 2
a <= -1
a > 0 & abs(a) == 2
a > 0 | abs(a) == 2


# Indexacion: 
#Cuidado los que vengáis de python,se empieza en 1!!!!
v7 <- c(-3:3)
v7[1]
v7[2:4]
v7[-(2:length(v7))]
v7[c(2,4,6)]
v7[v7>0]
v7[v7!=0]
v7[abs(v7)==3]
v7[v7>0 & v7!=2]
v7[v7<0 | v7==3]

a <- c(0,3,0,1,3,0)
which(a==0)
which(a>0 & a<=2)


# Matrices 
#matrix (datos, nfiles, ncolumnas)
matrix(1:8,2,4)
matrix(1:8,2,4,byrow=TRUE)
matrix(1:8,nrow=2,byrow=TRUE)
matrix(1:8,ncol=2,byrow=TRUE)
a<-matrix(0,2,2)
b<-matrix(1,1,2)
rbind(a,b)
cbind(1,1:4)
a<-matrix(5*1:4,2,2)
diag(a)


# Indexacion de matrices
a <- matrix(1:9*3,3,3)
a[,3]
a[2,]
a[-1,-3]
a[2:3,1]


# Operaciones matriciales basicas
dim(a)
t(a)
det(a)
solve(a)
rank(a)
nrow(a)
ncol(a)
rowSums(a)
colSums(a)
rowMeans(a)
colMeans(a)


# is/as: 
#para saber qué tipo de  tengo/cambiarlos
a <- c(1,3,6)
is.matrix(a)
is.vector(a)
class(a)

(A <- matrix(1,2,2))
(a <- as.vector(A))


# Funciones
cubic <- function(x) x^3
x0 <- (-4:4)
cubic(x0)


fibonacci <- function(n) {
  a = 0
  b = 1
  for (i in 1:n) {
    tmp = b
    b = a
    a = a + tmp
  }
  return(a)
}


# apply y tapply
m <- matrix(seq(1,16), 4, 4)
m
apply(m, 2, max)
apply(m, 1, min)

#factores
altura <- c(131,125,126,140,152,119)
genero <- c('m','m','f','m','f','f')
f.genero <- factor(genero)
cbind(f.genero,altura)
tapply(altura,f.genero,mean)


# Data.frame
g <- c('m','m','f','m','f','f') # Genero
v1 <- c(131,125,126,140,152,119) # Altura
v2 <- c(48,53,45,40,49,50) # Peso
datos <- data.frame(genero=g,
                    altura=v1,peso=v2)
datos

datos2 <- subset(datos,genero=="m" & peso<50)

##carga de datos

getwd()
setwd("D:/Diego/OneDrive - Universidad Nacional de Colombia/maestria_big_data/clases/estadistica_avanzada/scripts/")
#Seleccionar filas de la tabla según condiciones.
sangre<-read.csv('datosDonacionesCorea.csv')
portu<-read.csv('estudiantesPortugues.csv')
str(sangre)
head(sangre)
tail(portu)
names(sangre)
names(sangre)[1]='Id'
names(sangre)

condicionFemenina=(portu$sexo=='F')
portuFem<-portu[condicionFemenina, ]
portuMasc<-portu[!condicionFemenina, ]


## ESTADISTICA DESCRIPTIVA

# Frecuencias relativas y absolutas
datos <- c("Vainilla", "Fresa", "Vainilla",
           "Fresa", "Chocolate", "Avellana",
           "Chocolate", "Vainilla", "Cafe",
           "Vainilla", "Fresa", "Chocolate",
           "Turron", "Menta", "Menta", "Menta")
(N <- length(datos))
(n <- table(datos))
sum(n)
(f <- n/N)
sum(f)
f100 <- round(100*f,2)
sum(f100)


# Tabla
datos <- c("0", "1", "1", "2", "2", "1",  "1", "1",
           "2", "2", "1", "1", "2", "3", "3", "3")
N <- length(datos)
n <- table(datos)
cumsum(n)
cumsum(n)/N
round(100*cumsum(n)/N,2)

tiempos <- c(40,45,33,44,56,31,
             30,33,53,52,59,41)



#medidas de resumen
summary(sangre)
mean(sangre$vDonado)
median(sangre$vDonado)
sd(sangre$vDonado)
max(sangre$vDonado)
min(sangre$vDonado)
quantile(sangre$vDonado)
quantile(sangre$vDonado, p=c(0.576,0.99))
range(sangre$vDonado)
IQR(sangre$vDonado)


#cuasi-varianza y varianza
equipoA <- c(0,1,2,1,4,0,2,
             2,1,3,5,0,0,1)
equipoB <- c(0,0,1,1,1,0,1,
             2,0,2,1,1,1,6)
meanA <- mean(equipoA)
meanB <- mean(equipoB)

sum((equipoA - meanA)^2)/(length(equipoA)) #varianza
sum((equipoA - meanA)^2)/(length(equipoA)-1) #cuasi
var(equipoA)
var(equipoA) > var(equipoB)

sd(equipoA)
sd(equipoB)
sd(equipoA) > sd(equipoB)


####PLOTS
# Diagrama de barras
barplot(n)
barplot(n, main = "Diagrama de barras:
        Helado de menta", space = 0.5,
        xlab = "Categorias",
        ylab = "Frecuencia absoluta",
        col="pink", border="purple")


# Diagrama de sectores
lbls <- paste(names(n), "\n(", n, ")",sep="")
pie(n, labels = lbls,
    main = "Diagrama de sectores: Helado de menta",
    col=rainbow(4), border="yellow")


# Diagrama de dispersión (scatterplot)
plot(sangre$nDonaciones, sangre$vDonado)
plot(sangre$mesesDonante, sangre$vDonado)


# Histograma
h=hist(sangre$vDonado)
h
h=hist(sangre$vDonado, n=5)
h=hist(sangre$vDonado, n=20)
h=hist(portuMasc$nota3)
abline(v=c(mean(portuMasc$nota3)-2*sd(portuMasc$nota3), mean(portuMasc$nota3), mean(portuMasc$nota3)+2*sd(portuMasc$nota3)), col=2
)

# Gráfico de cajas: Boxplot
boxplot(sangre)
par(mfrow=c(1,2))#así pongo 1 fila y 2 cols de gráficos
l <- min(c(equipoA,equipoB))
u <- max(c(equipoA,equipoB))
boxplot(equipoA,ylim=c(l,u), col="blue",
        main = "Equipo A",
        ylab="Goles por partido")
boxplot(equipoB,ylim=c(l,u), col="green",
        main = "Equipo B")

######



