#------------
# Librerías o paquetes requeridos
library(MASS)
library(ellipse)
library(lattice)


#------------
# 1) Crear la base de datos "datos"
datos2= read.csv2("datos1.csv",row.names = 1)  # Base de datos original
datos2    # Falta un dato al estudiante 13

datos2=na.omit(datos2)      # na.omit, para eliminar al estudiante 13
datos2

colnames(datos2) <- c("Sexo","LTot","Cint","LEsp","LBra")   	# Rótulos de la base de datos
head(datos2)          # Base de datos abreviada

datos2$Sexo = as.factor(datos2$Sexo)  # convertir columna Sexo a factor
summary(datos2$Sexo)    # 14 mujeres y 10 hombres


#------------
# 2) Matrices por sexo 
str(datos2)
hombres = datos2[datos2$Sexo == "M",]   # Matriz de hombres
hombres [1:4, 1:5]      # cuatro filas y 5 columnas de esta base de datos

mujeres = datos2[datos2$Sexo == "F",]   # Matriz de mujeres
mujeres [1:4, 1:5]      # cuatro filas y 5 columnas de esta base de datos


#------------
# 3) Matrices centradas por sexos
var.h = hombres[,c(2:5)]        # Variables morfométricas de hombres
promedio.h = colMeans(var.h)    # Promedios de las variables
promedio.h

var.m = mujeres[,c(2:5)]        # Variables morfométricas de mujeres
promedio.m = colMeans(var.m)    # Promedios de las variables
promedio.m

# *Opcional - matriz centrada
m.centrada.h <- t(t(var.h) - promedio.h) # Restar cada dato a los promedios
m.centrada.h  

m.centrada.m <- t(t(var.m) - promedio.m) # Restar cada dato a los promedios
m.centrada.m 



#------------
# 4) Operaciones vectoriales
dif = promedio.h - promedio.m      # Diferencia de medias (m1-m2)
dif
t.dif = t(dif)    # Transpuesta de la diferencia (m1-m2)'
t.dif

sum = promedio.h + promedio.m      # Suma de medias (m1+m2)
sum


#------------
# 5) Matriz de covarianza generalizada o compuesta (S-1)

cov.h = var(hombres[,c(2:5)])     # Matriz de varianza y covarianzas de hombres
round(cov.h,1)

cov.m = var(mujeres[,c(2:5)])     # Matriz de varianza y covarianzas de mujeres
round(cov.m,1)

summary(datos2$Sexo)    # 14 mujeres y 10 hombres

cov.g = (10*cov.h + 14*cov.m)/24  # Covarianza genealizada o compuesta
round(cov.g,2)

cov.g.i = solve(cov.g) # Cov. generalizada invertida
round(cov.g.i,3)




#------------
# 3) Función discriminante de Wlad y Anderson (Wx)
# W(x) = [(m1-m2)'.S-1.x]-[1/2.(m1-m2)'.S-1 .(m1+m2)]

head(datos2)      # Base de datos
datos3 = as.matrix (datos2[2:5])
head(datos3) 

# 10 primeros alumnos como vectores

x1 = as.vector(datos3[1, 1:4])   # Datos del alumno 1
x2 = as.vector(datos3[2, 1:4])   # datos3[2, 1:4]: fila 2, columnas 1 a 4. alumno 1
x3 = as.vector(datos3[3, 1:4])
x4 = as.vector(datos3[4, 1:4])
x5 = as.vector(datos3[5, 1:4])
x6 = as.vector(datos3[6, 1:4])
x7 = as.vector(datos3[7, 1:4])
x8 = as.vector(datos3[8, 1:4])
x9 = as.vector(datos3[9, 1:4])
x10 = as.vector(datos3[10, 1:4])  # Datos del alumno 10


# Discriminantes de los 10 primeros alumnos
# Si da negativo discrimina en mujer y positivo en hombre
# W(x) = [(m1-m2)'.S-1.x]-[1/2.(m1-m2)'.S-1 .(m1+m2)]

W1 = ((t.dif%*%cov.g.i%*%x1)-(1/2*(t.dif%*%cov.g.i%*%sum)))   # Datos del alumno 1, 
W2 = ((t.dif%*%cov.g.i%*%x2)-(1/2*(t.dif%*%cov.g.i%*%sum)))   
W3 = ((t.dif%*%cov.g.i%*%x3)-(1/2*(t.dif%*%cov.g.i%*%sum)))
W4 = ((t.dif%*%cov.g.i%*%x4)-(1/2*(t.dif%*%cov.g.i%*%sum)))
W5 = ((t.dif%*%cov.g.i%*%x5)-(1/2*(t.dif%*%cov.g.i%*%sum)))
W6 = ((t.dif%*%cov.g.i%*%x6)-(1/2*(t.dif%*%cov.g.i%*%sum)))
W7 = ((t.dif%*%cov.g.i%*%x7)-(1/2*(t.dif%*%cov.g.i%*%sum)))
W8 = ((t.dif%*%cov.g.i%*%x8)-(1/2*(t.dif%*%cov.g.i%*%sum)))
W9 = ((t.dif%*%cov.g.i%*%x9)-(1/2*(t.dif%*%cov.g.i%*%sum)))
W10 = ((t.dif%*%cov.g.i%*%x10)-(1/2*(t.dif%*%cov.g.i%*%sum))) # Discriminante alumno 10

ad = data.frame (W1,W2,W3,W4,W5,W6,W7,W8,W9,W10)  # Discriminantes compilados
ad
t(ad)   # Transpuesto

clase1 <- sample("F", size = 3, replace = TRUE)   # vector de las 3 mujeres
clase2 <- sample("M", size = 7, replace = TRUE)   # vector de los 7 hombres
clase <- c(clase1, clase2)      # Fusión de vectores
clase

ad = data.frame(LD1 = t(ad),clase)
ad


#------------
# 4) Comparación del discriminante manual con el automatizado
# *Nota: Los detalles de este análisis de presentarán en el capítulo de discriminantes lineales.
library(MASS)
ad1<-lda(Sexo~LTot+Cint+LEsp+LBra,data=datos2)    # Discriminante lineal de Fisher
ad1

names(ad1)  # Insumos del discriminante

group<-predict(ad1,method="plug-in")$class  # Desempeño del discriminante
(tabla<-table(datos2$Sexo,group))

result = predict(ad1,method="plug-in")  # Otros insumos del discriminante
result

result$class     # Discriminante realizado
result$x         # Función discriminante en cada alumno

result = data.frame(result$x, clase=result$class)   # Data frame
ad2 = result[1:10,]   # Data frame de los 10 primeros alumnos
ad2

ad == ad2  # Igualdad ente el discriminante manual (ad) y el automatizado (ad2)



#------------
# 5) Figuras

# Librerías
library(ellipse)
library(lattice)

str(datos2)
x11()
boxplot(LTot~Sexo, data = datos2, ylab ="Altura (cm)",    # Cajas grises
        cex.lab=1.3)

boxplot(LTot~Sexo, data = datos2, ylab ="Altura (cm)",	  # Cajas amarillas con muescas 
        cex.lab=1.3,notch=T,col="bisque")

boxplot(LTot~Sexo, data = datos2, ylab ="Altura (cm)",	  # Cajas azules con muescas
        cex.lab=1.3,notch=T,col="lightblue")


x11()
boxplot(LD1~clase, data = ad, ylab ="Eje 1",
        cex.lab=1.3)

x11()
boxplot(LD1~clase, data = ad2, ylab ="Eje 1",
        cex.lab=1.3)



