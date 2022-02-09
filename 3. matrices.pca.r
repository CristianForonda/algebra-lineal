#---------------
# Cargar la base de datos de Excel *.csv
datos= read.csv2("datos1.csv")  
str(datos)      # Estructura de la base de datos

# Librerías
library(ggplot2)
library(vegan)

#---------------
# 1) Ajuste de la base de datos para el trabajo matricial
datos = datos[-13,] # Eliminar el alumno 13 por faltarle un dato
datos
head(datos)     # Encabezado de la base datos
str(datos)      # Estructura de la base de datos
datos[,3:6]     # Variables morfométricas

# Abreviaturas de los estudiantes (filas de la matriz)
str(datos)      # Estructura de la base
datos1 <- datos[,c(3:6)]  # Variables morfométricas
head(datos1)
1:nrow(datos1)  # Generar datos de 1 a n: 25 estudiantes
LETTERS[1:nrow(datos1)]  # Letras para los nombres de los estudiantes
nombres <- LETTERS[1:nrow(datos1)]  # Asignar como "nombres" al comando anterior

# Abreviaturas de las variables (Columnas de la matriz).
colnames(datos1) <- c("LTot","Cint","LEsp","LBra")
head(datos1)

# Base de datos con nombres abreviados (filas y columnas)
dimnames(datos1)=(list(nombres,colnames(datos1)))
datos1
str(datos1)     # Estructura de la base abreviada

datos1$Estud <- rownames(datos1) # Insertar columna de observaciones
head(datos1)    # Encabezado de la base de datos

# Guardar una base datos en el directorio de trabajo
write.csv2(datos1, "datos.csv")

datos = datos1  # Cambiar "datos1" a "datos"
datos[,5]       # Siglas de los estudiantes
rownames(datos)= datos[,5]    # Siglas para las operaciones

#---------------
# 2) Análisis de mediciones de los estudiantes
str(datos)

# 2.1) Sumas y norma de los vectores (variables)
datos = datos[,1:4]             # asignar "datos" solo a las variables morfométricas
sumas <- rowSums(datos) 	# Suma de las variables por cada estudiante (rowSums)
sumas
sumas1 <- colSums(datos) 	# Suma los estudiantes por cada variable (colSums)
sumas1
normas <- sqrt(rowSums(datos ^ 2)) 	# Norma de cada estudiante (vector fila)
normas
round(normas,2) 	# Resumen de resultados a dos decimales 

# Base (datos.t) que integra a las sumas y las normas
datos.t = data.frame(datos,sumas,norma= round(normas,2))
head(datos.t)

# 2.2) Figura de proyección vectorial de los estudiantes  
x11()   # Figura de cada estudiante como un vector 
plot(datos.t[,c(1,3)]) # figura de las variables 1 y 3

# Edición de la figura
x|Linea horizontal del plano cartesiano


# Figuras de distancias de cada estudiante como vectores
plot(sumas, normas)
text(sumas, normas, labels = names(sumas), 
     cex = 0.8, pos = 2, col = "red")                   # Rótulos de los puntos
abline (lm(normas~sumas),lty=2, lwd=2, col="lightblue") # Ajuste lineal (línea sólida)
lines(lowess(sumas, normas),lty=1,lwd=1,col=2)  # Ajuste suavizado  (línea punteada)


# Figura de relación con ggplot2
library(ggplot2)
ggplot(data=datos.t,                            
       aes(x=datos.t$sumas, y=datos.t$norma)) +
        geom_point(na.rm=T) +                    # Nube de puntos      
        geom_smooth(method='loess',na.rm=T) +    # curva tipo loess
        labs(x='Sumas', y='Normas') +
        theme_bw()                               # Fondo blanco

ggplot(data=datos.t,                            
       aes(x=datos.t$sumas, y=datos.t$norma)) +
        geom_point() +
        labs(x='Sumas', y='Normas') +
        geom_smooth(method = lm, se = FALSE)


#---------------
# 3) Cálculo de la matriz centrada de las variables morfométricas

var = datos.t[,c(1:4)]        # Variables morfométricas
promedio = colMeans(var)      # Promedios de las variables
promedio

m.centrada <- t(t(var) - promedio) # Restar cada dato a los promedios
m.centrada  

# Figura de las observaciones centradas
plot(m.centrada[,c(1,3)])                       # Figura general
plot(m.centrada[,c(1,3)],xlim = c(-20, 20),     # Figura editada
     ylim = c(-15, 20), asp = 1, pch = 19)
text(m.centrada[,c(1,3)],                       # Rotulos de los puntos
     labels = row.names(m.centrada[,c(1,3)]), 
     pos = 3, cex=0.7)
grid()          # Grilla en la figura
abline(h = 0, col = "red", lty = 4)             # Plano Cartesiano
abline(v = 0, col = "red", lty = 4)  


#---------------
# 4) Operaciones matriciales - Ordenación multivariada

# 1. Cálculo de valores y vectores propios
cov = var(datos.t[,c(1:4)])     # Matriz de varianza y covarianzas
cov = var(var)                  # Opción 2
round(cov,1)

promedio = colMeans(var)        # Promedios de las variables
promedio

m.centrada <- t(t(var) - promedio) # Variables centradas
head(m.centrada)

vc.centrada = var(m.centrada)   # Matriz de varianza y covarianzas centrada
vc.centrada

v.propios= eigen(vc.centrada)   # Vectores y valores propios de m.centrada
v.propios
v.propios$vectors               # Extraer vectores propios


# 2. Proyección matricial (matriz rotada), usando a los vectores propios
m.centrada=as.matrix(m.centrada)        # Variables centradas como matriz
head(m.centrada)

# Matriz rotada
(m.Rotada <- m.centrada %*% v.propios$vectors)




#---------------
# 5) Figuras de la matriz proyectada "m.Rotada"

# Figura sin editar
plot(m.Rotada) 	# m.Rotada es la matriz rotada

# Figura editada 
plot(m.Rotada, asp = 2, pch = 19,        	# asp=2 son los círculos, pch= 19 es el relleno de los círculos
     xlab="Eje 1",ylab="Eje 2") 		# Rótulos de los ejes
text(m.Rotada, 				# Rótulos de los datos (estudiantes)
     labels = row.names(datos.t[,c(1:4)]), 
     pos = 3, cex=0.7)				# cex() relaciona el tamaño del texto
abline(h=0, col = "red")			# abline (h=0) línea horizontal del plano cartesiano
abline(v=0, col = "red")			# abline (v=0) línea vertical del plano cartesiano
grid()						# Grilla en la figura



# Comparar con el Análisis de Componentes Principales - pca

library(vegan)          # Librería requerida

# Datos del procedimiento 2.1) Sumas y norma de los vectores
head(datos)             # Variables y observaciones (estudiantes)
pca <- rda(datos)       # Realización del pca
x11()
biplot(pca)             # Figura del pca
abline(h=0, col = "blue")			# abline (h=0) línea horizontal del plano cartesiano
abline(v=0, col = "blue")			# abline (v=0) línea vertical del plano cartesiano
grid()			




#-----------------------
# 6) * Avanzados
# Figura con nombres de los estudiantes
datos2= read.csv2("datos1.csv")         # Base de datos original
datos2=na.omit(datos2)          # na.omit, para eliminar al estudiante 13
datos2
str(datos2)
head(datos2)    # Encabezado de la la base "datos2"
datos2$Nombre   # Valores de la columna "nombres"

m.Rotada        # Matriz rotada original
m.Rotada1 = as.data.frame (m.Rotada)    # Matriz rotada como data frame
m.Rotada1$nombres <-  datos2$Nombre     # Insertar nombres de los estudiantes
m.Rotada1
m.Rotada1 = as.matrix(m.Rotada1)        # M. rotada como matriz
rownames(m.Rotada1)= m.Rotada1[,5]      # Siglas para las operaciones

x11()
plot(m.Rotada1[,1:4], asp = 2, pch = 19,        # Edición de la figura
     xlab="Eje 1", ylab="Eje 2")
text(m.Rotada1, labels = row.names(m.Rotada1[,1:4]), 
     pos = 3, cex=0.7, col = "blue")
abline(h=0, col = "red")
abline(v=0, col = "red")
grid()



#---------------
# 7) Análisis de Componentes Principales - pca
library(vegan)
datos2= read.csv2("datos1.csv")  # Base de datos original
datos2=na.omit(datos2)           # na.omit, para eliminar al estudiante 13
datos2
head(datos2)            # Base de datos con nombres de los alumnos
colnames(datos2) <- c("Nombre","Sexo","LTot","Cint","LEsp","LBra")
head(datos2)            # Base de datos abreviada

pca <- rda(datos2[3:6]) # REalización del pca
summary(pca)            # Insumos del pca
rownames(datos2)= datos2[,1]      # Siglas para las operaciones

# Figura del pca
x11()
biplot(pca, type = c("text", "n"),scaling =2,     # Figura solo con variables morfométricas
       main="PCA - Scaling 2",cex=2)
text(pca, display="sites", cex=0.7,               # Graficas a los estudiantes
     col="blue", lwd=1.5, pos=3,
     labels = as.character(datos2$Nombre))        # Nombres de los estudiantes



#---------------
# 8) Figura en 3D de la matriz proyectada "m.Rotada"
library(plot3D)

m.Rotada        # Matriz rotada original
m.Rotada1 = as.data.frame (m.Rotada)    # Matriz rotada como data frame
m.Rotada1$nombres <-  datos2$Nombre     # Insertar nombres de los estudiantes
colnames(m.Rotada1) <- c("Eje1","Eje2","Eje3","Eje4","Nombre")
m.Rotada1

plot(m.Rotada1[, c(1, 2)], pch = 19, asp = 1)   # Figura con ejes 1 y 2
plot(m.Rotada1[, c(1, 3)], pch = 19, asp = 1)   # Figura con ejes 1 y 3
plot(m.Rotada1[, c(2, 3)], pch = 19, asp = 1)   # Figura con ejes 2 y 3

x11()
str(m.Rotada1)
scatter3D (m.Rotada1[, 1], m.Rotada1[, 2],      # Figura con ejes 1, 2 y 3
           m.Rotada1[, 3], pch = 19, asp = 2)



