#########################
#Regresión Lineal Simple#
#########################

#Función lm

lm(formula, data, subset, weights, na.action,
   method = "qr", model = TRUE, x = FALSE, y = FALSE, qr = TRUE,
   singular.ok = TRUE, contrasts = NULL, offset, ...)

file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)
head(datos) 

#Para crear un diagrama de dispersión
library(ggplot2)
ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() + theme_light()

#Para obtener las estimaciones de los parámetros
mod1 <- lm(Resistencia ~ Edad, data=datos)
mod1

#Para obtener una tabla de resumen con detalles del modelo ajustado
summary(mod1)

#Para incluir la recta de regresión que representa el modelo ajustado anterior
ggplot(datos, aes(x=Edad, y=Resistencia)) + 
  geom_point() +
  geom_smooth(method='lm', formula=y~x, se=FALSE, col='dodgerblue1') +
  theme_light()

#Dentro de todo objeto de la clase lm hay doce elementos o valores que se pueden utilizar. 
class(mod1)
names(mod1)
mod1$coefficients #extrae los coeficientes estimados
mod1$fitted.values #extrea los valores predichos o ajustados, y_i gorro. También se puede hacer con la función fitted()
mod1$residuals #extrae los residuales usuales, e_i = Y_i - Y_i gorro. También se puede hacer con la función residuals()

#crear un diagrama de dipersion con los puntos originales, las estimaciones y los residuos.
datos$predicciones <- predict(mod1)
ggplot(datos, aes(x=Edad, y=Resistencia)) +
  geom_smooth(method="lm", se=FALSE, color="lightgrey") +
  geom_segment(aes(xend=Edad, yend=predicciones), col='red', lty='dashed') +
  geom_point() +
  geom_point(aes(y=predicciones), col='red') +
  theme_light()

#Mínimos cuadrados ponderados
#Ejemplo:
x <- c(4, 6, 8, 7, 8, 5)
y <- c(1, 2, 3, 4, 5, 4)
w <- c(0.1, 0.1, 0.2, 0.1, 0.2, 0.9) #pesos

mod_sin_pesos <- lm(y ~ x)
mod_con_pesos <- lm(y ~ x, weights=w)

coef(mod_sin_pesos)
coef(mod_con_pesos)

symbols(x=x, y=y, circles=w, pch=20, las=1, inches=0.1, fg='red', lwd=2)
abline(mod_sin_pesos, col='seagreen')
abline(mod_con_pesos, col='dodgerblue1')
legend('topleft', legend=c('Sin pesos', 'Con pesos'), 
       col=c('seagreen', 'dodgerblue1'), bty='n', lwd=1) #diagrama de dispersión usando la función symbols. La información de la importancia de cada observación se incluye en el dibujo usando circles=w. Finalmente se agregan las rectas de los dos modelos ajustados.


###########################
#Regresión lineal múltiple#
###########################

#datos
library(MPV)
colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')
head(softdrink)

#Gráfico en 3D para explorar la relación entre las variables
library(scatterplot3d)
attach(softdrink)
scatterplot3d(x=cantidad, y=distancia, z=tiempo, pch=16, cex.lab=1,
              highlight.3d=TRUE, type="h", xlab='Cantidad de cajas',
              ylab='Distancia (pies)', zlab='Tiempo (min)')

#Otra forma: El lector puede jugar con el diagrama, puede moverlo, girarlo, acercarse y muchas cosas más
library(plotly)
plot_ly(x=cantidad, y=distancia, z=tiempo, type="scatter3d", color=tiempo) %>% 
  layout(scene = list(xaxis = list(title = 'Cantidad'),
                      yaxis = list(title = 'Distancia (pies)'),
                      zaxis = list(title = 'Tiempo (min)')))

#Otra forma
library(rgl)
plot3d(x=cantidad, y=distancia, z=tiempo, type='s', col='pink',
       xlab='Cantidad',
       ylab='Distancia (pies)',
       zlab='Tiempo (min)')

#Ajuste del modelo
mod <- lm(tiempo ~ cantidad + distancia, data=softdrink)
summary(mod)

#Para incluir el plano de regresión que representa el modelo ajustado
mi_3d <- scatterplot3d(x=cantidad, y=distancia, z=tiempo, pch=16, cex.lab=1,
                       highlight.3d=TRUE, type="h", xlab='Cantidad de cajas',
                       ylab='Distancia (pies)', zlab='Tiempo (min)') #Se crea el grafico 3d y se guarda en un objeto, por ejemplo mi_3d
mi_3d$plane3d(mod, lty.box="solid", col='mediumblue') #Para agregar el plano usamos $plane3d( ) con argumento modelo ajustado

