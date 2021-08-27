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


############
#Predicción#
############

#Función predict
predict.lm(object, newdata, se.fit = FALSE, scale = NULL, df = Inf, 
           interval = c("none", "confidence", "prediction"), 
           level = 0.95, type = c("response", "terms"), 
           terms = NULL, na.action = na.pass, pred.var = res.var/weights, 
           weights = 1, ...) 

#Ajuste del modelo, Datos  longley
mod <- lm(Employed ~ Unemployed + Armed.Forces + Year, data=longley)

library(broom)
tidy(mod, quick=TRUE)

#Para construir un nuevo marco de datos con la información de las covariables, usando los mismos nombres y los mismos tipos de variables (cuali o cuanti) que en el conjunto de datos con el cual se entrenó el modelo.
nuevo <- data.frame(Year=c(1963, 1964),
                    Unemployed=c(420, 430),
                    Armed.Forces=c(270, 250))
nuevo

#para hacer la predicción 
predict(object=mod, newdata=nuevo)

#Intervalo de confianza con otro ejemplo (otros datos)
file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)
mod1 <- lm(Resistencia ~ Edad, data=datos)

#IC del 95% para E(y/x_0) cuando x_0 = 13.3625semanas
nuevo <- data.frame(Edad=13.3625)
predict(object=mod1, newdata=nuevo, interval="confidence", level=0.95)
#IC del 95% para y gorro cuando x_0 = 10 semanas
nuevo <- data.frame(Edad=10)
predict(object=mod1, newdata=nuevo, interval="prediction", level=0.95)

#Para obtener todos los IC  y_0 gorro y los vamos a almacenar en el objeto que luego luego vamos a agregar al marco de datos original.
future_y <- predict(object=mod1, interval="prediction", level=0.95)
nuevos_datos <- cbind(datos, future_y)

#se construye el diagrama de dispersión y se agrega la línea de regresión (en azul) y los IC para  E(y|x_0) (en rosado) por medio de geom_smooth. Los IC para y_0 gorro (en rojo) se agregan por medio de geom_line.
library(ggplot2)
ggplot(nuevos_datos, aes(x=Edad, y=Resistencia))+
  geom_point() +
  geom_line(aes(y=lwr), color="red", linetype="dashed") +
  geom_line(aes(y=upr), color="red", linetype="dashed") +
  geom_smooth(method=lm, formula=y~x, se=TRUE, level=0.95, col='blue', fill='pink2') +
  theme_light()


#########################
#Intervalos de confianza#
#########################

#Función confint
confint(object, parm, level = 0.95, ...)

#Ejemplo. Para cargar los datos y ajustar el modelo de interés.
file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)
mod1 <- lm(Resistencia ~ Edad, data=datos)

#construir un intervalo de confianza del 95% para el parámetro β_1
confint(object=mod1, parm="Edad", level=0.95)

#Funcion confint_sigma2
if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/model', force=TRUE)

#construir un intervalo de confianza del 95% para σ^2
library(model)
confint_sigma2(object=mod1, level=0.95)


####################################
#Modelos con variables cualitativas#
####################################


#############################
#Pruebas de hpótesis parte I#
#############################


###############################
#Pruebas de hipótesis parte II#
###############################


#####################
#Diagnóstico parte I#
#####################
























