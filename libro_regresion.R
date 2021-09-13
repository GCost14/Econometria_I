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

##Función summary cuando  β_j0 = 0
file <- "https://raw.githubusercontent.com/fhernanb/datos/master/propelente"
datos <- read.table(file=file, header=TRUE)
mod <- lm(Resistencia ~ Edad, data=datos)
summary(mod)

##Función beta_test cuando  β_k0 ≠ 0
#paquete model (se encuentra en github)
if (!require('devtools')) install.packages('devtools')
devtools::install_github('fhernanb/model', force=TRUE) 

#Función beta_test
beta_test(object, alternative = c("two.sided", "less", "greater"), parm, ref.value) 

#Ejemplo
library(model)
beta_test(object=mod, parm='(Intercept)', ref.value=2700, alternative='two.sided')



###############################
#Pruebas de hipótesis parte II#
###############################

##Prueba sobre todos los coeficientes - prueba de significancia de la regresión.
#Ajuste del modelo
require(MPV)
colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')
mod <- lm(tiempo ~ cantidad + distancia, data=softdrink, x=TRUE)

#Para calcular los elementos de la tabla ANOVA
y <- softdrink$tiempo
n <- length(y)
ss_t <- sum(y^2) - sum(y)^2 / n
ss_r <- matrix(coef(mod), nrow=1) %*% t(mod$x) %*% matrix(y, ncol=1) - sum(y)^2 / n
ss_res <- ss_t - ss_r
ms_r <- ss_r / (length(coef(mod))-1)
ms_res <- ss_res / (n-length(coef(mod)))
F0 <- ms_r / ms_res
valorP <- pf(F0, df1=length(coef(mod))-1, df2=(n-length(coef(mod))), lower.tail=FALSE)
tabla <- matrix(NA, ncol=5, nrow=3)
tabla[1, ] <- c(ss_r, length(coef(mod))-1, ms_r, F0, valorP)
tabla[2, 1:3] <- c(ss_res, n-length(coef(mod)), ms_res)
tabla[3, 1:2] <- c(ss_t, n-1)
colnames(tabla) <- c('Suma Cuadrados', 'gl', 'Cuadrado medio', 'F0', 'Valor-P')
rownames(tabla) <- c('Reg', 'Resid', 'Total')
tabla

#Otra forma de aplicar la prueba de significancia de la regresión es usando la función summary la cual nos entrega una parte de la tabla anova anterior (no toda la tabla anova).
summary(mod)


##Prueba para comparar modelos anidados#########################################
#Datos y ajuste de los dos modelos de interes
require(MPV)
data(table.b4) 
head(table.b4, n=4)
redu_mod <- lm(y ~ x1 + x2, data=table.b4, x=TRUE)
comp_mod <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4, x=TRUE)

#Para construir el estadístico F_0
n <- 24 # numero de observaciones
p0 <- 3 # numero de betas en modelo reducido
p1 <- 5 # numero de betas en modelo completo

ssr_reduced  <- sum(table.b4$y) - sum(redu_mod$residuals^2)
ssr_complete <- sum(table.b4$y) - sum(comp_mod$residuals^2)
ms_res <- summary(comp_mod)$sigma^2
F0 <- ((ssr_complete - ssr_reduced) / (p1-p0)) / ms_res
F0

#Para calcular el parámetro de no centralidad λ
beta2 <- matrix(c(0.3233333, -0.2176622), ncol=1)
x1 <- comp_mod$x[, 1:3]
x2 <- comp_mod$x[, 4:5]
a1 <- t(beta2) %*% t(x2)
a2 <- diag(n) - x1 %*% solve(t(x1) %*% x1) %*% t(x1)
a3 <- x2 %*% beta2
lambda <- (a1 %*% a2 %*% a3) / summary(comp_mod)$sigma^2
lambda

#Para calcular el valor-P de la prueba usando la distribución F no central y la distribución F (usual)
pf(q=F0, df1=p1-p0, df2=n-p1, ncp=lambda, lower.tail=FALSE)
pf(q=F0, df1=p1-p0, df2=n-p1, lower.tail=FALSE)


##Función anova#################################################################
anova(object, test, scale=0)

#Para crear y comparar los modelos sin covariables y el modelo con solo Horsepower.
library(MASS)
mod0 <- lm(Price ~ 1, data=Cars93)
mod1 <- lm(Price ~ Horsepower, data=Cars93)
anova(mod0, mod1)

#Para crear y comparar el modelo con sólo Horsepower con el modelo con Horsepower y Type.
mod2 <- lm(Price ~ Horsepower + Type, data=Cars93)
anova(mod1, mod2)

#Para crear y comparar el modelo con Horsepower y Type con el modelo con las tres covariables.
mod3 <- lm(Price ~ Horsepower + Type + Weight, data=Cars93)
anova(mod2, mod3)

#segunda parte del ejemplo: se usará la función anova directamente sobre el modelo completo mod3
anova(mod3)

#Otro ejemplo con otros datos
require(MPV)
data(table.b4) 
head(table.b4, n=4)
mod1 <- lm(y ~ x1 + x2, data=table.b4)
mod2 <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)
anova(mod1, mod2, test='F')


##summary versus anova##########################################################
#código para simular los datos
set.seed(8867)  # this makes the example exactly reproducible
y <- c(rnorm(10, mean=0,    sd=1),
       rnorm(10, mean=-0.5, sd=1),
       rnorm(10, mean=0.5,  sd=1))
g <- rep(c("A", "B", "C"), each=10)

#Ajuste del modelo
model <- lm(y ~ g)

summary(model)
anova(model)
#Cuando se quiera explorar el efecto de una variable cualitativa en un modelo es mejor usar la función anova que los resultados del summary.

##Prueba razon de verosimilitud#################################################
#Datos y ajuste de ambos modelos
require(MPV)
data(table.b4) 
head(table.b4, n=4)
mod0 <- lm(y ~ x1 + x2, data=table.b4)
mod1 <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)

#Para aplicar la prueba razón de verosimilitud. Los grados de libertad en esta prueba son 2.
lambda <- -2 * (logLik(mod0) - logLik(mod1))
lambda

pchisq(q=lambda, df=2, lower.tail=FALSE)


##Comparaciones múltiples#######################################################
#Ajuste del modelo
require(MPV)
data(table.b4) 
mod <- lm(y ~ x1 + x2 + x3 + x4, data=table.b4)
coef(mod)

#Para la prueba simultánea de las hipótesis
library(multcomp)
C <- matrix(c(0, 0, 0, 1, 0,
              0, 0, 0, 0, 1), ncol=5, byrow=TRUE)
mult_test <- glht(model=mod, linfct=C, alternative='two.sided', rhs=c(0, 0))
summary(mult_test, test = adjusted(type="single-step"))



#####################
#Diagnóstico parte I#
#####################

##Residuales####################################################################
#Para obtener los residuales 
residuals(object, type=c("working", "response", "deviance", "pearson", "partial"))
rstandard(object)
rstudent(object)

#Ejemplo
x <- c(4, 6, 8, 7, 8, 5)
y <- c(1, 2, 3, 4, 5, 4)
w <- c(0.1, 0.1, 0.2, 0.1, 0.2, 0.9)

mod <- lm(y ~ x, weights=w)

ei <- y - fitted(mod)
pi <- ei * sqrt(mod$weights)
hii <- lm.influence(mod)$hat
di <- ei * sqrt(mod$weights) / sqrt(summary(mod)$sigma^2 * (1-hii))
ri <- ei * sqrt(mod$weights) / sqrt(lm.influence(mod)$sigma^2 * (1-hii))

cbind(ei=ei, pi=pi, di=di, ri=ri)

cbind(ei=residuals(mod, type='working'),
      pi=residuals(mod, type='pearson'),
      di=rstandard(mod),
      ri=rstudent(mod)) #calculo de los resudiales usando las funciones de R


##Chequeando normalidad de los errores##########################################
##Chequeando si errores con media cero
##Chequeando si los errores tiene varianza constante
##Chequeando si errores no están correlacionados

#Para simular los datos y ajustar el modelo.
gen_dat <- function(n) {
  varianza <- 16
  x <- runif(n=n, min=-5, max=6)
  media <- 4 - 6 * x
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}
datos <- gen_dat(n=500)
mod <- lm(y ~ x, data=datos)

#Para los gráficos de residuales
par(mfrow=c(2, 2))
plot(mod, las=1, col='deepskyblue4', which=1:3)


##Ejemplo violando los supuestos################################################
gen_dat <- function(n) {
  varianza <- 16
  x <- runif(n=n, min=-5, max=6)
  media <- 4 - 6 * x + 2 * x^2
  y <- rnorm(n=n, mean=media, sd=sqrt(varianza))
  marco_datos <- data.frame(y=y, x=x)
  return(marco_datos)
}
datos <- gen_dat(n=500)
mod <- lm(y ~ x, data=datos)
par(mfrow=c(2, 2))
plot(mod, las=1, col='darkseagreen3', which=1:3)


##Gráficos de residuales usando car#############################################
ibrary(car)
prestige_mod1 <- lm(prestige ~ education + income + type, data=Prestige)

##Para construir los gráficos de los residuos de Pearson versus cada término del predictor lineal y los valores ajustados  μ_i gorro
#(las=1 para poner los números vertical en el eje Y).
residualPlots(prestige_mod1, las=1)

##Para dibujar los gráficos marginales de  y_i versus cada covariable cuantitativa y los valores ajustados μ_i gorro
marginalModelPlots(prestige_mod1, las=1)
#sería bueno agregar un término I(income^2)
prestige_mod2 <- lm (prestige ~ education + income + I(income^2) + type, data=Prestige)
residualPlots(prestige_mod2, las=1)

anova(prestige_mod1, prestige_mod2)


######################
#Diagnóstico parte II#
######################
##Extrapolación oculta##########################################################
y <- c(2, 3, 6, 5)
x <- c(3, 5, 6, 7)
z <- c(5, 4, 6, 3)

#tres formas para obtener los valores hii hii
# Forma 1
X <- cbind(1, x, z)
H <- X %*% solve(t(X) %*% X) %*% t(X)
H
diag(H)
# Forma 2
mod <- lm(y ~ x + z)
hatvalues(mod)
# Forma 3
lm.influence(mod)$hat


##Prueba de Bonferroni para detectar outliers###################################
outlierTest(model, cutoff=0.05, n.max=10, order=TRUE, labels=names(rstudent), ...) #del paquete car

#Ejemplo
url <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(file=url, sep="\t", header=TRUE)

mod <- lm(Peso ~ Estatura + circun_cuello + circun_muneca, data=datos)

library(car)
outlierTest(mod, cutoff=Inf, n.max=4)

influenceIndexPlot(mod, vars="Bonf", las=1)


##Distancia de Cook#############################################################
url <- "https://raw.githubusercontent.com/fhernanb/datos/master/medidas_cuerpo2"
datos <- read.table(file=url, sep="\t", header=TRUE)
head(datos, n=5)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...) {
  usr <- par("usr"); on.exit(par(usr))
  par(usr=c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex=cex.cor * r, col=gray(1-r))
}

# Creamos el grafico SOLO para las variables cuantitativas
pairs(datos[, c("Peso", "Estatura", "circun_cuello", "circun_muneca")], 
      pch=19, las=1,
      upper.panel=panel.smooth, lower.panel=panel.cor)

mod1 <- lm(Peso ~ Estatura + circun_cuello + circun_muneca, data=datos)
summary(mod1)

mod2 <- mixlm::backward(mod1, alpha=0.04) #Vamos a realizar una selección de variables de manera que sólo queden variables significativas con un α=0.04 
summary(mod2)

# Para construir el grafico de dispersion
with(datos, 
     plot(x=circun_cuello, y=Peso, pch=19, las=1,
          xlab="Circunferencia cuello (cm)", ylab="Peso (Kg)"))
# Ahora agregamos la linea de tendencia
abline(mod2, lwd=3, col='blue2')
# por ultimo un texto con la ecuacion o modelo ajustado
text(x=34, y=95, expression(hat(Peso) == -44.61 + 3.10 * C.cuello), 
     col='blue3' )

cooks.distance(mod2) #Vamos a calcular las distancias de Cook para las observaciones del modelo 2
cutoff <- 4 / (26-2-2)  # Cota
plot(mod2, which=4, cook.levels=cutoff, las=1)
abline(h=cutoff, lty="dashed", col="dodgerblue2")

library(car)
influenceIndexPlot(mod2, vars="Cook") #otra forma de dibujar las distancias de Cook

#Ahora vamos a revisar los residuales del modelo 2
par(mfrow=c(2, 2))
plot(mod2, col='deepskyblue4', pch=19)


##DFFITS y DFBETAS##############################################################
x <- c(2, 5, 3, 4, 7)
y <- c(5, 9, 8, 7, 19)
plot(x=x, y=y, pch=19, col="tomato", las=1)
mod <- lm(y ~ x)

dffits(mod) #Vamos a calcular DFFITS

dfbetas(mod) #Vamos a calcular DFBETAS
dfbeta(mod) #otra función 


##Estadística PRESS y R^2 de predicción#########################################
PRESS <- function(linear.model) {
  # calculate the predictive residuals
  pr <- residuals(linear.model) / (1-lm.influence(linear.model)$hat)
  # calculate the PRESS
  PRESS <- sum(pr^2)
  return(PRESS)
}

pred_r_squared <- function(linear.model) {
  #' Use anova() to get the sum of squares for the linear model
  lm.anova <- anova(linear.model)
  #' Calculate the total sum of squares
  tss <- sum(lm.anova$'Sum Sq')
  # Calculate the predictive R^2
  pred.r.squared <- 1-PRESS(linear.model)/(tss)
  return(pred.r.squared)
}

#ejemplo
require(MPV)
colnames(softdrink) <- c('tiempo', 'cantidad', 'distancia')
mod <- lm(tiempo ~ cantidad + distancia, data=softdrink)

PRESS(mod)
pred_r_squared(mod)


#############################
#Pruebas de Homocedasticidad#
#############################
#H0:  los errores tienen varianza constante.
#H1:  los errores no tienen varianza constante.

##Breusch-Pagan Test############################################################
#simulacion  de los datos
gen_data <- function(n) {
  x1 <- rpois(n, lambda=5)
  x2 <- rbinom(n, size=6, prob=0.4)
  ei <- rnorm(n=n, sd=x2)
  y <- -3 + 2 * x1 + 4 * x2 + ei
  data.frame(y, x1, x2)
}

n <- 200
datos <- gen_data(n=n)
mod <- lm(y ~ x1 + x2, data=datos) # Modelo de interes

#Vamos a aplicar la prueba de forma manual
ei <- resid(mod)
fit <- lm(ei^2 ~ x1 + x2, data=datos) # Modelando ei^2 ~ x1 + x2
R2 <- summary(fit)$r.squared
k <- 2
estadistico <- n * R2
valorP <- pchisq(q=estadistico, df=k, lower.tail=FALSE)
cbind(estadistico, valorP)

#Vamos a aplicar la prueba de forma automática con la función bptest.
library(lmtest)
bptest(mod)


##White test####################################################################
bptest(mod, varformula = ~ x1 * x2 + I(x1^2) + I(x2^2), data=datos)

#de forma manual
fit <- lm(resid(mod)^2 ~ x1 + x2 + x1 * x2 + I(x1^2) + I(x2^2), data=datos) 
R2 <- summary(fit)$r.squared
estadistico <- n * R2
valorP <- pchisq(q=estadistico, df=5, lower.tail=FALSE)
cbind(estadistico, valorP)


##Score test for nonconstant error variance#####################################
library(car)
ncvTest(mod)

##Goldfeld-Quandt Test
#Este test está implementado en la función gqtest del paquete lmtest.

##Harrison-McCabe test
#Este test está implementado en la función hmctest del paquete lmtest.


#########################################
#Pruebas de independencia de los errores#
#########################################
#H0:  los errores son independientes.
#H1:  los errores no son independientes.

##Durbin-Watson test############################################################
# generate regressor
x <- rep(c(-1, 1), 50)
# generate the AR(1) error terms with parameter rho = 0 (white noise)
err1 <- rnorm(100)
# generate dependent variable
y1 <- 1 + x + err1
library(lmtest)
mod1 <- lm(y1 ~ x)
dwtest(mod1) # perform Durbin-Watson test

plot(residuals(mod1), pch=19, col="deepskyblue1")

# generate the AR(1) error terms with parameter rho = 0.9 respectively
err2 <- stats::filter(x=err1, filter=0.9, method="recursive")
# generate dependent variable
y2 <- 1 + x + err2

mod2 <- lm(y2 ~ x)
dwtest(mod2) # perform Durbin-Watson test

plot(residuals(mod2), pch=19, col="tomato")


##Breusch-Godfrey Test##########################################################
library(lmtest)
mod1 <- lm(y1 ~ x)
bgtest(mod1) ## perform Durbin-Watson test

mod2 <- lm(y2 ~ x)
bgtest(mod2) ## perform Durbin-Watson test


######################
#Modelos polinomiales#
######################


########################
#Selección de variables#
########################

##Akaike Information Criterion (AIC)############################################
#Funciones logLik y AIC

#Función stepAIC################################################################
#Ejemplo

#Aplicación del método backward

#Aplicación del método forward

#Aplicación del método both

#Comparando  R^2_Adj

#Comparando  σ gorro

#Comparando los residuales

##Funciones addterm y dropterm##################################################
#Ejemplo

##Paquete olsrr#################################################################
#Ejemplo

##Paquete mixlm#################################################################
#Ejemplo

##Paquete leaps#################################################################
#ejemplo


###################
#Multicolinealidad#
###################

##Multicolinealidad inducida####################################################


##Matriz de correlaciones#######################################################


##Factor de Inflación de la Varianza (VIF)######################################






















































































