#(1)

#Instalamos los paquetes necesarios:
install.packages(c('XLConnect','corrplot','GGally','ggplot2','tidyverse','gridExtra'))
install.packages("psych",repos="https://personality-project.org/r",type="source")

#Importamos las librerías:
install.packages('tidyverse')
library(XLConnect)
library(corrplot)
library(GGally)
library(ggplot2)
library(tidyverse)
library(psych)
library(gridExtra)
library(olsrr)

#(2)

#Summary: Entrega el resumen de los resultados al aplicar varios ajustes
#de modelos, dependiendo de la clase del argumento entregado.
summary(A6)

#View: Abre una pestaña nueva mostrando el argumento de la función. También lo
#muestra en la consola.
View(A6)

#Pairs: Retorna una matriz de gráficos de dispersión de las variables del
#dataframe entregado como argumento.
pairs(A6) #A6[,c(x,y,z)] para analizar columnas seleccionadas

#(3)

#Regresión lineal

model <- lm(AMI ~ GEN + AMT + PR + DIAP + QRS, data = A6)
#también se puede plantear como lm(AMI ~., data = A6)

summary(model)

#Resultados: los parámetros que no cumplen con el nivel de confianza de 95%
#son PR, DIAP y QRS. El coeficiente de determinación es de 0.8202 y el
#coeficiente de determinación ajustado es de 0.8764. A simple vista, el ajuste
#lineal no es muy bueno, por lo que puede ser útil eliminar el parámetro QRS.

#(4)

#Usando p-value = 1-IC = 0.05, eliminamos variable QRS, pues es la que tiene el
#valor más alto para p-value

model2 <- lm(AMI ~ GEN + AMT + PR + DIAP, data = A6)
#también se puede plantear como lm(AMI ~. -QRS, data = A6)

summary(model2)

#Cambios: Los parámetros PR y DIAP siguen teniendo un p-value mayor a 0.05, 
#por otro lado el RSS ha aumentado, mientras que el R^2 y el R^2 ajustado han
#disminuido.

#Usando p-value = 1-IC = 0.05, eliminamos variable DIAP
model3 <- lm(AMI ~. -QRS -DIAP, data = A6)

summary(model3)

#Cambios: El parámetro PR sigue teniendo un p-value mayor a 0.05, 
#por otro lado el RSS ha aumentado, mientras que el R^2 y el R^2 ajustado han
#disminuido.

#Usando p-value = 1-IC = 0.05, eliminamos variable PR
model4 <- lm(AMI ~. -QRS -DIAP -PR, data = A6)

summary(model4)

#Cambios: Todos lo parámetros cumplen con tener un p-value menor a 0.05.
#Por otro lado, el RSS ha aumentado, mientras que el R^2 y el R^2 ajustado han
#disminuido.

#(5)

#El R-cuadrado ajustado aumenta cuando el nuevo término mejora el modelo 
#más de lo esperado por casualidad. Disminuye cuando un predictor mejora
#el modelo menos de lo esperado. Por lo que analizando el R^2 ajustado,
#se puede notar que el modelo va empeorando, ya que el R^2 ajustado va
#disminuyendo en cada iteración. Si bien no se espera que el valor sea cercano
#a 1, pues la regresión lineal debe representar la tendencia de los datos 
#experimentales y no estos mismos. Por otro lado, la amitriptilina es un 
#antidepresivo que no parece tener relación con el corazón, por lo que 
#puede tener sentido eliminar los predictores asociados a este tema (PR, DIAP y
#QRS).

#(6)

#Relación lineal entre los predictores y la variable

#Modelo 1:

plot1 <- ggplot(data = A6, aes(GEN, model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  labs(y = 'Residuos del modelo') + theme_bw()
plot2 <- ggplot(data = A6, aes(AMT, model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  labs(y = 'Residuos del modelo') + theme_bw()
plot3 <- ggplot(data = A6, aes(PR, model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  labs(y = 'Residuos del modelo') + theme_bw()
plot4 <- ggplot(data = A6, aes(DIAP, model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  labs(y = 'Residuos del modelo') + theme_bw()
plot5 <- ggplot(data = A6, aes(QRS, model$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  labs(y = 'Residuos del modelo') + theme_bw()
grid.arrange(plot1, plot2, plot3, plot4,plot5)

#Modelo definitivo (model4)

plot1 <- ggplot(data = A6[1:2], aes(GEN, model4$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  labs(y = 'Residuos del modelo') + theme_bw() 
plot2 <- ggplot(data = A6[1:2], aes(AMT, model4$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  labs(y = 'Residuos del modelo') +  theme_bw()
grid.arrange(plot1, plot2)

#Histograma

multi.hist(x = A6[1:5], dcol = c("blue", "red"), dlty = c("dotted", "solid"))
#default: main es el nombre de la variable del df

#(7)

#Dsitribución normal de los residuos (análisis de homocedasticidad)

#Modelo 1
qqnorm(model$residuals)
qqline(model$residuals)

#Modelo 4 (definitivo)
qqnorm(model4$residuals)
qqline(model4$residuals)

#Shapiro test

shapiro.test(model$residuals) #Modelo 1
shapiro.test(model2$residuals) #Modelo 2
shapiro.test(model3$residuals) #Modelo 3
shapiro.test(model4$residuals) #Modelo 4 (definitivo)
#-> Modelo 2 menor p-value: 0.3611


#Variabilidad constante de los residuos (homocedasticidad):

#Al representar los residuos frente a los valores ajustados por el modelo,
#los primeros se tienen que distribuir de forma aleatoria en torno a cero,
#manteniendo aproximadamente la misma variabilidad a lo largo del eje X. 
#Si se observa algún patrón específico, por ejemplo forma cónica o mayor 
#dispersión en los extremos, significa que la variabilidad es dependiente 
#del valor ajustado y por lo tanto no hay homocedasticidad.

#Modelo 1:
ggplot(data = A6, aes(model$fitted.values, model$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores ajustados por el modelo 1', y = 'Residuos del modelo 1') +
  theme_bw()

#Modelo 4 (definitivo):
ggplot(data = A6, aes(model4$fitted.values, model4$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  labs(x = 'Valores ajustados por el modelo 4', y = 'Residuos del modelo 4') +
  theme_bw()

#Cook's distance

#Para detectar observaciones que influenciaron mucho los valores ajustados por
#el modelo

ols_plot_cooksd_chart(model)
ols_plot_cooksd_chart(model2)
ols_plot_cooksd_chart(model3)
ols_plot_cooksd_chart(model4)
  
ols_plot_cooksd_bar(model)
ols_plot_cooksd_bar(model2)
ols_plot_cooksd_bar(model3)
ols_plot_cooksd_bar(model4)

#Correlaciones

#Diagramas de dispersión, valores de correlación para cada par de variables
#y la distribución de cada una de las variables

corrplot(cor(dplyr::select(A6, GEN, AMT, PR, DIAP, QRS)),
         method = "number", tl.col = "black")

#Para ver las distribuciones de las variables (exponencial, lineal, etc)
#y analizar cuáles variables tienen una alta correlación, pues no es útil
#introducir predictores con una alta correlación.

ggpairs(A6[1:5], lower = list(continuous = "smooth"),
        diag = list(continuous = "barDiag"), axisLabels = "none")

#(8)

#Transformación logarítmica

A6$QRS <- log(A6$QRS)
#Revertir: A6$QRS <- exp(A6$QRS)

#Producto de variables

A6$QRS <- A6$GEN*A6$PR

#También podemos crear una función y asignársela a una variable del df.