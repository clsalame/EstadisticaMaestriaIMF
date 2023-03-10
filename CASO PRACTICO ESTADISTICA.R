library(ggplot2)
library(plotly)
library(corrplot)
library(dplyr)
library(summarytools)
library(knitr)
library(corrplot)
library(grDevices)
library(Hmisc)
library(psych)
library(xtable)
install.packages("xtable")


#Carga de dataframe
heart <- read.csv("C:/Users/clsal/Downloads/heart.csv")

#An?lisis descriptivo
str(heart)
descr(heart)
summary(heart) 

#Limpieza de datos
#Limpio los ceros para obtener mejor la media del colesterol
heart2=filter(heart, heart$Cholesterol>0)
str(heart2)
descr(heart2)
summary(heart2) 
dim(heart2)

#Tablas
#Tabla de sexos
Tablasexo=table(heart2$Sex)

kable(Tablasexo, caption = "Tabla 1. Cantidad de hombres y mujeres",
      align = 'c', digits = round(2),
      col.names = c("Sexo","Cantidad"))

#Tabla de ECV por sexo
Tablaecvsexo=table(heart2$Sex, heart2$HeartDisease)
kable(Tablaecvsexo, caption = "Tabla 2. Cantidad de ECV hombres y mujeres",
      align = 'c', digits = round(2),
      col.names = c("No padece","Si padece"))

#Histograma de edades
hist(heart2$Age,col="yellow", xlab="Edad", ylab="Frecuencia", main="Histograma de edades")
heartedadecv=filter(heart2, heart2$HeartDisease>0)
heartedadecv
hist(heartedadecv$Age,col="orange", xlab="Edad", ylab="Frecuencia", main="Histograma de edades de pacientes con ECV", ylim =c(0,100))

#Tabla de medias
Mediaritmo=tapply(heart2$MaxHR, heart2$HeartDisease, mean)
kable(Mediaritmo, caption = "Tabla 3. Media de ritmo cardiaco de pacientes vs. si padecen o no",
      align = 'c', digits = round(2),
      col.names = c("Media de Ritmo cardiaco"))


mediaColesterol=tapply(heart2$Cholesterol, heart2$HeartDisease, mean)
kable(mediaColesterol, caption = "Tabla 4. Media de colesterol de pacientes vs. si padecen o no",
      align = 'c', digits = round(2),
      col.names = c("Media de Colesterol"))

mediaPS=tapply(heart2$RestingBP, heart2$HeartDisease, mean)
kable(mediaPS, caption = "Tabla 5. Media de presi?n sangu?nea de pacientes vs. si padecen o no",
      align = 'c', digits = round(2),
      col.names = c("Media de Presi?n sangu?nea"))

#Gr?fica de cajas de Colesterol
boxplot(formula = heart2$Cholesterol ~ heart2$HeartDisease,xlab="Padece enfermedad", ylab="Nivel de colesterol",main="Gr?fica de cajas", col=c("light yellow","light blue"))
abline(h = 239, col = "red", lwd = 1)   

#Realci?n con az?car elevada y padecimiento de enfermedad
mediaAzucar=tapply(heart2$FastingBS, heart2$HeartDisease, sum)
kable(mediaAzucar, caption = "Tabla 6. Cantidad de personas con Azucar alta que padecen o no ECV",
      align = 'c', digits = round(2),
      col.names = c("Pacientes con az?car elevada"))
barplot(mediaAzucar,xlab="Padece enfermedad", ylab="Pacientes con az?car elevada",main="Gr?fica de barras de relaci?n entre az?car elevada y ECV")

#Gr?fica de cajas de pulso card?aco
boxplot(formula = heart2$MaxHR ~ heart2$HeartDisease,xlab="Padece enfermedad", ylab="Pulso card?aco",main="Gr?fica de cajas",col=c("light yellow","light blue"))
abline(h = 130, col = "red", lwd = 1)  

#Gr?fica de cajas de presi?n sangu?nea
boxplot(formula = heart2$RestingBP ~ heart2$HeartDisease,xlab="Padece enfermedad", ylab="Presi?n sangu?nea",main="Gr?fica de cajas",col=c("light yellow","light blue"))
abline(h = 130, col = "red", lwd = 1)   

#An?lisis de ECG
ECGr=tapply(heart2$HeartDisease, heart2$RestingECG, sum)
kable(ECGr, caption = "Tabla 7. Cantidad de personas con Azucar alta que padecen o no ECV",
      align = 'c', digits = round(2),
      col.names = c("Pacientes con Enfermedad cardiovascular con resultado ECG"))
barplot(ECGr,xlab="Resultado ECG", ylab="Pacientes con enfermedad",main="Gr?fica de barras de relaci?n entre resultado ECG y ECV", ylim=c(0,200),col=c("light yellow","light blue", "pink"))
ECGrpor=round(prop.table(ECGr)*100,2)
ECGrpor

Frececg=table(heart2$RestingECG, heart2$HeartDisease)
Frececg
Frececgp=round(prop.table(table(heart2$RestingECG, heart2$HeartDisease))*100, 2)

kable(Frececgp, caption = "Tabla 8. Frecuencia relativa de ECG vs. pacientes con enfermedad cardiovascular o no",
      align = 'c', digits = round(2),
      col.names = c("Sin enfermedad","Con enfermedad"))
Frececgp
plot(Frececgp, color=c("red","blue"),main="Gr?fico de mosaico Frecuencia relativa de ECG vs. ECV", xlab="Resultado ECG", ylab="ECV")

#Selecci?n de columnas num?ricas
nume=heart2[, c(1,4,5,6,8,10,12)]

#Realizar correlaci?n de variables
cor(nume)

rcorr(as.matrix(nume))

corPlot(nume, cex = 1.2, main = "Matriz de correlaci?n")
corrplot(cor(nume), type="upper", main="Correlaci?n c?rculo")

#Regresi?n de variables con mayor correlaci?n
#Correlaci?n entre Enfermedad y Oldpeak
regresion1 <- lm(heart2$HeartDisease ~ heart2$Oldpeak, data = heart2)
summary(regresion1)
plot(heart2$HeartDisease, heart2$Oldpeak, xlab='Heart Disease', ylab='Oldpeak',main = "ECV vs. Oldpeak")

#Correlacio?n entre HR y Edad
regresion2 <- lm(heart2$MaxHR ~ heart2$Age, data = heart2)
summary(regresion1)
plot(heart2$Age, heart2$MaxHR, xlab='Edad', ylab='Pulso',main = "Edad vs. Pulso card?aco")
abline(regresion2, col="red")

#Correlaci?n entre Enfermedad y HR
regresion3 <- lm(heart2$HeartDisease ~ heart2$MaxHR, data = heart2)
summary(regresion3)
plot(heart2$HeartDisease, heart2$MaxHR, xlab='Heart Disease', ylab='Pulso', main = "ECV vs. Pulso card?aco")

#Correlaci?n entre Colesterol y Presi?n
regresion4 <- lm(heart2$RestingBP ~ heart2$Cholesterol, data = heart2)
summary(regresion4)
plot(heart2$Cholesterol, heart2$RestingBP, xlab='Colesterol', ylab='Presi?n', main = "Colesterol vs. Presi?n")
abline(regresion4, col="red")

#Tabla de frecuencias por edades
TablaFrec <- freq(heart2$Age)
TablaFrec
print(xtable(TablaFrec, caption = "Pregunta 11"), type = "html",
      file = "tabla_p11.html")

#Mediana de edad
median(heart$Age)

#Grupos de edad
heart2$Edadrango=ifelse(heart2$Age < 55, "Menor a 55", "Igual o mayor a 55")
heart2
Frecee=table(heart2$Edadrango,heart2$HeartDisease)
Frecee
#Tabla de frecuencias por grupo de edad
TablaFrecrango <- freq(heart2$Edadrango)
TablaFrecrango

#Tabla de frecuencia por Sexo
TablaFrec1 <- freq(heart2$Sex)
TablaFrec1
print(xtable(TablaFrec1, caption = "Pregunta 11"), type = "html",
      file = "tabla_p12.html")

#Tabla de frecuencias absoluta y relativa
Frec=table(heart2$Sex, heart2$HeartDisease)
Frec
Frecp=round( prop.table(table(heart2$Sex, heart2$HeartDisease))*100, 2)
Frecp

#Test de hip?tesis
x1=filter(heart2, heart2$HeartDisease==1)
x1
mediax1=mean(x1$Age)
mediax1

x2=filter(heart2, heart2$HeartDisease==0)
x2
mediax2=mean(x2$Age)
mediax2

xa=rnorm(x1,mediax1)
xb=rnorm(x2,mediax2)

test <- t.test(xa,xb) # Prueba t-Student

print(test)
