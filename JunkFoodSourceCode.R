#Fijando la carpeta de trabajo
setwd("C:/Users/idpdl/Downloads")

#Cargando datos
Datos = read.csv("Libro1.csv", header = TRUE)

##Modelo No-lineal
#Primer intento modelo para clase baja
mod1_clase_baja = nls(ClaseBaja ~ a1*exp(-a2*Impuesto), Datos, 
                      start = list(a1=1, a2=0.2),
                      lower = c(0.001, 0.0001), upper = c(10, 20))

summary(mod1_clase_baja)

pred = mod1_clase_baja$m$fitted()

residuales = mod1_clase_baja$m$resid()

plot(Datos$ClaseBaja, Datos$Impuesto, col="blue", main = "Lower Class",
     xlab="Q", ylab = "P")
lines(pred, Datos$Impuesto, col = "red")
legend("topright", legend=c("Actual", "Predicted"),
       col=c("blue", "red"), lty = 1:1, cex=0.8)

#Independencia de residuales
hist(residuales)
plot(residuales)
shapiro.test(residuales)
RMSE_clase_baja = sqrt(mean((Datos$ClaseBaja - pred)^2))

##Modelo de clase media
mod1_clase_media = nls(ClaseMedia ~ a1*exp(-a2*Impuesto), Datos, start = list(a1=1,a2=1),
                        lower = c(0.0001, 0.0001), upper = c(10,20))
summary(mod1_clase_media)
pred_clase_media = mod1_clase_media$m$fitted()

residuales_clase_media = mod1_clase_media$m$resid()

plot(Datos$ClaseMedia, Datos$Impuesto, col="blue", main = "Middle Class", 
     xlab="Q", ylab = "P")
lines(pred_clase_media, Datos$Impuesto, col = "red")
legend("topright", legend=c("Actual", "Predicted"),
       col=c("blue", "red"), lty = 1:1, cex=0.8)

#Independencia de residuales
hist(residuales_clase_media)
plot(residuales_clase_media)
shapiro.test(residuales_clase_media)

RMSE_clase_media = sqrt(mean((Datos$ClaseMedia - pred_clase_media)^2))

##Clase Alta
mod1_clase_alta = nls(ClaseAlta ~ a1*exp(-a2*Impuesto), Datos, start = list(a1=1,a2=1),
                      lower = c(0.0001, 0.0001), upper = c(10,20))
summary(mod1_clase_alta)
pred_clase_alta = mod1_clase_alta$m$fitted()

residuales_clase_alta = mod1_clase_alta$m$resid()

plot(Datos$ClaseAlta, Datos$Impuesto, col="blue", main = "Upper Class", 
     xlab="Q", ylab = "P")
lines(pred_clase_alta, Datos$Impuesto, col = "red")
legend("topright", legend=c("Actual", "Predicted"),
       col=c("blue", "red"), lty = 1:1, cex=0.8)

#Independencia de residuales
hist(residuales_clase_alta)
plot(residuales_clase_alta)
shapiro.test(residuales_clase_alta)

RMSE_clase_alta = sqrt(mean((Datos$ClaseAlta - pred_clase_alta)^2))

##Proyectar datos futuros
newdata = as.data.frame(matrix(data = c(1:5),nrow = 5, ncol = 1))
colnames(newdata) <- "Impuesto"

y_pred <- predict(mod1_clase_baja, newdata)

plot(1:5,y_pred)
