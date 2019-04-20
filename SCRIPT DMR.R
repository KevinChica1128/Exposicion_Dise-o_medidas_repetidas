#Kevin Steven García - 1533173
#Alejandro Vargas - 1525953
#Diseño de medidas repetidas

#EJEMPLO 1
Rata<-c(rep(c("1","2","3","4","5"),4))
Tiempo<-c(rep("Semana 08",5),rep("Semana 12",5),rep("Semana 16",5),rep("Semana 20",5))
Peso<-c(164,164,158,159,155,220,230,226,227,222,261,275,264,280,272,306,326,320,330,312)
Datos<-data.frame(Rata=as.factor(Rata),Tiempo=as.factor(Tiempo),Peso=Peso)
head(Datos)
#Análisis descriptivos:
#Por tratamiento(Tiempo):
Datosdiagrama<-data.frame(Peso[1:5],Peso[6:10],Peso[11:15],Peso[16:20])
summary(Datosdiagrama)
medias<-c(mean(Datosdiagrama$Peso.1.5.),mean(Datosdiagrama$Peso.6.10.),mean(Datosdiagrama$Peso.11.15.),mean(Datosdiagrama$Peso.16.20.))
desviaciones<-c(sd(Datosdiagrama$Peso.1.5.),sd(Datosdiagrama$Peso.6.10.),sd(Datosdiagrama$Peso.11.15.),sd(Datosdiagrama$Peso.16.20.))
cv<-c(desviaciones/medias)*100

#Por sujetos(Rata):
Datosrata<-data.frame(Peso[c(1,6,11,16)],Peso[c(2,7,12,17)],Peso[c(3,8,13,18)],Peso[c(4,9,14,19)],Peso[c(5,10,15,20)])
summary(Datosrata)
mediasr<-c(mean(Datosrata$Peso.c.1..6..11..16..),mean(Datosrata$Peso.c.2..7..12..17..),mean(Datosrata$Peso.c.3..8..13..18..),mean(Datosrata$Peso.c.4..9..14..19..),mean(Datosrata$Peso.c.5..10..15..20..))
desviacionesr<-c(sd(Datosrata$Peso.c.1..6..11..16..),sd(Datosrata$Peso.c.2..7..12..17..),sd(Datosrata$Peso.c.3..8..13..18..),sd(Datosrata$Peso.c.4..9..14..19..),sd(Datosrata$Peso.c.5..10..15..20..))
cvr<-c(desviacionesr/mediasr)*100


x11()
boxplot(Datosdiagrama,xlab="Tiempo",ylab="Peso(gramos)",
        names = c("Semana 8","Semana 12","Semana 16","Semana 20"))
x11()
boxplot(Datosrata,xlab="Rata",ylab="Peso(gramos)",
        names=c("Rata 1","Rata 2", "Rata 3","Rata 4","Rata 5"))

x11()
interaction.plot(Tiempo,Rata,Peso,ylab = "Peso(gramos)",col = c("black","red","blue","green","purple"))

#Supuestos
#Normalidad intra sujeto
shapiro.test(Datosdiagrama$Peso.1.5.)
shapiro.test(Datosdiagrama$Peso.6.10.)
shapiro.test(Datosdiagrama$Peso.11.15.)
shapiro.test(Datosdiagrama$Peso.16.20.)

shapiro.test(Datosrata$Peso.c.1..6..11..16..)
shapiro.test(Datosrata$Peso.c.2..7..12..17..)
#Esfericidad
mauchly.test(mod)

#Análisis de varianza
mod<-lm(Peso~Tiempo+Rata,data = Datos)
anova(mod)


#------------------------------------------------------------#
library(car)
weight<-c(164,164,158,159,155,220,230,226,227,222,261,275,264,280,272,306,326,320,330,312)
my.new.matrix<-matrix(weight, nrow=5, ncol=4)
mlm<-lm(my.new.matrix~1)
mauchly.test( mlm , x =  ~  1 )

my.new.matrix
model<-lm(my.new.matrix ~ 1)
design<-factor(c("week08", "week12", "week16", "week20"))
options(contrasts=c("contr.sum", "contr.poly"))
results<-Anova(model, idata=data.frame(design), idesign=~design, type="III")
summary(results, multivariate=F) #Esfericidad


#Verificamos los supuestos:
#Se obtienen los residuales:
resid<-residuals(mod)

#Normalidad:
#Pruebas gráficas:
x11()
hist(resid, freq=FALSE)
curve(dnorm(x,mean(resid), sd(resid)), xlim=c(-10,10), add=TRUE, col=2)

shapiro.test(residuals(mod))
#-------------------------------------------------------------#
dvm=my.new.matrix
mlm1 <- lm(dvm ~ 1)
rfactor<-factor(c("Semana 8","Semana 12","Semana 16","Semana 20"))
library(car)
mlm1.aov <- Anova(mlm1, idata = data.frame(rfactor),
                   idesign = ~rfactor, type="III")
summary(mlm1.aov, multivariate=FALSE)


#EJEMPLO 2 
Sujeto<-c(rep("1",4),rep("2",4),rep("3",4),rep("4",4),rep("5",4),rep("6",4),
          rep("7",4),rep("8",4),rep("9",4))
Tiempos<-c(rep(c("H","D","S","M"),9))
Recuerdo<-c(16,8,8,12,12,9,9,10,12,10,10,8,15,13,7,11,18,12,12,12,13,13,8,10,
            18,16,10,13,15,9,6,6,20,9,11,8)
Datos1<-data.frame(Sujeto=as.factor(Sujeto),Tiempos=as.factor(Tiempos),Recuerdo=Recuerdo)
head(Datos1)

mean(Recuerdo)
#Análisis descriptivos
#Por tratamiento:
#Hora
summary(Recuerdo[c(1,5,9,13,17,21,25,29,33)])
sd(Recuerdo[c(1,5,9,13,17,21,25,29,33)])
#Dia
summary(Recuerdo[c(2,6,10,14,18,22,26,30,34)])
sd(Recuerdo[c(2,6,10,14,18,22,26,30,34)])
#Semana
summary(Recuerdo[c(3,7,11,15,19,23,27,31,35)])
sd(Recuerdo[c(3,7,11,15,19,23,27,31,35)])
#Mes
summary(Recuerdo[c(4,8,12,16,20,24,28,32,36)])
sd(Recuerdo[c(4,8,12,16,20,24,28,32,36)])

#Por sujeto:
#1
summary(Recuerdo[1:4])
sd(Recuerdo[1:4])
#2
summary(Recuerdo[5:8])
sd(Recuerdo[5:8])
#3
summary(Recuerdo[9:12])
sd(Recuerdo[9:12])
#4
summary(Recuerdo[13:16])
sd(Recuerdo[13:16])
#5
summary(Recuerdo[17:20])
sd(Recuerdo[17:20])
#6
summary(Recuerdo[21:24])
sd(Recuerdo[21:24])
#7
summary(Recuerdo[25:28])
sd(Recuerdo[25:28])
#8
summary(Recuerdo[29:32])
sd(Recuerdo[29:32])
#9
summary(Recuerdo[33:36])
sd(Recuerdo[33:36])

#Gráficos de cajas
#Tratamiento
DatosT<-data.frame(Recuerdo[c(1,5,9,13,17,21,25,29,33)],Recuerdo[c(2,6,10,14,18,22,26,30,34)],
                   Recuerdo[c(3,7,11,15,19,23,27,31,35)],Recuerdo[c(4,8,12,16,20,24,28,32,36)])
x11()
boxplot(DatosT,xlab="Tiempo",ylab="Calidad del recuerdo",
        names = c("Hora","Día","Semana","Mes"))

#Sujetos
x11()
boxplot(Datos1$Recuerdo~Datos1$Sujeto,xlab="Individuo",ylab="Calidad del recuerdo")

x11()
interaction.plot(Tiempos,Sujeto,Recuerdo,ylab = "Calidad del recuerdo",col = c("black","red","blue","green","purple"))

#ANOVA
mod1<-lm(Recuerdo~Tiempos+Sujeto,data = Datos1)
anova(mod1)


#Supuestos
#Normalidad intra sujeto
shapiro.test(DatosT$Recuerdo.c.1..5..9..13..17..21..25..29..33..)
shapiro.test(DatosT$Recuerdo.c.2..6..10..14..18..22..26..30..34..)
shapiro.test(DatosT$Recuerdo.c.3..7..11..15..19..23..27..31..35..)
shapiro.test(DatosT$Recuerdo.c.4..8..12..16..20..24..28..32..36..)

shapiro.test(Recuerdo[1:4])
shapiro.test(Recuerdo[5:8])
shapiro.test(Recuerdo[9:12])
shapiro.test(Recuerdo[13:16])
shapiro.test(Recuerdo[17:20])
shapiro.test(Recuerdo[21:24])
shapiro.test(Recuerdo[25:28])
shapiro.test(Recuerdo[29:32])
shapiro.test(Recuerdo[33:36])


#Matriz de datos
matriz=matrix(Recuerdo,nrow = 9,ncol = 4,byrow = T)
mlm11 <- lm(matriz ~ 1)
rfactor1<-factor(c("H","D","S","M"))
library(car)
mlm11.aov <- Anova(mlm11, idata = data.frame(rfactor1),
                  idesign = ~rfactor1, type="III")
summary(mlm11.aov, multivariate=FALSE) #Esfericidad

#Normalidad en los errores
#Se obtienen los residuales:
resid1<-residuals(mod1)

#Normalidad:
#Pruebas gráficas:
x11()
hist(resid1, freq=FALSE)
curve(dnorm(x,mean(resid1), sd(resid1)), xlim=c(-4,4), add=TRUE, col=2)

shapiro.test(residuals(mod1))


