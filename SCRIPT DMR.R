#Kevin Steven García - 1533173
#Alejandro Vargas - 1525953
#Diseño de medidas repetidas


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
summary(results, multivariate=F)


#Verificamos los supuestos:
#Se obtienen los residuales:
resid<-residuals(model)

#Normalidad:
#Pruebas gráficas:
x11()
hist(resid, freq=FALSE)
curve(dnorm(x,mean(resid), sd(resid)), xlim=c(-16,16), add=TRUE, col=2)

shapiro.test(residuals(model))
#-------------------------------------------------------------#
dvm=my.new.matrix
mlm1 <- lm(dvm ~ 1)
rfactor<-factor(c("Semana 8","Semana 12","Semana 16","Semana 20"))
library(car)
mlm1.aov <- Anova(mlm1, idata = data.frame(rfactor),
                   idesign = ~rfactor, type="III")
summary(mlm1.aov, multivariate=FALSE)
