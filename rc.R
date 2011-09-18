#!/usr/bin/env Rscript

esperar <- function() {
	invisible(identify(x=c(0),y=c(0),n=1,plot=FALSE,tolerance=20))
}
leyenda <- function(valores) {
	min(valores,na.rm=T) + diff(range(valores,na.rm=T))[1]/5
}

# Cargo el archivo seleccionado

archivo <- tail(commandArgs(),1)
datos <- read.table(archivo,skip=1,col.names=c("t","V"),na.strings="")

# Pido los extremos para recortar

X11()
plot(datos$t,datos$V,main="Recortar curva",
	sub="Haga click en el extremo izquierdo y el derecho de la porción que desea de la curva",
	xlab="Tiempo [s]",ylab="Tensión [V]")
p <- identify(x=datos$t,y=datos$V,n=2,plot=FALSE)
izq <- min(p)
der <- max(p)
datos <- datos[izq:der,]

# Pido el límite de la curva

plot(datos$t,datos$V,main="Límite de la curva",
	sub="Haga click en dos puntos cerca del límite de la curva",
	xlab="Tiempo [s]",ylab="Tensión [V]")
p <- identify(x=datos$t,y=datos$V,n=2,plot=FALSE)
piso <- mean(datos$V[min(p):max(p)])
# Dependiendo de si es una curva de carga o descarga invierto los datos
if (piso < mean(datos$V)) {
	datos$V <- datos$V - piso
	invertir <- FALSE
} else { 
	datos$V <- piso - datos$V
	invertir <- TRUE
}
datos$t <- datos$t - datos$t[1]

#Realizo el fit
datos$ln <- log(datos$V)
# El factor 4/5 es para tomar sólo los primeros valores, donde el error es menor
fit <- lm(datos$ln ~ datos$t,subset=datos$V>max(datos$V)*4/5)
datos$lnfit <- fit$coefficients[1]+fit$coefficients[2]*datos$t
plot(datos$t,datos$ln,type="l",col="black",main="log(V) y fit",
	sub="Haga click para continuar",
	xlab="Tiempo [s]",ylab="ln Tensión")
lines(datos$t,datos$lnfit,col="blue")
legend(x=leyenda(datos$t),y=leyenda(datos$ln),
	legend=c("Medido","Ajustado"),col=c("black","blue"),lty=c(1,1))
esperar()
# Guardo el plot
dev.copy(pdf,gsub(".dat",".fitln.pdf",archivo))
dev.off()
if (invertir) {
	datos$fit <- -exp(datos$lnfit)
	datos$V <- -datos$V
} else {
	datos$fit <- exp(datos$lnfit)
}
plot(datos$t,datos$V,type="l",col="black",main="V y fit",
	sub="Haga click para continuar",
	xlab="Tiempo [s]",ylab="Tensión [V]")
lines(datos$t,datos$fit,col="blue")
#p <- identify(x=datos$t,y=datos$V,n=1,plot=FALSE,tolerance=20)
esperar()
dev.copy(pdf,gsub(".dat",".fit.pdf",archivo))
dev.off()

cat("RC =",-1/fit$coefficients[2])
