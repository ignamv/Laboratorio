#!/usr/bin/env Rscript

esperar <- function() {
	invisible(identify(c(0),tolerance=100,n=1))
}

picos <- function(x,bilateral=T) {
	lista <- c(which.max(x))
	# Alterno entre buscar máximos y mínimos
	maximo = T
	# Cuántos busco (poco elegante)
	npicos = 9
	while(npicos > 0) {
		izq <- lista[1]
		der <- lista[length(lista)]
		if (maximo) {
			picoIzq <- which.min(x[1:izq])
			picoDer <- der+which.min(x[-(1:der)])
		} else {
			picoIzq <- which.max(x[1:izq])
			picoDer <- der+which.max(x[-(1:der)])
		}
		maximo <- !maximo
		if(bilateral)
				lista <- append(picoIzq,lista,picoDer)
		else
				lista <- append(lista,picoDer)
		npicos <- npicos - 1
	}
	lista
}
archivo <- tail(commandArgs(),1)
#cat(archivo,"\n")
d <- read.table(archivo,na.strings="",sep=" ",skip=1,header=F,fill=T,
	col.names=c("t","vr","t2","vg",""))
deltaT <- median(diff(d$t))

# Por cada NA copio el valor del anterior
while(length(which(is.na(d$vg))) > 0)
	d$vg <- ifelse(is.na(d$vg),d$vg[0],d$vg)
while(length(which(is.na(d$vr))) > 0)
	d$vr <- ifelse(is.na(d$vr),d$vr[-1],d$vr)


# Calculo la frecuencia
auto <- acf(d$vg,lag.max=length(d$vg))
autod <- diff(auto$lag[sort(picos(auto$acf,bilateral=F))])
periodo <- median(autod)*2*deltaT
errorPeriodo <- sd(autod)*2*deltaT

correlacion <- ccf(d$vg,d$vr,lag.max=length(d$vg))
# Angulo de desfasaje
angulo <- correlacion$lag[which.max(correlacion$acf)]*deltaT/periodo*2*pi

rmsIn <- sqrt(sum(d$vg**2))/length(d$vg)/deltaT
rmsOut <- sqrt(sum(d$vr**2))/length(d$vr)/deltaT

cat(periodo,errorPeriodo,angulo,rmsIn,rmsOut,sep="\t")
cat("\n")
