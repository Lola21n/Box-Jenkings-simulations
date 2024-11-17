#Importar cosas
source(file.choose())
#Orden del ar
set.seed(0000231902)
p<-round(runif(1, min=0, max=6))
set.seed(000023190)
q<-round(runif(1, min=0, max=6))
p
q
#Hacer raices y factoresss
#Ar(1)
phi1 <- 0.5
if(abs(1/polyroot(c(1,phi1)))<1){
  "Si es estacionario"
}
#MA(2)
teta1 <- 0.2
teta2 <- -0.3
teta3 <- 0.001
check <- as.matrix(abs(1/polyroot(c(1,teta1,teta2,teta3))))
if(check[1] & check[2]<1){
  "Si es invertible"
  
}

#Crear la serie diferenciada 1 vez
n <- 999
set.seed(123)
Xjo <- runif(n,max=13,min=-5)
Xjo
set.seed(123)
a <- rnorm(n+4,mean=0,sd=1)
plot(a,type="l",main="Ruido Blanco",col="green4",xlab="Time",ylab="At")
#Dado que cada numero viene del mismo proceso generador las diferencias también
Xjoo <- matrix(data=NA,nrow=(n+3)-2,ncol=1)
c <- 1/(1-phi1)
Xjo <- rbind(2,as.matrix(Xjo))
for (i in 1:n){
  Xjoo[i] <--c+(Xjo[i]*phi1)+a[i]-teta1*a[i+1]-teta2*a[i+2]-teta3*a[i+3]
}
Xjo <- Xjoo[1:n]
mean(Xjo)
Xj <- Xjo
#Diferenciarla 2 veces más para llegar hasta j3
n <- length(Xj)
dif <- 1
j <- 2
U <- matrix(data=NA, nrow=n, ncol=1)
JA <- matrix(data=NA, nrow=n, ncol=j)
for(j in 1:j){
  for(w in 1:(n-dif)){
    U[w+1] <- Xj[w+dif]-Xj[w]
  }
  JA[,j] <- U
  Xj <- U
}
JA
Xj <- Xjo
Xjj <- JA[2:n,1]
Xjjj <- JA[3:n,2]

#Sacar las varianzas
var <- function(X){
  n <- length(X)
  Xbarra <- sum(X)/n
  var <- sum((X-Xbarra)^2)/n
}  

Xjv <- var(Xj)
Xjsd <- sqrt(Xjv)
Xjjv <- var(Xjj)
Xjjsd <- sqrt(Xjjv)
Xjjjv <- var(Xjjj)
Xjjjsd <- sqrt(Xjjjv)
#Revisar que la de j1 sea la menor
Stdserie <- cbind(Xjsd,Xjjsd,Xjjjsd)
colnames(Stdserie) <- c("j1","j2","j3")
Stdserie
#Crear la serie
vo <- -100
X <- matrix(data=NA, nrow=(n+1),ncol=1)
X[1] <- vo
plot(Xj,type="l")
for(i in 2:(n+1)){
  X[i] <- X[i-1]+Xj[i-1]
}
Xo <- (X/200)
plot(Xo, type="l")
#Crear datos abrrantes
for(i in 1:5){
  set.seed(1+i)
  o<- runif(1,max=max(Xo),min=min(Xo))
  posicion <- floor(runif(1,max=1000,min=1))
  Xo[posicion] <- Xo[posicion]+o/100
}
# set.seed(72)
# posicion2 <- floor(runif(1,max=1000, min= 1))
# posicion2+10
# Xo[posicion2+(1:25)] <- exp(Xo[posicion2+(1:25)])/200
plot(Xo,type="l",col="aquamarine4",xlab="Time",ylab="Data",main="Serie de Juguete")
X <- Xo
#Revisar si de verdad se estabiliza en nivel 
P <-X
dif <- 1
j <- 3
#Diferencias (j veces)
U <- matrix(data=NA, nrow=n, ncol=1)
JA <- matrix(data=NA, nrow=n, ncol=j)
for(j in 1:j){
  for(w in 1:(n-dif)){
    U[w+1] <- X[w+dif]-X[w]
  }
  JA[,j] <- U
  X <- U
}
JAA <- JA
JA[is.na(JA)] <- 0
#media de cada j
first <- matrix(data=NA, nrow=1, ncol=j)
for( w in 1:j){
  first[,w] <- sum(JA[,w])/(n-w)
}

#Segunda parte
second <- JAA
awa <- matrix( data=NA, nrow=n, ncol=j)
for (w in 1:j){
  awa[,w] <- (second[,w]-first[w])^2
}

awa[is.na(awa)] <- 0
#Todo completo 
Js <- matrix(data=NA, nrow=1, ncol=j)
for (y in 1:j){
  Js[,y] <- (1/(n-y-1))*sum(awa[,y])
}
Js <- sqrt(Js)
#J0 calculo
VA <- sum(P)/(n)
jo <- sqrt(sum((P-VA)^2)/(n-1))
#Todos los niveles
niveles <- cbind(jo,Js)
colnames(niveles) <- c("jo","j1","j2","j3")
#Revisar
b <- rbind("jo","j1","j2","j3")
rev2 <- as.data.frame(cbind(t(niveles),b))
tantan2 <- rev2[order(rev2$V2),]
chosen2<- tantan2[1,2]
chosen2
tantan2
#Para asegurarnos que la estabilización de varianza sea 0 (log) la ponemos como porcentaje
X <- Xo
#Revisar datos negativos y si hay los desplazamos para poder estabilizar varianza
neg_data(X)
Xnoneg <- arreglo_negdata(X)

#Graficar
df <- data.frame (x = c (1:1000),
                  y = Xo)
dfnn <- data.frame (x = c (1:1000),
                    y = Xnoneg)
plot(df$x, df$y,col="#00008B",type="l",xlab="Time",ylab="variable", main = "Serie de Juguete",ylim=c(min(X)-0.2,max(Xnoneg)+0.2))
lines(dfnn$y,col="cadetblue3")
legend("left",legend = c("Serie original", "Serie desplazada"),
       lwd = 3, col = c("#00008B", "cadetblue3"),lty=1:2, cex=0.5)
#Revisar la estabilización de varianza
Estvar <- Estabilizacion_var(Xnoneg,12)
Estvar[1]
estable <- as.matrix(as.data.frame(Estvar[2]))
estable
sas <- estable-abs(1+abs(min(Xo)))
sas
min(sas)

plot(dfsindes,type="l")
plot(df$x, df$y,col="#00008B",type="l",xlab="Time",ylab="variable", main = "Serie de Juguete",ylim=c(min(X)-1,max(Xnoneg)+0.2))
lines(dfnn$y,col="cadetblue3")
lines(sas,col="red")
legend("left",legend = c("Serie original", "Serie desplazada","Serie estabilizada en varianza"),
       lwd = 3, col = c("#00008B", "cadetblue3","red"),lty=1:1, cex=0.5)
#Estabilizacion de nivel
#Revisar si de verdad se estabiliza en nivel 
Estabilizacion_nivel(sas,1,3)[1]
Js <- as.matrix(as.data.frame(Estabilizacion_nivel(sas,1,3)[2]))
Zt <- Js[,2]
N <- 1000
Zt <- Zt[2:N]
#Determinación del modelo 
Rhos <- as.matrix(acf_estimada(Zt))

Rhos
#Varianza
d <- 1
t <- length(Rhos)
cuadss<- as.matrix(Rhos^2)
varacf <- (1/(t-d))*(1+2*sum(cuadss[1:(t-d)]))
varacfa <- (1/(t-d))*(1+2*(0)^2)
#Desviación estandar
stdevacf <- sqrt(varacf)
stdevacfa <- sqrt(varacfa)
stdevacfa
#BANDAS
sups<- 0+stdevacfa
infs <- 0-stdevacfa
#Gráfico
plot(Rhos,type="h", col="blue2",xlab="k", ylab="Rhos", main= "ACF juguete")
abline(h=0)
abline(h=sups,col="brown1",lty="dashed")
abline(h=infs,col="brown1",lty="dashed")
#Bandas reales
Rhoss <- Rhos[-1]
length(Rhoss)
#Varianza 
Varianzarhos <- matrix(data=NA, nrow=(length(Rhoss)), ncol=1)
for (k in 2:length(Rhoss)){
  Varianzarhos[k] <-sqrt((1+2*(sum((Rhos[1:k])^2)))/(length(Rhoss)))
}
Varianzarhos
#Bandas
bandas <- matrix(data=NA, nrow=(length(Rhoss)), ncol=2)
for (v in 1:length(Rhoss)){
  bandas[v,1] <- 0+Varianzarhos[v]*1.96
  bandas[v,2] <- 0-Varianzarhos[v]*1.96
}
bandas <- bandas[2:length(Rhoss),]
bandas
#Pasar a dataframes para gráficar
MA2.1 <-data.frame(x=2:249,y=bandas[2,1])
MA2.2 <- data.frame(x=2:249,y=bandas[2,2])
MA3.1 <-data.frame(x=3:249,y=bandas[3,1])
MA3.2 <- data.frame(x=3:249,y=bandas[3,2])
MA4.1 <-data.frame(x=4:249,y=bandas[4,1])
MA4.2 <- data.frame(x=4:249,y=bandas[4,2])
#Gráfico
plot(y=Rhos,x=0:248,type="h", col="blue2",xlab="k", ylab="Rhos", main= "ACF",ylim=c(-1,1))
abline(h=0)
#Revisar con para un MA2
lines(MA2.1, col="maroon")
lines(MA2.2, col="maroon")
#Resiar para un MA3
lines(MA3.1, col="blue",lwd=1.6)
lines(MA3.2, col="blue",lwd=1.6)
#Revisar para un MA4
lines(MA4.1, col="green",lwd=3)
lines(MA4.2, col="green",lwd=3)
#Bandas
abline(h=sups,col="brown1",lty="dashed")
abline(h=infs,col="brown1",lty="dashed")
#not visual
ordenma <- matrix(data=NA, nrow=t, ncol=2)
for (q in 2:length(Rhoss)){
  if(Rhos[q] < bandas[q-1,1] & bandas[q-1,2]< Rhos[q] ){
    ordenma[q,1] <- 0
  }
  else{
    ordenma[q,1] <- 1
    ordenma[q,2] <- Rhos[q]
  }
}
ordenma
ordenma <- ordenma[,1]*ordenma[,2]
ordenma <- as.matrix(na.omit(ordenma))
q <- dim(ordenma)[1]
q
##PACF
plot(pacfestable, type="h")
pacfestable <- pacf_estimada(Zt)
pacf(Zt)$acf
pacfn <- data.frame(x=1:249,y=pacfestable)
N <- length(Zt)
#Varianza de PhiKK
varphik <- 1/(length(pacfestable)-d)
sqrt(varphik)
#intervalo
limitesup <- sqrt(varphik)
limiteinf <- (-sqrt(varphik))
limiteinf
#Grafica
plot(pacfn,type="h", col="blue2",xlab="k", ylab="Rhos", main= "PACF")
abline(h=0)
abline(h=limitesup,col="brown1",lty="dashed")
abline(h=limiteinf,col="brown1",lty="dashed")
#determinar el orden del Ar
ordenar <- matrix(data=NA, nrow=t, ncol=2)
for (p in 1:(t-1)){
  if( pacfestable[p] < limitesup & limiteinf < pacfestable[p] ){
    ordenar[p,1] <- 0
  }
  else{
    ordenar[p,1] <- 1
    ordenar[p,2] <- pacfestable[p]
  }
}
ordenar <- ordenar[,1]*ordenar[,2]
ordenar <- as.matrix(na.omit(ordenar))
p <- dim(ordenar)[1]
p
#Revisar si hay estacionariedad
pas <- t(ordenar[,1])
coef <- c(1,pas)

#Polyroot nos da raices
groot<- polyroot(coef)
factory <- as.matrix(abs(1/groot))
estaci <- matrix(data=NA, nrow=q,ncol=1)
for(f in 1:p){
  if(abs(factory[f])<1){
    estaci[f] <- 1
  }else{
    estaci[f] <- 0
  }
}
if(sum(estaci)==p){
  "Si hay estacionariedad"
}
##ESTIMACIÓN
#datos
X <- Zt
#Estimacion
Coeficient <- arima(X,c(p,d,q))
Coefic <- as.matrix(as.data.frame(Coeficient[1]))
#residualesPENDIENTE
# Ztestimado <- matrix(data = NA,nrow=N,ncol=1)
# Ztestimado[1] <- sas[1]
# for (t in 2:N){
#   Ztestimado[t] <- coefic[1]
#residuales
residuales <- as.matrix(arima(X,c(p,d,q))$residuals)
N_d <- N-d
residuales <- as.matrix(residuales[(p):N_d])
Nr <- dim(residuales)[1]
##REVISAR SUPUESTOS
##Supuesto1 media 0
Masmed <- sum(residuales)/(N_d-p)
desmas<- sqrt(sum((residuales-Masmed)^2)/(N-q-p))
estadistico0 <- abs((sqrt(N-p)*Masmed)/desmas)

if(estadistico0<2){
  "Se confirma que la media de los residuales puede ser 0"
}
#Supuesto2 varianza constante
plot(residuales,type="l",col="#CD661D",xlab="t", ylab="Residuales", main= "Residuales")
#acf
Rhosres <- acf_estimada(residuales)
#varianza
varresiduales <- 1/(N-p)
desvres<- sqrt(varresiduales)
#supuesto3
#Revisar si es significante (1 significante diferente de 0, 0 mutualmente indeoendientes)
signific <- matrix(data=NA, nrow=Nr, ncol=1)
for(i in 1:Nr){
  if(Rhosres[i] > desvres){
    signific[i] <- 1
  }
  else{
    Rhosres[i] <- 0
  }
}
if(sum(signific[2:Nr])!= 0){
  "No hay dependencia estadistica"
}
#Box pierce
boxpierce_est(N,d,p,q,Nr,Rhos = Rhosres,alfa=0.05)

#Ljung-box
Ljung_box_est(N,p,q,d,Nr,Rhosres,0.05)
plot(residuales,type="l")
plot(a,type="l")
#Supuesto 4
Jarque_Bera_ess(0.5,Nr,sas,residuales)
Y <- as.matrix(sas[(p):N_d])
p
# Jarque_Bera_ess(0.05,Nr,Yniv,residuales)
#si dio 
alfa <- 0.05
n <- Nr-1
SRC <- sum(residuales^2)
sigmaJB <- (SRC/n)^(1/2)
mediay <- sum(Y)/n
miu3 <- (1/n)*sum((Y-mediay)^3)
miu4 <- (1/n)*sum((Y-mediay)^4)
S <- miu3/(sigmaJB^3)
K <- miu4/(sigmaJB^4)
JB <- ((n-K)/6)*((S^2)+((K-3)^2)/4)
if(JB < qchisq(alfa,(t-q))){
  "hay normalidad"
}else{
  "No hay normalidad"
}
