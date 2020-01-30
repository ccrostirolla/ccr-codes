################################

install.packages('ggplot2')
library(ggplot2)

n = 100
l = 10
nboot = 1000 # numero replicacoes bootstrap
nsim = 1000 # numero de simulacoes
boot_m <- numeric(nboot)
true <- numeric(nsim)
acf_boot <-numeric(nboot)

data <- arima.sim(list(order=c(3,0,0),ar=c(-0.5, 0.1, 0.6)),n=n)

true_mean <- mean(data) # media da serie simulada

for(j in 1:nboot){
  block <- sample(1:(n-l+1),(n/l),replace=TRUE) # faz a amostragem de cada inicial fixa do bloco
  boot.ts <- numeric(n)
  for(i in 1:(n/l)){ 
    boot.ts[(1+(i-1)*l):(l*i)] <- data[block[i]:(block[i]+l-1)] # link entre a posicao da serie boot com a posicao reamostrada nos dados
  }
  boot_m[j] = mean(boot.ts) # media da serie boot para cada replicacao boot
  acf_boot[j] = acf(boot.ts)
}

#media das 1000 medias bootstrap
boot_mean <- mean(boot_m) 

#distribuicao empirica
hist(boot_m,col='blue',main="Empirical Distribution Function")

#intervalo de confianca
ICboot = quantile(boot_m, c(0.975, 0.025))

# desvio padrao
sd_boot <- sqrt((1/(nboot-1))*sum(boot_m[j]-boot_mean)^2)

#vies
bias <- boot_mean - true_mean

#mean squared error
MSE <- ((sd_boot)^2) + bias^2

#########################################

# construindo grafico acf
dataacf <- acf(data)

data_acf <- t(do.call(cbind, dataacf))
data_acf <- data_acf[1,]
data_acf <- as.numeric(data_acf)
boot_acf <- t(do.call(cbind, acf_boot))
boot_acf[1,] = seq(0,20)
data_acf <- dataacf[1]
remove(data_acf)

boott_acf <- boot_acf[-1,]
remove(boot_acf)
boot_acf <- boott_acf
remove(boott_acf)

IC <- matrix(NA,2,21)
for (c in 1:21){
  IC[1,c] = quantile(boot_acf[,c],0.025)
  IC[2,c] = quantile(boot_acf[,c],0.975)
}

acfb <- matrix(NA,3,21)
acfb[1,] <- IC[1,]
acfb[2,] <- data_acf
acfb[3,] <- IC[2,]

lags <- seq(0,20)
plot(lags[1:5], acfb[2,1:5], type="l",ylim = c(-0.5,1))
lines(lags[1:5], acfb[1,1:5] , type="l", col="red")
lines(lags[1:5], acfb[3,1:5] , type="l", col="red")


windows() # multiplos graficos
par(mfrow=c(2,2))
ts.plot(data)
ts.plot(boot.ts)
ts.plot(boot.ts,data,col=c("blue","red"))

dataset <- data.frame(data,boot.ts,1:n)

windows()
ggplot(dataset,aes(1:n))+ geom_line(aes(y = data, colour = "data"),colour="blue") + 
  geom_line(aes(y = boot.ts, colour = "boot"),colour="red")+ geom_vline(xintercept = seq(1,100,10))
