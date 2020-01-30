
#nonoverlapping
n = 100
l = 10
nboot = 1000 # numero replicacoes bootstrap
nsim = 1000 # numero de simulacoes
boot_m <- numeric(nboot)
true <- numeric(nsim)
set.seed(1234)

for(k in 1:nsim){
  data <- arima.sim(list(order=c(3,0,0),ar=c(-0.5, 0.1, 0.6)),n=n) # xt = -0.5xt-1 +0.1xt-2 + 0.6xt-3
  true_mean <- mean(data) # media da serie verdadeira
  for(j in 1:nboot){
    block <- sample((0:(n/l-1)*l+1),n/l,replace=TRUE) # faz a amostragem de cada inicial fixa do bloco
    boot.ts <- numeric(n)
    for(i in 1:(n/l)){ 
      boot.ts[(1+(i-1)*l):(l*i)] <- data[block[i]:(block[i]+l-1)] # link entre a posicao da serie boot com a posicao reamostrada nos dados
    }
    boot_m[j] = mean(boot.ts) # media da serie boot para cada replicacao boot
  }
  true[k] <- ifelse(true_mean>quantile(boot_m,0.025) & true_mean<quantile(boot_m,0.975),1,0) #  
  t <- pvalor <- matrix(0,1,1)
  t <- sum(true)
  convergencia <- t/nsim # convergencia
}

boot_mean <- mean(boot_m) #media das boot medias

##################################################

remove(list = ls()) # limpando enviroment

#overlapping
n = 100
l = 10
nboot = 1000 # numero replicacoes bootstrap
nsim = 1000 # numero de simulacoes
boot_m <- numeric(nboot)
true <- numeric(nsim)
set.seed(1234)

for(k in 1:nsim){
  data <- arima.sim(list(order=c(3,0,0),ar=c(-0.5, 0.1, 0.6)),n=n) # xt = -0.5xt-1 +0.1xt-2 + 0.6xt-3
  true_mean <- mean(data) # media da serie verdadeira
  for(j in 1:nboot){
    block <- sample(1:(n-l+1),(n/l),replace=TRUE) # faz a amostragem de cada inicial fixa do bloco
    boot.ts <- numeric(n)
    for(i in 1:(n/l)){ 
      boot.ts[(1+(i-1)*l):(l*i)] <- data[block[i]:(block[i]+l-1)] # link entre a posicao da serie boot com a posicao reamostrada nos dados
    }
    boot_m[j] = mean(boot.ts) # media da serie boot para cada replicacao boot
  }
  true[k] <- ifelse(0>quantile(boot_m,0.025) & 0<quantile(boot_m,0.975),1,0) #  
  t <- pvalor <- matrix(0,1,1)
  t <- sum(true)
  convergencia <- t/nsim # convergencia
  
}

boot_mean <- mean(boot_m) #media das boot medias

##################################################

remove(list = ls()) # limpando environment

#circular

n = 100
l = 10
nboot = 1000 # numero replicacoes bootstrap
nsim = 1000 # numero de simulacoes
boot_m <- numeric(nboot)
true <- numeric(nsim)
acf_boot <-numeric(nboot)
set.seed(1234)

for(k in 1:nsim){
  data <- arima.sim(list(order=c(3,0,0),ar=c(-0.5, 0.1, 0.6)),n=n) # xt = -0.5xt-1 +0.1xt-2 + 0.6xt-3
  true_mean <- mean(data) # media da serie verdadeira
  mydata <- data[c(1:100,1:9)]
  for(j in 1:nboot){
    block <- sample(1:n,n/l,replace=TRUE) # faz a amostragem da posicao inicial
    boot.ts <- numeric(n)
    for(i in 1:(n/l)){
      boot.ts[(1+(i-1)*l):(l*i)] <- mydata[block[i]:(block[i]+l-1)]
    }
    boot_m[j] <- mean(boot.ts)
  }
  true[k] <- ifelse(0>quantile(boot_m,0.025) & 0<quantile(boot_m,0.975),1,0) #  
  t <- pvalor <- matrix(0,1,1)
  t <- sum(true)
  convergencia <- t/nsim # convergencia
}

boot_mean <- mean(boot_m) #media das boot medias

####################################################################################

remove(list = ls()) # limpando enviroment

# stationary bootstrap

install.packages("np")
install.packages("boot")
install.packages("forecast")

library(np)
library(boot)
library(forecast)

n = 100
nsim = 1000 # numero de simulacoes
boot_m <- numeric(1000)
mean_boot <- matrix(NA,1000,1)
true <- numeric(nsim)
true_m <- numeric(nsim)
l <- 10
# l <- b.star(data,round=TRUE)[1,1] #se quiser aplicar optimal block length

stat <- function(s) {s}

set.seed(1234)

for(k in 1:nsim){
  data <- arima.sim(list(order=c(3,0,0),ar=c(-0.5, 0.1, 0.6)),n=n) # xt = -0.5xt-1 +0.1xt-2 + 0.6xt-3
  boot.ts <- ts(tsboot(data, stat, R=1000, l=l, sim="geom"))
  true_mean[k] <- mean(data)
  
  for (i in 1:1000){
    boot_m[i] <- mean(boot.ts$t[i,])
  }
  boot_mean <- mean(boot_m)
  true[k] <- ifelse(true_mean>quantile(boot_m,0.025) & true_mean<quantile(boot_m,0.975),1,0)
  t <- pvalor <- matrix(0,1,1)
  t <- sum(true)
  convergencia <- t/nsim
}