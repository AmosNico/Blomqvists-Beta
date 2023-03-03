library(copBasic)
library(VineCopula)
library("car")

blomqvist <- function(x,y) {
  N = length(x)
  mx = median(x)
  my = median(y)
  n1 = 0
  for (i in 1:N) {
    if ((x[i]-mx)*(y[i]-my) > 0) {
      n1 = n1 + 1
    }
  }
  return(2*n1/N-1)
}


correct_CL <- function(theta){
  h = (2^(theta+1)-1)^(-1/theta)
  return(8*h*(1-2*h+(4*h^(theta+1)*2^theta-1)^2))
}

incorrect_CL <- function(theta){
  h = (2^(theta+1)-1)^(-1/theta)
  return (8*h*(1-2*h))
}

popbeta_CL <- function(theta){
  h = (2^(theta+1)-1)^(-1/theta)
  return(4*h-1)
}

N = 5000
kleinen = 1000
alfa = 0.05
gemiddeldes = c()

varianties = c()

thetas = seq(3.6,5,0.1)
for (i in 1:length(thetas)){
  if (thetas[i] != 0){
    bb = c()
    for (m in 1:N){
      simulation = simCOP(n=kleinen,cop=CLcop,para=thetas[i],graphics=FALSE,seed=m)
      bb = c(bb,blomqvist(simulation[,1],simulation[,2]))
    }
  
    # MEAN
    gemiddeldes = c(gemiddeldes,mean(bb))
    varianties = c(varianties,var(bb))
    print("Case done.")
  }
}

bb = c()
n = c(25,100,500,1000)
for (m in 1:N){
  simulation = simCOP(n=1000,cop=CLcop,para=2,graphics=FALSE,seed=m)
  for (i in 1:length(n)){
    bb = c(bb, blomqvist(simulation[1:n[i],1],simulation[1:n[i],2]))
  }
}
bb = matrix(bb,nrow=length(n))

#ecdf
par(mfrow=c(1,1))
beta = popbeta_CL(2)
kleur = c('black','red','green','blue')
plot(ecdf((bb[1,]-beta)*sqrt(n[1])),xlim=c(-3,3),main=NULL,cex=1.5)
for (i in 2:4){
  lines(ecdf((bb[i,]-beta)*sqrt(n[i])),col=kleur[i])
}
legend(-3.2, 1, legend=c("n = 25", "n = 100", "n = 500", "n = 1000"),
       col=kleur, lty=1, cex=1.8)

#qq
par(mfrow=c(2,2))
for (i in 1:4){
  qqnorm(bb[i,],main=sprintf("n = %d",n[i]) )
}

alfa = 0.05
left = kleinen*var(bb)*(N-1)/qchisq(1-alfa/2,df=(N-1))
right = kleinen*var(bb)*(N-1)/qchisq(alfa/2,df=(N-1))


simulation = simCOP(n=1e,cop=AMHcop,para=0.5,graphics=FALSE,seed=1)
bigsim=simulation

bb = c()
bigsim_matrix = matrix(bigsim,ncol=2000)
