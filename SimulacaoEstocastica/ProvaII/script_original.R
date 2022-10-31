require(survival)
require(stats4)
require(coda)




#3-Fun??o para gera??o de amostras sob censura tipo ii progressiva com distribui??o FW,

amostra.fw <- function(n,m, R, alpha, lambda){
  m = length(R)
  n = length(R) + sum(R)
  w <- runif(m,0,1)
  v <- numeric()
  U <- numeric()
  X <- numeric()
  for(i in 1:m){
    v[i] <- w[i]^(1/(i+sum(R[(m+1-i):m])))
  }
  for(j in 1:m){
    U[j] <- 1-prod(v[(m-j+1):m])
  }
  for(l in 1:m){
    X[l] = (log(-log(1-U[l]))+sqrt((log(-log(1-U[l])))^2 + 4*alpha*lambda))/(2*alpha)
  }
  return(X)
}



#Definir o vetor R do esquema de censura, e, automaticamente, os valores de m e n.
R <- c(rep(5,1), rep(0,19))
R = c(0, 0, 5, 0, 6, 5, 5, 5, 0, 5, 8)
R = c(1, 2, 2, 1, 0, 2, 2)

m = length(R)
n = length(R) + sum(R)
alpha.par = 2
lambda.par = 1
#x=amostra.fw(n,m,R,alpha.par,lambda.par)

x = c(0.062, 0.101, 0.273, 0.402, 0.605, 0.614, 1.06)
x=c(0.1, 0.2, 1, 3, 6, 18, 45, 63, 72, 75, 84)
#x=c(0.1, 0.2, 1, 1, 1, 1, 1, 2, 3, 6, 7, 1, 1, 12, 18, 18, 18, 18, 18, 21, 32, 36, 40, 45, 46, 47, 50, 55, 60, 63, 63,
#67, 67, 67, 67, 72, 75, 79, 82, 82, 83, 84, 84, 84, 85, 85, 85, 85, 85, 86, 86)
#R=c(rep(0,51))
x
#x=c(2.160, 0.746, 0.402, 0.954, 0.491, 6.560, 4.992, 0.347, 0.150, 0.358, 0.101, 1.359,
#    3.465, 1.060, 0.614, 1.921, 4.082, 0.199, 0.605, 0.273, 0.070, 0.062, 5.320)
#R=c(rep(0,23))

fw.lik <-  function(alpha, lambda){
  logl <- sum(log(alpha + lambda/(x^2))) + sum(alpha*x - lambda/x) - sum((1+R)*exp(alpha*x-lambda/x))
  return(-logl)
}

mle(fw.lik, start = list(alpha = alpha.par, lambda=lambda.par), method = "BFGS")

fit <- mle(fw.lik, start = list(alpha = alpha.par, lambda=lambda.par), method = "BFGS")
fit

vcov(fit)
sqrt(vcov(fit))


a=1
b=1
c=2
d=1



pi.alp=function(alpha, lambda){
  (prod(alpha + lambda/(x^2)))*(exp(-alpha*(b - sum(x))))*(exp(-sum((1+R)*exp(alpha*x - lambda/x))))*(alpha^(a-1))
}


pi.lam=function(lambda, alpha){
  (prod(alpha + lambda/(x^2)))*exp(-lambda*(d + sum(1/x)))*exp(-sum((1+R)*exp(alpha*x - lambda/x)))*lambda^(c-1)
}



 metr0 <- function(n, alphastart, lambdastart) {
   alpha = c(alphastart, rep(NA, n - 1))
   lambda = c(lambdastart, rep(NA, n-1))
   taxa.alpha = 0
   taxa.lambda = 0
   for (i in 2:n) {
     alpha.star = rnorm(1, alpha[i-1], sqrt(vcov(fit))[1,1])
     lambda.star = rnorm(1, lambda[i-1],sqrt(vcov(fit))[2,2])
    
     R.alpha = pi.alp(alpha.star, lambda[i-1])/pi.alp(alpha[i-1], lambda[i-1])
     
      prob.alpha = min(1, R.alpha)
    
      if (runif(1) < prob.alpha) {
        alpha[i] = alpha.star
        taxa.alpha = taxa.alpha + 1
      }
    else {
      alpha[i] = alpha[i - 1]
    }
      
      R.lambda = pi.lam(lambda.star, alpha[i])/pi.lam(lambda[i-1], alpha[i])
      
      prob.lambda = min(1, R.lambda)
      
      if (runif(1) < prob.lambda) {
        lambda[i] = lambda.star
        taxa.lambda = taxa.lambda + 1
      }
      else {
        lambda[i] = lambda[i - 1]
      }

    }
  return(list(alpha = alpha, lambda = lambda, taxa.alpha = taxa.alpha/n, taxa.lambda=taxa.lambda/n))
}

mm= metr0(10000, coef(fit)[1], coef(fit)[2])
mm$taxa.alpha
mm$taxa.lambda
mean(mm$alpha[501:10000]) 
mean(mm$lambda[501:10000])
sum((mm$alpha[501:10000] - mean(mm$alpha[501:10000]))^2)/9500
sum((mm$lambda[501:10000] - mean(mm$lambda[501:10000]))^2)/9500



a1=mm$alpha
l1=mm$lambda
z = as.mcmc(cbind(a1,l1))
colnames(z) = c("alpha", "lambda")
bb = summary(window(z, start = 501))
print(bb, digits = 3)
plot(z)



########################################################3
########################################################


#gerar amostras
B=1000

alpha.par = 2
lambda.par = 1

R <- c(15,rep(0,9))
m = length(R)
n = length(R) + sum(R)

amostras =data.frame(matrix(rep(NA, m*B),B,m ))


for(i in 1:B){
  amostras[i,] = amostra.fw(n,m,R,alpha.par,lambda.par)
  #print(100*i/B)
}


#calcular estimativas
estimativas = data.frame(matrix(rep(NA,B*6),B,6))
colnames(estimativas) = c("a_mle", "l_mle", "a_p0", "l_p0", "a_p1", "l_p1")




for( i in 1:B){
  
  x=as.numeric(amostras[i,])


  fw.lik <-  function(alpha, lambda){
    logl <- sum(log(alpha + lambda/(x^2))) + sum(alpha*x - lambda/x) - sum((1+R)*exp(alpha*x-lambda/x))
    return(-logl)
  }
  
  
  fit <- mle(fw.lik, start = list(alpha = alpha.par, lambda=lambda.par), method = "BFGS")
  
  estimativas[i,1] = coef(fit)[1]
  estimativas[i,2] = coef(fit)[2]
  
  a=0
  b=0
  c=0
  d=0
  
  
  pi.alp=function(alpha, lambda){
    (prod(alpha + lambda/(x^2)))*(exp(-alpha*(b - sum(x))))*(exp(-sum((1+R)*exp(alpha*x - lambda/x))))*(alpha^(a-1))
  }
  
  
  pi.lam=function(lambda, alpha){
    (prod(alpha + lambda/(x^2)))*exp(-lambda*(d + sum(1/x)))*exp(-sum((1+R)*exp(alpha*x - lambda/x)))*lambda^(c-1)
  }
  
  
  
  metr0 <- function(n, alphastart, lambdastart) {
    alpha = c(alphastart, rep(NA, n - 1))
    lambda = c(lambdastart, rep(NA, n-1))
    taxa.alpha = 0
    taxa.lambda = 0
    for (i in 2:n) {
      alpha.star = rnorm(1, alpha[i-1], sqrt(vcov(fit))[1,1])
      lambda.star = rnorm(1, lambda[i-1],sqrt(vcov(fit))[2,2])
      
      R.alpha = pi.alp(alpha.star, lambda[i-1])/pi.alp(alpha[i-1], lambda[i-1])
      
      prob.alpha = min(1, R.alpha)
      
      if (runif(1) < prob.alpha) {
        alpha[i] = alpha.star
        taxa.alpha = taxa.alpha + 1
      }
      else {
        alpha[i] = alpha[i - 1]
      }
      
      R.lambda = pi.lam(lambda.star, alpha[i])/pi.lam(lambda[i-1], alpha[i])
      
      prob.lambda = min(1, R.lambda)
      
      if (runif(1) < prob.lambda) {
        lambda[i] = lambda.star
        taxa.lambda = taxa.lambda + 1
      }
      else {
        lambda[i] = lambda[i - 1]
      }
      
    }
    return(list(alpha = alpha, lambda = lambda, taxa.alpha = taxa.alpha/n, taxa.lambda=taxa.lambda/n))
  }
  
  mm1= metr0(10000, coef(fit)[1], coef(fit)[2])
  
  estimativas[i,3] = mean(mm1$alpha[501:10000])
  estimativas[i,4] =  mean(mm1$lambda[501:10000])
  
  a=1
  b=1
  c=2
  d=1
  
  
  pi.alp=function(alpha, lambda){
    (prod(alpha + lambda/(x^2)))*(exp(-alpha*(b - sum(x))))*(exp(-sum((1+R)*exp(alpha*x - lambda/x))))*(alpha^(a-1))
  }
  
  
  pi.lam=function(lambda, alpha){
    (prod(alpha + lambda/(x^2)))*exp(-lambda*(d + sum(1/x)))*exp(-sum((1+R)*exp(alpha*x - lambda/x)))*lambda^(c-1)
  }
  
  
  
  metr0 <- function(n, alphastart, lambdastart) {
    alpha = c(alphastart, rep(NA, n - 1))
    lambda = c(lambdastart, rep(NA, n-1))
    taxa.alpha = 0
    taxa.lambda = 0
    for (i in 2:n) {
      alpha.star = rnorm(1, alpha[i-1], sqrt(vcov(fit))[1,1])
      lambda.star = rnorm(1, lambda[i-1],sqrt(vcov(fit))[2,2])
      
      R.alpha = pi.alp(alpha.star, lambda[i-1])/pi.alp(alpha[i-1], lambda[i-1])
      
      prob.alpha = min(1, R.alpha)
      
      if (runif(1) < prob.alpha) {
        alpha[i] = alpha.star
        taxa.alpha = taxa.alpha + 1
      }
      else {
        alpha[i] = alpha[i - 1]
      }
      
      R.lambda = pi.lam(lambda.star, alpha[i])/pi.lam(lambda[i-1], alpha[i])
      
      prob.lambda = min(1, R.lambda)
      
      if (runif(1) < prob.lambda) {
        lambda[i] = lambda.star
        taxa.lambda = taxa.lambda + 1
      }
      else {
        lambda[i] = lambda[i - 1]
      }
      
    }
    return(list(alpha = alpha, lambda = lambda, taxa.alpha = taxa.alpha/n, taxa.lambda=taxa.lambda/n))
  }
  
  mm2= metr0(10000, coef(fit)[1], coef(fit)[2])
  
  estimativas[i,5] = mean(mm2$alpha[501:10000])
  estimativas[i,6] =  mean(mm2$lambda[501:10000])
   
  print(100*i/B)
 
}

resultado=c(n,m,1000, mean(estimativas[ , 1]), mean((estimativas[,1] - alpha.par)^2),
            mean(estimativas[ , 3]), mean((estimativas[,3] - alpha.par)^2),
            mean(estimativas[ , 5]), mean((estimativas[,5] - alpha.par)^2),
            mean(estimativas[ , 2]), mean((estimativas[,2] - alpha.par)^2),
            mean(estimativas[ , 4]), mean((estimativas[,4] - alpha.par)^2),
            mean(estimativas[ , 6]), mean((estimativas[,6] - alpha.par)^2))



resumo.ib[13, ] = resultado


mean(estimativas[ , 1])
mean(estimativas[ , 2])
mean(estimativas[ , 3])
mean(estimativas[ , 4])
mean(estimativas[ , 5])
mean(estimativas[ , 6])

mean((estimativas[,1] - alpha.par)^2)
mean((estimativas[,2] - lambda.par)^2)
mean((estimativas[,3] - alpha.par)^2)
mean((estimativas[,4] - lambda.par)^2)
mean((estimativas[,5] - alpha.par)^2)
mean((estimativas[,6] - lambda.par)^2)


resultado=c(n,m,1000, mean(estimativas[ , 1]), mean((estimativas[,1] - alpha.par)^2),
           mean(estimativas[ , 3]), mean((estimativas[,3] - alpha.par)^2),
           mean(estimativas[ , 5]), mean((estimativas[,5] - alpha.par)^2),
           mean(estimativas[ , 2]), mean((estimativas[,2] - alpha.par)^2),
           mean(estimativas[ , 4]), mean((estimativas[,4] - alpha.par)^2),
           mean(estimativas[ , 6]), mean((estimativas[,6] - alpha.par)^2))



resumo.ib[1, ] = resultado
#resumo.ib = data.frame(matrix(rep(NA,50*3),50,15))
#colnames(resumo.ib) = c("n","m","R","a_mle", "EQM",  "a_p0", "EQM", "a_p1", "EQM","l_mle", "EQM", "l_p0", "EQM", "l_p1", "EQM")


require(xtable)

xtable(resumo.ib)

x=x[1]
r=R[1]
alpha.par=mm$alpha[1]
lambda.par=mm$lambda[1]
int.exp <- function(n,a,b){
  t = runif(n,a,b)
  y = ((alpha.par*(x+t/(1-t)) + lambda.par/(x + t/(1-t)))*exp(alpha.par*(x + t/(1-t) - lambda.par/(x+t/(1-t))))
      *exp((1-r)*exp(alpha.par*(x+t/(1-t)) - lambda.par/(x+t/(1-t)))))/((1-t)^2)
  int.exp = (b-a)*mean(y)
  return(int.exp)
  }
int.exp(n=100,a=0,b=1)

soma12=0
for(i in 0:r){
  soma12 = soma12 + choose(r-1, i)*((-1)^(r-i-1))*exp(r-i)*exp(alpha.par*x - lambda.par/x)
}
soma12

alpha2 = mm$alpha
lambda2=mm$lambda
soma1 = 0
integral12 = numeric()
int.exp = function(n,a,b, alpha, beta){
  t = runif(n,a,b)
  y = ((alpha*(x+t/(1-t)) + lambda.par/(x + t/(1-t)))*exp(alpha.par*(x + t/(1-t) - lambda.par/(x+t/(1-t))))
       *exp((1-r)*exp(alpha.par*(x+t/(1-t)) - lambda.par/(x+t/(1-t)))))/((1-t)^2)
  int.exp = (b-a)*mean(y)
  return(int.exp)
}


y = function(t){
  ((alpha.par*(x+t/(1-t)) + lambda.par/(x + t/(1-t)))*exp(alpha.par*(x + t/(1-t) - lambda.par/(x+t/(1-t))))*exp((1-r)*exp(alpha.par*(x+t/(1-t)) - lambda.par/(x+t/(1-t)))))/((1-t)^2)}

integrate(y, 0, 1)

for(l in 1:10000){
 integral12[l] =  int.exp(n=100,a=0,b=1, alpha2[l], lambda2[l])
  }
  
  integral12
  
  soma1 = soma1 + soma12*integral12
}
