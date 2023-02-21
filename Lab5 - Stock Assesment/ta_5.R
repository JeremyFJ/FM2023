# Q1

laketrout = read.csv('laketrout.csv')
length = laketrout$Length
weight = laketrout$Weight

fun_negLL = function(a, b) {
  W.pred = a*(length^b)
  -sum(dnorm(weight, mean = W.pred, sd = 1, log = T))
}

a_list = seq(0.01,10,0.01)
b_list = seq(0.01,10,0.01)
params = matrix(NA,nrow=1000,ncol=1000,dimnames = list(a_list,b_list))
for (i in 1:length(a_list)){ # for a
  for (j in 1:length(b_list)){ # for b
    params[i,j] = fun_negLL(a = a_list[i], b = b_list[j])
  }
}
# the optimized parameters are 
aopt = as.numeric(rownames(params)[which(params==min(params),arr.ind=T)[1]]) # a 
bopt = as.numeric(colnames(params)[which(params==min(params),arr.ind=T)[2]]) # b
cat(paste0("a: ",aopt,"\nb: ",bopt))

W.pred = aopt*(length^bopt)
residuals = weight - W.pred
plot(length,residuals)

ggplot() +
  geom_point(aes(x=log(length), y=log(weight)), color="black") +
  geom_line(aes(x=log(length), y=log(W.pred)), color="red")

lm4 = lm(log(length)~log(W.pred))
summary(lm4)

pars = c(a=1, b=1, sigma=5)
fun_negLL = function(par, x, y) {
  a = par[1]
  b = par[2]
  err.sigma = par[3]
  W.pred = a*(length^b)
  -sum(dnorm(weight, mean = W.pred, sd = err.sigma, log = T))
}

paropt = optim(par = pars, fn = fun_negLL, x=length, y=weight, 
               lower = c(0.01, 1, 3), upper=c(2,4,10), method="L-BFGS-B")
paropt$par

aopt = paropt$par[1]
bopt = paropt$par[2]

W.pred = aopt*(length^bopt)

ggplot() +
  geom_point(aes(x=log(length), y=log(weight)), color="black") +
  geom_line(aes(x=log(length), y=log(W.pred)), color="red")

# Q2

bluecat = BlueCatfish
age = bluecat$age
length = bluecat$tl


vb = vbFuns(param="Typical") # from FSA package

f.starts <- vbStarts(tl~age, data=bluecat) # vbStarts is an optimization algorithm designed to find the best params
f.fit <- nls(tl~vb(age,Linf,K,t0),data=bluecat,start=f.starts)
coef(f.fit)

pars = c(Linf = 1600, K=0.05, t0=-0.7)
fun_negLL = function(par, x, y) {
  # Parameters
  Linf = par[1]
  K = par[2]
  t0 = par[3]
  # Model
  Lpred <- Linf * (1 - exp(-K * (age - t0)))
  # -LL
  -sum(dnorm(length, mean = Lpred, sd=5, log = T))
}

paropt = optim(par = pars, fn = fun_negLL, x=age, y=length, # parameters, function to optimize, x,y
               lower=c(1200,0.009,-1), upper = c(1800,1,1), # lower and upper bounds of parameter
               method="L-BFGS-B", hessian = T) # Byrd etal, 1995 opt method allows bounds for params
paropt$par
Linf = paropt$par[1]
K = paropt$par[2]
t0 = paropt$par[3]

Lpred <- Linf * (1 - exp(-K * (age - t0)))

residual <- length-Lpred
plot(age, residual, ylab="Residuals", xlab="Age")
abline(h=0, col="red")

bluemodel = lm(log(Lpred)~log(age))
summary(bluemodel)

ggplot() +
  geom_point(aes(x=age, y=length), color="black") +
  geom_line(aes(x=age, y=Lpred), color="red")




