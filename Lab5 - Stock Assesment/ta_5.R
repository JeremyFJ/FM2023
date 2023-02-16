# Q1

laketrout = read.csv('laketrout.csv')
length = laketrout$Length
weight = laketrout$Weight

# ruffe = RuffeSLRH92[(!is.na(RuffeSLRH92$weight)),]
# length = ruffe$length
# weight = ruffe$weight

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

ggplot() +
  geom_point(aes(x=log(length), y=log(weight)), color="black") +
  geom_line(aes(x=log(length), y=log(W.pred)), color="red")


pars = c(a=50, b=50, sigma=sigma)
fun_negLL = function(par) {
  b = par[1]
  m = par[2]
  err.sigma = par[3]
  W.pred = a*(length^b)
  -sum(dnorm(weight, mean = W.pred, sd = sigma, log = T))
}

paropt = optim(par = pars, fn = fun_negLL)
paropt$par

aopt = paropt$par[1]
bopt = paropt$par[2]

W.pred = aopt*(length^bopt)

ggplot() +
  geom_point(aes(x=length, y=weight), color="black") +
  geom_line(aes(x=length, y=W.pred), color="red")

# Q2

yellow = read.csv("yellowfin1957.csv")
getlag(yellow)

# Q3


