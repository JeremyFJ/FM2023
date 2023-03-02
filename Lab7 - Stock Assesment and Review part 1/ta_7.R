# Lab Exercises
schaefer = function(pars, catch) {
  r = pars['r']
  K = pars['K']
  C = catch
  B = c()
  B[1] = pars['Binit']
  for (i in 1:length(catch)) {
    B[i+1] = B[i] + r*B[i]*(1-B[i]/K) - C[i] 
  }
  return(B)
}
KB_ratio = fish$cpue[1] / max(fish$cpue)
KB_ratio # Binit = 82% of K
K = 3000
pars = c(r=0.2,K=K,Binit=(K*KB_ratio)) # initial parameters assuming biomass is ~0.5K
out <- optim(par=pars, fn=negLL, callfun=simpspm,
             indat=fish)
pars = out$par
B = schaefer(pars=pars, catch=fish$catch)
ggplot() +
  geom_line(aes(x=fish$year, y=B[1:31]))
cpue = fish$cpu
qs = cpue/B
q = mean(qs)
pred.cpue.e = B*q 

plot(fish$year,B[1:31])

pred.cpue.e2 = simpspm(pars, fish, schaefer = TRUE)
ggplot() +
  geom_point(data=fish, aes(x=year, y=cpue)) +
  geom_line(aes(x=fish$year, y=pred.cpue.e[1:31]), color="red") +
  geom_line(aes(x=fish$year, y=pred.cpue.e2[1:31]), color="green") +
  scale_color_manual(values=c(""))



# Q1
fishdatlag = getlag(yellowfin, maxlag = 10, plotout = TRUE, indexI = 1) # -2 most significant
model1 <-lm(yellowfin$cpue~yellowfin$catch)
model2 <-lm(yellowfin[3:22,"cpue"]~yellowfin[1:20,"catch"])
summary(model1) # lag = 0 for cpue
summary(model2) # lag = 2 for cpue, better fit
ggplot() +
  geom_point(aes(x=yellowfin$catch[1:20], y=yellowfin$cpue[3:22])) +
  geom_line(aes(x=yellowfin$catch[1:20], y=predict(model2)), color="maroon")

# Q2
yellowfin = read.csv("yellowfin1934.csv")

plot(data=yellowfin, cpue~year, type="l")
KB_ratio = yellowfin$cpue[1] / max(yellowfin$cpue)
KB_ratio

K = 2250000
pars = c(r=0.1,K=K,Binit=K*0.7,sigma=0.5)
out <- optim(par=pars, fn=negLL, callfun=simpspm, 
             indat=yellowfin)
out$par
B = schaefer(out$par, yellowfin$catch)
ggplot() +
  geom_line(aes(x=yellowfin$year, y=(0.001*B[1:22]))) +
  xlab("Year") +
  ylab("Biomass (millions lbs)") +
  ggtitle("Pacific yellowfin tuna") +
  theme_bw()

# Q3
cpue = yellowfin$cpue
qs = cpue/B
q = mean(qs)
pred.cpue.e = B*q
pred.cpue.e2 = simpspm(out$par, yellowfin, schaefer = TRUE)
ggplot() +
  geom_point(data=yellowfin, aes(x=year, y=cpue)) +
  geom_line(aes(x=yellowfin$year, y=pred.cpue.e[1:22]), color="red") +
  geom_line(aes(x=yellowfin$year, y=pred.cpue.e2[1:22]), color="green")

res1 = pred.cpue.e[1:22] - yellowfin$cpue
res2 = pred.cpue.e2[1:22] - yellowfin$cpue
sum(res1^2)
sum(res2^2) # better fit

# Q4
ans2 = displayModel(out$par,yellowfin,schaefer = TRUE, addrmse = TRUE) 
spmphaseplot(ans2, fnt=7)

summspm(ans2)
stats = round(ans2$Dynamics$sumout,3)
stats['FinalDepl'] - stats['InitDepl']
