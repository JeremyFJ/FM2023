library(FSA)
library(FSAdata)
library(rfishbase)

# Q1
## Herring
lm1 = lm(log(HerringBWE$ssb)~log(HerringBWE$spawning.year))
ggplot() + geom_point(data=HerringBWE, aes(x=log(spawning.year), y=log(ssb))) + geom_line(data=HerringBWE, aes(x=log(spawning.year), y=lm1$fitted.values))
plot(lm1)
lm2 = lm(data=HerringBWE, log(ssb/recruits)~spawning.year)
ggplot() + geom_point(data=HerringBWE, aes(x=log(spawning.year), y=log(ssb/recruits))) + geom_line(data=HerringBWE, aes(x=log(spawning.year), y=lm2$fitted.values))
plot(lm2)

## Black Drum
BlackDrum2001_new = na.omit(BlackDrum2001)
blm1 = lm(data=BlackDrum2001_new, log(weight)~log(tl))

p1 = ggplot() + 
  geom_point(data=BlackDrum2001_new, aes(x=log(tl), y=log(weight))) + 
  geom_line(data=BlackDrum2001_new, aes(x=log(tl), y=predict(blm1))) + 
  xlab("log total length (mm)") + 
  ylab("log weight (kg)") + 
  ggtitle("Weight vs Length of Atlantic Black Drum")
model_bd = nls(weight~a*tl^b, start=list(a=160,b=3), data=BlackDrum2001_new)
p2 = ggplot() + 
  geom_point(data=BlackDrum2001_new, aes(x=tl, y=weight)) + 
  geom_line(data=BlackDrum2001_new, aes(x=tl, y=predict(model_bd)))
gridExtra::grid.arrange(p1, p2)

## Bonito
vb = vbFuns(param="Typical")
Bonito_f = subset(Bonito, sex=="Female")
Bonito_m = subset(Bonito, sex=="Male")
f.starts_f <- vbStarts(fl~age,data=Bonito_f) 
f.fit_f <- nls(fl~vb(age,Linf,K,t0),data=Bonito_f,start=f.starts_f) 
f.starts_m <- vbStarts(fl~age,data=Bonito_m) 
f.fit_m <- nls(fl~vb(age,Linf,K,t0),data=Bonito_m,start=f.starts_m) 
ggplot() + 
  geom_point(data=Bonito, aes(x=age, y=fl, color=sex), pch=20) + 
  geom_line(data=Bonito_f, aes(x=age, y=predict(f.fit_f)), color="red") +
  geom_line(data=Bonito_m, aes(x=age, y=predict(f.fit_m)), color="blue") +
  scale_color_manual(values=c("red", "grey", "blue"))

params = coef(f.fit_f)
Linf = params[1]
K = params[2]
t0 = params[3]
La = Linf*(1-exp(-K*(1.5-t0)))
La[[1]]

## Q2
b = c(0.15,0.2,0.25) # birth rates
d = c(0.07,0.07,0.07) # death rates
N0 = c(1500,1500,1500) # starting population number
Time = seq(0,30,2) # time steps
Nt = matrix(NA, nrow = length(Time), ncol = 3)
for (i in 1:3){
  Nt[,i] = N0[i]*exp((b[i]-d[i])*Time)
}
plot(Nt[,1]~Time, type = "l", ylim = range(Nt))
lines(Nt[,2]~Time, col = "red")
lines(Nt[,3]~Time, col = "blue")


## Q3
hadLife = subset(popgrowth(species_list = "Melanogrammus aeglefinus"), Locality=="North Sea")
hadMat = subset(maturity(species_list = "Melanogrammus aeglefinus"), Locality=="North Sea")
hadFec = subset(fecundity(species_list = "Melanogrammus aeglefinus"), Locality=="North Sea")

lifespan = mean(hadLife$tmax, na.rm=T)
agemat = sum(c(hadMat$AgeMatMin,hadMat$AgeMatMin2), na.rm=T) / 4
fecundity = hadFec$FecundityMax[2]

rmax = getRmax(method = "Smith.etal.1998", mortality = "Then.etal.2014", fec = fecundity, lifespan=lifespan, agemat=agemat)


