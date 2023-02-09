## Q1
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


## Q2
hadLife = subset(popgrowth(species_list = "Melanogrammus aeglefinus"), Locality=="North Sea")
hadMat = subset(maturity(species_list = "Melanogrammus aeglefinus"), Locality=="North Sea")
hadFec = subset(fecundity(species_list = "Melanogrammus aeglefinus"), Locality=="North Sea")

lifespan = mean(hadLife$tmax, na.rm=T)
agemat = sum(c(hadMat$AgeMatMin,hadMat$AgeMatMin2), na.rm=T) / 4
fecundity = hadFec$FecundityMax[2]

rmax = getRmax(method = "Smith.etal.1998", mortality = "Then.etal.2014", fec = fecundity, lifespan=lifespan, agemat=agemat)

## Q3


## Q4
lmbass = BassFL

ggplot() + 
  geom_point(data=lmbass, aes(x=age, y=log(num)), pch=19) + 
  xlab("Age (years)") + 
  ylab("Log Catch") + 
  theme_bw()

thcr_lmbass <- chapmanRobson(num~age,data=lmbass,ages2use=2:8)
cbind(summary(thcr_lmbass),confint(thcr_lmbass))
plot(thcr_lmbass)

tmp <- filter(lmbass,age>=2) %>% mutate(lnct=log(num)) # filter for ages >2 and log catch
lm1 <- lm(lnct~age,data=tmp) # linear regression
coef(lm1)

thcc_lmbass <- catchCurve(num~age,data=lmbass,ages2use=2:8,weighted=TRUE)
cbind(summary(thcc_lmbass),confint(thcc_lmbass))
plot(thcc_lmbass,pos.est="bottomleft")



