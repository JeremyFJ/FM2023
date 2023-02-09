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






iccat = read.csv("iccat2000_2005.csv")

names(iccat)

ggplot() + 
  geom_point(data=iccat, aes(x=as.factor(yearc), y=bft*0.001)) +
  xlab("Year") + 
  ylab("Catch (t)") + 
  ggtitle("Atlantic Bluefin Tuna")


hooks = subset(iccat, eff1type="NO.HOOKS")
newhooks = aggregate(data=hooks, eff1~flagname, FUN=sum)
newhooks = newhooks[order(newhooks$eff1, decreasing = T),]
newhooks = newhooks[c(1:5),]
colors = c("springgreen4", "pink", "red", "orange", "maroon2")
ggplot() + 
  geom_bar(data=newhooks, aes(x=reorder(flagname, -eff1), y=0.001*eff1, fill=flagname), stat="identity") +
  scale_fill_manual(values=colors)


hooks_bft = hooks[-(which(hooks$bft==0 | hooks$eff1==0)),]
ggplot() + geom_point(data=hooks_bft, aes(x=bft, y=eff1)) + ylim(0, 1*10^7) 


iccat$bft_cpue = (iccat$bft / iccat$eff1) 



# iccat$bft_cpue = (iccat$bft / iccat$eff1) 

# plot(iccat$bft_cpue)
