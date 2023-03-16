step2fun = function(x, N, C){
  
  (x/(x+M)*N*(exp(x+M)-1)) - C
  
}
# Exercise whiting
data(whiting)
M = 0.2 # natural mortality rate
Fi = rep(0,8) # empty vector for fishing mortality
Fi[8] = 0.2 # fishing mortality rate when t=8
Z = M + Fi[8] # total mortality rate
C = whiting$catch[,1] # catch in 1974 for 8 age classes
N = rep(0,8) # empty vector for population size

N[8] = C[8]/((Fi[8]/Z)*(1-exp(-Z))) # this is the final N

for (i in 7:1){
  
  Fi[i] = uniroot(step2fun, N = N[i+1], C = C[i], interval = c(0,2))$root
  N[i] = N[i+1]*exp(M+Fi[i])
  
}

# Exercise hake
M = hake$M
Fi = hake$FM
C = hake$catch
N = rep(0,14)
Z = M + Fi[14]

N[14] = C[14]/((Fi[14]/Z)*(1-exp(-Z)))

for (i in 13:1){
  
  Fi[i] = uniroot(step2fun, N = N[i+1], C = C[i], interval = c(0,2))$root
  N[i] = N[i+1]*exp(M+Fi[i])
  
}

# Exercise YPR
N0 = 100 # number of individual at age 1
years = 1:10 # follow the cohort for 10 years
Fi = 0.6 # fishing mortality
M = 0.2 # natural mortality
Z = Fi+M # total mortality
Nt = c(N0,N0*exp(-(Fi+M)*years[1:9])) # calculate numbers in following ages assuming a
Cn = Fi/(Z)*Nt*(1-exp(-Z)) # catches in Numbers from the catch equation derived in box 7.2

### If we want to find the Yield per recruit
W = c(0.6,0.9,2.1,4.1,6.3,8.4,10,11.2,12.6,13.5) # these are average weight at age
Cw = Cn*W # catch in biomass (kg)
Y = sum(Cw) # this is the yield
Y/N0
### Biomass per recruit
Pb = Nt*W # Population biomass is found mutiplying average weight for number of individuals
sum(Pb)/N0

Z = c(0.1, 0.3, 0.9)

N1 = c(N0,N0*exp(-(Z[1])*years[1:9]))
N2 = c(N0,N0*exp(-(Z[2])*years[1:9]))
N3 = c(N0,N0*exp(-(Z[3])*years[1:9]))

ggplot() +
  geom_line(aes(x=years, y=N1), lty=2) +
  geom_line(aes(x=years, y=N2), lty=2) +
  geom_line(aes(x=years, y=N3)) +
  annotate(geom="text", label="Z=0.9", x=3, y=28) +
  annotate(geom="text", label="Z=0.3", x=4, y=49) +
  annotate(geom="text", label="Z=0.1", x=5.2, y=73) +
  ylab("Population size") +
  ggtitle("Effect of total mortality rate on population size") +
  theme(panel.grid.major = element_line(color="black", size=0.05),
        panel.border = element_rect(size=0.9, fill=NA))
  

# Assignment Q1
menhaden = FSAdata::Menhaden1
m =14
C = as.numeric(menhaden[14,][3])
for (i in 4:8) {
C = append(C,as.numeric(menhaden[m+1,][i]))
m=m+1
}

Fi = rep(0, 6) # empty vector for fishing mortality values
Fi[6] = 0.2 # fishing mortality for oldest age class (assumed)
Fi = rep(0, 6) # empty vector for fishing mortality values
Fi[6] = 0.2 # fishing mortality for oldest age class (assumed)
M = 3/11 # approximate natural mortality rate
Z = M + Fi[6] # total mortality
N = rep(0, 6) # empty vector for population size
N[6] = C[6] / ((Fi[6]/Z)*(1-exp(-Z))) # Calculate population size at t=6
N

for (i in 5:1){
  
  Fi[i] = uniroot(step2fun, N = N[i+1], C = C[i], interval = c(-1,2))$root
  N[i] = N[i+1]*exp(M+Fi[i])
  
}


# Assignment Q2

W = c(0.68, 1.3, 1.89, 2.09, 2.5, 2.8)
Cw = C*W
Y = sum(Cw)
Y / N[1]

Pb = N*W
sum(Pb) / N[1]

# Assignment Q3

getYpR = function(Fi){
  N0 = N[1]
  years = 1:6
  
  M = 3/11
  Z = Fi+M
  Nt = c(N0,N0*exp(-(Z)*years[1:5]))
  W = c(0.68, 1.3, 1.89, 2.09, 2.5, 2.8)
  Pb = Nt*W # Population biomass
  Cn = Fi/(Z)*Nt*(1-exp(-Z)) # catches in Numbers
  Cw = Cn*W
  Y = sum(Cw)
  
  # Yield per recruit
  YpR = Y/N0
  # Biomass per recruit
  BpR = sum(Pb)/N0
  c(YpR = YpR,BpR = BpR)
}

Fis = seq(0,1,.1) # range of fishing mortality values 

dat = do.call(rbind, lapply(Fis, getYpR)) # apply the function to all elements of Fis and then combine the results in a table
dat = as.data.frame(dat) # we need to make sure this table is a data.frame
dat$Fi = Fis

par(mar = c(5,5,2,5)) # set the margins for the plotting device
plot(YpR~Fi, dat, type = "l", axes = FALSE, ylab = "Yield per Recruit (kg)")
axis(1)
axis(2)
par(new = T) # allows overplotting on the same figure
plot(BpR~Fi, dat, type = "l", lty = 2, axes=FALSE, xlab=NA, ylab=NA, ylim = c(0,20)) # this is the dashed line
axis(side = 4)
mtext(4,line = 3 ,text = "Biomass per Recruit (kg)")
# best Fi
abline(v = dat$Fi[dat$YpR==max(dat$YpR)], lty = 2)
text(0.6, 0.1, expression('Fi at max YpR'))
text(0.85,0.42, expression('BpR'))
text(0.82,18, expression('YpR'))
dat$Fi[dat$YpR==max(dat$YpR)]
