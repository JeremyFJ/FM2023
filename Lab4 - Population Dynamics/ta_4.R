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







######## MAPPING

dat=wcpfc
dat$lon2 = with(dat,ifelse(lon<0, 180+lon,lon-180)) # this should be for a pacific centric map - check 
bbox = c(-55, 40,-75,45) # for a pacific centric map on shifted coordinate systems
wcpfc2 = with(dat, aggregate.data.frame(dat[,c("hooks","catch")],list(lat,lon,lon2, year),sum))
names(wcpfc2) = c("lat","lon","lon2","year","hooks","catch")
wcpfc2 = wcpfc2[wcpfc2$hooks!=0,]
wcpfc2$cpue = (wcpfc2$catch / wcpfc2$hooks)*1000
names(wcpfc2) = c("lat","lon","lon2","year","hooks","catch", "cpue")
wcpfc2=wcpfc2[wcpfc2$year==2019,]
ylim <- c(bbox[1], bbox[2]) # define our map's ylim
xlim <- c(bbox[3], bbox[4]) 
datGrid = with(wcpfc2, mapplots::make.grid(x = lon2, y = lat, z = cpue ,xlim=xlim,ylim =ylim,byx=5,byy=5, fun = mean))
long = as.numeric(dimnames(datGrid)[[1]])
lati = as.numeric(dimnames(datGrid)[[2]]) 
long = long+180
fields::image.plot(long,lati,datGrid, las = 1, ylab = "latitude", xlab = "longitude", main = paste("Swordfish predicted log(CPUE)", sep = ""))
maps::map("world2", xlim = xlim+180, ylim = ylim, fill = T, add = T) # this works

scale_cols = c("#FFFFFF", hcl.colors(6))
world2 <- map_data("world2") #world2 is centered on antimeridian
wcpfc$lon2 <- ifelse(wcpfc$lon < 0, wcpfc$lon + 360, wcpfc$lon) #IF lon is negative, we add 360 to correct for antimeridian, ELSE means its positive so we leave it
ggplot() + 
  geom_tile(data=wcpfc, aes(x=lon2, y=lat, fill=cpue)) + #lon2 is our corrected lon
  geom_map(
    data = world2, map = world2, #world 2 is our antimeridian map
    aes(long, lat, map_id = region)) +
  scale_fill_gradientn(colours=scale_cols, 
                       breaks=c(0, 1, 2, 3), 
                       limits=c(0,3),
                       name="CPUE\n(Catch/1000 hooks)\n") +
  labs(y="Latitude") + #Rename y axis
  scale_x_continuous(name="Longitude", breaks=c(0,90,180,270), labels=c(0,90,180,-90)) #Correct longitude to show negative values



scale_cols = c("#FFFFFF", hcl.colors(6))
world2 <- map_data("world2") #world2 is centered on antimeridian
wcpfc$lon2 <- ifelse(wcpfc$lon < 0, wcpfc$lon + 360, wcpfc$lon) #IF lon is negative, we add 360 to correct for antimeridian, ELSE means its positive so we leave it
ggplot() + 
  geom_tile(data=wcpfc, aes(x=lon2, y=lat, fill=log(cpue))) + #lon2 is our corrected lon
  geom_map(
    data = world2, map = world2, #world 2 is our antimeridian map
    aes(long, lat, map_id = region)) +
  scale_fill_gradientn(colours=topo.colors(7), 
                       # breaks=c(0, 1, 2, 3), 
                       # limits=c(0,3),
                       name="CPUE\n(Catch/1000 hooks)\n") +
  labs(y="Latitude") + #Rename y axis
  scale_x_continuous(name="Longitude", breaks=c(0,90,180,270), labels=c(0,90,180,-90)) #Correct longitude to show negative values
