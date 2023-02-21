


# dataset with tows
require(rmedits)
require(maps)
require(mapdata)
require(tidyverse)

# ta = read.csv("../data/medits_ta.csv")
# names(ta)[3]<- "gsa"
# ta[,c("s_lat","s_lon")] = with(ta,formatLat(shooting_latitude, shooting_longitude, shooting_quadrant)) # converting coordinates to decimal format from medits format
# 
# # select for the Adriatic Sea
# adr.a = ta[ta$gsa==17,]
# write.csv(adr.a, "~/VTech/Teaching/FMC/2021/data/ta.csv", row.names = F)
# 
# with(adr.a, map("world", ylim = range(s_lat)+c(-0.5,0.5), xlim = range(s_lon)+c(-0.5,0.5), fill = TRUE))
# with(adr.a, points(s_lat~s_lon, pch = 16, col = "red"))
# 
# 
# tc = read.csv("../data/medits_tc.csv")
# names(tc)[3]<- "gsa"
# tcmer = with(tc, tc[gsa==17 & paste(genus,species,sep="")=="MERLMER",])
# write.csv(tcmer, "~/VTech/Teaching/FMC/2021/data/tc.csv")

ta = read.csv("ta.csv")
tc = read.csv("tc.csv")

haulid = c("country","gsa","year","month","day","vessel","haul_number") # common features identifying unique id

totdat = merge(tc, ta, by = haulid, all.x = TRUE)
#with(unique(totdat[,c("s_lat","s_lon")]), points(s_lat~s_lon, pch = 16, col = "blue"))


# how many years 20 years
# only for hake
tcmer = with(totdat, totdat[paste(genus,species,sep="")=="MERLMER",])
#with(unique(tcmer[,c("s_lat","s_lon")]), points(s_lat~s_lon, pch = 16, col = "green"))

#select one year

tcmerh = subset(tcmer, year == 2011)
with(unique(tcmerh[,c("s_lat","s_lon")]), points(s_lat~s_lon, pch = 16, col = "purple"))

scale_cols = c("#FFFFFF", hcl.colors(6))
world2 <- map_data("world2") #world2 is centered on antimeridian
ggplot() + 
  geom_tile(data=totdat, aes(x=s_lon, y=s_lat, fill=hauling_depth)) + 
  geom_map(
    data = world2, map = world2, #world 2 is our antimeridian map
    aes(x=long, y=lat, map_id = region)) +
  scale_fill_gradientn(colours=scale_cols,
                       # breaks=c(0, 1000, 2000),
                       # limits=c(0,3),
                       name="Hauling time") +
  labs(y="Latitude") + #Rename y axis
  scale_x_continuous(name="Longitude", breaks=c(0,90,180,270), labels=c(0,90,180,-90)) #Correct longitude to show negative values

world <- map_data("world")
worldmap = ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) 
worldmap
worldmap + 
  # Generate grids (x,y) and fill them with CPUE data (z). The `bins` argument controls       how many grids there are, and the size.
  stat_summary_2d(data = totdat, aes(x = s_lon, y = s_lat, 
                                      z = hauling_depth), alpha = 0.6, bins = 100) +  
  # Create the legend and grid color scheme
  scale_fill_gradient(name = "Hauling Depth", low = "green", high = "red") +
  # Control where the center of the map is with xlim and ylim
  xlim(0, 25) + ylim(35, 46) + 
  xlab("Longitude") + ylab("Latitude") +
  # Map title
  ggtitle("Adriatic  (1982-1984)") +
  # Customize map by removing grid lines, adding a border and background color
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background= element_rect(fill = "skyblue2"))

# just one haul
#tcmerh = subset(tcmer, area==11 & year == 2011 & haul_number==43)


# can you show the depth distribution of the speciea and the geographic distribnution

require(rfishbase)
# now we estract the parameters of the growth function with
popdat = as.data.frame(popgrowth("Merluccius merluccius", fields = c("K","to","Loo")))
meanpar =colMeans(popdat, na.rm=TRUE) # Loo is in cm


# with TropFishR
require(TropFishR)
#t <- VBGF(L = 100, list(Linf=meanpar$Loo, K=meanpar$K, to= meanpar$to))

# so time of tcmer would be 
tcmerh$age = VBGF(L = tcmerh$length_class/10, list(Linf=meanpar["Loo"], K=meanpar["K"], t0= meanpar["to"])) # we need to divide by ten because MEDITS data are reported in millimeters
tcmerh$age = round(tcmerh$age)

af = hist(tcmerh$age, breaks = seq(min(tcmerh$age),max(tcmerh$age),1)) # age frequency - to have a look at the ages to remove
# counts are correspondent to breaks[2:end]
# let's make a dataset
afd = data.frame(age = af$breaks[-1], count = af$count)

plot(log(count)~age, afd, pch=16)

# let's remove the ages for which recruitment may not be complete. that would be age 0
# so there is a linear regression there
# lets remove the first age as recruitment may not be complete
mod1 = lm(log(count)~age, afd[-c(1),])
summary(mod1)
coef(mod1)
z = coef(mod1)["age"]

H = (1-exp(-z))

