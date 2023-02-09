
setwd("~/VTech/Teaching/FM/2022/W9-FisheriesManagement")

swo = read.csv('WCPFC_L_PUBLIC_BY_FLAG_MON.CSV')

new_df = data.frame(CPUE=(swo$swo_c/swo$hhooks), year=swo$yy)
df1 = aggregate(data=swo, swo_c~yy, FUN=sum)
df2 = aggregate(data=swo1, hhooks~yy, FUN=sum)
swo1 = swo[swo$swo_c!=0,]
length(unique(swo1$flag_id))
par(mfrow=c(1,2))
plot(swo_c~yy,data=df1, xlab="year", ylab="Catch")
plot(hhooks~yy, data=df2)

plot(CPUE~year, data=new_df)

gsub("N",swo1$lat_short[1])

swo1= read.csv('WCPFC_L_PUBLIC_BY_FLAG_MON.CSV')
species = "swo_c" 
dat = swo1[,c("yy","mm","lat_short","lon_short","hhooks","flag_id", species)]
dat$hooks = dat$hhooks*100 # because they were hundred hooks
names(dat) = c("year","month","lat","lon","hhooks","fleet","species","hooks")





# to create lat and lon from their string values
dat$lat = with(dat, ifelse(substr(lat,3,3)=="S", as.numeric(substr(lat,1,2))*-1,as.numeric(substr(lat,1,2))))
dat$lon = with(dat, ifelse(substr(lon,4,4)=="W", as.numeric(substr(lon,1,3))*-1,as.numeric(substr(lon,1,3))))
dat$lon2 = with(dat,ifelse(lon<0, 180+lon,lon-180)) # this should be for a pacific centric map - check 
bbox = c(-55, 40,-75,45) # for a pacific centric map on shifted coordinate systems  


wcpfc = with(dat, aggregate.data.frame(dat[,c("hooks","species")],list(lat,lon,lon2, year),sum))
names(wcpfc) = c("lat","lon","lon2","year","hooks","species")

wcpfc = wcpfc[wcpfc$hooks!=0,] # there were instances wi

wcpfc$cpue = wcpfc$species/wcpfc$hooks*1000 



#wcpfc$fleet = "mixed"
wcpfc = wcpfc[wcpfc$hooks!=0,] # there were instances wi



names(wcpfc) = c("lat","lon","lon2","year","hooks","catch", "cpue")

require(mapplots)
wcpfc=wcpfc[wcpfc$year==2019,]
#summary(wcpfc)


ylim <- c(bbox[1], bbox[2]) #define our map's ylim
xlim <- c(bbox[3], bbox[4]) 
datGrid = with(wcpfc, mapplots::make.grid(x = lon2, y = lat, z = cpue ,xlim=xlim,ylim =ylim,byx=5,byy=5, fun = mean))
long = as.numeric(dimnames(datGrid)[[1]])
lati = as.numeric(dimnames(datGrid)[[2]]) 
 

long = long+180

pdf(paste("cpuePlot.pdf",sep=""))
fields::image.plot(long,lati,datGrid, las = 1, ylab = "latitude", xlab = "longitude", main = paste(species," predicted log(CPUE)", sep = ""))
maps::map("world2", xlim = xlim+180, ylim = ylim, fill = T, add = T) # this works
dev.off()

### get data with the sCPUEdb

iattc = getIATTCdataCE(con, code = code, index = "n")



