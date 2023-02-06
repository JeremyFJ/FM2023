# (Your name)
# Sanitizing WCPFC data on Western Pacific Swordfish catches
#-------------------------------------------------------------------------------
swo1= read.csv('WCPFC_L_PUBLIC_BY_FLAG_MON.CSV')
species = "swo_c" 
dat = swo1[,c("yy","mm","lat_short","lon_short","hhooks","flag_id", species)]
dat$hooks = dat$hhooks*100 # because they were hundred hooks
names(dat) = c("year","month","lat","lon","hhooks","fleet","species","hooks")
# to create lat and lon from their string values
dat$lat = with(dat, ifelse(substr(lat,3,3)=="S", as.numeric(substr(lat,1,2))*-1,
                           as.numeric(substr(lat,1,2))))
dat$lon = with(dat, ifelse(substr(lon,4,4)=="W", as.numeric(substr(lon,1,3))*-1,
                           as.numeric(substr(lon,1,3))))
nrow(dat)
wcpfc = with(dat, aggregate.data.frame(dat[,c("hooks","species")],
                                       list(lat,lon, year),sum))
names(wcpfc) = c("lat","lon","year","hooks","species")
wcpfc = wcpfc[wcpfc$hooks!=0,] 
wcpfc$cpue = wcpfc$species/wcpfc$hooks*1000 
#wcpfc$fleet = "mixed"
wcpfc = wcpfc[wcpfc$hooks!=0,] 
names(wcpfc) = c("lat","lon","year","hooks","catch", "cpue")

mode = aggregate(data=wcpfc, catch~year, FUN=mean)
mode$hooks = aggregate(data=wcpfc, hooks~year, FUN=mean)$hooks
mode$cpue = mode$catch/mode$hooks*1000