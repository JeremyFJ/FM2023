remove_NA = function(dataframe) {
  dataframe = na.omit(dataframe)
}

ebs1982$temp_cat = ifelse(ebs1982$BOT_TEMP <= 1, "Cold", "Warm")
ebs1982$BOT_TEMP[180:190]
ebs1982$temp_cat[180:190]


# Q1
nbs_rm = nbs[!nbs$BOT_TEMP==-9999,]
par(mfrow=c(1,2))
hist(nbs_rm$BOT_DEPTH, xlab="Depth (m)", ylab="Trawls", col="black", border = "green",
     xlim=c(0, 80), ylim=c(0, 5000), main="Northern Bering Sea Trawl Survey Depth (1982-2019)")
hist(nbs_rm$BOT_TEMP, xlab="Temperature (C)", ylab="Trawls", col="black", border="green",
     xlim=c(-5, 18), main="Northern Bering Sea Trawl Survey Temperature (1982-2019)")

# Q2
hauldat=aggregate(data=nbs, WTCPUE~HAUL, FUN=mean)
ggplot() +
  geom_point(data = hauldat, 
            aes(x = HAUL, y = WTCPUE)) +
  labs(x = "HAUL #", y = "WTCPUE") + theme_bw()  # set axis breaks (min, max, by)

# Q3
fish_dat = subset(nbs, SID<40000 & between(YEAR, 2000, 2019))
mean(fish_dat$WTCPUE)
sum(fish_dat$WTCPUE)
sd(fish_dat$WTCPUE)

# Q4
nbs_inv = subset(nbs, SID<40000)
fish_sum = as.data.frame(table(nbs_inv$COMMON))
fish_sum = fish_sum[-c(1),] # From inspecting the new dataframe, the first row is empty and therefore needs to be removed
head(fish_sum)

H_i = abdiv::shannon(fish_sum$Freq)
H_i
evenness = H_i/log(nrow(fish_sum))
evenness

# Q5
hist(fish_sum$Freq,
     main = paste0("Species Occurences (n species=",nrow(fish_sum),")"), # Use paste and paste0 to write text and code output together
     xlab = "Number of occurrences",
     ylab = "Number of species",
     col = "bisque"
)

# Q6
nbs_map = subset(nbs, SID<40000)
nbs_map = aggregate(data=nbs_map, WTCPUE~HAUL, FUN=sum)
nbs_map$latitude = aggregate(data=nbs, LATITUDE~HAUL, FUN=mean)$LATITUDE # I can take the mean here because latitude is the same for each haul number, otherwise you should not take the mean latitude ever
nbs_map$longitude = aggregate(data=nbs, LONGITUDE~HAUL, FUN=mean)$LONGITUDE

ebs = read.csv('ebs1982_2019.csv')
ebs_map = subset(ebs, SID<40000, na.rm=T)
ebs_map = aggregate(data=ebs_map, WTCPUE~HAUL, FUN=sum)
ebs_map$latitude = aggregate(data=ebs, LATITUDE~HAUL, FUN=mean, na.rm=T)$LATITUDE # I can take the mean here because latitude is the same for each haul number, otherwise you should not take the mean latitude ever
ebs_map$longitude = aggregate(data=ebs, LONGITUDE~HAUL, FUN=mean)$LONGITUDE

newmap = rbind(nbs_map, ebs_map)

world <- map_data("world")
ggplot() +
  geom_map(
    data = world, map = world,
    aes(long, lat, map_id = region)
  ) + 
  # Generate grids (x,y) and fill them with CPUE data (z). The `bins` argument controls       how many grids there are, and the size.
  stat_summary_2d(data = newmap, aes(x = longitude, y = latitude, 
                                      z = WTCPUE), alpha = 0.6, bins = 20) +  
  # Create the legend and grid color scheme
  scale_fill_gradient(name = "Weight (tons) CPUE", low = "green", high = "red") +
  # Control where the center of the map is with xlim and ylim
  xlim(-185, -150) + ylim(50, 70) + xlab("Longitude") + ylab("Latitude") +
  # Map title
  ggtitle("Northern and Eastern Berring Sea Trawl Surveys (1982-2019)") +
  # Customize map by removing grid lines, adding a border and background color
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),
        axis.line = element_line(colour = "black"),
        panel.background= element_rect(fill = "skyblue2"))
