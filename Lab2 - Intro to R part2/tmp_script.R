dat = read.csv('ebs1982_1984.csv')
dat


allcountries = of_country_codes()$iso3c # get list of all country codes
global_landings = data.frame(year=NA, catch=NA, country=NA) # set up an empty dataframe

for (i in allcountries) {
  country_landings = of_landings(country = i)
  country_landings$country = allcountries[i]
  global_landings = rbind(global_landings, country_landings)
  print(paste0(i, " processed"))
}


removeNA = function(dataframe){
  cleaned_dat = na.omit(dataframe)
  print(nrow(dataframe) - nrow(cleaned_dat))
  return(cleaned_dat)
}






