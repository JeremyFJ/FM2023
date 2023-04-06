connectKennedy = function (dbuser, dbpass) 
{
    require(RPostgreSQL)
    require(RH2)
    dbname = "kennedylakes"
    dbhost <- "sp2.cs.vt.edu"
    dbport <- 5432
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, host = dbhost, port = dbport, dbname = dbname, 
        user = dbuser, password = dbpass)
}

con = connectKennedy("francesco","")

selectData = function(con, statement){
    dat = fetch(dbSendQuery(con, statement = statement), n = -1)
    dat
}

dat = selectData(con, "select * from fishdat;")

require(ggplot2)
specs = aggregate.data.frame(list(inds = dat$date), list(common = dat$common), length)
specs = specs[order(specs$inds),]
specs$perc = specs$inds/sum(specs$inds)*100

lakes = aggregate(list(inds = dat$date), list(lake = dat$lake), length)
lakes$perc = lakes$inds/sum(lakes$inds)*100

p<-ggplot(specs, aes(x=common, y=inds, fill=common)) +
  geom_bar(stat="identity")+theme_minimal()
p

par(oma = c(1,7,2,1))
barplot(specs$inds, names.arg = paste(specs$common," (",round(specs$perc,1),"%)",sep = ""), horiz=T, las = 1)


## tournment 

td = dat[dat$date >= "2023-04-01",]
nrow(td)



# select data for big lake - last session
setwd("~/VTech/Research/KennedyTreeFarm/s")
thd = dat[dat$date == "2023-03-30",] # data for thursdat march 30th
write.csv(thd, "../data/thursdaySampling.csv", row.names = F)

bl = thd[thd$lake=="big",]
# CPUE I assume 4 hours of fishing and 6 people fishing there
blc = nrow(bl)/(2*2*5) # one fish per hour
hist(bl$length_cm)

sl = thd[thd$lake=="small",]
slc = nrow(sl)/(2*2) # one fish per hour

np = thd[thd$lake=="nursery",]
npc = nrow(np)/(2*2)

hd = thd[thd$lake=="hidden",]
hdc = nrow(hd)/(2*2)

cpues = data.frame(lake = c("Big Lake","Small Pond","Nursery","Hidden"),cpue = c(blc,slc,npc, hdc))

# L-W relationships

model_ruf = nls(weight_g~a*length_cm^b, start=list(a=0.004,b=3), data=bl) # non linear model for length~weight relationship
model2 = lm(log(weight_g)~log(length_cm), bl) # linear model


newdata = data.frame(length_cm = seq(0,max(bl$length_cm, na.rm = TRUE),1))
predW = predict(model2, newdata, se.fit = TRUE)



newdata$predW = predW$fit
newdata$predStdErr = predW$se.fit
newdata$lower = exp(predW$se.fit - 1.96*predW$se.fit)
newdata$upper = exp(predW$se.fit + 1.96*predW$se.fit) 


plot(weight_g~length_cm, bl, pch = 16)
lines(exp(predW)~length_cm, newdata)


# compare with species average
# condition

require(rfishbase)
dat2 = length_weight("Micropterus salmoides")
dat2 = as.data.frame(dat2)
Ma = exp(mean(log(dat2$a))) # geometric mean of a
Mb = mean(dat2$b) # mean of b


newdata$fb.fit = Ma*newdata$length^Mb
lines(fb.fit~length_cm, newdata, col = "red") # length weight relationship from fishbase data

condition = function(W,L){

    cond = 100*(W/(Ma*L^Mb))
    cond
}

bl$cond = condition(bl$weight, bl$length)

par(mfrow = c(2,2))
# size structure
hist(bl$length_cm, xlab = "Length (cm)", main = paste("sampled fish, N= ",nrow(bl),sep=""), nclass = 20)
abline(v= c(20,30, 38, 51, 63), lty= 2)
text(x = c(20,30, 38, 51, 63), y = rep(9,5), labels = c("Stock","Quality","Preferred","Memorable","Trophy"), adj = 0)    

#L-W
plot(weight_g~length_cm, bl, pch = 16, ylab = "Weight (g)", xlab = "Length (cm)", xlim = c(0,max(length_cm)), ylim = c(0,max(weight_g, na.rm = TRUE)))
lines(exp(predW)~length_cm, newdata)
lines(fb.fit~length_cm, newdata, col = "red")

#condition
plot(cond~length_cm,data = bl, pch =16, xlab = "Length (cm)", ylab = "Condition (Wr)")
abline(h = 100, lty = 2)

# catch rates
barplot(cpues$cpue, names.arg = cpues$lake, main = "Catch Rates",ylab = "# ind./fishing hour")



