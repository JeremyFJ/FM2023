# Q1

# Q2 
H = (1-exp(-z))

# Q3
datrecap_small = dat[(!is.na(dat$tagid) & dat$tagid!='' & dat$date > "2021-01-01"),] # subset master dataframe for only tagged fish caught in 2023
recaprows = which(grepl("recap", tolower(datrecap_small$notes))) # explore the rows where recaptures were reported. Which of these are from Big Lake, and which of these were tagged on our class trip on March 30th?

M = nrow(subset(datrecap_small, (common=="largemouth bass" & lake=="small" & date<"2023-04-01" & date>"2022-04-07"))) # amount of tagged fish from the class trip
n = nrow(subset(dat, (common=="largemouth bass" & lake=="small" & date>"2022-04-07"))) # amount of tagged+recaptured fish sampled after the class trip
m = 1 # amount of recaptured fish from M

mr = FSA::mrClosed(M = M, n = n, m = m, method="Chapman", chapman.mo=TRUE) # FSA function for mark-recapture in a closed system
mrsum = summary(mr, incl.SE=TRUE) # Use Poisson distribution when m < 50 or m/n > 0.1, otherwise use binomial distribution
mrsum
confint(mr, verbose=TRUE)


# Q4 
datlmb = dat[(dat$common=='largemouth bass' & !is.na(dat$length_cm) & !is.na(dat$weight_g)),]
model1 = nls(weight_g~a*length_cm^b, start=list(a=0.004,b=3), data=datlmb) # non linear model
model2 = lm(log(weight_g)~log(length_cm), datlmb) # linear model
a = summary(model_ruf)$param[1,1]
b = summary(model_ruf)$param[2,1]
lengths = seq(0,50,1)
predweight = a*lengths^b

newdata = data.frame(length_cm = lengths)
predW = predict(model2, newdata, se.fit = TRUE)
newdata$predW = predW$fit
newdata$predStdErr = predW$se.fit
newdata$lower = exp(predW$se.fit - 1.96*predW$se.fit)
newdata$upper = exp(predW$se.fit + 1.96*predW$se.fit) 

plot(weight_g~length_cm, datlmb, pch = 16)
lines(predweight~lengths, col="black")
lines(exp(predW)~length_cm, newdata, col="red")

dat2 = length_weight("Micropterus salmoides")
dat2 = as.data.frame(dat2)
Ma = exp(mean(log(dat2$a))) # geometric mean of a
Mb = mean(dat2$b) # mean of b

fb.fit = Ma*lengths^Mb
lines(fb.fit~lengths, col = "blue") # length weight relationship from fishbase data


# Q CPUE
thd = dat[dat$date == "2023-03-30",] # data for Thursday 3-30-2023

bl = thd[thd$lake=="big",]
# CPUE I assume 4 hours of fishing and 6 people fishing there
blc = nrow(bl)/(2*2*6) # one fish per hour

sl = thd[thd$lake=="small",]
slc = nrow(sl)/(2*2) # one fish per hour

np = thd[thd$lake=="nursery",]
npc = nrow(np)/(2*2)

hd = thd[thd$lake=="hidden",]
hdc = nrow(hd)/(2*2)

cpues = data.frame(lake = c("Big Lake","Small Pond","Nursery","Hidden"),cpue = c(blc,slc,npc, hdc))

ggplot(data=cpues, aes(x=lake, y=cpue)) + 
  geom_bar(stat='identity') +
  ylab("CPUE") +
  xlab("Lake") +
  geom_hline(yintercept=mean(cpues$cpue), color="red", lty=2) +
  annotate(y=1.2, x="Hidden",geom = "text", label=paste0("average CPUE=", round(mean(cpues$cpue),2))) + 
  theme(panel.grid = element_blank(),
        panel.border = element_rect(fill = NA))