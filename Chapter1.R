## Question 1
#check the dimensions of the data
dim(turnout)
#14 rows, 9 columns

#obtain a summary of the data
summary(turnout)
# summary includes min, 1st quartile, median, etc. for each column

#how many observations?
nrow(turnout) * ncol(turnout)
#126 total observations (including NA observations)

#what is the range of years?
range(turnout$year)
#1980-2008

## Question 2
#calculate turnout rate based on VAP (voting age population)
# total = total number of ballots cast for that year
# must add overseas to VAP
eligiblevoters <- (turnout$VAP + turnout$overseas)

print(eligiblevoters)

votingrate <- (turnout$total / eligiblevoters)
print(votingrate)
summary(votingrate)

#next, calculate the turnout rate using the VEP. 
#The book doesn't say VEP doesn't include overseas, so I'm going to assume
# it does since it is defined as "voting eligible population" which should
# be a total that includes overseas, excludes felons (which is unethical btw), etc. 

votingratevep <- (turnout$total / turnout$VEP)
print(votingratevep)
summary(votingratevep)

#What difference do you observe?

votingratediff <- (votingratevep - votingrate)

print(votingrate)
print(votingratevep)
print(votingratediff)
mean(votingrate)
mean(votingratevep)
mean(votingratediff)

#the voting rate appears lower if you look at the VAP +overseas vs the VEP
#this makes sense because VAP includes people who aren't actually eligible to vote

##Question 3
#compute differences between the vap and anes
anesrate <- (turnout$ANES / 100)
print(anesrate)
anesdiff <- (votingrate - anesrate)
print(anesdiff)
#how big is the difference on average?
mean(anesdiff)
#On average, the ANES reported rates are about 20 points higer than the rate
# calculated by VAP 

#What is the range of the differences?
range(anesdiff)
# Anes is between 26 and 11 points higher

# Conducts the same comparison but with VEP

anesdiffvep <- (votingratevep - anesrate)
print(anesdiffvep)

mean(anesdiffvep)
# ANES is about 17 points higher than the VEP rate on average
range(anesdiffvep)
# ANES is between 22 and about 9 points higher than VEP
# So, if VEP takes into account people who are not eligible to vote, 
# then calculating the turnout rate based on VAP is less accurate
# and the ANES rate is closer to the VEP rate than to the VAP rate
# so, without knowing how ANES is calculated, I would guess that it is closer
# to reality than a turnout rate calculated on VEP
# There are probably some additional things that are taken into consideration for 
# ANES that change the percentage, but I don't know what those are

## Question 4
# Compare VEP turnout rate with the ANES turnout rate separately for presidential 
# elections and midterm elections. Data set excludes 2006
#presidential years = 2008, 2004, 2000, 1996, 1992, 
# 1988, 1984, 1980
# midterm years = 82, 86, 90, 94, 98, 02, 06 (not included)

presidentyears <- c(1,3,5,7,9,11,13,14)
#I'd rather do this buy storing the presidential election years as a string
# and matching them based on that string, but can't figure out how to do that
presidentyears
midtermyears <- c(2,4,6,8,10,12)
midtermyears

presidentveprate <- (turnout$total[presidentyears] / turnout$VEP[presidentyears])
presidentveprate
mean(presidentveprate)

presidentanesrate <- (turnout$ANES[presidentyears] / 100)
presidentanesrate
mean(presidentanesrate)

#vep vs anes for pres election years
summary(presidentveprate)
summary(presidentanesrate)

#difference between anes and vep for presidential years
presidentyearsdiff <- (presidentanesrate - presidentveprate)
presidentyearsdiff
mean(presidentyearsdiff)
# anes rate is about 18 points higher than vep rate in election years

#now let's do midterms

midtermveprate <- (turnout$total[midtermyears] / turnout$VEP[midtermyears])
midtermveprate
mean(midtermveprate)

midtermanesrate <- (turnout$ANES[midtermyears] / 100)
midtermanesrate
mean(midtermanesrate)

#vep vs anes for midterm years
summary(midtermveprate)
summary(midtermanesrate)

#difference between anes and vep for midterm years
midtermyearsdiff <- (midtermanesrate - midtermveprate)
midtermyearsdiff
mean(midtermyearsdiff)
# anes rate is about 15 points higher than vep rate in midterm years

summary(midtermyearsdiff)
summary(presidentyearsdiff)

anesbiaspresvsmidterm <- (mean(presidentyearsdiff) - mean(midtermyearsdiff))
anesbiaspresvsmidterm

questionanswer <- paste("ANES bias is about", anesbiaspresvsmidterm, "points higher in presidential election years vs midterm")
questionanswer

## Question 5
# Divide the data into half by election years.

period1 <- c(1:7)
period1

period2 <- c(8:14)
period2

period1anes <- (turnout$ANES[period1] / 100)
period1anes

period1vep <- (turnout$total[period1] / turnout$VEP[period1])
period1vep

period2anes <- (turnout$ANES[period2] / 100)
period2anes

period2vep <- (turnout$total[period2] / turnout$VEP[period2])
period2vep

#Calculate the difference between the VEP turnout rate and the ANES turnout rate
# seperately for each year within each period

period1diff <- (period1anes - period1vep)
period1diff

period2diff <- (period2anes - period2vep)
period2diff

# Has the bias of ANES increased over time? 

periodsdiff <- (period1diff - period2diff)
periodsdiff

meanperiodsdiff <- (mean(period1diff) - mean(period2diff))
meanperiodsdiff
paste("The bias of ANES has decreased", meanperiodsdiff, "points between period 1 and period 2")


## Question 6
# Adjust 2008 VAP turnout rate. Subtract the total number of ineligible felons 
# and noncitizens from VAP to calculate an adjusted VAP. 

adjustedVAP <- (turnout$VAP[14] - turnout$felons[14] - turnout$noncit[14])
adjustedVAP

# Calculate an adjusted VAP turnout rate, taking care to subtract the number
# of overseas ballots counted from the total ballots in 2008

adjustedballots <- (turnout$total[14] - turnout$overseas[14])
adjustedballots

adjustedVAPrate <- (adjustedballots / adjustedVAP)
adjustedVAPrate

unadjustedVAPrate <- (turnout$total[14] / turnout$VAP[14])
unadjustedVAPrate

unadjustedVEPrate <- (turnout$total[14] / turnout$VEP[14])
unadjustedVEPrate

unadjustedANES <- (turnout$ANES[14] / 100)
unadjustedANES

turnoutrates <- c(adjustedVAPrate, unadjustedVAPrate, unadjustedVEPrate, unadjustedANES)
turnoutrates
names(turnoutrates) <- c("Adjusted VAP", "Unadjusted VAP", "Unadjusted VEP", "Unadjusted ANES")
turnoutrates
summary(turnoutrates)
# Adjusted VAP and unadjusted VEP are very similar, which makes sense because you
# would think VEP would be VAP minus ineligible voters. I'm curious to know why 
# they aren't exactly the same. ANES rates are still much higher than the others. 
# I would think the voting rate of number of ballots calculated with VEP or adjusted
# VAP would be most accurate. Reading about ANES rates, they seem to be based on
# polling data, but if we know the number of ballots cast and we know the total 
# number of people who are eligible to vote, I do wonder what utility the ANES
# rate has. 