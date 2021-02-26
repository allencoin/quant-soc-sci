library(readr)
leaders <- read_csv("Documents/qss/CAUSALITY/leaders.csv")
View(leaders)

#################### Chapter 2.8.3 Problem Set / Allen Coin ####################

############################ Question 1 ############################
# How many assassination attempts are recorded in the data? 

nrow(leaders)
# 250 attempts

# How many countries experience at least one leader assassination attempt? 
countries <- unique(leaders$country)
length(countries)
# 88 countries

# What is the average number of such attempts per year among these countries? 
years <- unique(leaders$year)
years
length(years)

nrow(leaders) / length(years)
# 2.45 assassination attempts per year

############################ Question 2 ############################
# Create a new binary variable called success
unique(leaders$result)
# so we want "dies between a day and a week", "dies within a day after the attack",
# "dies, timing unknown", and "dies between a week and a month" 

# I'd like to do "make this equal 1 if this other thing contains this string, 
# but I don't think we have covered how to do that yet
leaders$success <- 0
leaders$success[leaders$result == "dies between a day and a week"] <- 1
leaders$success[leaders$result == "dies within a day after the attack"] <- 1
leaders$success[leaders$result == "dies, timing unknown"] <- 1
leaders$success[leaders$result == "dies between a week and a month"] <- 1

mean(leaders$success)
# The success rate is 21.6%. 

leaders$success <- as.double(leaders$success)

successes <- subset(leaders, subset=(leaders$success==1))
summary(successes)
length(successes)
# just verifying this is correct
nrow(successes) / nrow(leaders)

# Does the success rate  speak to the assumption that the success of assassination attempts
# is randomly determined? 

# I'm not too sure what this question is asking, mostly because it doesn't make
# sense to me that the success rate would be random. There are a lot of factors, 
# just like the "success" rate of any murder wouldn't be random — there would be 
# the availability of emergency medical care involved, which would be readily available
# for national leaders, and, for instance, getting shot isn't going to have a 
# completely random death rate, and other forms of murder wouldn't have the same
# death rates, so I don't really understand how the results could possibly be random. 
# I would expect the success rate to be 50% success or fail if it's a binary 
# and it was truly random. 


############################ Question 3 ############################

# investigate whether the polity score over three years prior to an assassination
# attempt differs on average between successful and failed attempts

failures <- subset(leaders, subset=(leaders$success==0))
mean(leaders$politybefore)
mean(successes$politybefore)
mean(failures$politybefore)

# The mean politybefore for successes is -0.7 while the mean politybefore for
# failures is -1.7. This suggests countries in which an asssassination fails
# have a lower polity score by one point on the 20-point scale. 
# The average politybefore an assassination attempt is -1.52. 

# also age
mean(leaders$age)
mean(successes$age)
mean(failures$age)
range(leaders$age)
range(successes$age)
range(failures$age)
# The average age of all the leaders in the dataset is 53.5. The average age at a 
# successful assassination attempt is 56.5, while the average age at a failed assassination
# attempt is 52.7. This suggests that age is a factor in whether the attempt is successful,
# which makes sense colloquially — an older person is less likely to recover. 

# Both of these suggest that assassination attempts are more likely to be successful
# if the average politybefore an attempt is lower than average, and if the age of the 
# leader is lower than average. This makes sense if you consider that this is describing
# a dictator who is younger and has greater totalitarian control over the country. 

############################ Question 4 ############################

leaders$warbefore <- 0
leaders$warbefore <- as.double(leaders$warbefore)
leaders$warbefore[leaders$civilwarbefore == 1] <- 1
leaders$warbefore[leaders$interwarbefore == 1] <- 1

# have to redo the successes and failures to include this
successes <- subset(leaders, subset=(leaders$success==1))
failures <- subset(leaders, subset=(leaders$success==0))

mean(leaders$warbefore)
mean(successes$warbefore)
mean(failures$warbefore)

# The mean warbefore for all attempts is 0.368. The mean warbefore for successes
# is 0.352. The mean warbefore for failures is 0.372. This seems to suggest that
# an assassination attempt is slightly more likely to be successful if there isn't 
# a war 3 years before the attempt. This makes some colloquial sense because
# you would expect a country to be on "high alert" with hieghtened security
# when the country is at war or has just been at war. 

############################ Question 5 ############################

# Does successful leader assassination cause democratization? 
mean(leaders$politybefore)
mean(leaders$polityafter)
mean(leaders$polityafter) - mean(leaders$politybefore)
# For all assassination attempts, polity tends to fall by .13 points
# after an attempt. So any assassination attempt tends to decrease
# democracy in the country. 

mean(successes$politybefore)
mean(successes$polityafter)
mean(successes$polityafter) - mean(successes$politybefore)
# For successful assassination attempts, polity still falls but by 0.06 points

mean(failures$politybefore)
mean(failures$polityafter)
mean(failures$polityafter) - mean(failures$politybefore)
# For failed attempts, polity decreases by 0.15 points, so a failed attempt 
# has a bigger affect on decreasing polity than a successful attempt, but polity
# still tends to fall. Does that mean the best thing for increasing polity
# is to not attempt an assassination? 
# Basically, "if you're gonna come at the king, you best not miss." 


# Does successful leader assassination lead countries to war?
leaders$warafter <- 0
leaders$warafter <- as.double(leaders$warafter)
leaders$warafter[leaders$civilwarafter == 1] <- 1
leaders$warafter[leaders$interwarafter == 1] <- 1

successes <- subset(leaders, subset=(leaders$success==1))
failures <- subset(leaders, subset=(leaders$success==0))

mean(leaders$warbefore)
mean(leaders$warafter)
mean(leaders$warafter) - mean(leaders$warbefore)
# For all assassination attempts, the likelihood that the country will go to 
# war afterwards falls by 0.092 points. 

mean(successes$warbefore)
mean(successes$warafter)
mean(successes$warafter) - mean(successes$warbefore)
# With a successful assassination attempt, the likelihood that a country will go to 
# war falls more than average, by 0.148 points

mean(failures$warbefore)
mean(failures$warafter)
mean(failures$warafter) - mean(failures$warbefore)
# A failed assassination attempt makes it more likely than average that a country
# will go to war, with a rate of warafter of 0.077 points lower
