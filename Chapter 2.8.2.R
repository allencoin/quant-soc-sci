############################ Chapter 2.8.2 Problem Set / Allen Coin ############################

library(readr)
gay <- read_csv("Documents/qss/CAUSALITY/gay.csv")
View(gay)

dim(gay)
summary(gay)

############################ Question 1 ############################
# Using baseline interview wage, examine whether randomization
# was properly conducted. Base analysis on 3 groups from study 1

unique(gay$study)

Study1 <- subset(gay, subset = (study == 1))
summary(Study1)

Study1$treatment <- as.factor(Study1$treatment)
summary(Study1)

Study2 <- subset(gay, subset = (study == 2))
summary(Study2)

Study2$treatment <- as.factor(Study2$treatment)
summary(Study2)

Study1Wave1 <- subset(Study1, subset = (wave == 1))
summary(Study1Wave1)

Study1Wave1Table <- tapply(Study1Wave1$ssm, Study1Wave1$treatment, mean)
Study1Wave1ControlMean <- mean(Study1Wave1$ssm[Study1Wave1$treatment == "No Contact"])
Study1Wave1ControlMean

Study1Wave1TableRandomizationTest <- Study1Wave1Table - Study1Wave1ControlMean
Study1Wave1TableRandomizationTest

# I'm not sure if I'm looking at this data correctly, but it appears that 
# gay canvassers have more success with the recycling script than straight canvassers, 
# but it's actually the opposite with the same-sex marriage script 

# What was the effect of the recycling script on opinions about SSM?
RecyclingEffect <- mean(Study1Wave1TableRandomizationTest[2], Study1Wave1TableRandomizationTest[3])
RecyclingEffect

# What was the effect of the SSM script on opinions about SSM? 
SSMEffect <- mean(Study1Wave1TableRandomizationTest[4], Study1Wave1TableRandomizationTest[5])
SSMEffect

# Adjusted for the control, the recycling script had a very slightly higher positive effect
# on opinions about SSM. I would expect the recycling script to have no effect, but 
# I also am not sure if this is a statistically significant finding. 

SSMEffect - RecyclingEffect
# Respondents who received the SSM script from either gay or straight canvassers
# Expressed an opinion about SSM that was about 0.02 points lower than those who
# received the Recycling script. Now that I think about it, if the goal is to test
# whether contact with a gay canvasser effects opinions about SSM, then averaging the 
# gay and straight canvassers and the effect from recycling and SSM scripts should be 
# basically the same, and 0.02 is a very small difference. 


############################ Question 2 ############################

# The second wave of the survey was implemented two months after canvassing. 
# Using study 1, estimate the average treatment effects of gay and straight canvassers
# on support for same-sex marriage, separately. Give a brief interpretation. 
# 
# I'm interpreting this question as asking me to measure the differences between 
# wave 1 and wave 2. 
# My interpretation is "Compare attitudes about SSM in wave 2 based on how people
# responded to wave 2 depending on whether they had a gay or straight canvassser

Study1Wave2 <- subset(Study1, subset = (wave == 2))
summary(Study1Wave2)

# I think I need to conduct the same analysis I did for Wave 1, then compare 
# Wave 2 to Wave 1

Study1Wave2Table <- tapply(Study1Wave2$ssm, Study1Wave2$treatment, mean)
Study1Wave2Table
Study1Wave2ControlMean <- mean(Study1Wave2$ssm[Study1Wave2$treatment == "No Contact"])
Study1Wave2ControlMean

Study1Wave2ControlMean - Study1Wave1ControlMean
# The control group decreased slightly during wave 2

Study1Wave2TableRandomizationTest <- Study1Wave2Table - Study1Wave2ControlMean
Study1Wave2TableRandomizationTest

Study1vsStudy2Table <- Study1Wave2TableRandomizationTest - Study1Wave1TableRandomizationTest
Study1vsStudy2Table
# During wave 2, it looks like the SSM opinions of those who received the recycling
# script decreased very slightly, whereas the opinions of those who received the SSM script
# increased slightly, with the gay canvassers with the SSM script having better results
# than the straight canvassers with the SSM script

RecyclingEffectStudy2 <- mean(Study1vsStudy2Table[2], Study1vsStudy2Table[3])
RecyclingEffectStudy2

SSMEffectStudy2 <- mean(Study1vsStudy2Table[4], Study1vsStudy2Table[5])
SSMEffectStudy2

# The SSM script had a positive effect in wave 2 over wave 1, while the recycling
# script had a very slight, almost zero, negative effect. This makes me think the
# SSM script is doing something. 

GayCanvasserEffect <- mean(Study1vsStudy2Table[2], Study1vsStudy2Table[4])
GayCanvasserEffect

StraightCanvasserEffect <- mean(Study1vsStudy2Table[3], Study1vsStudy2Table[5])
StraightCanvasserEffect

DiffEffectGayStraight <- GayCanvasserEffect - StraightCanvasserEffect
DiffEffectGayStraight

# I'm looking at just the effect of the orientation of the canvasser here. 
# It looks like having a straight canvasser actually negatively impacted opinions
# about SSM less than having a gay canvasser, which seems to be contrary to the 
# hypothesis about the contact effect

Study1vsStudy2Table

SSMGayEffectAdjusted <-Study1vsStudy2Table[4] - Study1vsStudy2Table[2]
SSMGayEffectAdjusted

# I think I should adjust for the script used? 

SSMStraightEffectAdjusted <- Study1vsStudy2Table[5] - Study1vsStudy2Table[3]
SSMStraightEffectAdjusted

SSMGayEffectAdjusted - SSMStraightEffectAdjusted

# So, gay canvassers were slightly more effective at improving opinions about
# SSM by using the SSM script than their straight counterparts, adjusted for 
# the effect of using the recycling script

############################ Question 3 ############################

# I think I accidentally answered most of this question, or at least did most 
# of the groundwork, in the previous question. 

Study1Wave2Table

Study1Wave2Table[1]

Study1Wave2TableAdjusted <- Study1Wave2Table - Study1Wave2Table[1]
Study1Wave2TableAdjusted

# Compare outcomes from the treatement SSMGay to RecyclingGay, repeat the same for straight

Study1Wave2TableAdjusted[4] - Study1Wave2TableAdjusted[2]

Study1Wave2TableAdjusted[5] - Study1Wave2TableAdjusted[3]

# This makes it seem that, during wave 2, straight canvassers had better luck
# improving opinions about SSM than their gay counterparts when we adjust for the
# type of script used. 

############################ Question 4 ############################

# Separately compute the average effects of straight and gay canvassers with
# the SSM script for each of the subsequent waves (relateive to the control condition)

# Go back to the first wave
Study1Wave1Table

# Adjust for the control
Study1Wave1TableAdjusted <- Study1Wave1Table - Study1Wave1Table[1]
Study1Wave1TableAdjusted

# I already have wave 2
Study1Wave2TableAdjusted
Study1Wave2vsStudy1Wave1 <- Study1Wave2TableAdjusted - Study1Wave1TableAdjusted
Study1Wave2vsStudy1Wave1
Question4Wave2 <- (Study1Wave2vsStudy1Wave1[4] - Study1Wave2vsStudy1Wave1[2]) - 
  (Study1Wave2vsStudy1Wave1[5] - Study1Wave2vsStudy1Wave1[3])
Question4Wave2

# get wave3
Study1Wave3 <- subset(Study1, subset = (wave == 3))
summary(Study1Wave3)
Study1Wave3Table <- tapply(Study1Wave3$ssm, Study1Wave3$treatment, mean)
Study1Wave3Table
Study1Wave3TableAdjusted <- Study1Wave3Table - Study1Wave3Table[1]
Study1Wave3TableAdjusted
Study1Wave3vsStudy1Wave1 <- Study1Wave3TableAdjusted - Study1Wave1TableAdjusted
Study1Wave3vsStudy1Wave1
Question4Wave3 <- (Study1Wave3vsStudy1Wave1[4] - Study1Wave3vsStudy1Wave1[2]) - 
  (Study1Wave3vsStudy1Wave1[5] - Study1Wave3vsStudy1Wave1[3])
Question4Wave3

# get Wave4
Study1Wave4 <- subset(Study1, subset = (wave == 4))
summary(Study1Wave4)
Study1Wave4Table <- tapply(Study1Wave4$ssm, Study1Wave4$treatment, mean)
Study1Wave4Table
Study1Wave4TableAdjusted <- Study1Wave4Table - Study1Wave4Table[1]
Study1Wave4TableAdjusted
Study1Wave4vsStudy1Wave1 <- Study1Wave4TableAdjusted - Study1Wave1TableAdjusted
Study1Wave4vsStudy1Wave1
Question4Wave4 <- (Study1Wave4vsStudy1Wave1[4] - Study1Wave4vsStudy1Wave1[2]) - 
  (Study1Wave4vsStudy1Wave1[5] - Study1Wave4vsStudy1Wave1[3])
Question4Wave4

# get Wave5
Study1Wave5 <- subset(Study1, subset = (wave == 5))
summary(Study1Wave5)
Study1Wave5Table <- tapply(Study1Wave5$ssm, Study1Wave5$treatment, mean)
Study1Wave5Table
Study1Wave5TableAdjusted <- Study1Wave5Table - Study1Wave5Table[1]
Study1Wave5TableAdjusted
Study1Wave5vsStudy1Wave1 <- Study1Wave5TableAdjusted - Study1Wave1TableAdjusted
Study1Wave5vsStudy1Wave1
Question4Wave5 <- (Study1Wave5vsStudy1Wave1[4] - Study1Wave5vsStudy1Wave1[2]) - 
  (Study1Wave5vsStudy1Wave1[5] - Study1Wave5vsStudy1Wave1[3])
Question4Wave5

# get Wave6
Study1Wave6 <- subset(Study1, subset = (wave == 6))
summary(Study1Wave6)
Study1Wave6Table <- tapply(Study1Wave6$ssm, Study1Wave6$treatment, mean)
Study1Wave6Table
Study1Wave6TableAdjusted <- Study1Wave6Table - Study1Wave6Table[1]
Study1Wave6TableAdjusted
Study1Wave6vsStudy1Wave1 <- Study1Wave6TableAdjusted - Study1Wave1TableAdjusted
Study1Wave6vsStudy1Wave1
Question4Wave6 <- (Study1Wave6vsStudy1Wave1[4] - Study1Wave6vsStudy1Wave1[2]) - 
  (Study1Wave6vsStudy1Wave1[5] - Study1Wave6vsStudy1Wave1[3])
Question4Wave6

# get Wave7
Study1Wave7 <- subset(Study1, subset = (wave == 7))
summary(Study1Wave7)
Study1Wave7Table <- tapply(Study1Wave7$ssm, Study1Wave7$treatment, mean)
Study1Wave7Table
Study1Wave7TableAdjusted <- Study1Wave7Table - Study1Wave7Table[1]
Study1Wave7TableAdjusted
Study1Wave7vsStudy1Wave1 <- Study1Wave7TableAdjusted - Study1Wave1TableAdjusted
Study1Wave7vsStudy1Wave1
Question4Wave7 <- (Study1Wave7vsStudy1Wave1[4] - Study1Wave7vsStudy1Wave1[2]) - 
  (Study1Wave7vsStudy1Wave1[5] - Study1Wave7vsStudy1Wave1[3])
Question4Wave7

# Put these in an array
SSMOverTime <- c(Question4Wave2, Question4Wave3, Question4Wave4, Question4Wave5, Question4Wave6, Question4Wave7)
SSMOverTime
names(SSMOverTime) <- c("Wave 2", "Wave 3", "Wave 4", "Wave 5", "Wave 6", "Wave 7")
SSMOverTime
plot(SSMOverTime)

# After the treatment, the effects seem to peak at Wave 4 , then diminish 
# over time back to there being virtually no effect. I am not sure what to make
# of this, or if I did this right! 

############################ Question 5 ############################

#This seems to be basically the same question as Question 2, but for Study 2

summary(Study2)

Study2Wave1 <- subset(Study2, subset = (wave == 1))
summary(Study2Wave1)

Study2Wave1Table <- tapply(Study2Wave1$ssm, Study2Wave1$treatment, mean)
Study2Wave1Table
#Study2Wave1ControlMean <- mean(Study2Wave1$ssm[Study2Wave1$treatment == "No Contact"])
#Study2Wave1ControlMean
# This can be simplified now

Study2Wave1TableRandomizationTest <- Study2Wave1Table - Study2Wave1Table[1]
Study2Wave1TableRandomizationTest

# The difference is neglible, which makes me think the randomization was good. 

############################ Question 6 ############################

Study2Wave2 <- subset(Study2, subset = (wave == 2))
summary(Study2Wave2)

Study2Wave2Table <- tapply(Study2Wave2$ssm, Study2Wave2$treatment, mean)
Study2Wave2Table

S2W2vS2W1 <- Study2Wave2Table - Study2Wave1Table
S2W2vS2W1

S2W2vS2W1Adjusted <- S2W2vS2W1 - S2W2vS2W1[1]
S2W2vS2W1Adjusted

S2W2vS2W1Adjusted/5

# When you filter out the control, the treatment effect is about 2.4%

Study1Wave2vsStudy1Wave1[4] / 5

# This is consistent with Study 1, which was also about 2.4% 

############################ Question 7 ############################

unique(Study2$wave)
Study2Wave1Baseline <- mean(Study2$ssm[Study2$treatment=="Same-Sex Marriage Script by Gay Canvasser" & Study2$wave==1]) - mean(Study2$ssm[Study2$treatment=="No Contact" & Study2$wave==1])

Q7W2 <- mean(Study2$ssm[Study2$treatment=="Same-Sex Marriage Script by Gay Canvasser" & Study2$wave==2]) - mean(Study2$ssm[Study2$treatment=="No Contact" & Study2$wave==2]) - Study2Wave1Baseline
Q7W3 <- mean(Study2$ssm[Study2$treatment=="Same-Sex Marriage Script by Gay Canvasser" & Study2$wave==3]) - mean(Study2$ssm[Study2$treatment=="No Contact" & Study2$wave==3]) - Study2Wave1Baseline
Q7W4 <- mean(Study2$ssm[Study2$treatment=="Same-Sex Marriage Script by Gay Canvasser" & Study2$wave==4]) - mean(Study2$ssm[Study2$treatment=="No Contact" & Study2$wave==4]) - Study2Wave1Baseline
Q7W5 <- NULL
Q7W6 <- NULL
Q7W7 <- mean(Study2$ssm[Study2$treatment=="Same-Sex Marriage Script by Gay Canvasser" & Study2$wave==7]) - mean(Study2$ssm[Study2$treatment=="No Contact" & Study2$wave==7]) - Study2Wave1Baseline

Q7Array <- c(Q7W2, Q7W3, Q7W4, Q7W7)
Q7Array
names(Q7Array) <- c("Wave2", "Wave 3", "Wave 4", "Wave 7")
plot(Q7Array)
Q7Array

# It looks like it remained pretty flat, but then suddenly during wave 7 the effect was way more prominent

SSMOverTime
Q7Array

mean(Q7Array) - mean(SSMOverTime)

Q7Array[1] - SSMOverTime[1]
Q7Array[2] - SSMOverTime[2]
Q7Array[3] - SSMOverTime[3]
Q7Array[4] - SSMOverTime[6]

# The results of Study 2 found a mean treatment effect that was about .12 points higher. 
# This seems like a pretty big jump up from the findings of Study 1. 
# Study 2 in general found a higher treatment effect. 