library(readr)
resume <- read_csv("Documents/qss/CAUSALITY/resume.csv")

cols(
  firstname = col_character(),
  sex = col_character(),
  race = col_character(),
  call = col_double()
)

View(resume)

dim(resume)

head(resume)

summary(resume)

racecalltable <- table(race=resume$race,call=resume$call)
racecalltable
summary(racecalltable)

addmargins(racecalltable)

racecalltablewithmargins <- addmargins(racecalltable)
racecalltablewithmargins

summary(racecalltablewithmargins)

#overall callback rate: total callbacks divided by the sample size
nrow(resume)
resumerows <- nrow(resume)
resumerows

racecalltable[,2]
racecalltable[,1]
racecalltable[,2] + racecalltable[,1]
totalcalls <- racecalltable[,2] + racecalltable[,1]
totalcalls

callbackratesblackvswhite <- racecalltable[,2] / totalcalls
callbackratesblackvswhite


blackpplcallbackrate <- callbackratesblackvswhite[1]
blackpplcallbackrate

whitepplcallbackrate <- callbackratesblackvswhite[2]
whitepplcallbackrate

overallcallbackrate <- sum(racecalltable[,2]) / nrow(resume)
overallcallbackrate

racecalltable[1,2]
racecalltable[1,]
sum(racecalltable[1,])

blackpplcallbackrate2 <- racecalltable[1,2] / sum(racecalltable[1,])
blackpplcallbackrate2
blackpplcallbackrate

racecalltable[2,2]
racecalltable[1,]
sum(racecalltable[1,])

whitepplcallbackrate2 <- racecalltable[2,2] / sum(racecalltable[1,])
whitepplcallbackrate2
whitepplcallbackrate

mean(resume$call)

resume$call[resume$race == "black"]
sum(resume$call[resume$race == "black"])

summary(resume)

mean(resume$call[resume$race == "black"])

resume$race[1:5]
(resume$race == "black")[1:5]

resumeswhite <- resume[resume$race == "white",]
resumesblack <- resume[resume$race == "black",]

resumeswhitemales <- resumeswhite[resumeswhite$sex == "male",]
resumeswhitefemales <- resumeswhite[resumeswhite$sex == "female",]
resumesblackmales <- resumesblack[resumesblack$sex == "male",]
resumesblackfemales <- resumesblack[resumesblack$sex == "female",]
summary(resumesblackfemales)

nrow(resumesblackfemales)

sum(resumesblackfemales$call == 1) / nrow(resumesblackfemales)
sum(resumeswhitemales$call == 1) / nrow(resumesblackmales)
sum(resumesblackmales$call == 1) / nrow(resumesblackmales)
sum(resumeswhitefemales$call ==1) / nrow(resumeswhitefemales)

resumesmale <- resume[resume$sex == "male",]
resumesfemale <- resume[resume$sex == "female",]

sum(resumesmale$call ==1) / nrow(resumesmale)
sum(resumesfemale$call ==1)/ nrow(resumesfemale)

resume$type <- NA
resume$type

resume$type[resume$race=="black" & resume$sex=="female"] <- "BlackFemale"
resume$type[resume$race=="black" & resume$sex=="male"] <- "BlackMale"
resume$type[resume$race=="white" & resume$sex =="male"] <- "WhiteMale"
resume$type[resume$race=="white" & resume$sex=="female"] <- "WhiteFemale"
summary(resume)
levels(resume$type)
resume$type <- as.factor(resume$type)
summary(resume)
resume$type == "BlackMale"
nrow(resume[resume$type=="BlackMale",])
levels(resume$type)

ResumeTypeTable <- table(resume$type)
ResumeCallbackRateTable <- tapply(resume$call, resume$type, mean)
ResumeCallbackRateTable

ResumeCallbackRateTableGender <- tapply(resume$call, resume$sex, mean)
ResumeCallbackRateTableGender

ResumeCallbackRateTableRace <- tapply(resume$call, resume$race, mean)
ResumeCallbackRateTableRace

resume$firstname <- as.factor(resume$firstname)
resume$race <- as.factor(resume$race)
resume$sex <- as.factor(resume$sex)
summary(resume)

CallbackRateName <- tapply(resume$call, resume$firstname, mean)
CallbackRateName
sort(CallbackRateName)
unique(resume$firstname)

NamesUsed <- unique(resume$firstname)
NamesUsed
length(NamesUsed)
NamesUsedTable <- table(NamesUsed)
NamesUsedTable

CallbackRateName

