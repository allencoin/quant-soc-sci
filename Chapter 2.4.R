library(readr)
social <- read.csv("Documents/qss/CAUSALITY/social.csv")
View(social)

summary(social)
social$sex <- as.factor(social$sex)
social$messages <- as.factor(social$messages)
summary(social)

TurnoutRatesPerMessage <- tapply(social$primary2006, social$messages, mean)
TurnoutRatesPerMessage

TurnoutRateControl <- mean(social$primary2006[social$messages == "Control"])
TurnoutRateControl

TurnoutRatesPerMessage - TurnoutRateControl
# This represents each number as the amount it is higher or lower than the control
# The neighbors message was highly convincing

social$age <- 2006 - social$yearofbirth
mean(social$age)

AgeOfMessageRecipient <- tapply(social$age, social$messages, mean)
AgeOfMessageRecipient
mean(social$age)

VotingRateOfMessageRecipients <- tapply(social$primary2004, social$messages, mean)
VotingRateOfMessageRecipients

mean(social$primary2004)

HouseholdSizeOfMessageRecipients <- tapply(social$hhsize, social$messages, mean)
HouseholdSizeOfMessageRecipients

# They did a good job randomizing

mean(social$hhsize)


