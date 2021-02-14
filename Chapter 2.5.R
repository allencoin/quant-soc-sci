library(readr)
minwage <- read_csv("Documents/qss/CAUSALITY/minwage.csv")
View(minwage)

dim(minwage)
summary(minwage)

unique(minwage$location)

minwageNJ <- subset(minwage, subset = (location != "PA"))
minwagePA <- subset(minwage, subset = (location == "PA"))

# Check if restaurants are following the law

NJWageBefore <- mean(minwageNJ$wageBefore < 5.05)
NJWageAfter <- mean(minwageNJ$wageAfter < 5.05)

PAWageBefore <- mean(minwagePA$wageBefore <5.05)
PAWageAfter <- mean(minwagePA$wageAfter <5.05)

minwageNJ$fullPropAfter <- minwageNJ$fullAfter / (minwageNJ$fullAfter + minwageNJ$partAfter)

minwagePA$fullPropAfter <- minwagePA$fullAfter / (minwagePA$fullAfter + minwagePA$partAfter)

DifferenceInMeans <- mean(minwageNJ$fullPropAfter - mean(minwagePA$fullPropAfter))
DifferenceInMeans

prop.table(table(minwageNJ$chain))

prop.table(table(minwagePA$chain))

minwageNJBK <- subset(minwageNJ, subset = (chain == "burgerking"))
minwagePABK <- subset(minwagePA, subset = (chain == "burgerking"))

mean(minwageNJBK$fullPropAfter) - mean(minwagePABK$fullPropAfter)

minwageNJBKsubset <- subset(minwageNJBK, subset = ((location != "shoreNJ") & (location != "centralNJ")))

mean(minwageNJBKsubset$fullPropAfter) - mean(minwagePABK$fullPropAfter)

minwageNJ$fullPropBefore <- minwageNJ$fullBefore / (minwageNJ$fullBefore + minwageNJ$partBefore)

NJdiff <- mean(minwageNJ$fullPropAfter) - mean(minwageNJ$fullPropBefore)
NJdiff

minwagePA$fullPropBefore <- minwagePA$fullBefore / (minwagePA$fullBefore + minwagePA$partBefore)

PAdiff <- mean(minwagePA$fullPropAfter) - mean(minwagePA$fullPropBefore)

NJPADiD <- NJdiff - PAdiff
NJPADiD

median(minwageNJ$fullPropAfter)
median(minwagePA$fullPropAfter)

median(minwageNJ$fullPropAfter) - median(minwagePA$fullPropAfter)

NJdiffMed <- median(minwageNJ$fullPropAfter) - median(minwageNJ$fullPropBefore)
NJdiffMed

PAdiffMed <- median(minwagePA$fullPropAfter) - median(minwagePA$fullPropBefore)
PAdiffMed

NJPAIDiDMed <- NJdiffMed - PAdiffMed #typo
NJPAIDiDMed

summary(NJdiffMed)

summary(minwageNJ$wageBefore)
summary(minwageNJ$wageAfter)
IQR(minwageNJ$wageBefore)
IQR(minwageNJ$wageAfter)

quantile(minwageNJ$wageBefore, probs = seq(from=0, to=1, by=0.1))
quantile(minwageNJ$wageAfter, probs = seq(from=0, to=1, by=0.1))

plot(quantile(minwageNJ$wageBefore, probs = seq(from=0, to=1, by=0.1)))
plot(quantile(minwageNJ$wageAfter, probs = seq(from=0, to=1, by=0.1)))

sqrt(mean((minwageNJ$fullPropAfter - minwageNJ$fullPropBefore)^2))
mean(minwageNJ$fullPropAfter - minwageNJ$fullPropBefore)

sd(minwageNJ$fullPropBefore)
sd(minwageNJ$fullPropAfter)

var(minwageNJ$fullPropBefore)
var(minwageNJ$fullPropAfter)
