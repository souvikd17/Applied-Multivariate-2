##Problem 1

#Importing and cleaning the given data
Nutrient_data <- read.csv("C://Users//HP//Desktop//AP//ProblemSet_2//nutrient_data.csv")
which(is.na(Nutrient_data))
Nutrient_data <- Nutrient_data[,-1]

#Rechecking given means
xbar <- sapply(Nutrient_data, mean)
xbar

#Performing Hotelling T2 test manually
S <- cov(Nutrient_data)
mu0 <- c(1000, 15, 60, 800, 75)
p <- ncol(Nutrient_data)
n <- nrow(Nutrient_data)
alpha <- 0.01

HT2 <- n * t(xbar - mu0) %*% solve(S) %*% (xbar - mu0)
Test.Statistic <- HT2 * (n-p)/ (p*(n-1))
Test.Statistic
Critical.Value <- qf(1-alpha, p, n-p)
Critical.Value

#Performing Hotelling T2 test using ICSNP
library(ICSNP)
test <- HotellingsT2(Nutrient_data, mu=mu0, test='f')
test

#library(Hotelling)

#Calculating Simultaneous Interval
MargSC <- sqrt(p * (n-1) * qf(1-alpha, p, (n-p)) / (n-p)) * sqrt(diag(S) / n)
MargSC <- as.matrix(MargSC)
SCI <- data.frame('lower.bound' = as.vector(xbar - MargSC), 
                  'upper bound' = xbar + MargSC)
SCI

#Calculating Bonferoni Interval
MargBI <- qt(1-(alpha / (2*p)), n-1) * sqrt(diag(S) / n)
MargBI <- as.matrix(MargBI)
BI <- data.frame('lower bound' = as.vector(xbar - MargBI),
                 'upper bound' = xbar + MargBI)
BI

#Profile Plot for CIs
#standardizing observations
st_data <- Nutrient_data / mu0
#finding means
new_mean <- sapply(st_data, mean)
new_mean

#Profile plot for Simultaneous CI
RatioS <- SCI / mu0
Result <- data.frame(Nutrients = c(1,2,3,4,5),
                     Ratio = (RatioS$lower.bound + RatioS$upper.bound)/2,
                     Lower_Bound = RatioS$lower.bound,
                     Upper_Bound = RatioS$upper.bound)
library(plotrix)
plotCI(x = Result$Nutrients, y = Result$Ratio,
       li = Result$Lower_Bound, ui = Result$Upper_Bound,
       col ='red', scol='black',
       xlab = "Nutrients", ylab = "Ratio",
       main = "Profile Plot for Simultaneous CI")
abline(h=1, v=0, col = 'blue')

#Profile plot for Bonferoni CI
RatioB <- BI/mu0
ResultB <- data.frame(Nutrients = c(1,2,3,4,5),
                      Ratio = (RatioB$lower.bound + RatioB$upper.bound)/2,
                      Lower_Bound = RatioB$lower.bound,
                      Upper_Bound = RatioB$upper.bound)
library(plotrix)
plotCI(x = ResultB$Nutrients, y = ResultB$Ratio,
       li = ResultB$Lower_Bound, ui = ResultB$Upper_Bound, 
       col ='red', scol='black',
       xlab = "Nutrients", ylab = "Ratio",
       main="Profile Plot for Bonferoni's CI")
abline(h=1, v=0, col = 'blue')

#library(ggplot2)
#ggplot(Result, aes(x=Nutrients, y=Ratio) +
 #        geom_point(size=4) +
 #        geom_errorbar(aes(ymax = Upper_Bound, ymin = Lower_Bound))).
