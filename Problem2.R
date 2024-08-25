#Problem 2
#loading the data
shoe <- read.csv("C://Users//HP//Desktop//AP//ProblemSet_2//shoe.csv", header = FALSE)
which(is.na(shoe))
shoe <- shoe[c(3:28),]
names(shoe) <- shoe[1,]
row.names(shoe) <- shoe[,1]
Model1 <- data.matrix(shoe[c(-1),c(2:6)])
Model2 <- data.matrix(shoe[c(-1),c(8:12)])
which(is.na(Model1))
which(is.na(Model2))



#(a)
#calculation of yi's
Y <- Model1 - Model2
Ybar <- apply(Y, 2, mean)
Ybar

#Performing paired Hotelling's T-square Test
S <- cov(Y)
p <- ncol(Y)
n <- nrow(Y)
alpha <- 0.05

HT2 <- n * t(Ybar) %*% solve(S) %*% (Ybar)
Test.Statistic <- HT2 * (n-p)/ (p*(n-1))
Test.Statistic
Critical.Value <- qf(1-alpha, p, n-p)
Critical.Value

#Test using ICSNP
library(ICSNP)
HotellingsT2(Y, test='f')


#(b)
#Calculating Simultaneous Interval
MargSC <- sqrt(p * (n-1) * qf(1-alpha, p, (n-p)) / (n-p)) * sqrt(diag(S) / n)
MargSC <- as.matrix(MargSC)
SCI <- data.frame('lower.bound' = as.vector(Ybar - MargSC), 
                  'upper bound' = Ybar + MargSC)
SCI

#Calculating Bonferoni Interval
MargBI <- qt(1-(alpha / (2*p)), n-1) * sqrt(diag(S) / n)
MargBI <- as.matrix(MargBI)
BI <- data.frame('lower bound' = as.vector(Ybar - MargBI),
                 'upper bound' = Ybar + MargBI)
BI

#Profile plot for Simultaneous CI
ResultS <- data.frame(Nutrients = c(1,2,3,4,5),
                     Ratio = (SCI$lower.bound + SCI$upper.bound)/2,
                     Lower_Bound = SCI$lower.bound,
                     Upper_Bound = SCI$upper.bound)
library(plotrix)
plotCI(x = ResultS$Nutrients, y = ResultS$Ratio, 
       li = ResultS$Lower_Bound, ui = ResultS$Upper_Bound,
       col ='red', scol='black',
       xlab = "Criteria", ylab = "Value", 
       main="Profile Plot for Simultaneous CI")
abline(h=1, v=0, col='blue')

#Profile plot for Bonferoni CI
ResultB <- data.frame(Nutrients = c(1,2,3,4,5),
                      Ratio = (BI$lower.bound + BI$upper.bound)/2,
                      Lower_Bound = BI$lower.bound,
                      Upper_Bound = BI$upper.bound)
library(plotrix)
plotCI(x = ResultB$Nutrients, y = ResultB$Ratio, 
       li = ResultB$Lower_Bound, ui = ResultB$Upper_Bound,
       col ='red', scol='black',
       xlab = "Criteria", ylab = "Value", 
       main="Profile Plot for Bonferoni's CI")
abline(h=1, v=0, col='blue')