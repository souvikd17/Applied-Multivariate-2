##Problem 3
#Importing and cleaning the given data
Drug_data <- read.csv("C://Users//HP//Desktop//AP//ProblemSet_2//drug.csv")
which(is.na(Drug_data))

for (i in 1:ncol(Drug_data))
{
  Drug_data[,i] = as.numeric(Drug_data[,i])
}

data <- Drug_data[c(-1,-2),]
names(data) <- Drug_data[2,]
row.names(data) <- c(1:20)
Drug <- data.frame(data[,c(1:3)])
Placebo <- data.frame(data[,c(5:7)])
Placebo <- na.omit(Placebo)
which(is.na(Drug))
which(is.na(Placebo))


#(a)Population covariance matrix is equal
alpha <- 0.05
p <- ncol(Drug)
n1 <- nrow(Drug)
n2 <- nrow(Placebo)
x1bar <- apply(Drug, 2, mean)
x2bar <- apply(Placebo, 2, mean)
S1 <- cov(Drug)
S2 <- cov(Placebo)
Sp <- ((n1 - 1) * S1 + (n2 - 1) * S2)/(n1 + n2 - 2)

HT2 <- t(x1bar - x2bar) %*% solve(Sp * ((1/n1) + (1/n2))) %*% (x1bar - x2bar)
test_statistic <- (n1 + n2 - p - 1)/(p * (n1 + n2 - 2)) * HT2
test_statistic
critical_value <- qf(1-alpha, p, (n1 + n2 - p - 1))
critical_value



#(b)Population covariance matrix is not equal
St <- (S1 * (1/n1) + S2 * (1/n2))
HT2UN <- t(x1bar - x2bar) %*% solve(St) %*% (x1bar - x2bar)
test_statisticUN <- (n1 + n2 - p - 1)/(p * (n1 + n2 - 2)) * HT2UN
test_statisticUN

A <- t(x1bar - x2bar) %*% solve(St) %*% ((1/n1)*S1) %*% solve(St) %*% (x1bar - x2bar)
B <- t(x1bar - x2bar) %*% solve(St) %*% ((1/n2)*S2) %*% solve(St) %*% (x1bar - x2bar)
v <- (1/(n1-1))*(A/HT2UN)^2 + (1/(n2-1))*(B/HT2UN)^2
criticalUN <- qf(1-alpha, p, 1/v)
criticalUN

#Using ICSNP
library(ICSNP)
HotellingsT2(Drug, Placebo, test='f')
