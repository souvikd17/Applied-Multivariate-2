##Problem 3
#Importing and cleaning the given data
soil_data <- read.csv("C://Users//HP//Desktop//AP//ProblemSet_2//soil.csv")
which(is.na(soil_data))
names(soil_data)[1] <- 'SoilType'

#performing MANOVA
model <- manova(cbind(yield, water, herbicide) ~ SoilType, data = soil_data)
summary(model, test = 'Pillai')
summary(model, test = 'Wilks')
summary(model, test = 'Roy')
summary(model, test = 'Hotelling-Lawley')

#performing tests only on clay and salty
data1 <- soil_data[which(soil_data$SoilType == 'clay' | soil_data$SoilType == 'salty'),]
md1 <- manova(cbind(yield, water, herbicide) ~ SoilType, data = data1)
summary(md1)
summary(md1, test = 'Wilks')
summary(md1, test = 'Roy')
summary(md1, test = 'Hotelling-Lawley')

#performing tests only on loam and sandy
data2 <- soil_data[which(soil_data$SoilType == 'loam' | soil_data$SoilType == 'sandy'),]
md2 <- manova(cbind(yield, water, herbicide) ~ SoilType, data = data2)
summary(md2)
summary(md2, test = 'Wilks')
summary(md2, test = 'Roy')
summary(md2, test = 'Hotelling-Lawley')

#performing tests for clay and salty VS loam and sandy
data3 <- soil_data
data3[which(soil_data$SoilType == 'clay' | soil_data$SoilType == 'salty'), 1] <- 'Group1'
data3[which(soil_data$SoilType == 'loam' | soil_data$SoilType == 'sandy'), 1] <- 'Group2'
md3 <- manova(cbind(yield, water, herbicide) ~ SoilType, data = data3)
summary(md3)
summary(md3, test = 'Wilks')
summary(md3, test = 'Roy')
summary(md3, test = 'Hotelling-Lawley')


##finding 95% simultaneous and bonferoni CI for individual means
Group1 <- data.matrix(data3[1:16,c(-1)])
Group2 <- data.matrix(data3[17:32,c(-1)])
Y <- Group1 - Group2
Ybar <- apply(Y, 2, mean)
S <- cov(Y)
p <- ncol(Y)
n <- nrow(Y)
alpha <- 0.01

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