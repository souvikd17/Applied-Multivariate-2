##Problem5

#Importing and cleaning data
study <- read.csv("C://Users//HP//Desktop//AP//ProblemSet_2//study.csv")
which(is.na(study))

#performing two way MANOVA
model <- manova(cbind(kindness, optimism) ~ gender + economic + gender*economic, data = study)

#test results
summary(model, test = 'Pillai')
summary(model, test = 'Wilks')
summary(model, test = 'Roy')
summary(model, test = 'Hotelling-Lawley')