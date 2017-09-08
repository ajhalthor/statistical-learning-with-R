
college = read.csv("Datasets/College.csv")
head(college) #Use fix(college) in R-Studio to display in internal beditor

names(college)

rownames(college)[1:10] #Display the first 10 row names

rownames(college) = college[ , 1]
head(college)

college = college[ , -1] #Exclude first column
head(college)

summary(college)

pairs(college[,1:10])

plot(college$Private, college$Outstate, xlab="Public/Private Indicator", ylab="Out of State Tuition($)", main="Boxplot of Outstate Vs. Private")

Elite = rep("No", length(rownames(college))) #Initialize all entries of Elite to 'No'
Elite[college$Top10perc > 50] = "Yes" #If Top10Perc > 50, assign field as 'Yes'
Elite = as.factor(Elite)
college = data.frame(college, Elite)

head(college)

summary(college$Elite)

plot(college$Elite, college$Outstate, ylab="Out of State Tuition ($)", xlab="Is the University Elite?",main="Elite Vs. Outstate")

par(mfrow=c(2,2))
for (numBins in 1:20){
    hist(college$Outstate, breaks=numBins, xlab="OutState", ylab="Freq", main=paste("Bins = ",numBins))
}

par(mfrow=c(2,2))
for (numBins in 1:20){
    hist(college$Books, breaks=numBins, xlab="Book Cost", ylab="Freq", main=paste("Bins = ",numBins))
}

par(mfrow=c(5,2))
for (numBins in 1:20){
    hist(college$Books, breaks=numBins, xlab="Book Cost", ylab="Freq", main=paste("Bins = ",numBins))
}
