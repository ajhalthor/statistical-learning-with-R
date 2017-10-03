
# I uploaded the  my response to the ﻿applied questions ﻿﻿﻿﻿﻿(8 to 10) in jupyter notebooks at the following link

# http://nbviewer.jupyter.org/github/ajhalthor/statistical-learning-with-R/tree/master/Chapter%202/notebooks/

# Just click on the question numbers to see my notebooks on the response. I used jupyter becuase you can also see the output for the code dynamically for every statement. 

# Note: I just extracted the R code from the notebooks in this file. So there is no sub-numbering (a,b, etc.) This is there in the notebooks. 

# Question 8

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


# Question 9

Auto = read.csv("Datasets/Auto.csv", header=TRUE, na.strings="?")
head(Auto)

print(paste("Total Number of Sample cars = ", dim(Auto)[1]))
Auto=na.omit(Auto)
print(paste("Number of sample cars after deleting incomplete sample data = ", dim(Auto)[1]))

n_occur = data.frame(table(Auto$name)) # This is a Frequency Distribution Table
n_occur[n_occur$Freq > 1,] #Get only duplicate entries

car_names = Auto[,ncol(Auto)] #Store Car Names for later use
Auto = Auto[,-ncol(Auto)] #Remove the names column
head(Auto)

variable = c("mpg", "displacement", "horsepower", "weight", "acceleration", "year")
minimum = c(min(Auto$mpg), min(Auto$displacement), min(Auto$horsepower), min(Auto$weight), min(Auto$acceleration), min(Auto$year))
maximum = c(max(Auto$mpg), max(Auto$displacement), max(Auto$horsepower), max(Auto$weight), max(Auto$acceleration), max(Auto$year))
tab = data.frame(variable, minimum, maximum)
tab

range(Auto$mpg)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

summary(Auto)

mean = c(mean(Auto$mpg), mean(Auto$displacement), mean(Auto$horsepower), mean(Auto$weight), mean(Auto$acceleration), mean(Auto$year))
sd = c(sd(Auto$mpg), sd(Auto$displacement), sd(Auto$horsepower), sd(Auto$weight), sd(Auto$acceleration), sd(Auto$year))
tab = data.frame(variable, mean, sd)
tab

sub_Auto = Auto[-c(10:85),] #Remove 10th to 85th row, keep all columns

minimum = c(min(sub_Auto$mpg), min(sub_Auto$displacement), min(sub_Auto$horsepower), min(sub_Auto$weight), min(sub_Auto$acceleration), min(sub_Auto$year))
maximum = c(max(sub_Auto$mpg), max(sub_Auto$displacement), max(sub_Auto$horsepower), max(sub_Auto$weight), max(sub_Auto$acceleration), max(sub_Auto$year))
mean = c(mean(sub_Auto$mpg), mean(sub_Auto$displacement), mean(sub_Auto$horsepower), mean(sub_Auto$weight), mean(sub_Auto$acceleration), mean(sub_Auto$year))
sd = c(sd(sub_Auto$mpg), sd(sub_Auto$displacement), sd(sub_Auto$horsepower), sd(sub_Auto$weight), sd(sub_Auto$acceleration), sd(sub_Auto$year))
tab = data.frame(variable, minimum, maximum, mean, sd)
tab

pairs(c(Auto['mpg'], Auto['displacement'], Auto['horsepower'], Auto['weight'], Auto['acceleration'], Auto['year']))


# Question 10 

library(MASS)
head(Boston)
# ?Boston for information

dim(Boston)

pairs(Boston)

corr_matrix = cor(Boston, method="pearson") # Generate Correlation Matrix
corr_matrix

#heatmap(corr_matrix, col=heat.colors(256))

hist(Boston$crim, breaks=20, xlab="Crime Rate", main="Histogram of Crime Rates")

hist(Boston$tax, breaks=20, xlab="Tax Rate", main="Histogram of Tax Rates")

hist(Boston$ptratio, breaks=20, xlab="Pupil Teacher Ratio", main="Histogram of Pupil Teacher Ratios")

length(Boston$ptratio[20 < Boston$ptratio & Boston$ptratio < 20.5])

length(Boston$chas[Boston$chas == 1])

median(Boston$ptratio)

index = which.min(Boston$medv) #Get index  minimum medv
Boston[index,] #Access this row.

percentile = ecdf(Boston$crim) #ecdf takes a vector and returns function for computing percentile.
print(paste("Crime Rate = ", percentile(Boston[index,'crim'])))#We can now compute the "percentile" of a value

fields = names(Boston)
for (field in 1:length(fields)){
    percentile = ecdf(Boston[[field]])
    print(paste(fields[field], " = ", percentile(Boston[index,'crim'])))
}

length(Boston$rm[Boston$rm > 7])

length(Boston$rm[Boston$rm > 8])

Boston[Boston$rm > 8,]

summary( Boston[Boston$rm > 8,] )

summary(Boston)

