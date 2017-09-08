
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
