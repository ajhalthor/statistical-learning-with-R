
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
