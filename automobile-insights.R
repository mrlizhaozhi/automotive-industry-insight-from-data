#import the dataset and attach it
mydata <- read.csv(file.choose())
attach(mydata)

#descriptive stats of all quant vars
summary(mydata)

#histagram of fuel capacity
hist(FuelCapacity, main="Fuel Capacity Distribution of Vehicles")

#frequency table - Manufacturers
table(Manufacturer)

#create a plot chart for all data
plot(mydata)

#scatter chart of Curb Weight VS mpg
plot(CurbWeight, mpg, main = "The Relationship Between Vehicle
     Weight and Fuel Efficiency", ylab = "Fuel Efficiency", xlab = "Car Weight")
trend <- lm(mpg~CurbWeight)
abline((trend))

#boxplot: mpg by PPF Group
boxplot(mpg~GroupPPF, main = "Miles per Gallon by PPF Group", par(cex.axis = .5), col = "Yellow",
        ylab="Miles per Gallon (mpg)", xlab="PPF Group")

#ANOVA test
anova(lm(Price~factor(GroupPPF)))

#linear regression: price as dependent, mpg horsepower, and curb weight as independent vars.
pricereg <- lm(Price~mpg + Horsepower + CurbWeight)
summary(pricereg)
