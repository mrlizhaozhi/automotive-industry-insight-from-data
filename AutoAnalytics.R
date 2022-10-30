# Reading a file into R (there are other ways too - via link, setting up a directory, etc.)
mydata <- read.csv(file.choose())
attach(mydata)

# see names of your variables
names(mydata)

# See a headline of the data
head(mydata)

# Descriptive Statistics
summary(mydata)
sd(UnitSales)
sd(Price)
sd(mpg)

# Correlations between variables
cor(Price, Horsepower)
cor(Price, mpg)
cov(Price, mpg)

# summing a variable - how many total cars sold?
sum(UnitSales)

# Creating a frequency table for each brand and our categorical variable
table(Manufacturer)
table(GroupPPF)

# Creating a frequency table for each brand
table(Manufacturer, GroupPPF)

# Create a histogram for mpg variable, add a title and then change color of bars
hist(mpg)
hist(mpg, c=5)
hist(mpg, col=5)
hist(mpg, main="Miles per Gallon Histagram chart", col=4)

# Plotting all of your data (to get a quick overview)
plot(mydata)

# Using the quantile function -- what variable is in the 60th percentile?
quantile(Price, probs = seq(0,1,.2))

# Plot Price vs. PPF Rating
plot(Price~PPFrating)

# Labeling a scatterplot
plot(Horsepower, PPFrating)
plot(Horsepower, PPFrating, 
     main="The Relationship between Horsepower and Performance", 
     ylab="PPF Rating", xlab="Horsepower")

# Create a boxplot of the PPF rating groups on Price, add color and change size
boxplot(Price~GroupPPF)
boxplot(Price~GroupPPF, col="Yellow")
boxplot(Price~GroupPPF, col="Yellow", par(cex.axis=.75))

# Plotting a variable relationship with different aesthetics (col = color, pch = point character changes)
plot(Price, mpg, col="red", pch=18, main="The Price/mpg Relationship", xlab= "Miles per Gallon", ylab="Price")

# Create a column/bar graphs with the first 4 observations
brandv <- c(3, 3, 3, 4)
barplot(brandv)
barplot(brandv, names.arg = c("Aura", "Audi", "BMW", "Buick"), par(cex.axis = 1))

# create a column/bar graph with all observations for each auto mfr and re-size label text
Maker <- table(Manufacturer)
barplot(Maker, cex.names = .25)
barplot(Maker, cex.names = .25, main = "Total Number of Vehicles by Automaker", xlab = "Automaker")

# Isolating variables -- what if I just want to look at certain variables (Ford models)?
Fordmpg <- mpg[Manufacturer == "Ford"]
Fordprice <- Price[Manufacturer == "Ford"]
plot(Fordprice, Fordmpg)

# Creating an XY plot with different colors for different groups
plot(Horsepower, Price)
ppfgrouping <- as.factor(mydata[,8])
plot(Horsepower, Price, pch = 19, col = ppfgrouping)
plot(Horsepower, Price, pch = 19, col = ppfgrouping, main = "The Price/mpg Relationship by Performance Group")
legend("topleft", legend = c("SundayDrivers", "Need4Speed", "Carswkick", "MaxPower"), col = c("blue", "green", "black", "red"), pch = 19)

# Creating an XY plot with different colors for different groups
plot(mpg, Price)
plot(mpg, Price, pch=19, col=ppfgrouping)
plot(mpg, Price, pch=19, col=ppfgrouping, main = "The Price/mpg Relationship by Performance Group")
legend("topright", legend = c("SundaryDrivers", "Need4Speed", "Carswkick", "MaxPower"), col = c("blue", "green", "black", "red"), pch = 19)

# t-test for the mean of one group
t.test(mpg, mu = 20)

# ANOVA - on PPF ratings
anova(lm(Price~factor(GroupPPF)))
groupprice <- anova(lm(Price~factor(GroupPPF)))
summary(groupprice)

# Regression with Price as the dependent variable and mpg + ppf as independents
pricereg <- lm(Price~mpg + PPFrating)
summary(pricereg)

# adding a trend line
plot(PPFrating, Price)
pricereg1 <- lm(Price~PPFrating)
abline((pricereg1))

# Redefining variables
y <- cbind(Price)
x <- cbind(mpg, Horsepower)
summary(x)
summary(y)
lm(y~x)
olsreg2 <- lm(y~x)
summary(olsreg2)

# Removing objects
remove(pricereg1)
remove(pricereg)

# Saving & Exporting



