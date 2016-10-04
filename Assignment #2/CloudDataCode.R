CD <- read.csv("~/Desktop/UVA/Fall 2016/DS/Homework #2/CloudData1.csv", header=FALSE, stringsAsFactors=FALSE)

#bring in librarys
library(DMwR)
library(ggplot2)
library(data.table)

#change attribute names according to the info given
colnames(CD) <- c('VIS_mean','VIS_max','VIS_min','VIS_mean_distribution','VIS_contrast','VIS_entropy','VIS_second_angular_momentum','IR_mean','IR_max','IR_min')

View(CD)
summary(CD)

#read values as numbers instead of characters
CD$VIS_mean <- as.numeric(CD$VIS_mean)
CD$VIS_max <- as.numeric(CD$VIS_max)
CD$VIS_min <- as.numeric(CD$VIS_min)
CD$VIS_mean_distribution <- as.numeric(CD$VIS_mean_distribution)
CD$VIS_contrast <- as.numeric(CD$VIS_contrast)
CD$VIS_entropy <- as.numeric(CD$VIS_entropy)
CD$VIS_second_angular_momentum <- as.numeric(CD$VIS_second_angular_momentum)
CD$IR_mean <- as.numeric(CD$IR_mean)
CD$IR_max <- as.numeric(CD$IR_max)
CD$IR_min <- as.numeric(CD$IR_min)

View(CD)
summary(CD)

#switch VIS_mean and VIS_min attribute names as well as IR_mean and IR_min
colnames(CD) <- c('VIS_min','VIS_max','VIS_mean','VIS_mean_distribution','VIS_contrast','VIS_entropy','VIS_second_angular_momentum','IR_min','IR_max','IR_mean')
summary(CD)

#outlier detection

#defining outlier function
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}

#VIS_min: replace outliers above 80
hist(CD$VIS_min)
outlierReplace(CD,"VIS_min",which(as.numeric(CD$VIS_min) > 80),NA)
hist(CD$VIS_min)
boxplot(CD$VIS_min, main="Boxplot of Minimum Brightness Values (VIS)",
        ylab="Brightness")

#VIS_max: looks good
hist(CD$VIS_max)
boxplot(CD$VIS_max, main="Boxplot of Maximum Brightness Values (VIS)",
        ylab="Brightness")

#VIS_mean: replace outliers above 140
hist(CD$VIS_mean)
outlierReplace(CD,"VIS_mean",which(as.numeric(CD$VIS_mean) > 140),NA)
hist(CD$VIS_mean)
boxplot(CD$VIS_mean, main="Boxplot of Mean Brightness Values (VIS)",
        ylab="Brightness")
summary(CD)

#VIS_mean_distribution: looks good
hist(CD$VIS_mean_distribution)
boxplot(CD$VIS_mean_distribution, main="Boxplot of Mean Distribution Brightness Values (VIS)",
        ylab="Brightness")

#VIS_contrast: based on boxplot, got rid of values above 2750
hist(CD$VIS_contrast)
boxplot(CD$VIS_contrast, main="Boxplot of Contrast Values (VIS)",
        ylab="Contrast")
outlierReplace(CD,"VIS_contrast",which(as.numeric(CD$VIS_contrast) > 2750),NA)
hist(CD$VIS_contrast)
boxplot(CD$VIS_contrast, main="Boxplot of Contrast Values (VIS)",
        ylab="Contrast")

#VIS_entropy: looks good
hist(CD$VIS_entropy)
boxplot(CD$VIS_entropy, main="Boxplot of Entropy Values (VIS)",
        ylab="Entropy")

#VIS_second_angular_momentum: replaced outliers above 200
hist(CD$VIS_second_angular_momentum)
boxplot(CD$VIS_second_angular_momentum, main="Boxplot of Second Angular Momentum Values (VIS)",
        ylab="Second Angular Momentum")
outlierReplace(CD,"VIS_second_angular_momentum",which(as.numeric(CD$VIS_contrast) > 200),NA)
hist(CD$VIS_second_angular_momentum)
boxplot(CD$VIS_second_angular_momentum, main="Boxplot of Second Angular Momentum Values (VIS)",
        ylab="Second Angular Momentum")

#IR_min: replaced outliers less than 25
hist(CD$IR_min)
boxplot(CD$IR_min, main="Boxplot of Min Brightness Values (IR)",
        ylab="Brightness")
outlierReplace(CD,"IR_min",which(as.numeric(CD$IR_min) < 25),NA)
hist(CD$IR_min)
boxplot(CD$IR_min, main="Boxplot of Min Brightness Values (IR)",
        ylab="Brightness")

#IR_max: looks good
hist(CD$IR_max)
boxplot(CD$IR_max, main="Boxplot of Max Brightness Values (IR)",
        ylab="Brightness")

#IR_mean: replaced outliers below 125
hist(CD$IR_mean)
boxplot(CD$IR_mean, main="Boxplot of Mean Brightness Values (IR)",
        ylab="Brightness")
outlierReplace(CD,"IR_mean",which(as.numeric(CD$IR_mean) < 125),NA)
hist(CD$IR_mean)
boxplot(CD$IR_mean, main="Boxplot of Mean Brightness Values (IR)",
        ylab="Brightness")

#remove any rows with less than 20% of data
manyNAs(CD,0.2)
nrow(CD)
sum(is.na(CD))
CD <- CD[-manyNAs(CD),]
nrow(CD)
sum(is.na(CD))

#checking whether there are any correlations
symnum(cor(CD, use = "complete.obs"))

#finding all lines where VIS_contrast is NA
which(is.na(CD$VIS_contrast))

#making a linear model using VIS_contrast and VIS_mean_distribution
lm(VIS_contrast~VIS_mean_distribution,data=CD)

#assign the missing values of VIS_contrast based on the eqn found from the linear model
CD[135,"VIS_contrast"]<- -181+12646*CD[135,"VIS_mean_distribution"]
CD[158,"VIS_contrast"]<- -181+12646*CD[158,"VIS_mean_distribution"]
CD[569,"VIS_contrast"]<- -181+12646*CD[569,"VIS_mean_distribution"]
CD[644,"VIS_contrast"]<- -181+12646*CD[644,"VIS_mean_distribution"]
CD[671,"VIS_contrast"]<- -181+12646*CD[671,"VIS_mean_distribution"]
which(is.na(CD$VIS_contrast))

summary(CD)

#normalize each attribute before kNN imputation
CD$VIS_min <- (CD$VIS_min - min(CD$VIS_min, na.rm = TRUE))/(max(CD$VIS_min,na.rm=TRUE)-min(CD$VIS_min,na.rm = TRUE))
CD$VIS_max <- (CD$VIS_max - min(CD$VIS_max, na.rm = TRUE))/(max(CD$VIS_max,na.rm=TRUE)-min(CD$VIS_max,na.rm = TRUE))
CD$VIS_mean <- (CD$VIS_mean - min(CD$VIS_mean, na.rm = TRUE))/(max(CD$VIS_mean,na.rm=TRUE)-min(CD$VIS_mean,na.rm = TRUE))
CD$VIS_mean_distribution <- (CD$VIS_mean_distribution - min(CD$VIS_mean_distribution, na.rm = TRUE))/(max(CD$VIS_mean_distribution,na.rm=TRUE)-min(CD$VIS_mean_distribution,na.rm = TRUE))
CD$VIS_contrast <- (CD$VIS_contrast - min(CD$VIS_contrast, na.rm = TRUE))/(max(CD$VIS_contrast,na.rm=TRUE)-min(CD$VIS_contrast,na.rm = TRUE))
CD$VIS_entropy <- (CD$VIS_entropy - min(CD$VIS_entropy, na.rm = TRUE))/(max(CD$VIS_entropy,na.rm=TRUE)-min(CD$VIS_entropy,na.rm = TRUE))
CD$VIS_second_angular_momentum <- (CD$VIS_second_angular_momentum - min(CD$VIS_second_angular_momentum, na.rm = TRUE))/(max(CD$VIS_second_angular_momentum,na.rm=TRUE)-min(CD$VIS_second_angular_momentum,na.rm = TRUE))
CD$IR_min <- (CD$IR_min - min(CD$IR_min, na.rm = TRUE))/(max(CD$IR_min,na.rm=TRUE)-min(CD$IR_min,na.rm = TRUE))
CD$IR_max <- (CD$IR_max - min(CD$IR_max, na.rm = TRUE))/(max(CD$IR_max,na.rm=TRUE)-min(CD$IR_max,na.rm = TRUE))
CD$IR_mean <- (CD$IR_mean - min(CD$IR_mean, na.rm = TRUE))/(max(CD$IR_mean,na.rm=TRUE)-min(CD$IR_mean,na.rm = TRUE))

summary(CD)

#kNN imputation, using sqrt(1008) as the value for k
CD <- knnImputation(CD, k=31.7490157328)
sum(is.na(CD))

summary(CD)


#save clean data file
write.csv(CD, file = "clouddata_clean.csv")






