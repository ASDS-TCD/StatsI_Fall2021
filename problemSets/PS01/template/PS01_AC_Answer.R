#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}
library(ggplot2)


# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set working directory
#setwd("C:\Users\Andrej Cvetic\Documents\GitHub\StatsI_Fall2021\problemSets\PS01\template\PS01.R")


#####################
# Problem 1
#####################
#[A]
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)

z90 <- qnorm((1 - .90)/2, lower.tail = FALSE) ## (1-confidence coefficient)/2
n <- length(y)
sample_mean <- mean(y, na.rm = TRUE)
sample_sd <- sd(y, na.rm = TRUE)
lower_90 <- sample_mean - (z90 * (sample_sd/sqrt(n)))
upper_90 <- sample_mean + (z90 * (sample_sd/sqrt(n)))
confint90 <- c(lower_90, upper_90) 

#[B]

#Null Hip (IQ null): sample mean = 100
#Alt Hip: sample mean =/= IQ count

#mu = 100 [I assume the true mean is 100 as per task]

?t.test

IQ_null <- t.test(y, mu = 100, conf.level = .05)
IQ_null  

#we cannot reject the null hypothesis?

#####################
# Problem 2
#####################

expenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2021/main/datasets/expenditure.txt", header=T)
class (expenditure)
typeof (expenditure)
objects (expenditure)
ls.str (expenditure)
attributes (expenditure)
summary (expenditure)

#Q2.1.
qplot (X1,Y, data=expenditure)
qplot (X2,Y, data=expenditure)
qplot (X3,Y, data=expenditure)

#Q2.2.
Region_mode <- aggregate(expenditure$Y, by = list(expenditure$Region), FUN = mean)
#b. Plot our grouped means
bp <- barplot(Region_mode[,2], #use square bracket subsetting to select the second col
              names.arg = Region_mode[,1],
              horiz = TRUE, #Flip our axes
              las = 1, #rotate our text to fit it in,
              cex.names = 0.7, #make our axis text a bit smaller to fit
              main = "Mean of per capita expenditure on shelters/housing per region")
#text(bp, 0.1, round(Trump_mode[,2], 2)) #adding some text to our barplot with the mean

#Q2.3.
expenditure$Region1 <- as.factor(expenditure$Region)
#created new var Region1 from database expenditure and treated it as a factor 
ggplot(expenditure, aes(x=X1, y=Y, color=Region1)) +
  geom_bar(stat="identity", fill="white")
#indep = X axis
#dep = y axis
