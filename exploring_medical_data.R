# Brady Lange
# Course: CSIS 239
# R Script 2
# 3/3/18
# This program explores data from a file that contains provider ids and their average procedure cost 
# and extracts data from the most expensive provider and finds the outliers from the average procedure costs

graphics.off()
rm(list = ls())
setwd("C:/Users/brady/Documents/CSIS 239")

# 1.)
# Loading data file into R
load("a2.RData")

# 2.)
# Understanding the data file
class(d)
colnames(d)
dim(d)

# 3.)
# Plotting a density histogram of the data
hist(d[ , 2], freq = F, breaks = "scott")

# 4.)
# Overlaying a density curve
lines(density(d[ , 2]), col = "firebrick", lwd = 3)

# 5.)
# Converting the data into normal distribution
d_normal <- rnorm(d, mean = log(d))

# 6.)
# Plotting a probability density histogram of the data
hist(d_normal, freq = F, breaks = "scott", xlab = "log(Average charge)", ylab = "Density", 
     main = "Histogram of Average Charges of US Medical Providers")

# 7.)
# Using the curve function to overlay a density curve
curve(dnorm(x, mean = mean(d_normal), sd = sd(d_normal)), col = "firebrick", lwd = 3, add = T)

# 8.)
# Extracting data for providers wih IDs in the 10,000 to 19,999 range
id_range <- d[d[ , 1] >= 10000 & d[ , 1] <= 19999 , ]

# 9.)
# Using the data from 8 to create a scatterplot of AverageCharge for the providers
plot(id_range, xlab = "Provider ID", ylab = "Average Charge ($)", 
     main = "Average Procedure Charge by Provider", pch = 20)

# 10.)
# Identifying the ProviderID with the most expensive average procedure costs
max_exp_prov <- id_range[id_range[ , 2] == max(id_range[ , 2]), 1]

# 11.)
# Creating a box plot of the most expensive provider
max_exp_prov_boxp <- boxplot(id_range[id_range[ , 1] == 10139, 2], xlab = "Provider ID: 10139", ylab = "Average Procedure Cost", 
                             main = "Average Procedure Costs of the Most Expensive Provider")

# 12.)
# Outputting payment outliers in the boxplot
max_exp_prov_boxp$out
# This statement proves that the above output is an outlier
max_exp_prov_boxp$out < max_exp_prov_boxp$stats[2,1] | max_exp_prov_boxp$out > max_exp_prov_boxp$stats[4,1]
