library(openxlsx)
#loading the data from IMDB Movie dataset
DATA <- read.csv("IMDb movies.csv")


#filtering of Null values in the column "year"
filter_data <- subset(DATA,(!is.na(DATA[,4])))
head(filter_data, n = c(6L,2L))
#print(filter_data)
#filtering of Null values in the column "avg_votes"
filter_data <- subset(filter_data,(!is.na(filter_data[,15])))
#converting the column "year" into vector/list form 
filter_data[,4] <- sapply(filter_data[,4],as.numeric)

#converting the column "avg_votes" into vector/list form
filter_data[,15] <- sapply(filter_data[ ,15],as.numeric)
pop_mean_total = mean(filter_data[,15])
pop_var_total = var(filter_data[,15])

#collecting the data of all the movies up to "year" 2016
filter_data1 <- subset(filter_data,(filter_data[,4]<=2016))
head(filter_data1, n = c(6L,2L))

#Calculate population mean of avg_votes of all the movies up to 2016.
populationMean <- mean(filter_data1[,15])

#calculates the population variance of avg_votes of all the movies up to 2016.
populationVariance <- var(filter_data1[,15])
print(populationVariance)

#prints the population mean
print(c("Population mean of all movies upto 2016 = ",populationMean))

#Collect a sample of all the movies in the year 2017.
filter_data2 <- subset(filter_data,(filter_data[,4]==2017))

#calculates mean of avg_votes of all the movies in the year 2017.  
sampleMean <- mean(filter_data2[,15])
print(sampleMean)

# calculates variance of avg_votes of all the movies in the year 2017
sampleVariance <- var(filter_data2[,15])
print(sampleVariance)


x <- seq(0, 10, by = 0.2)
y <- dnorm(x, mean=5.92409370951109, sd=1.21803325)
plot(x, y, type="l", lwd=0.1, main = " Normal Distrubution")
n <- nrow(filter_data2)

pop_mean = DATA[]

#number of rows in the sample
#when sample variance is known we use z-test
zknown <- (sampleMean-pop_mean_total)/(pop_var_total/sqrt(n))
#significance level
alpha <- 0.01

#value in the z-table for given significance level 
z.alpha <- qnorm(1-alpha)

#critical value of z
print(c("Z at alpha = 0.01 ",-z.alpha))

#values of z when sample variance is known
print(c("Z known variance = ",zknown))



#when sample variance is unknown we use t-test that is z unknown
zunknown <- (sampleMean-pop_mean_total)/(sampleVariance/sqrt(n))

#printing z unknown
print(c("Z unknown variance = ",zunknown))

#here we are getting the value of z known and z unknown variances are less 
#than z-alpha  so we are rejecting this hypothesis.

#For alpha = 0.05 & alpha = 0.01  we are getting the z known and unknown variances 
#are smaller than z-alpha .So, we will reject the hypothesis for both significance 
#levels