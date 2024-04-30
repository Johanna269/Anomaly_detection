library(anomalize)
library(dplyr)
library(tibbletime)
library(ggplot2)
library(reshape2)
dataset <- read.table("sales.csv", sep=",", header=T)
dataset$date <- as.Date(dataset$date, format = "%d-%m-%Y") 
dataset <- as_tbl_time(dataset, index = date)
head(dataset, 50)

anomalizeddataset <- dataset %>% anomalize(sales) #Produce lower and upper bound per time unit for values which are considered anomalies
head(anomalizeddataset)

ggplot(melt(dataset %>% anomalize(sales) %>% select(-anomaly), id.vars="date"),
       aes(x=date, y=value, group=variable, color=variable)) + geom_point()

decomposeddata <- dataset %>% time_decompose(sales, frequency="1 year") #Remove repeating trends

ggplot(melt(decomposeddata, id.vars="date"), aes(x=date, y=value, group=variable, color=variable)) + geom_line() + facet_grid(variable ~ .)


#Calculate upper and lower bounds on decomposed series
head(anomalizeddecomposeddata <- dataset %>% time_decompose(sales, frequency="1 year") %>%
       anomalize(remainder))

#Remove anomalies
anomalizeddecomposeddata <- dataset %>% time_decompose(sales, frequency="1 year") %>%
  anomalize(remainder) %>% select(-anomaly)

#Normal levels of observed values
anomalizeddecomposeddata <- dataset %>% time_decompose(sales) %>%
  anomalize(remainder) %>% select(-anomaly) %>% time_recompose() #This step reconstructs bands around the “normal” levels of observed values. It uses information from the remainder_l1 and remainder_l2 levels produced during the anomalize() step, as well as the season and trend/median spans values from the time_decompose() step.

#Visualize
ggplot(melt(anomalizeddecomposeddata, id.vars="date"), aes(x=date, y=value, group=variable, color=variable)) + geom_line() + facet_grid(variable ~ .)

ggplot(melt(anomalizeddecomposeddata %>% select(date, observed, recomposed_l1, recomposed_l2), id.vars="date"), aes(x=date, y=value, group=variable, color=variable)) + geom_line()



#Change Point detection 
library(bcp)
set.seed(1)
df <- read.csv("changepoint.csv", sep=",", header=F)
dataplot <- df$V1
plot(dataplot)
data=ts(df, start=1, end=200, frequency=1)
bcp_posterior <- bcp(data, return.mcmc = TRUE)
plot(bcp_posterior) #Visualize changepoint

#Another algorithm gets the same result
library(changepoint)
result <- cpt.meanvar(df$V1)
changepoint_indices <- attributes(result)$cpts
print(changepoint_indices)
plot(result, cpt.col = "blue")
