#####################################################################
#calculate confidence interval for population mean of sleep time
#####################################################################

#clear data
rm(list=ls())

#read data from csv
path = paste(getwd() , "Health_Condition_of_Software_Engineers.csv", sep = "/")
health_data = read.csv(path, header=TRUE)
number_of_entries = nrow(health_data)

#sleep time data
sleep_time_sample = health_data$How.long.do.you.sleep.per.day...in.hours.
hist(x = sleep_time_sample, xlab = "Sleep Time (Hrs)", border="blue", col="green",breaks=seq(3,9,by=0.5))
boxplot(sleep_time_sample)

#original sample mean
sample_mean = mean(sleep_time_sample)
sample_std_dev = sd(sleep_time_sample)

#create bootstrap distribution
bootstrap_sample_mean_distribution = c()
for (i in (1:10000))
{
  bootstrap_sample = sample(x=sleep_time_sample, size = number_of_entries, replace = TRUE)
  bootstrap_sample_mean = mean(bootstrap_sample)
  bootstrap_sample_mean_distribution = c(bootstrap_sample_mean_distribution, bootstrap_sample_mean)
}

#boostrap distribution analysis
hist(bootstrap_sample_mean_distribution,border="blue", col="green")
boxplot(bootstrap_sample_mean_distribution)
bootstrap_dist_mean = mean(bootstrap_sample_mean_distribution)
bootstrap_dist_sd = sd(bootstrap_sample_mean_distribution)

#95% CI for sleep time
upper_limit = sample_mean + qnorm(0.975)*bootstrap_dist_sd
lower_limit = sample_mean - qnorm(0.975)*bootstrap_dist_sd

confidence_interval = c(lower_limit, upper_limit)
