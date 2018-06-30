#####################################################################
#hypothesis test to check whether there is a difference in hours of
#sleep between males and females
#####################################################################

#Clear data
rm(list=ls())

#Read data from csv
path = paste(getwd() , "Health_Condition_of_Software_Engineers.csv", sep = "/")
health_data = read.csv(path, header=TRUE)
number_of_entries = nrow(health_data)

#medical checkup duration data
male_medical_test_data = subset(health_data, health_data$What.is.your.gender. == "Male")
number_of_male_entries = nrow(male_medical_test_data)
female_medical_test_data = subset(health_data, health_data$What.is.your.gender. == "Female")
number_of_female_entries = nrow(female_medical_test_data)

#male sleep time data
male_sleep_time_sample = male_medical_test_data$How.long.do.you.sleep.per.day...in.hours.
hist(x = male_sleep_time_sample, xlab = "Male Sleep Time (Hrs)", border="blue", col="green",breaks=seq(3,9,by=0.5))
boxplot(male_sleep_time_sample)

#original male sample mean
male_sample_mean = mean(male_sleep_time_sample)

#female sleep time data
female_sleep_time_sample = female_medical_test_data$How.long.do.you.sleep.per.day...in.hours.
hist(x = female_sleep_time_sample, xlab = "Male Sleep Time (Hrs)", border="blue", col="green",breaks=seq(3,9,by=0.5))
boxplot(female_sleep_time_sample)

#original sample mean
female_sample_mean = mean(female_sleep_time_sample)

#calculate mean difference
observed_mean_diff = female_sample_mean - male_sample_mean

#adjust male sample
adj_male_sleep_time_sample = male_sleep_time_sample + observed_mean_diff
adj_male_sample_mean = mean(adj_male_sleep_time_sample)

#create bootstrap distribution
bootstrap_sample_mean_diff_distribution = c()
for(i in (1:10000))
{
  bootstrap_female_sample = sample(female_sleep_time_sample, number_of_female_entries, replace=TRUE)
  bootstrap_male_sample = sample(adj_male_sleep_time_sample, number_of_male_entries, replace=TRUE)
  
  boostrap_sample_mean_diff = mean(bootstrap_female_sample) - mean(bootstrap_male_sample)
  bootstrap_sample_mean_diff_distribution = c(bootstrap_sample_mean_diff_distribution, boostrap_sample_mean_diff)
}

#calculate standard deviation
std_dev = sd(bootstrap_sample_mean_diff_distribution)

p_value = pnorm(q= observed_mean_diff, mean = 0, sd = std_dev, lower.tail = FALSE)