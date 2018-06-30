##########################################################################
#calculate confidence interval for the population proportion of engineers
#who are doing medical checkups
##########################################################################


#Clear data
rm(list=ls())

#Read data from csv
path = paste(getwd() , "Health_Condition_of_Software_Engineers.csv", sep = "/")
health_data = read.csv(path, header=TRUE)
number_of_entries = nrow(health_data)

#medical checkup duration data
medical_test_data = health_data$How.often.do.you.attend.medical.checkups. == "Not at all"

#count calculation
totalCount = number_of_entries
trueCount = 0
for(i in (1:number_of_entries))
{
  if(medical_test_data[i] == TRUE)
  {
    trueCount = trueCount + 1;
  }
}

falseCount = totalCount -trueCount

#create sample
sample_proportion = falseCount/totalCount
medical_chkup_sample = c(rep(0, trueCount), rep(1, falseCount))
medical_chkup_sample = sample(medical_chkup_sample, size = number_of_entries, replace = FALSE)

hist(medical_chkup_sample, xlab = "Propotion", border="blue", col="green",breaks=seq(-1,1,by=1))

#create bootstrap distribution
bootstrap_sample_proportion_distribution = c()
for(i in (1:10000))
{
  bootstrap_sample = sample(x = medical_chkup_sample, size = number_of_entries, replace = TRUE)
  bootstrap_sample_proportion = sum(bootstrap_sample)/number_of_entries
  bootstrap_sample_proportion_distribution = c(bootstrap_sample_proportion_distribution, bootstrap_sample_proportion)
}

#boostrap distribution analysis
hist(bootstrap_sample_proportion_distribution, xlab = "Propotion", border="blue", col="green")
bootstrap_dist_mean = mean(bootstrap_sample_proportion_distribution)
bootstrap_dist_sd = sd(bootstrap_sample_proportion_distribution)
boxplot(bootstrap_sample_proportion_distribution)

#95% CI for sleep time
upper_limit = sample_proportion + qnorm(0.975)*bootstrap_dist_sd
lower_limit = sample_proportion - qnorm(0.975)*bootstrap_dist_sd

confidence_interval = c(lower_limit, upper_limit)