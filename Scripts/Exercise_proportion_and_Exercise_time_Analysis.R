#Clear all data
rm(list=ls())

#Read data from csv
path = paste(getwd() , "Health_Condition_of_Software_Engineers.csv", sep = "/")
health_data = read.csv(path, header=TRUE)

####################################Exercise Time#######################################################

#Preparing sample and calculating sample statistics
do_exercise_or_not = health_data$Are.you.doing.any.physical.activities...............Jogging..Running.and.Sports.etc.
exercise_time = health_data$How.long.do.you.spend.for.physical.activities.within.a.week...in.hours.

hist(do_exercise_or_not, breaks=seq(-0.5,1.5,by=1), border="blue", col="green")
hist(exercise_time, breaks=seq(-2,15,by=1), border="blue", col="green")

getProportion <- function(x) {

  proportion = 0
  for (i in x) {
    proportion = proportion + i
  }
  proportion = proportion/length(x)
  
  return(proportion)
}

proportion_do_exercise = getProportion(do_exercise_or_not)
proportion_do_exercise

sample_mean_exercise_time = mean(exercise_time)
sample_mean_exercise_time

#Bootstrapping for Exercise Time sample and Exercise Or Not sample
sampleSize = length(exercise_time)
noOfBootstraps = 1000

sample_means = c()
sample_proportions = c()

for(k in 1:noOfBootstraps){
  sample_ex_time = sample(exercise_time, sampleSize, replace = TRUE)
  sample_means <- c(sample_means,mean(sample_ex_time))
  
  sample_do_exercise_or_not = sample(do_exercise_or_not, sampleSize, replace = TRUE)
  sample_proportions = c(sample_proportions, getProportion(sample_do_exercise_or_not))
}


hist(sample_means, border="blue", col="green")
meanOfSampleMeans = mean(sample_means)
sdOfSampleMeans = sd(sample_means)
meanOfSampleMeans
sdOfSampleMeans

lower_limit_for_mean = meanOfSampleMeans - 1.96*sdOfSampleMeans
upper_limit_for_mean = meanOfSampleMeans + 1.96*sdOfSampleMeans
CI_vec_means = cbind(lower_limit_for_mean,upper_limit_for_mean)
CI_vec_means


hist(sample_proportions, border="red", col ="blue")
meanOfSampleProportions = mean(sample_proportions)
sdOfSampleProportions = sd(sample_proportions)
meanOfSampleProportions
sdOfSampleProportions

lower_limit_for_proportion = meanOfSampleProportions - 1.96*sdOfSampleProportions
upper_limit_for_proportion = meanOfSampleProportions + 1.96*sdOfSampleProportions
CI_vec_proportions = cbind(lower_limit_for_proportion,upper_limit_for_proportion)
CI_vec_proportions