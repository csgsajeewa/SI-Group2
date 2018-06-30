#Clear all data
rm(list=ls())

#Read data from csv
path = paste(getwd() , "Health_Condition_of_Software_Engineers.csv", sep = "/")
health_data = read.csv(path, header=TRUE)

time = health_data['How.long.have.you.been.working.in.the.IT.sector..in.years..']

########################################################################################################
#Get Height and Weight data
height = health_data['What.is.your.height...in.Centimeters.........Feet.to.Centimeter.Converter..https...goo.gl.TBKZqW']
weight = health_data['What.is.your.weight...in.Kilograms.']

#Calculate BMI indices
number_of_entries = nrow(health_data)
BMI = vector(length = number_of_entries)

for (i in 1:number_of_entries)
{
  BMI[i] = weight[i, 1]/((height[i, 1]/100)^2)
}

#Histogram of BMI values
hist(BMI)     #Approximately normal

#Create bootstrap for BMI
bootstrap_vec = vector()
number_of_iterations = 1000

for (i in 1:number_of_iterations)
{
  bootstrap_vec <- c(bootstrap_vec, sample(BMI, size = number_of_entries, replace = TRUE))
}

reshaped_samples = matrix(bootstrap_vec, nrow = number_of_iterations)
BMI_means = rowMeans(reshaped_samples)

hist(BMI_means)
BMI_std = sd(BMI_means)
BMI_mean = mean(BMI_means)

BMI_lower = BMI_mean - 1.96*BMI_std
BMI_higher = BMI_mean + 1.96*BMI_std

#Within acceptable values

########################################################################################################
# <18.5       Underweight
# 18.5, 24.9  Optimal weight
# 25, 30      Overwight
# >30         Obese
########################################################################################################

########################################################################################################
opinion = health_data$Do.you.think.that.your.occupation.has.any.significant.impact.on.your.health.condition.
proportion_of_positives = sum(opinion[opinion==1])/length(opinion)

#Hypothesis H0: The proportion of Engineers who believe their career affects their health is = 0.5
# Ha: The proportion of Engineers who believe their career affects their health is > 0.5
# Right tailed test
t_value = (proportion_of_positives - .5)/sqrt(proportion_of_positives*(1-proportion_of_positives)/length(opinion))
p_value = pt(t_value, df = length(opinion)-1)

# The p_value is over .999. So we can reject the H0.

########################################################################################################

experience = health_data$How.long.have.you.been.working.in.the.IT.sector..in.years..
hist(experience) #Skewed
mean_experience = mean(experience)

age = health_data$How.old.are.you.
hist(age)  #Skewed
mean_age = mean(age) #Around 28. Biased.

#More symmetrical
hist(health_data$What.is.your.height...in.Centimeters.........Feet.to.Centimeter.Converter..https...goo.gl.TBKZqW)
hist(health_data$What.is.your.weight...in.Kilograms.)
