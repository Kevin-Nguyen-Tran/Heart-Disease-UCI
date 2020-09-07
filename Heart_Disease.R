#1) Loading packages, reading in data set, and reformatting.----------------------------------------------------------------------------------------------------------

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values
library(forcats) # transform integers to factors

#read the data set into RStudio and stored into object
data <- read.csv("C:/Users/Kevin/Desktop/Heart-Disease-UCI/datasets_33180_43520_heart.csv")

heart_data <- as_tibble(data)

heart_data <- rename(heart_data, age = ï..age)

heart_data <- heart_data %>%
  mutate(status = target)

heart_data$sex[heart_data$sex == "1"] <- "male" #change values from 1 to male in the sex variable
heart_data$sex[heart_data$sex == "0"] <- "female" #change values from 0 to female in the sex variable
heart_data$sex <- as.factor(heart_data$sex)

heart_data$cp <- as.factor(heart_data$cp)
heart_data$fbs <- as.factor(heart_data$fbs)
heart_data$restecg <- as.factor(heart_data$restecg)
heart_data$exang <- as.factor(heart_data$exang)
heart_data$slope <- as.factor(heart_data$slope)

heart_data$status[heart_data$status == "0"] <- "Dz" #change values from 0 to disease in the target variable
heart_data$status[heart_data$status == "1"] <- "No Dz" #change values from 1 to no disease in the target variable
heart_data$status <- as.factor(heart_data$status)

#RESOURCES USED:
#https://www.kaggle.com/ronitf/heart-disease-uci/discussion/105877
# https://archive.ics.uci.edu/ml/datasets/heart+Disease
# above is the discussion on Kaggle for the correct explanation of the attributes used within the data set above
# add bullet points for the attributes/variables used in this analysis in the rMarkdown
# https://www.healthline.com/health/serum-cholesterol
# used to reference normal range of serum cholesterol


tidy_data <- heart_data %>%
  select(age:oldpeak, status) 

#=============================================================================================================================================================================================================================
# SEX AND DISEASE STATUS
#=============================================================================================================================================================================================================================

ggplot(tidy_data, mapping = aes(x = sex)) +
  geom_bar()

# Within this dataset there are more males than females

ggplot(tidy_data, mapping = aes(x = sex, fill = status)) +
  geom_bar(position = "fill")
# As you can see, within the females of this analysis, roughly 25% have heart disease
# within the males of this analysis, roughly 60% have heart disease

#=============================================================================================================================================================================================================================
# AGE AND DISEASE STATUS
#=============================================================================================================================================================================================================================

# We can look at the mean age of individual with and without the dz to see if they differ

ggplot(tidy_data, mapping = aes( x = status, y = age)) +
  geom_boxplot()
# We can see that the average age of an indivdual with heart disease is greater than that of someone without heart disease.
# There is also less variability in age of those with heart dz vs those without
# The youngest individual with heart dz, as shown as an outlier at 35 years old.

tidy_data %>%
  group_by(status, sex) %>%
  summarise(
            average_age = mean(age),
            min = min(age),
            max = max(age)
            )
#Within this analysis:
# average age of someone with heart dz is 56.6 y.o. 1st quantile is 52 y.o and 3rd quantile is 62 y.o.
# youngest age of someone with heart dz is 35 and and oldest with heart dz is 77
# average age of someone without heart dz is 52.5 y.o. 1st quantile is 44 y.o and 3rd quantile is 59 y.o.
# youngest age of someone without heart dz is 29 and and oldest with heart dz is 76

#Now we can run the t-test keeping in mind, our null hypothesis is equal to zero, alternative is two.sided, 95% confidence interval, variance is not equal and they are independent populations.
t.test(tidy_data$age ~ tidy_data$status, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# This null hypothesis tells us that there is no difference in age between those with or without heart dz
# The p-value is close to zero, therefore we can reject our null hypothesis and with 95% confidence say there is a statistically significant differnece between the average age of those who have heart dz vs those who do not.
# The confidence interval also does not include zero, therefore, it supports our rejection of the null

#=============================================================================================================================================================================================================================
# CHOLESTORAL LEVELS AND DISEASE STATUS
#=============================================================================================================================================================================================================================


tidy_data %>%
  group_by(status, chol) %>%
  ggplot(tidy_data, mapping = aes(x = status, y = chol)) +
  geom_boxplot()

t.test(tidy_data$chol ~ tidy_data$status, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# interesting to see that cholesterol does not seem to be a good indicator of heart disease as there is not a significant difference in average cholestoral level between someone who has heart dz vs someone who doesn't

#=============================================================================================================================================================================================================================
# CHEST PAIN AND DISEASE STATUS
#=============================================================================================================================================================================================================================

#Chest pain and disease status
ggplot(tidy_data, aes(x = cp, fill = status)) +
  geom_bar(position = "stack") +
  xlab("Increasing Chest Pain")
#As shown in the above plot, the majority of patients within the data set shows asymptomatic chest pains. Within this majority, most of which have heart disease.
# This indicates that the majority of patients with heart disease are asymptomatic

#=============================================================================================================================================================================================================================
# RESTING BLOOD PRESSURE AND DISEASE STATUS
#=============================================================================================================================================================================================================================

# Histogram to show the resting blood pressure of patients with and without heart disease
ggplot(tidy_data, aes(x = trestbps, fill = status)) +
  geom_histogram() +
  xlab("Resting Blood Pressure (mmHg)")
# As you can see that the histogram is positively skewed due to the outliers of high rest bp greater than 180 mmHg (which both patients suffer from heart disease).
# The average is within 120 mmHg and 140 mmHg. According to https://www.heart.org/en/health-topics/high-blood-pressure/understanding-blood-pressure-readings, the normal systolic blood pressure is < 120 mmHg

tidy_data %>%
  group_by(status) %>%
  summarise(
    mean(trestbps)
  )
#As calculated above, the average resting bp of patients with the disease is 134 mmHg (which is greater than the average)
# The average resting bp of patients without heart disease is 129 mmHg, which is still greater than the average bp.

#==================================================================================================================================================================================================================================================================================================================================================================
# MAXIMUM HEART RATE ACHIEVED PER PATIENT AND DISEASE STATUS
#=============================================================================================================================================================================================================================


# Now we can see what the relationship is between maximum heart rate achieved and disease status
ggplot(tidy_data, aes(x = thalach, fill = status)) +
  geom_histogram() +
  xlab("Maximum Heart Rate Achieved")
# Upon researching the average maximum heart rate, mayoclinic <https://www.mayoclinic.org/healthy-lifestyle/fitness/in-depth/exercise-intensity/art-20046887#:~:text=You%20can%20calculate%20your%20maximum,beat%20per%20minute%20during%20exercise.> suggests that it varies by age. To calculate average maximum heart rate: you will subtract your age from 220.
# This implies that the older you are the lower your maximum heart rate is (on average).
# Therefore, we will look at a plot that includes age as a variable and see what it implies.

ggplot(tidy_data, aes(x = age, y = thalach, color = status)) +
  geom_point() +
  geom_smooth(se = FALSE, size = 2) +
  xlab("Age") +
  ylab("Maximum Heart Rate Achieved")
# As seen in the above graph, this is true for those without heart disease. As age increases, the maximum heart rate achieved decreases
# Within patients with heart disease the maximum heart rate achieved remains consistent around 150 beats per minute.

thalach_age_model <- lm(tidy_data$thalach ~ tidy_data$age + tidy_data$status)
summary(thalach_age_model)
plot(thalach_age_model)
# this function above is the base R, 4 built-in regression diagnostic plots
# The first plot has a line that is relatively flat and the relationship between the fitted values and residual is cloud shaped
# this shows linearity between the 3 variabels
# Second plot shows that if the residuals are normally distributed, the points should fall in a diagonal line as it does!
# This, along with the p-values in the summary, show that our linear model effectively captures the relationship between the 3 variables!


#=============================================================================================================================================================================================================================
# LOGISTIC LINEAR REGRESSION - BINOMIAL AND DISEASE STATUS
#====================================================================================================================================================================

predicted <- glm(status ~ ., family = "binomial", data = tidy_data)
# fitting a generalized linear model to our data to see if we can predict the status of a patient based on the confounding variables within the dataset
summary(predicted)
# As shown by the stars next to the p-values, there are variables that are statistically significant to predict the probability of dz
# such as being male, the chest pains, and old peak.

probability_data <- data.frame(fitted.values = predicted$fitted.values, status = tidy_data$status)

probability_data <- probability_data %>%
  arrange(fitted.values)


probability_data <- probability_data %>%
  mutate(rank = 1:nrow(probability_data))

ggplot(probability_data, aes(x = rank, y = fitted.values, color = status)) +
  geom_point(alpha = 1, shape = 1, stroke = 2) +
  xlab("Rank") +
  ylab("Predicted Probability of Not Getting the Disease")
# Due to how the data is represented with 0 indicating heart disearse and 1 indicating no heart dz, our plot will reflect this as shown above.
# By plotting our predicted values to the known status of someone having the dz or not having the dz, we can confidently say that our generalized inear model
# predicts the relationship between variables accurately
# Therefore, given a sample of the same variables, we can predict the likilihood of an individual not getting heart dz depending on their values









