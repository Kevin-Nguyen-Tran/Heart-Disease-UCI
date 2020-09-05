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


ggplot(tidy_data, mapping = aes(x = sex)) +
  geom_bar()

# Within this dataset there are more males than females

ggplot(tidy_data, mapping = aes(x = sex, fill = status)) +
  geom_bar(position = "fill")
# As you can see, within the females of this analysis, roughly 25% have heart disease
# within the males of this analysis, roughly 60% have heart disease


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

tidy_data %>%
  group_by(status, chol) %>%
  ggplot(tidy_data, mapping = aes(x = status, y = chol)) +
  geom_boxplot()

t.test(tidy_data$chol ~ tidy_data$status, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# interesting to see that cholesterol does not seem to be a good indicator of heart disease as there is not a significant difference in average cholestoral level between someone who has heart dz vs someone who doesn't

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









