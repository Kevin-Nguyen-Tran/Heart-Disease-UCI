#1) Loading packages, reading in data set, and reformatting.----------------------------------------------------------------------------------------------------------

rm(list = ls()) #removes all variables stored previously in Environment (good habit to include)

library(tidyverse) #Run tidyverse, if you do not have: install.packages("tidyverse")
library(lubridate) #to modify date-time entries, if you do not have it: install.packages("lubridate")
library(scales) #allows to modify scientific notation for values

#read the data set into RStudio and stored into object
data <- read.csv("C:/Users/Kevin/Desktop/Heart-Disease-UCI/datasets_33180_43520_heart.csv")

heart_data <- as_tibble(data)

heart_data <- rename(heart_data, age = ï..age)

heart_data$sex[heart_data$sex == "1"] <- "male" #change values from 1 to male in the sex variable
heart_data$sex[heart_data$sex == "0"] <- "female" #change values from 0 to female in the sex variable
heart_data$target[heart_data$target == "0"] <- "Dz" #change values from 0 to disease in the target variable
heart_data$target[heart_data$target == "1"] <- "No Dz" #change values from 1 to no disease in the target variable

#https://www.kaggle.com/ronitf/heart-disease-uci/discussion/105877
# above is the discussion on Kaggle for the correct explanation of the attributes used within the data set above

tidy_data <- heart_data %>%
  select(age:oldpeak, target)

ggplot(tidy_data, mapping = aes(x = sex)) +
  geom_bar()

# Within this dataset there are more males than females

ggplot(tidy_data, mapping = aes(x = sex, fill = target)) +
  geom_bar(position = "fill")
# As you can see, within the females of this analysis, roughly 25% have heart disease
# within the males of this analysis, roughly 60% have heart disease


# We can look at the mean age of individual with and without the dz to see if they differ

ggplot(tidy_data, mapping = aes( x = target, y = age)) +
  geom_boxplot()
# We can see that the average age of an indivdual with heart disease is greater than that of someone without heart disease.
# There is also less variability in age of those with heart dz vs those without
# The youngest individual with heart dz, as shown as an outlier at 35 years old.

tidy_data %>%
  group_by(target, sex) %>%
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
t.test(tidy_data$age ~ tidy_data$target, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# This null hypothesis tells us that there is no difference in age between those with or without heart dz
# The p-value is close to zero, therefore we can reject our null hypothesis and with 95% confidence say there is a statistically significant differnece between the average age of those who have heart dz vs those who do not.
# The confidence interval also does not include zero, therefore, it supports our rejection of the null

tidy_data %>%
  group_by(target, chol) %>%
  ggplot(tidy_data, mapping = aes(x = target, y = chol)) +
  geom_boxplot()

t.test(tidy_data$chol ~ tidy_data$target, mu = 0, alt = "two.sided", conf = 0.95, var.eq = FALSE, paired = FALSE)
# interesting to see that cholesterol does not seem to be a good indicator of heart disease as there is not a significant difference in average cholestoral level between someone who has heart dz vs someone who doesn't














