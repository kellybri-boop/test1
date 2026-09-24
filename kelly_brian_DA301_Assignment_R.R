#install necessary libraries and packages
library(tidyverse)
library(skimr)
library(ggplot2)
library(dplyr)

#set home directory

setwd(dir = '/Users/Kelly/Documents/Accelerartor Course') 
#load the turtle review csv file in a datafame
#sense check the data

rvs <- read.csv('turtle_reviews.csv', header=TRUE)
head(rvs)

#check for nulls
sum(is.na(rvs))

#create a summary of the data
summary(rvs)

#drop unnessary columns and sense check data
rvs <- subset(rvs, select=-c(language,platform))
head(rvs)

#rename columns

names(rvs)[3] <- "remuneration"
names(rvs)[4]<- "spending_score"

head(rvs)

#create aggregate by spending score

df_sc <- rvs %>% group_by(spending_score) %>%
  summarise(mean_points=mean(loyalty_points),
            .groups='drop')

df_sc

#visualise relationship between spending score and loyalty point

ggplot(df_sc,
       mapping=aes(x=spending_score, y=mean_points)) +
  geom_point()


#let's add a line of best fit

ggplot(df_sc,
       aes(x = spending_score, y = mean_points)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)
 
#now let's have a look at relationship between education and loyalty points
df_ed <- rvs %>% group_by(education) %>%
  summarise(mean_points=mean(loyalty_points),
            .groups='drop')

df_ed

#throw that into a barchart
ggplot(df_ed, aes(x = education, y = mean_points)) +
  geom_col()

#let's have a look at remuneration

df_rem <- rvs %>% group_by(remuneration) %>%
  summarise(mean_points=mean(loyalty_points),
            .groups='drop')

df_rem

#visualise with scatterplot again
ggplot(df_rem,
       aes(x = remuneration, y = mean_points)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

#let's have a look at age and gender together

df_gender_age <- rvs %>% group_by(gender,age) %>%
  summarise(mean_points=mean(loyalty_points),
            .groups='drop')

df_gender_age


#visualise

ggplot(df_gender_age, aes(x = age, y = mean_points)) +
  geom_point(color = "green", size = 3, alpha = 0.5) +
  facet_wrap(~gender) + geom_smooth(method = "lm", se = FALSE)+
  labs(
    x = "Age",
    y = "Mean Points",
    title = "Relationship Between Age and Loyalty Points and Gender",
    subtitle = "Scatterplot comparing Loyalty Points across gender"
  )


#let have a look at some stats, correlations and think about liner regression
# Determine correlation between variables.
head(rvs)

#let's grab all the numeric data
numeric_data <- rvs[sapply(rvs, is.numeric)]
cor(numeric_data, use = "complete.obs")

# Create a new object and 
# specify the lm function and the variables.
modela = lm(loyalty_points~remuneration, data=rvs)

# Print the summary statistics.
summary(modela)

modelb= lm(loyalty_points~remuneration+spending_score, data=rvs)
summary(modelb)

modelc=lm(loyalty_points~remuneration+spending_score+product, data=rvs)
summary(modelc)

#no difference between b and c, go with b


# Create a new object and specify the predict function.
predictTest = predict(modelc, newdata=rvs,
                      interval='confidence')

# Print the object.
predictTest 


rvs$predicted <- predict(modelb)

plot(rvs$predicted, rvs$loyalty_points,
     xlab = "Predicted Loyalty Points",
     ylab = "Actual Loyalty Points",
     main = "Actual vs Predicted Loyalty Points")

abline(0, 1, col = "red")
