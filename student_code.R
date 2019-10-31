###-----------------------------------------------------------------###
###-----------------------------------------------------------------###
###-----------------------------------------------------------------###
### INTRODUCTION TO REGRESSION: STUDENT CODE
### This code goes with the lesson that introduces students to
### regression. 
###
### The first half of the code helps you follow along with the 
### examples presented in class.
###
### The second half of the code prepares you for the in-class activity
###-----------------------------------------------------------------###
###-----------------------------------------------------------------###
###-----------------------------------------------------------------###

###---Install libraries if you don't have them
# install.packages("tidyverse"); install.packages("skimr")


###---Load libraries
library(tidyverse)
library(skimr)


###-----------------------------------------------------------------###
### In-class examples
###-----------------------------------------------------------------###

# read in data
grades <- read_csv("grades_2018.csv")

# check data
head(grades) # look at first 5 rows
sample_n(grades, 5) # look at random 5 rows
skim(grades)

###---Scatterplot
ggplot(grades) + 
  geom_point(aes(pretest, grade))

###---Correlation
cor(grades$pretest, grades$grade)

###---Regression line
summary(lm(grade ~ pretest, grades))


###---ALL THREE AT ONCE
r <- round(cor(grades$pretest, grades$grade)^2, 2)
r2_text = bquote(R^2 == .(r))
lin_mod <- lm(grade ~ pretest, grades)
b0 <- coef(lin_mod)[1]
b1 <- coef(lin_mod)[2]
ggplot(grades) + 
  geom_point(aes(pretest, grade)) + 
  geom_smooth(aes(pretest, grade), 
              method = "lm", 
              se = FALSE) + 
  annotate("text", 
           x = 80, y = 40, 
           label = r2_text) +
  annotate("text", 
           x = 80, y = 30, 
           label = paste("Grade =", 
                         round(b0, 2), "+", 
                         round(b1, 2), "Pre-test"))




###-----------------------------------------------------------------###
### Activity: Football
###-----------------------------------------------------------------###

# read in data
fb <- read_csv("football.csv") %>%
  filter(league == "Eredivisie") %>% # get Dutch teams
  select(season, # select relevant columns
         team, 
         avg_att,
         total_att, 
         finish, 
         points, 
         goals_for, 
         goals_against, 
         goal_diff)

# check the data
skimr::skim(fb)
sample_n(fb, 10)


###--------------
###---Question 1: Interpret the following to describe the relationship between 
###               points scored in a season and total attendance

# Plot the data with a regression line
ggplot(fb) + 
  geom_point(aes(points, total_att)) + 
  geom_smooth(aes(points, total_att), 
              method = "lm", se = FALSE)

# Correlation
R <- cor(fb$points, fb$total_att)
R; R^2

# Regression
summary(lm(total_att ~ points, fb))


###--------------
###---Question 2: Examine the relationship between total attendance and goals scored.
## YOUR CODE HERE


###--------------
###---Question 3: Examine the relationship between total attendance and goal differential.
## YOUR CODE HERE





###-----------------------------------------------------------------###
### Activity: Poverty & Education
###-----------------------------------------------------------------###


# read in data
dat <- read_csv("municipalities.csv") %>%
  select(fed_spending, # select the relevant columns
         hs_grad, 
         persons_per_household, 
         poverty, 
         median_household_income)

# quick check of the data
skimr::skim(dat)
sample_n(dat, 10)




###--------------
###---Question 1: Examine the relationship between average persons per household
###               and median household income.

# Plot the data with a regression line
ggplot(dat) + 
  geom_point(aes(persons_per_household, median_household_income)) + 
  geom_smooth(aes(persons_per_household, median_household_income), 
              method = "lm", se = FALSE)

# Correlation
R <- cor(dat$median_household_income, dat$persons_per_household)
R; R^2

# Regression
summary(lm(median_household_income ~ persons_per_household, dat))




###--------------
###---Question 2: Examine the relationship between poverty and secondary school graduation.
## YOUR CODE HERE



###--------------
###---Question 3: Examine the relationship between median income and secondary school graduation.
## YOUR CODE HERE