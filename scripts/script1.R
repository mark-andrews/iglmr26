library(tidyverse)

weight_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr26/refs/heads/main/data/weight.csv")

M_1 <- lm(weight ~ height, data = weight_df)
coef(M_1) # estimates of the coefficients of the linear equation
sigma(M_1) # estimate of the (residual) standard deviation
 
M_2 <- lm(weight ~ height + age, data = weight_df)
coef(M_2)
sigma(M_2)

# Categorical predictor variables -----------------------------------------

count(weight_df, gender)
count(weight_df, race)

M_3 <- lm(weight ~ height + age + gender, data = weight_df)
coef(M_3)
sigma(M_3)

M_4 <- lm(weight ~ height + age + race, data = weight_df)
coef(M_4)

# Uncertainty of estimates ------------------------------------------------

summary(M_3)
confint(M_3)

# The "nullest of the nulls" model null hypothesis
M_5 <- lm(weight ~ 1, data = weight_df)
anova(M_5, M_3) # "anova" = nested model comparison

