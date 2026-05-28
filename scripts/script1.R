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

anova(M_2, M_3)

# log likelihood
logLik(M_2)
logLik(M_3)

# Binary logistic regression ----------------------------------------------


affairs_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr26/refs/heads/main/data/affairs.csv")

affairs_df <- mutate(affairs_df, had_affair = affairs > 0)

# log odds, logits ...

p <- c(0.1, 0.2, 0.3, 0.5, 0.8, 0.95)
# odds = p / (1-p)
odds <- p / (1-p)

# logit: log of the odds
log(odds)

p <- c(0.1, 0.2, 0.5, 0.8, 0.9)
p/(1-p)
phi <- log(p/(1-p))

1/(1 + exp(-phi))
ilogit <- function(phi) 1/(1 + exp(-phi))
ilogit(phi)
plogis(phi) # cumulative distribution function for the logistic distribution

M_6 <- glm(had_affair ~ yearsmarried, 
           family = binomial(link = 'logit'),
           data = affairs_df)

summary(M_6)

exp(coef(M_6)[2]) # odds ratio for predictor "yearsmarried"

confint(M_6)
# confidence interval on odds ratio
exp(confint(M_6, parm='yearsmarried'))


# Predictions in logistic regression --------------------------------------

affairs_df2 <- tibble(yearsmarried = c(1, 2, 5, 10, 20))

b <- coef(M_6)

M_6_pred <- mutate(
  affairs_df2, 
  predicted_logodds = b[1] + b[2] * yearsmarried,
  predicted_prob = plogis(predicted_logodds))

ggplot(M_6_pred, aes(yearsmarried, predicted_logodds)) + geom_point() + geom_line()
ggplot(M_6_pred, aes(yearsmarried, predicted_prob)) + geom_point() + geom_line()
       
# automatic prediction using `predict` and `add_predictions`
predict(M_6, newdata = affairs_df2)

library(modelr) # to get add_predictions
add_predictions(affairs_df2, M_6) # predicted log odds
add_predictions(affairs_df2, M_6, type = 'response') # predicted probabilities
 
summary(M_6)

logLik(M_6)
logLik(M_6) * -2
deviance(M_6)

# the nullest of the nulls ...
M_7 <- glm(had_affair ~ 1, 
           family = binomial(link = 'logit'),
           data = affairs_df)
logLik(M_7) * -2
deviance(M_7)

deviance(M_7) - deviance(M_6)

anova(M_7, M_6)
