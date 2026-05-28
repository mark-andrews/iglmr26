library(tidyverse)
library(modelr)

doctor_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr26/refs/heads/main/data/DoctorAUS.csv")

count(doctor_df, doctorco)

summarize(doctor_df, mean(doctorco))

# Poisson regression
M_14 <- glm(doctorco ~ age, 
            family = poisson(link = 'log'),
            data = doctor_df)
summary(M_14)

b <- coef(M_14)
exp(b) # not an "odds ratio" 
exp(confint(M_14))

doctor_new <- tibble(age = seq(0.2, 0.8, by = 0.1))

M_14_pred <- mutate(
  doctor_new, 
  pred_log_rate = b[1] + b[2] * age,
  pred_rate = exp(pred_log_rate))

ggplot(M_14_pred, aes(age, pred_log_rate)) + geom_point() + geom_line()
ggplot(M_14_pred, aes(age, pred_rate)) + geom_point() + geom_line()

add_predictions(doctor_new, M_14)
add_predictions(doctor_new, M_14, type = 'response')

summary(M_14)

summary(M_14)$deviance
deviance(M_14)
-2 * logLik(M_14)

M_15 <- glm(doctorco ~ 1, 
            family = poisson(link = 'log'),
            data = doctor_df)
deviance(M_15) - deviance(M_14)
(-2 * logLik(M_15)) - (-2* logLik(M_14))

anova(M_15, M_14)

M_16 <- glm(doctorco ~ sex + age + income,
            family = poisson(link = 'log'),
            data = doctor_df)
summary(M_16)

anova(M_14, M_16)



biochem_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr26/refs/heads/main/data/biochemist.csv")

count(biochem_df, publications)

summarize(biochem_df, mean(publications), var(publications))


# Quasi-poisson -----------------------------------------------------------

M_17 <- glm(publications ~ gender + married + children + prestige + mentor,
            data = biochem_df,
            family = poisson(link = 'log'))

summary(M_17)

M_18 <- glm(publications ~ gender + married + children + prestige + mentor,
            data = biochem_df,
            family = quasipoisson(link='log'))

summary(M_18)

summary(M_17)$coef
summary(M_18)$coef


# Negative binomial distribution ------------------------------------------

data_df <- tibble(x = seq(0, 20))
data_df <- mutate(data_df, pois = dpois(x, lambda = 3.75))

ggplot(data_df, aes(x,pois)) + geom_point() + geom_line()

data_df <- mutate(data_df, pois = dpois(x, lambda = 9.75))

ggplot(data_df, aes(x,pois)) + geom_point() + geom_line()


data_df <- mutate(data_df, negbin = dnbinom(x, mu = 9.75, size = 5))

ggplot(data_df, aes(x,negbin)) + geom_point() + geom_line()

data_df <- mutate(data_df, negbin = dnbinom(x, mu = 9.75, size = 20))
ggplot(data_df, aes(x,negbin)) + geom_point() + geom_line()

M_19 <- glm.nb(publications ~ gender + married + children + prestige + mentor,
               data = biochem_df)
summary(M_19)

M_20 <- glm.nb(publications ~ gender + children + mentor,
               data = biochem_df)

anova(M_20, M_19)



# Excess zero modelling ---------------------------------------------------


smoking_df <- read_csv("https://raw.githubusercontent.com/mark-andrews/iglmr26/refs/heads/main/data/smoking.csv")

ggplot(smoking_df, aes(cigs)) + geom_bar()
ggplot(doctor_df, aes(doctorco)) + geom_bar()
ggplot(biochem_df, aes(publications)) + geom_bar()

M_21 <- glm(cigs ~ educ, family = poisson(link = 'log'), data = smoking_df)
summary(M_21)

# M_22 <- glm.nb(cigs ~ educ, data = smoking_df)
library(pscl)
M_22 <- zeroinfl(cigs ~ educ, data = smoking_df) # zero inflated Poisson
M_23 <- zeroinfl(cigs ~ educ, dist = "negbin", data= smoking_df) # zero inflated Neg Bin
summary(M_22)
summary(M_23)

smoking_new <- tibble(educ = c(1, 5, 10, 15, 20))
# prediction of the latent variable according to the logistic regression
add_predictions(smoking_new, M_22, type = 'zero')

# prediction of the mean of the count according to Poisson regression
add_predictions(smoking_new, M_22, type = 'count')
# average over both groups
add_predictions(smoking_new, M_22, type = 'response')


M_24 <- hurdle(cigs ~ educ, data = smoking_df) # hurdle Poisson
summary(M_24)

add_predictions(smoking_new, M_24, type = 'zero') # probability of clearing the hurdle
add_predictions(smoking_new, M_24, type = 'count') # mean of Poisson distribution for those over hurdle
