library(tidyverse)

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
