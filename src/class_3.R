# Dependencies
library(tidyverse)
library(rio)

# Import
pisa <- import("data/2012 PISA multiple countries selected variables.sav") %>%
  as_tibble()
pisa

# Recode gender to 0 and 1 (female)
pisa$gender <- pisa$ST04Q01 - 1
table(pisa$gender)

# Regression model
model <- lm(PV1MATH ~ ESCS + gender + ST25Q01, data = pisa)

# Summarize the model
summary(model)
