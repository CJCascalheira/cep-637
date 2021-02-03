# Dependencies
library(rio)
library(tidyverse)

# Import
pisa <- import("data/2012 PISA multiple countries selected variables.sav") %>%
  as_tibble()
pisa

# Conduct a linear regression
math_ses <- lm(PV1MATH ~ ESCS, data = pisa)

# Summarize the model
summary(math_ses)
