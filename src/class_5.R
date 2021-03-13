# Dependencies
library(tidyverse)
library(rio)

# Import
pisa <- import("data/2012 PISA multiple countries selected variables.sav") %>%
  as_tibble()
pisa

# Rename pisa
pisa_1 <- pisa %>%
  rename(country = CNT, language = ST25Q01, enjoy_math = ST29Q04,
         gender = ST04Q01, math_career = ST48Q05, applied_math = ST76Q01,
         solve_equation = ST37Q05, math_score = PV1MATH, ses = ESCS)

# Recode
pisa_label <- pisa_1 %>%
  mutate(
    # Country
    country = recode(country, "CAN" = "Canada", "FIN" = "Finland",
                      "JOR" = "Jordan", "MEX" = "Mexico", "NZL" = "New Zealand"),
    # Language of the test same as native language
    language = recode(language, `1` = "Same", `2` = "Different"),
    # Enjoy math
    enjoy_math = recode(enjoy_math, `1` = "strongly agree", `2` = "agree", 
                     `3` = "disagree", `4` = "strongly disagree"),
    # Gender
    gender = recode(gender, `1` = "female", `2` = "male"),
    # Math career
    math_career = recode(math_career, `1` = "math", `2` = "science"),
    # Experience with applied math
    applied_math = recode(applied_math, `1` = "frequently", `2` = "sometimes", 
                     `3` = "rarely", `4` = "never"),
    # Confidence in solving equation
    solve_equation = recode(solve_equation, `1` = "very confident", `2` = "confident",
                     `3` = "not very confident", `4` = "not at all confident")
  ) 
pisa_label

# Recode dichotomous values
pisa_dummy <- pisa_1 %>%
  mutate(
    # 0 = same; 1 = different
    language = recode(language, `1` = 0, `2` = 1),
    # 0 = male; 1 = female
    gender = recode(gender, `1` = 0, `2` = 1)
  )
pisa_dummy

# Assessing linearity
# ----------------------
# If Q-Q plot of residuals shows points close to the line, then there is evidence
# of linearity; if normal, then the points would align with the line perfect; the
# residuals are plotted against a line if the distribution was normal.

# If there is no cone shape in a scatter plot of residuals versus predicted values
# (i.e., randomly distributed), then there is evidence of linearity. In other words,
# there should be no relationship between the residuals and the predicted values.

# We can also assess a histogram of residuals to evaluate the assumption of linearity. 

# TEST MODEL --------------------------------------------------------------

# Define the linear regression
model <- lm(math_score ~ ses + gender + language + enjoy_math, data = pisa_dummy)

# Summary of the model
summary(model)

# ASSUMPTIONS -------------------------------------------------------------

# Check independence of observations
durbinWatsonTest(model)

# Check linearity
par(mfrow = c(2, 2))
plot(model)

# Remove missing values
pisa_dummy %>%
  filter(!is.na(math_score), !is.na(gender), !is.na(ses), !is.na(language), !is.na(enjoy_math)) %>%
  # Create a plot of standarized residuals vs. fitted values only
  ggplot(aes(model$fitted.values, rstandard(model))) +
  geom_point() +
  geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Residuals")
