# Dependencies
library(car)
library(tidyverse)
library(rio)
library(psych)

# Import data
roller <- import("data/rollercoasters.sav")
roller

# Set default plot theme
theme_set(theme_bw())

# Check first few rows
roller %>% 
  as_tibble()

# Check level of measurement
map_chr(roller, typeof)

# NOTE ABOUT ASSUMPTIONS --------------------------------------------------

# Technically, assumption checking should occur prior to conducting tests
# of significance. This step was omitted for exploratory purposes.

# EXPLORING ASSOCIATION ---------------------------------------------------

# Among continuous variables, which have the highest correlation?

# Select only continuous data
roller_1 <- roller %>%
  select(-c("park_id", "theme", "rollercoaster_type", "excitement_rating", 
            "intensity_rating", "nausea_rating")) %>%
  as_tibble()
roller_1

# Check for dichotomous values
View(roller_1)

# Remove binary
roller_2 <- roller_1 %>%
  select(-c("custom_design", "inversions"))

# Print the names of subsetted data fram
names(roller_2)

# Explore continuous data
pairs.panels(roller_2)

# Scatter plot in class
roller %>%
  ggplot(aes(x = total_air_time, y = excitement)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 2)

# Correlation matrix
roller_matrix <- cor(roller_2) %>%
  as.data.frame()
roller_matrix

# Find strongest correlations
rownames_to_column(roller_matrix) %>%
  as_tibble() %>%
  gather(key = "column_name", value = "value", -rowname) %>%
  # Remove singularity
  filter(value != 1) %>%
  # Filter for unique correlations
  distinct(value, .keep_all = TRUE) %>%
  # Greatest values first
  arrange(desc(value))

# Scatter plot w/ LS line 1
roller %>%
  ggplot(aes(x = highest_drop_height, y = max_speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 2)

# Scatter plot w/ LS line 2
roller %>%
  ggplot(aes(x = ride_length, y = ride_time)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 2)

# Scatter plot w/ LS line 3
roller %>%
  ggplot(aes(x = avg_speed, y = max_speed)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, size = 2)

# EXPLORING DIFFERENCE ----------------------------------------------------

# When the author created custom roller coasters, which ride features were 
# different?

# Should we assume equal variances?

# Select variables of interest
roller_diff <- roller %>%
  select(custom_design, highest_drop_height, max_speed, ride_length, ride_time)

# Initialize empty list
equal_var_test <- list()

# Call Levene's test on all variables
for (i in 2:length(roller_diff)) {
  
  # Store as a list
  equal_var_test[[i]] <- leveneTest(roller_diff[[i]], group = roller_diff$custom_design)
}

# Print all test of homogeneity of variance
equal_var_test

# Equal variances cannot be assumed

# Original roller coasters
custom_no <- roller %>%
  filter(custom_design == 0)

# Custom roller coasters
custom_yes <- roller %>%
  filter(custom_design == 1)

# Is there a difference in: highest drop?
t.test(custom_no$highest_drop_height, custom_yes$highest_drop_height, 
       var.equal = FALSE)

# Is there a difference in: maximum speed?
t.test(custom_no$max_speed, custom_no$max_speed,
       var.equal = FALSE)

# Is there a difference in: ride length?
t.test(custom_no$ride_length, custom_yes$ride_length,
       var.equal = FALSE)

# Is there a difference in: ride time?
t.test(custom_no$ride_time, custom_yes$ride_time, 
       var.equal = FALSE)

# These variables are not significantly different
# Failed normality assumption?
roller %>%
  select(custom_design, highest_drop_height, max_speed, ride_length, ride_time) %>%
  gather(key = "variable", value = "value", -custom_design) %>%
  ggplot(aes(log10(value))) +
    geom_histogram() +
    facet_wrap(~ variable + custom_design)

# Visual inspection of normality suggests that the assumption is not tenable

# Check group sizes
roller %>%
  count(custom_design)

# Group sizes do not seem approximately equal
# Custom roller coasters are 2x more numerous 