# Name: Chris O'Dell
# Course: COP2073C - R Programming
# Assignment: Module 11 Programming Assignment (PA11)
# Date: November 16, 2026

# Install these packages once in Posit.cloud if they are missing:
# install.packages(c("tidyverse", "modelr"))
library(tidyverse)
library(modelr)

# Keep only cylinders and weight, treating cylinders as categories.
cars <- mtcars %>%
  as_tibble() %>%
  select(cyl, wt) %>%
  mutate(cyl = factor(cyl))

# Fit a model that predicts weight from cylinder category.
weight_model <- lm(wt ~ cyl, data = cars)

# Create one row per cylinder group and predict the mean weight.
predictions <- cars %>%
  data_grid(cyl) %>%
  add_predictions(weight_model)

# Plot actual weights in black and predicted group means in red.
ggplot(cars, aes(x = as.numeric(as.character(cyl)), y = wt)) +
  geom_point(color = "black") +
  geom_point(
    data = predictions,
    aes(x = as.numeric(as.character(cyl)), y = pred),
    color = "red",
    size = 4
  ) +
  labs(x = "cyl", y = "wt")

# Analysis:
# The red points show the model's predicted average weight for each
# cylinder group. The 4-cylinder cars are the lightest on average,
# the 6-cylinder cars fall in the middle, and the 8-cylinder cars
# are the heaviest on average. The black points show how individual
# cars vary within each group. Cylinder category helps explain
# differences in weight, but it does not predict every car exactly.
