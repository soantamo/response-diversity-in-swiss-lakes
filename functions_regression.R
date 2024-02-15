

# fit model with interaction
# https://medium.com/@josef.waples/partial-regression-added-variable-plots-in-r-f1228e7612d9

partial_regression_x1 <- function(y, x1, x2, df){
  require(patchwork)
  require(tidyverse)
  require(ggplot2)
  
  model_x1_and_x2 <- lm(y ~ x1 + x2, data = df)
  
  data_added_variables <- df
  
  model_x1 <- lm(y ~ x1, data = df)
  data_added_variables$x1_residuals <- residuals(model_x1)
  print(plot(model_x1))
  
  model_x2 <- lm(y ~ x2, data = df)
  data_added_variables$x2_residuals <- residuals(model_x2)
  print(plot(model_x2))
  
  model_x1_against_x2 <- lm(x1 ~ x2, data = df)
  data_added_variables$x1_against_x2_residuals <- residuals(model_x1_against_x2)
  print(plot(model_x1_against_x2))
  
  model_x2_against_x1 <- lm(x2 ~ x1, data = df)
  data_added_variables$x2_against_x1_residuals <- residuals(model_x2_against_x1)
  print(plot(model_x2_against_x1))
  
  
  ggplot(data_added_variables, aes(x = x1_against_x2_residuals, y = x2_residuals)) +
    geom_point() +
    geom_smooth(method = 'lm', color = '#542788') +
    labs(subtitle = "Partial Regression: y ~ x1 + x2") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    labs(caption = "holding x2 constant") +
    xlab(" \n (x1 given others)") +
    ylab(" \n (y given others)")


  
}

partial_regression_x2 <- function(y, x1, x2, df){
  require(patchwork)
  require(tidyverse)
  require(ggplot2)
  
  model_x1_and_x2 <- lm(y ~ x1 + x2, data = df)
  
  data_added_variables <- df
  
  model_x1 <- lm(y ~ x1, data = df)
  data_added_variables$x1_residuals <- residuals(model_x1)
  print(summary(model_x1))
  
  model_x2 <- lm(y ~ x2, data = df)
  data_added_variables$x2_residuals <- residuals(model_x2)
  print(summary(model_x2))
  
  model_x1_against_x2 <- lm(x1 ~ x2, data = df)
  data_added_variables$x1_against_x2_residuals <- residuals(model_x1_against_x2)
  print(summary(model_x1_against_x2))
  
  model_x2_against_x1 <- lm(x2 ~ x1, data = df)
  data_added_variables$x2_against_x1_residuals <- residuals(model_x2_against_x1)
  print(summary(model_x2_against_x1))
  

  ggplot(data_added_variables, aes(x = x2_against_x1_residuals, y = x1_residuals)) +
    geom_point() +
    geom_smooth(method = 'lm', color = '#E08214') +
    labs(subtitle = "Partial Regression: y ~ x2 + x1") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    labs(caption = "holding x1 constant") +
    xlab(" \n (x2 given others)") +
    ylab(" \n (y given others)") 

  
}