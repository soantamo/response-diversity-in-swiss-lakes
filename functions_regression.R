

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
  
  model_x2 <- lm(y ~ x2, data = df)
  data_added_variables$x2_residuals <- residuals(model_x2)
  
  model_x1_against_x2 <- lm(x1 ~ x2, data = df)
  data_added_variables$x1_against_x2_residuals <- residuals(model_x1_against_x2)
  
  model_x2_against_x1 <- lm(x2 ~ x1, data = df)
  data_added_variables$x2_against_x1_residuals <- residuals(model_x2_against_x1)
  
  
  ggplot(data_added_variables, aes(x = x1_against_x2_residuals, y = x2_residuals)) +
    geom_point() +
    geom_smooth(method = 'lm', color = '#e76254') +
    labs(title = "Regression", subtitle = "Partial Regression: y ~ x1 + x2") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    labs(caption = "holding x2 constant") +
    xlab(" \n (x1_against_x2_residuals)") +
    ylab(" \n (x2_residuals)")


  
}

partial_regression_x2 <- function(y, x1, x2, df){
  require(patchwork)
  require(tidyverse)
  require(ggplot2)
  
  model_x1_and_x2 <- lm(y ~ x1 + x2, data = df)
  
  data_added_variables <- df
  
  model_x1 <- lm(y ~ x1, data = df)
  data_added_variables$x1_residuals <- residuals(model_x1)
  
  model_x2 <- lm(y ~ x2, data = df)
  data_added_variables$x2_residuals <- residuals(model_x2)
  
  model_x1_against_x2 <- lm(x1 ~ x2, data = df)
  data_added_variables$x1_against_x2_residuals <- residuals(model_x1_against_x2)
  
  model_x2_against_x1 <- lm(x2 ~ x1, data = df)
  data_added_variables$x2_against_x1_residuals <- residuals(model_x2_against_x1)
  
  

  ggplot(data_added_variables, aes(x = x2_against_x1_residuals, y = x1_residuals)) +
    geom_point() +
    geom_smooth(method = 'lm', color = '#ef8a47') +
    labs(title = "Regression", subtitle = "Partial Regression: y ~ x2 + x1") +
    theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
    theme(plot.subtitle = element_text(hjust = 0.5)) +
    labs(caption = "holding x1 constant") +
    xlab(" \n (x2_against_x1_residuals)") +
    ylab(" \n (x1_residuals)") 

  
}