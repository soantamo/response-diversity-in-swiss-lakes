library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(lattice)
library(broom)
library(mgcViz)
library(DHARMa)
#https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html

#####continue with df_one prediction
#this model for species with binomial data and random effects
# 
# df_final <- readRDS("df_final.rds")
# # testing if s.cephalus works if we only have one fish caught in brienz 
# 
# squalius <- df_final |> 
#   filter(Species == "Squalius_cephalus")

df_binomial_re <- readRDS("data_frame_models/df_binomial_re")
  # filter(!Species == "Coregonus_brienzii")

table(df_binomial_re$Abundance) 
str(df_binomial_re)
head(df_binomial_re)

df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)

####LOOOP#######################################################################

#adding the manual margin effect calculation

species_list <- df_binomial_re |> 
  # filter(Species == "Squalius_cephalus") |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()
grid <- list()
pred_df <- list()
unique_lakes <- list()
unique_protocol <- list()
tiff_filename <- list()
tiff_file_2 <- list()
temp_data <- list()

df_binomial_re$fLake <- as.factor(df_binomial_re$Lake)
df_binomial_re$fProtocol <- as.factor(df_binomial_re$Protocol)

df_binomial_re$fLake <- as.factor(df_binomial_re$Lake)
df_binomial_re$fProtocol <- as.factor(df_binomial_re$Protocol)


str(df_binomial_re)
#make new loop 
###predict.gam needs something else

for (i in species_list) {
  data <- df_binomial_re |> 
    filter(Species == i)
  unique_lakes <- distinct(data, fLake)
  unique_protocol <- distinct(data, fProtocol)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ), fLake = unique_lakes$fLake, fProtocol = unique_protocol$fProtocol)
  gam_output[[i]]  <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = "re")
                          + s(fProtocol, bs = 're'), family = binomial)
  # prepare residuals
  simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
  # # Main plot function from DHARMa, which gives 
  # # Left: a qq-plot to detect overall deviations from the expected distribution
  # # Right: a plot of the residuals against the rank-transformed model predictions
  tiff_filename <- paste("model_3/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(plot(simulationOutput))
  dev.off()
  # # get rid of NAs in temp datat
  temp_data <- data |>
    drop_na(mean_last_7days)
  # # Plotting standardized residuals against predictors
  tiff_file_2 <- paste("model_3/gam_check/predictor_", i, ".tiff", sep = "")
  tiff(tiff_file_2, width = 800, height = 600)
  print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
  dev.off()

  print(glance(gam_output[[i]]))

  model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid, type = "response", se.fit = TRUE)
  model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
  pred_df <- model_bind |>
    group_by(mean_last_7days) |>
    mutate(fit = mean(fit)) |>
    mutate(lower = fit - 1*se.fit, upper = fit + 1*se.fit) |>
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
    rename(temp = mean_last_7days) |> 
    #with across() we can retain our column for se.fit
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_3/predictions/predictions_",i,".rds"))
}


# predictions df

df_pred_mod3 <- list.files(path = "model_3/predictions", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
saveRDS(df_pred_mod3, "total_models/pred_model_3_total")



df_pred_mod3 |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species) +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE) 


pred_df |> 
  ggplot(aes(temp, fit)) +
  geom_line()


###################derivatives


species_list <- df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_binomial_re$fLake <- as.factor(df_binomial_re$Lake)
df_binomial_re$fProtocol <- as.factor(df_binomial_re$Protocol)

str(df_binomial_re)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()

for (i in species_list) {
  data <- df_binomial_re |> 
    filter(Species == i)
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = "re")
                    + s(fProtocol, bs = 're'), family = binomial)
  lake_list <- distinct(data, Lake) |> 
    pull()
  
  for (j in lake_list){
    
    data_lake <- df_binomial_re |> 
      filter(Species == i) |> 
      filter(fLake == j)
    
    unique_lakes <- distinct(data_lake, fLake)
    unique_protocol <- distinct(data_lake, fProtocol)
    
    newdata <- tibble(mean_last_7days = seq(
      from = min(data_lake$mean_last_7days, na.rm = TRUE),
      to = max(data_lake$mean_last_7days, na.rm = TRUE), length = 200),
      fLake = unique_lakes$fLake, fProtocol = sample(levels(unique_protocol$fProtocol), size = 200, replace = TRUE))

    derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
      mutate(fLake = factor(j)) |>
      mutate(species = factor(i)) |>
      rename(temp = data)
    saveRDS(derivatives, paste0("model_3/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}



#prepare total df for derivatives

df_deriv_mod3 <- list.files(path = "model_3/derivatives", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# # save total derivatives as RDS
saveRDS(df_deriv_mod3, "total_models/deriv_model_3_total")


#prepare for all df
mean_se_model_3 <- df_pred_mod3 |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  mutate(max_se = max(se.fit)) |> 
  mutate(min_se = min(se.fit)) |> 
  distinct(mean_se, max_se, min_se)


test3 <- df_binomial_re |> 
  group_by(Species, Presence) |> 
  summarize(n_observations = sum(Presence)) |> 
  select(Species, n_observations) |> 
  filter(n_observations != 0) |> 
  rename(species = Species)


n_lake3 <- df_binomial_re |>
  group_by(Species) |>
  summarize(n_lake = n_distinct(Lake)) |>
  rename(species = Species)

two_bind <- merge(test3, n_lake3, by.x = "species")

bind_3 <- merge(two_bind, mean_se_model_3)

# saveRDS(bind_3, "model_3/bind_3.rds")

