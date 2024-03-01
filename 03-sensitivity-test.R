library(tidyverse)
library(here)
library(readr)
library(viridis)
library(lattice)
library(gt)
library(mgcv)
library(gratia)
library(here)
library(mgcViz)
library(readxl)
library(gridExtra)
library(grid)
library(broom)

# look at df models 
df_models <- readRDS("df_models.rds")

# 
test <- df_models |>
  filter(Species == "Leuciscus_leuciscus") |>
  select(Lake, mean_last_7days, Depth_sample, Abundance, Presence) |>
  filter(Presence == 1)

# 
# df_models |> 
#   filter(Species == "Leuciscus_leuciscus") |>
#   ggplot(aes(mean_last_7days, Abundance)) +
#   geom_point()
# 
# 
# df_models |> 
#   filter(Species == "Leuciscus_leuciscus") |>
#   ggplot(aes(mean_last_7days, Depth_sample)) +
#   geom_point() +
#   scale_x_reverse()
#   
#   
#   df_models |>
#     filter(Species == "Leuciscus_leuciscus") |>
#     ggplot(aes(Depth_sample, Abundance)) +
#     geom_point() +
#     scale_x_reverse()


# take the 11 most abundant  species
species_list <- df_models |> 
  filter(tot_abu > 300) |> 
  distinct(Species) |> 
  pull(Species)
  
 # "Alburnus_alburnus", "Coregonus_sp_albeli", "Coregonus_sp","Gasterosteus_aculeatus",
 # "Gobio_gobio", "Leuciscus_leuciscus", "Perca_fluviatilis", "Rutilus_rutilus", 
 # "Sander_lucioperca", "Scardinius_erythrophthalmus","Gymnocephalus_cernua"


df_sensitivity <- df_models |> 
  filter(tot_abu > 300) |> 
  select(Lake, Species, Protocol, Depth_sample, Abundance, Presence, mean_last_7days) |> 
  #there are some NAs in depth -> drop them
  drop_na(Depth_sample, mean_last_7days)
  # we set depth to the mean of each species
  # mutate(mean_depth = mean(Depth_sample))

df_sensitivity$fLake <- as.factor(df_sensitivity$Lake)
df_sensitivity$fProtocol <- as.factor(df_sensitivity$Protocol)
# df_sensitivity$mean_depth <- as.numeric(df_sensitivity$mean_depth)


species_list <- df_sensitivity |> 
  distinct(Species) |> 
  pull(Species)
# 
i <- "Leuciscus_leuciscus"
for (i in species_list){
  
  data <- df_sensitivity |> 
    filter(Species == i)
  
  # gam for temperature and depth with depth kept constant at mean depth ->
  # does not work we get error 
  model1 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(Depth_sample, k = 3)
                + s(fLake, bs = 're')
                +  s(fProtocol, bs = 're'), family = ziP())
  
  model2 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3)
                + s(fLake, bs = 're')
                +  s(fProtocol, bs = 're'), family = ziP())
  

  unique_lakes <- distinct(data, Lake) |>
    pull()
  
  random_lake <- sample(unique_lakes, 1)
  
  # mean depth only in the depth where fish is present
  mean_depth <- data |>
    filter(Presence == 1) |>
    mutate(mean_depth = mean(Depth_sample)) |>
    pull(mean_depth)

  presence_depth <- data |> 
    filter(Presence == 1) |>
    distinct(Depth_sample) |> 
    pull(Depth_sample)
  
  # take randomly 5 values 
  
  random_depth <- sort(sample(presence_depth, 5))
  
  # Convert sorted values to factors with original order
  factor_random_depth <- factor(random_depth, levels = random_depth, ordered = TRUE)
  
  # grid for each of the models
 grid1 <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), length = 200),
    fProtocol = factor("VERT"), fLake = factor(random_lake), 
    Depth_sample = factor_random_depth)
  
  grid2 <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), length = 1000),
    fProtocol = factor("VERT"), fLake = factor(random_lake))
  
  
  print(glance(model1))
  print(glance(model2))
  
  
  
  model_prediction1 <- predict.gam(model1, newdata = grid1,
                                  exclude = c("s(fProtocol)", "s(fLake)"), 
                                  term = "s(mean_last_7days)", type = "response",                                # type = "response", 
                                  se.fit = TRUE)
  
  model_bind1 <- cbind(grid1, as.data.frame(model_prediction1)) |> 
    # rename(fit1 = fit) |>
    # rename(se.fit1 = se.fit) |>
    # rename(temp = mean_last_7days) |>
    mutate(model = factor("temp_and_depth"))
    # select(-Depth_sample)
  
  model_bind1 |> 
    ggplot(aes(mean_last_7days, fit, color = Depth_sample)) +
    geom_line()

    
  model_prediction2 <- predict.gam(model2, newdata = grid2,
                                   exclude = c("s(fProtocol)", "s(fLake)"), 
                                   type = "response", se.fit = TRUE)
  
  model_bind2 <- cbind(grid2, as.data.frame(model_prediction2)) |> 
    mutate(model = factor("temp")) 
  

  df_pred <- bind_rows(model_bind1, model_bind2) |> 
    rename(temp = mean_last_7days) |>
    select(-fProtocol, -fLake) |> 
    mutate(Depth_sample = factor(ifelse(is.na(Depth_sample), "temp", as.character(Depth_sample))))


  # Extract numeric levels
  numeric_levels <- levels(df_pred$Depth_sample)[!levels(df_pred$Depth_sample) %in% "temp"]
  
  # Sort numeric levels
  sorted_numeric_levels <- sort(as.numeric(numeric_levels))
  
  # Combine sorted numeric levels with "temp" level
  sorted_levels <- c(as.character(sorted_numeric_levels), "temp")
  
  # Reorder levels
  df_pred$Depth_sample <- factor(df_pred$Depth_sample, levels = sorted_levels)
  
  # Check the levels to confirm they are ordered from lowest to highest
  levels(df_pred$Depth_sample)
  
  df_pred_rescaled <- df_pred |> 
    group_by(model) |> 
    mutate(fit_rescaled = (fit - min(fit)) / (max(fit) - min(fit))) |> 
    mutate(se_fit_rescaled = (se.fit - min(se.fit)) / (max(se.fit) - min(se.fit)))
  
  plot_pred <- df_pred_rescaled |>
    ggplot(aes(temp, fit_rescaled, color = Depth_sample, fill = Depth_sample)) +
    geom_line() +
    geom_ribbon(aes(ymin = (fit_rescaled - se.fit), ymax = (fit_rescaled + se.fit)), color = NA, alpha = 0.1) +
    theme_bw() +
    labs(title = paste("Sensitivity Test", i)) +
    scale_color_manual(values = c("#08306B", "#08519C", "#4292C6", "#6BAED6", "#9ECAE1",  "#D73027"), aesthetics = c("color", "fill"))
  
  plot_pred
  
  
  plot_pred_model <- df_pred_rescaled |>
    ggplot(aes(temp, fit_rescaled, color = model, fill = model)) +
    geom_line() +
    geom_ribbon(aes(ymin = (fit_rescaled - se.fit), ymax = (fit_rescaled + se.fit)), color = NA, alpha = 0.1) +
    theme_bw() +
    labs(title = paste("Sensitivity Test", i)) +
    scale_color_manual(values = c("#08306B", "#D73027"), aesthetics = c("color", "fill"))
  # facet_wrap(~model)

  
  tiff(paste("total_models/sensitivity/sensitivity_test_depth_", i ,".tiff", sep = ""), units="in", width=9, height=6, res=300)
  
  print(plot(plot_pred))
  
  dev.off()
  
  tiff(paste("total_models/sensitivity/sensitivity_test_model_", i ,".tiff", sep = ""), units="in", width=9, height=6, res=300)
  
  print(plot(plot_pred_model))
  
  dev.off()
  
}

