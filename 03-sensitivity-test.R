library(tidyverse)
library(here)
library(readr)
library(viridis)
library(lattice)
library(gt)
library(mgcv)
library(gratia)
library(here)
library(gamm4)
library(broom)
library(mgcViz)
library(readxl)
library(forcats)
library(gridExtra)
library(grid)
library(gghighlight)


# wie hat Conor das gemeint mit tiefe konstant halten? Und wie kann ich diese mean_depth
# in die predictions inkludieren?
# look at df models 
df_models <- readRDS("df_models.rds")

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
  drop_na(Depth_sample) |> 
  group_by(Species) |> 
  # we set depth to the mean of each species
  mutate(mean_depth = mean(Depth_sample))

df_sensitivity$fLake <- as.factor(df_sensitivity$Lake)
df_sensitivity$fProtocol <- as.factor(df_sensitivity$Protocol)
df_sensitivity$mean_depth <- as.numeric(df_sensitivity$mean_depth)


species_list <- df_sensitivity |> 
  distinct(Species) |> 
  pull(Species)

i <- "Alburnus_alburnus"
for (i in species_list){
  
  data <- df_sensitivity |> 
    filter(Species == i)
  
  str(data)
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
  
  # depth sample is way too long, why??????
 not_workign <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), length = 1500),
    fProtocol = factor("VERT"), fLake = factor(random_lake), 
    Depth_sample = seq(from = min(data$mean_depth),
                       to = max(data$mean_depth), length = 1500))
  
  grid2 <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), length = 1500),
    fProtocol = factor("VERT"), fLake = factor(random_lake))
  
  print(glance(model1))
  print(glance(model2))
  
  model_prediction1 <- predict.gam(model1, newdata = grid1,
                                  exclude = c("s(fProtocol)", "s(fLake)"), 
                                  type = "response", se.fit = TRUE)
  
  model_bind1 <- cbind(grid1, as.data.frame(model_prediction1))
  
  
  model_prediction2 <- predict.gam(model2, newdata = grid2,
                                   exclude = c("s(fProtocol)", "s(fLake)"), 
                                   type = "response", se.fit = TRUE)
  
  model_bind2 <- cbind(grid2, as.data.frame(model_prediction2))
  
  # pred_df1 <- model_bind1 |>
  #   rename(temp = mean_last_7days) |> 
  #   mutate(species = factor(i))
  # 
  # summary <- summary(gam_output1)
  # 
  # 
  # 
  # plot_pred1 <- pred_df1 |>
  #   ggplot(aes(temp, fit)) +
  #   geom_line() +
  #   geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), alpha = 0.3) +
  #   theme_bw() +
  #   # facet_wrap(~fLake, scale = "free") +
  #   theme(strip.background = element_rect(fill="lightgrey")) +
  #   labs(title = paste("Species = ", i,
  #                      "deviance explained = ", signif(summary[["dev.expl"]])))
  
  
  # gam(y ~ s(temp) + s(depth) + re) -> depth constant at mean depth of species and gam(y ~ s(temp) +re)
  # plot predictions of both in one plot and look at differences
  # rescale() after predictions to account for differences
  
}