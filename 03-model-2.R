library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)
library(broom)
library(mgcViz)
library(DHARMa)
library(readxl)

#second model for species with abundance data which only occur in one lake
#re-do with two additional species

#read df
df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")
df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
df_abundance_gam$fLake <- as.factor(df_abundance_gam$Lake)


table(df_abundance_gam$Abundance) 
str(df_abundance_gam)
head(df_abundance_gam)

#####Loop Model 2 with binomial ######
species_list <- df_abundance_gam |>
  # binomial species
  filter(Species == "Coregonus_sp_benthic_profundal") |>
  distinct(Species) |> 
  pull(Species)


species_list <- sort(species_list)

df_abundance_gam |>
  group_by(Species) |> 
  summarize(tot_abu = sum(Presence))


gam_output <- list()
model_prediction <- list()
derivatives <- list()
pred_df <- list()
grid <- list()
unique_method <- list()
simulationOutput <- list()
tiff_file_2  <- list()
temp_data <- list()


df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
str(df_abundance_gam)

df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
str(df_abundance_gam)

# binomial loop 
for (i in species_list) {
  data <- df_abundance_gam |> 
    filter(Species == i)
  unique_method <- distinct(data, fProtocol)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
    fProtocol = unique_method$fProtocol)
  gam_output[[i]] <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) +
                           s(fProtocol, bs = 're'), family = binomial())
  # prepare residuals
  simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
  # # Main plot function from DHARMa, which gives 
  # # Left: a qq-plot to detect overall deviations from the expected distribution
  # # Right: a plot of the residuals against the rank-transformed model predictions
  tiff_filename <- paste("model_2/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(plot(simulationOutput))
  dev.off()
  # # get rid of NAs in temp datat
  temp_data <- data |>
    drop_na(mean_last_7days)
  # # Plotting standardized residuals against predictors
  tiff_file_2 <- paste("model_2/gam_check/predictor_", i, ".tiff", sep = "")
  tiff(tiff_file_2, width = 800, height = 600)
  print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
  dev.off()
  # print(glance(gam_output[[i]]))
  # 
  model_prediction[[i]] <- predict.gam(gam_output[[i]], grid, type = "response", se.fit = TRUE) #adding se, $fit
  model_bind <- cbind(model_prediction[[i]], grid)
  pred_df <- model_bind |>
    group_by(mean_last_7days) |>
    mutate(fit = mean(fit)) |>
    mutate(lower = fit - 1*se.fit, upper = fit + 1*se.fit) |>
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
              across(se.fit), across(fProtocol)) |>
    rename(temp = mean_last_7days) |>
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_2/predictions/predictions_",i,".rds"))
}

#####Loop Model 2 ######
#ZIP

species_list <- df_abundance_gam |>
  # binomial species
  filter(!Species == "Coregonus_sp_benthic_profundal") |>
  distinct(Species) |> 
  pull(Species)

#less problems with comparing
species_list <- sort(species_list)

df_abundance_gam |>
  group_by(Species) |> 
  summarize(tot_abu = sum(Presence))
  

gam_output <- list()
model_prediction <- list()
derivatives <- list()
pred_df <- list()
grid <- list()
unique_method <- list()
simulationOutput <- list()
tiff_file_2  <- list()
temp_data <- list()


df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
str(df_abundance_gam)
#make new loop 

for (i in species_list) {
  data <- df_abundance_gam |> 
    filter(Species == i)
  unique_method <- distinct(data, fProtocol)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
    fProtocol = unique_method$fProtocol)
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                           s(fProtocol, bs = 're'), family = ziP())
  # prepare residuals
  simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
  # # Main plot function from DHARMa, which gives 
  # # Left: a qq-plot to detect overall deviations from the expected distribution
  # # Right: a plot of the residuals against the rank-transformed model predictions
  tiff_filename <- paste("model_2/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(plot(simulationOutput))
  dev.off()
  # # get rid of NAs in temp datat
  temp_data <- data |>
    drop_na(mean_last_7days)
  # # Plotting standardized residuals against predictors
  tiff_file_2 <- paste("model_2/gam_check/predictor_", i, ".tiff", sep = "")
  tiff(tiff_file_2, width = 800, height = 600)
  print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
  dev.off()
  # print(glance(gam_output[[i]]))
  # 
  model_prediction[[i]] <- predict.gam(gam_output[[i]], grid, type = "response", se.fit = TRUE) #adding se, $fit
  model_bind <- cbind(model_prediction[[i]], grid)
  pred_df <- model_bind |>
    group_by(mean_last_7days) |>
    mutate(fit = mean(fit)) |>
    mutate(lower = fit - 1*se.fit, upper = fit + 1*se.fit) |>
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
              across(se.fit), across(fProtocol)) |>
    rename(temp = mean_last_7days) |>
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_2/predictions/predictions_",i,".rds"))
}

# predictions df

df_pred_mod2 <- list.files(path = "model_2/predictions", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
saveRDS(df_pred_mod2, "total_models/pred_model_2_total")



###################derivatives

########################## 

# ######################## binomial 

species_list <- df_abundance_gam |> 
  # binomial one
  filter(Species == "Coregonus_sp_benthic_profundal") |>
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
df_abundance_gam$fLake <- as.factor(df_abundance_gam$Lake)

str(df_abundance_gam)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()

species_lake <- read_xlsx("species_lake.xlsx") 

species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)

str(species_lake)

for (i in species_list) {
  
  data <- df_abundance_gam |> 
    filter(Species == i)
  
  gam_output[[i]] <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) +
                           s(fProtocol, bs = 're'), family = binomial())
  
  lake_data <- species_lake |>
    filter(Species == i)
  
  lake_list <- distinct(lake_data, Lake) |>
    pull()

  for (j in lake_list){
    
    data_lake <- species_lake |>
      filter(Species == i) |>
      filter(Lake == j)
    
    unique_lakes <- distinct(data_lake, fLake)
    unique_protocol <- distinct(data_lake, fProtocol)
    
    newdata <- tibble(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), length = 200),
      fLake = unique_lakes$fLake, fProtocol = sample(levels(unique_protocol$fProtocol), size = 200, replace = TRUE))
  
    
    derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
      mutate(fLake = factor(j)) |>
      mutate(species = factor(i)) |>
      rename(temp = data)
    saveRDS(derivatives, paste0("model_2/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


########################## ZIP
species_list <- df_abundance_gam |> 
  # binomial ones
  filter(!Species == "Coregonus_sp_benthic_profundal") |>
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
df_abundance_gam$fLake <- as.factor(df_abundance_gam$Lake)

str(df_abundance_gam)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()


species_lake <- read_xlsx("species_lake.xlsx") 

species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)

str(species_lake)


for (i in species_list) {
  
  data <- df_abundance_gam |> 
    filter(Species == i)
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fProtocol, bs = 're'),
                         family = ziP())

  lake_data <- species_lake |>
    filter(Species == i)
  
  lake_list <- distinct(lake_data, Lake) |>
    pull()
  
  for (j in lake_list){
    
    data_lake <- species_lake |>
      filter(Species == i) |>
      filter(Lake == j)

    unique_lakes <- distinct(data_lake, fLake)
    unique_protocol <- distinct(data_lake, fProtocol)

    newdata <- tibble(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), length = 200),
      fProtocol = sample(levels(unique_protocol$fProtocol), size = 200, replace = TRUE))
    
    derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
      mutate(fLake = factor(j)) |>
      mutate(species = factor(i)) |>
      rename(temp = data)
    saveRDS(derivatives, paste0("model_2/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


#prepare total df for derivatives

df_deriv_mod2 <- list.files(path = "model_2/derivatives", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
saveRDS(df_deriv_mod2, "total_models/deriv_model_2_total")


# df for all species

# mean_se_model_2 <- df_pred_mod2 |>
#   group_by(species) |>
#   mutate(mean_se = mean(se.fit)) |>
#   mutate(max_se = max(se.fit)) |>
#   mutate(min_se = min(se.fit)) |>
#   distinct(mean_se, max_se, min_se)
# 
# 
# test2 <- df_abundance_gam |>
#  group_by(Species, Presence) |> 
#   summarize(n_observations = sum(Presence)) |> 
#   select(Species, n_observations) |> 
#   filter(n_observations != 0) |> 
#   rename(species = Species) |>
#   mutate(n_lake = factor("1"))

# double check if all occur in one
# n_lake <- df_abundance_gam |>
#   group_by(Lake, Species) |>
#   summarize(TotalAbundance = sum(Abundance), .groups = 'drop') |>
#   filter(TotalAbundance > 1) |>
#   group_by(Species) |>
#   summarize(n_lake = n_distinct(Lake)) |>
#   rename(species = Species)

# bind_2 <- merge(mean_se_model_2, test2)
# 
# saveRDS(bind_2, "model_2/bind_2.rds")

