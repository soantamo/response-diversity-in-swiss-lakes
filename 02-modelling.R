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

#loading subset of df for model 1

df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")

#####Loop Model 1 ######

species_list <- df_binomial_gam |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()
pred_df <- list()
grid <- list()
unique_method <- list()
simulationOutput <- list()
tiff_file_2  <- list()
temp_data <- list()


df_binomial_gam$fProtocol <- as.factor(df_binomial_gam$Protocol)
df_binomial_gam$fLake <- as.factor(df_binomial_gam$Lake)
str(df_binomial_gam)
#make new loop
#adding fProtocol as random effect

  

for (i in species_list) {
  data <- df_binomial_gam |> 
    filter(Species == i)
  unique_method <- distinct(data, fProtocol)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
    fProtocol = unique_method$fProtocol)
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                      s(fProtocol, bs = 're'), family = binomial)
  
  # prepare residuals
  simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
  # Main plot function from DHARMa, which gives 
  # Left: a qq-plot to detect overall deviations from the expected distribution
  # Right: a plot of the residuals against the rank-transformed model predictions
  tiff_filename <- paste("model_1/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(plot(simulationOutput))
  dev.off()
  # get rid of NAs in temp datat
  temp_data <- data |> 
    drop_na(mean_last_7days)
  # Plotting standardized residuals against predictors
  tiff_file_2 <- paste("model_1/gam_check/predictor_", i, ".tiff", sep = "")
  tiff(tiff_file_2, width = 800, height = 600)
  print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
  dev.off()
  print(glance(gam_output[[i]]))
  model_prediction[[i]] <- predict.gam(gam_output[[i]], grid, type = "response", se.fit = TRUE) #adding se, $fit
  model_bind <- cbind(model_prediction[[i]], grid)
  pred_df <- model_bind |>
    group_by(mean_last_7days) |>
    mutate(fit = mean(fit)) |>
    mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) |>
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
              across(se.fit), across(fProtocol)) |>
    rename(temp = mean_last_7days) |>
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_1/predictions/predictions_",i,".rds"))
}

# warning for "Gasterosteus_gymnurus" 
# 16: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS,  ... :
#                 Iterationsgrenze erreicht ohne volle Konvergenz -- sorgfältig pr

# predictions df
df_pred_mod1 <- list.files(path = "model_1/predictions", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
# saveRDS(df_pred_mod1, "total_models/pred_model_1_total")


df_pred_mod1 |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scales = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE) 



###################derivatives


########################## some problem with the derivatives, try this

species_list <- df_binomial_gam |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_binomial_gam$fLake <- as.factor(df_binomial_gam$Lake)
df_binomial_gam$fProtocol <- as.factor(df_binomial_gam$Protocol)

str(df_binomial_gam)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()

for (i in species_list) {
  data <- df_binomial_gam |> 
    filter(Species == i)
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fProtocol, bs = 're'), family = binomial)
  lake_list <- distinct(data, Lake) |> 
    pull()
  for (j in lake_list){
    
    data_lake <- df_binomial_gam |> 
      filter(fLake == j)
    
    unique_lakes <- distinct(data_lake, fLake)
    unique_protocol <- distinct(data_lake, fProtocol)
    
    newdata <- tibble(mean_last_7days = seq(
      from = min(data_lake$mean_last_7days, na.rm = TRUE),
      to = max(data_lake$mean_last_7days, na.rm = TRUE), length = 200),
      fProtocol = sample(levels(unique_protocol$fProtocol), size = 200, replace = TRUE))
    
    derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
      mutate(fLake = factor(j)) |>
      mutate(species = factor(i)) |>
      rename(temp = data)
    saveRDS(derivatives, paste0("model_1/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

# again Warnmeldung:
# In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#             Iterationsgrenze erreicht ohne volle Konvergenz -- sorgfältig prüfen
# probably for gasterosteus gymnurus

# df_binomial_gam |> 
#   filter(Species == "Salmo_marmoratus") |> 
#   group_by(Lake) |> 
#   summarize(observations = sum(Abundance))
#prepare total df for derivatives

df_deriv_mod1 <- list.files(path = "model_1/derivatives", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
# saveRDS(df_deriv_mod1, "total_models/deriv_model_1_total")

# prepare mean values of se.fit 

mean_se_model_1 <- total_model_1_pred |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  mutate(max_se = max(se.fit)) |> 
  mutate(min_se = min(se.fit)) |> 
  distinct(mean_se, max_se, min_se)


test1 <- df_binomial_gam |> 
  group_by(Species) |> 
  count(Abundance) |> 
  pivot_wider(names_from = Abundance, values_from = n) |> 
  rename(species = Species, observation_0 = `0`, total_abundance = `1`) |> 
  mutate(n_lake = factor("1"))


bind_1 <- merge(mean_se_model_1, test1) #ready
saveRDS(bind_1, "model_1/bind_1.rds")

