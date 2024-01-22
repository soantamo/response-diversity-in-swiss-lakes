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

#this model is for species with abundance data and random effects

df_abundance_re <- readRDS("data_frame_models/df_abundance_re")

df_abundance_re |> 
  distinct(Lake) 
  pull(Species)

# we have to delete the observation of lepomis gibbosus at 7.132625 degrees

df_abundance_re <- df_abundance_re |> 
  filter(!(mean_last_7days == 7.132625 & Species == "Lepomis_gibbosus" & Presence == 1))

# df_abundance_re <- df_abundance_re |>
#   filter(!(Species == "Phoxinus_csikii" & Abundance %in% c(8, 3)))

# tskhd <- df_abundance_re |> 
#   filter(Species == "Phoxinus_csikii") |> 
#   filter(mean_last_7days > 15)

table(df_abundance_re$Abundance) 
str(df_abundance_re)
head(df_abundance_re)

df_abundance_re |> 
  distinct(Species) |> 
  pull(Species)

###CONTINUE here

####################3#model testing for one species


#####Loop Model 4 ######
#lota_lota needs to run separately

species_list <- df_abundance_re |> 
  # binomial ones
  filter(!Species %in% c("Lota_lota", "Salmo_trutta", "Alburnus_arborella",
                        "Lepomis_gibbosus", "Blicca_bjoerkna", "Cyprinus_carpio",
                        "Phoxinus_csikii")) |> 
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



df_abundance_re$fLake <- as.factor(df_abundance_re$Lake)

df_abundance_re$fProtocol <- as.factor(df_abundance_re$Protocol)

str(df_abundance_re)
#make new loop 
###predict.gam needs something else

for (i in species_list) {
  data <- df_abundance_re |> 
    filter(Species == i)
  unique_lakes <- distinct(data, fLake)
  unique_protocol <- distinct(data, fProtocol)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ), fLake = unique_lakes$fLake, fProtocol = unique_protocol$fProtocol)
  # gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
  #                        +  s(fProtocol, bs = 're'), family = ziP())
  # salmo_trutta, alburnus_arborella test
  gam_output[[i]] <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                         +  s(fProtocol, bs = 're'), family = binomial)
  # prepare residuals
  simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
  # Main plot function from DHARMa, which gives
  # Left: a qq-plot to detect overall deviations from the expected distribution
  # Right: a plot of the residuals against the rank-transformed model predictions
  tiff_filename <- paste("model_4/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(plot(simulationOutput))
  dev.off()
  # get rid of NAs in temp datat
  temp_data <- data |>
    drop_na(mean_last_7days)
  # Plotting standardized residuals against predictors
  tiff_file_2 <- paste("model_4/gam_check/predictor_", i, ".tiff", sep = "")
  tiff(tiff_file_2, width = 800, height = 600)
  print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
  dev.off()

  print(glance(gam_output[[i]]))

  model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid, type = "response", se.fit = TRUE)
  model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
  pred_df <- model_bind |>
    group_by(mean_last_7days) |>
    mutate(fit = mean(fit)) |>
    mutate(lower = fit - 1 * se.fit, upper = fit +  1 * se.fit) |>
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit), across(fLake)) |>
    rename(temp = mean_last_7days) |> 
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_4/predictions/predictions_",i,".rds"))
}

# combined df

df_pred_mod4 <- list.files(path = "model_4/predictions", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total predictions as RDS
saveRDS(df_pred_mod4, "total_models/pred_model_4_total")

df_pred_mod4 |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species) +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE)

# The reason we can't keep p-value < 2.2e-16 in the output is that that
# would mean we'd need a character output rather than a numeric one, which 
# prevents any further programming with it. For example, suppose your next 
# step were to filter for tests with p.value < .05, or to sort in ascending 
# order of p-values. Having a character output would break that functionality.
#https://github.com/tidymodels/broom/issues/227

#and 


# 2.2e-16 is the scientific notation of 0.00000000000000022, 
# meaning it is very close to zero. Your statistical 
# software probably uses this notation automatically for very small numbers. 
# You may be able to change this in the settings.
# The notation alone is no reason to be suspicious. The result itself might be, 
# but you will have to be the judge of that.
# < 2.2e-16 as the p value would indicate a significant result, 
# meaning that the actual p value is even smaller than 2.2e-16 
# (a typical threshold is 0.05, anything smaller counts as statistically significant).
# https://stats.stackexchange.com/questions/403343/what-is-the-interpretation-of-the-p-value-of-2-2e-16

###################derivatives


species_list <- df_abundance_re |> 
  # binomial ones
  filter(Species %in% c("Lota_lota", "Salmo_trutta", "Alburnus_arborella",
                        "Lepomis_gibbosus", "Blicca_bjoerkna", "Cyprinus_carpio",
                        "Phoxinus_csikii")) |>
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_abundance_re$fLake <- as.factor(df_abundance_re$Lake)
df_abundance_re$fProtocol <- as.factor(df_abundance_re$Protocol)

str(df_abundance_re)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()


for (i in species_list) {
  data <- df_abundance_re |> 
    filter(Species == i)
  # gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
  #                        +  s(fProtocol, bs = 're'), family = ziP())
  # salmo trutta, alburnus_arborella
  gam_output[[i]] <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                         +  s(fProtocol, bs = 're'), family = binomial)
  lake_list <- distinct(data, Lake) |> 
    pull()
  
  for (j in lake_list){
    
   data_lake <- df_abundance_re |> 
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
    saveRDS(derivatives, paste0("model_4/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


#prepare total df for derivatives

df_deriv_mod4 <- list.files(path = "model_4/derivatives", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
saveRDS(df_deriv_mod4, "total_models/deriv_model_4_total")

df_deriv_mod4 |> 
  filter(species == "Cottus_gobio_Profundal") |> 
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line() +
  theme_bw() +
  facet_wrap(~species) +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)


# df for later

mean_se_model_4 <- df_pred_mod4 |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  mutate(max_se = max(se.fit)) |> 
  mutate(min_se = min(se.fit)) |> 
  distinct(mean_se, max_se, min_se)



test4 <- df_abundance_re |> 
  group_by(Species, Presence) |> 
  summarize(n_observations = sum(Presence)) |> 
  select(Species, n_observations) |> 
  filter(n_observations != 0) |> 
  rename(species = Species)


n_lake4 <- df_abundance_re |>
  group_by(Species) |>
  summarize(n_lake = n_distinct(Lake)) |>
  rename(species = Species)

two_bind4 <- merge(test4, n_lake4, by.x = "species")

bind_4 <- merge(two_bind4, mean_se_model_4)

# saveRDS(bind_4, "model_4/bind_4.rds")


# these residuals are okay, see
# https://stats.stackexchange.com/questions/531749/interpretation-of-dharma-residuals-for-gamma-glmm
# https://stats.stackexchange.com/questions/531749/interpretation-of-dharma-residuals-for-gamma-glmm
