library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(lattice)
library(broom)
library(mgcViz) #https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html

#####continue with df_one prediction
#this model for species with binomial data and random effects

df_binomial_re <- readRDS("data_frame_models/df_binomial_re")

table(df_binomial_re$Abundance) 
str(df_binomial_re)
head(df_binomial_re)

df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)

####LOOOP#######################################################################

#adding the manual margin effect calculation

species_list <- df_binomial_re |> 
  # filter(Species %in% c( "Ameiurus_melas", "Cobitis_bilineata", "Cottus_gobio_Aare_littoral",
  #                        "Esox_lucius", "Squalius_cephalus", "Salvelinus_umbla",
  #                        "Cottus_gobio_unknownlineage", "Coregonus_palaea", "Cottus_gobio_Rhine",
  #                        "Coregonus_brienzii"
  # )) |> 
  # # filter(Species %in% c("Ameiurus_melas", "Cobitis_bilineata","Cottus_gobio_Aare_littoral"
  # #                       # "Esox_lucius", "Barbus_barbus", "Coregonus_brienzii", "Cottus_gobio_Rhine", 
  # #                       # "Silurus_glanis", "Squalius_squalus"
  # #                       )) |> 
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
  # Main plot function from DHARMa, which gives 
  # Left: a qq-plot to detect overall deviations from the expected distribution
  # Right: a plot of the residuals against the rank-transformed model predictions
  tiff_filename <- paste("model_3/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(plot(simulationOutput))
  dev.off()
  # get rid of NAs in temp datat
  temp_data <- data |> 
    drop_na(mean_last_7days)
  # Plotting standardized residuals against predictors
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
    mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) |>
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
    rename(temp = mean_last_7days) |> 
    #with across() we can retain our column for se.fit
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_3/predictions/predictions_",i,".rds"))
  derivatives[[i]] <- derivatives(gam_output[[i]])
  saveRDS(derivatives[[i]], paste0("model_3/derivatives/derivatives_", i, ".rds"))
}

#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap


s1 <- readRDS("model_3/predictions/predictions_Ameiurus_melas.rds")
s2 <- readRDS("model_3/predictions/predictions_Barbus_barbus.rds")
s3 <- readRDS("model_3/predictions/predictions_Carassius_gibelio.rds")
s4 <- readRDS("model_3/predictions/predictions_Cobitis_bilineata.rds")
s5 <- readRDS("model_3/predictions/predictions_Coregonus_alpinus.rds")
s6 <- readRDS("model_3/predictions/predictions_Coregonus_brienzii.rds")
s7 <- readRDS("model_3/predictions/predictions_Coregonus_palaea.rds")
s8 <- readRDS("model_3/predictions/predictions_Cottus_gobio_Aare_littoral.rds")
s9 <- readRDS("model_3/predictions/predictions_Cottus_gobio_Rhine.rds")
s10 <- readRDS("model_3/predictions/predictions_Cottus_gobio_unknownlineage.rds")
s11 <- readRDS("model_3/predictions/predictions_Esox_cisalpinus.rds")
s12 <- readRDS("model_3/predictions/predictions_Esox_lucius.rds")
s13 <- readRDS("model_3/predictions/predictions_Micropterus_salmoides.rds")
s14 <- readRDS("model_3/predictions/predictions_Salvelinus_umbla.rds")
s15 <- readRDS("model_3/predictions/predictions_Silurus_glanis.rds")
s16 <- readRDS("model_3/predictions/predictions_Squalius_cephalus.rds")
s17 <- readRDS("model_3/predictions/predictions_Squalius_squalus.rds")
s18 <- readRDS("model_3/predictions/predictions_Thymallus_thymallus.rds")


total_model_3_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
                                s13, s14, s15, s16, s17, s18) |> 
  rename(prediction = fit)

#save all predictions as RDS
# saveRDS(total_model_3_pred, "total_models/total_model_3_pred")

total_model_3_pred |> 
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species) +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE) 

#prepare for all df
mean_se_model_3 <- total_model_3_pred |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  mutate(max_se = max(se.fit)) |> 
  mutate(min_se = min(se.fit)) |> 
  distinct(mean_se, max_se, min_se)


test3 <- df_binomial_re |> 
  group_by(Species) |> 
  count(Abundance) |> 
  pivot_wider(names_from = Abundance, values_from = n) |> 
  rename(species = Species, observation_0 = `0`, total_abundance = `1`)

n_lake3 <- df_binomial_re |>
  group_by(Species) |>
  summarize(n_lake = n_distinct(Lake)) |>
  rename(species = Species)

two_bind <- merge(test3, n_lake3, by.x = "species")

bind_3 <- merge(two_bind, mean_se_model_3)

saveRDS(bind_3, "model_3/bind_3.rds")

