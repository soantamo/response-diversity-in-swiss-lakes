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
  # filter(Species %in% c("Coregonus_confusus", "Coregonus_litoralis",
  #                       "Coregonus_macrophthalmus", "Coregonus_wartmanni",
  #                       "Coregonus_zuerichensis", "Salmo_sp_Blackspot",
  #                       "Salvelinus_sp_Profundal_Walen_I", "Coregonus_candidus",
  #                       "Coregonus_helveticus"
  # )) |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()
pred_df <- list()
viz <- list()
grid <- list()
unique_method <- list()
simulationOutput <- list()
tiff_file_2  <- list()


df_binomial_gam$fProtocol <- as.factor(df_binomial_gam$Protocol)

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
  
  simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
  # residuals(simulationOutput)
  # residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
  # plot(simulationOutput)
  # testDispersion(simulationOutput)
  tiff_filename <- paste("model_1/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(plot(simulationOutput))
  dev.off()
  tiff_file_2 <- paste("model_1/gam_check/dispersion_", i, ".tiff", sep = "")
  tiff(tiff_file_2, width = 800, height = 600)
  print(testDispersion(simulationOutput))
  dev.off()
  print(tidy(gam_output[[i]]))
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
  # derivatives[[i]] <- derivatives(gam_output[[i]])
  # saveRDS(derivatives[[i]], paste0("model_1/derivatives/derivatives_", i, ".rds"))
}

# no warnings!


s1 <- readRDS("model_1/predictions/predictions_Alosa_fallax.rds")
s2 <- readRDS("model_1/predictions/predictions_Chondrostoma_nasus.rds")
s3 <- readRDS("model_1/predictions/predictions_Chondrostoma_soetta.rds")
s4 <- readRDS("model_1/predictions/predictions_Coregonus_arenicolus.rds")
s5 <- readRDS("model_1/predictions/predictions_Coregonus_candidus.rds")
s6 <- readRDS("model_1/predictions/predictions_Coregonus_confusus.rds")
s7 <- readRDS("model_1/predictions/predictions_Coregonus_heglingus.rds")
s8 <- readRDS("model_1/predictions/predictions_Coregonus_helveticus.rds")
s9 <- readRDS("model_1/predictions/predictions_Coregonus_intermundia.rds")
s10 <- readRDS("model_1/predictions/predictions_Coregonus_litoralis.rds")
s11 <- readRDS("model_1/predictions/predictions_Coregonus_macrophthalmus.rds")
s12 <- readRDS("model_1/predictions/predictions_Coregonus_wartmanni.rds")
s13 <- readRDS("model_1/predictions/predictions_Coregonus_zuerichensis.rds")
s14 <- readRDS("model_1/predictions/predictions_Cottus_gobio_Profundal_Walen.rds")
s15 <- readRDS("model_1/predictions/predictions_Gasterosteus_gymnurus.rds")
s16 <- readRDS("model_1/predictions/predictions_Rutilus_aula.rds")
s17 <- readRDS("model_1/predictions/predictions_Salaria_fluviatilis_French.rds")
s18 <- readRDS("model_1/predictions/predictions_Salmo_labrax.rds")
s19 <- readRDS("model_1/predictions/predictions_Salmo_sp_Blackspot.rds")
s20 <- readRDS("model_1/predictions/predictions_Salmo_sp.rds")
s21 <- readRDS("model_1/predictions/predictions_Salvelinus_namaycush.rds")
s22 <- readRDS("model_1/predictions/predictions_Salvelinus_profundus.rds")
s23 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Limnetic_Thun.rds")
s24 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_dwarf_Thun.rds")
s25 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_dwarf_VWS.rds")
s26 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_extreme_Thun.rds")
s27 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_Walen_I.rds")
s28 <- readRDS("model_1/predictions/predictions_Coregonus_duplex.rds")
s29 <- readRDS("model_1/predictions/predictions_Salmo_marmoratus.rds")

total_model_1_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
                                s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23,
                                s24, s25, s26, s27, s28, s29) |> 
  rename(prediction = fit)

#save all predictions as RDS
saveRDS(total_model_1_pred, "total_models/total_model_1_pred")


total_model_1_pred |> 
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species) +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE) 


#new selection: 10.11.23
# library(readxl)
# model_1_selection <- read_excel("model_1/model_selection.xlsx")
# 
# library(gt)
# 
# model_1_selection |> 
#   select(-model_type) |> 
#   arrange(species) |> 
#   gt() |>
#   tab_header(title = "All models")

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

