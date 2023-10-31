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

#this model is for species with abundance data and random effects

df_abundance_re <- readRDS("data_frame_models/df_abundance_re")

table(df_abundance_re$Abundance) 
str(df_abundance_re)
head(df_abundance_re)

df_abundance_re |> 
  distinct(Species) |> 
  pull(Species)

###CONTINUE here

####################3#model testing for one species

#lota_lota: k = 3 not working
# "Lota_lota" not running with k = 3, also not k = 5
#k = 10 works
#k = 9 works
#k = 8 works
k = 7 works
k = 6 works
k = 5 missing 
#testing backwards to 5

df_one <- df_abundance_re |>
  filter(Species == "Lota_lota")

df_one$fLake <- as.factor(df_one$Lake)


M1 <- gam(Abundance ~ s(mean_last_7days, k = 5) + s(fLake, bs = 're'),
          re.test = FALSE, #nothing is calculated for re, might make calc faster
          family = ziP(), data = df_one)


summary.gam(M1)
gam.check(M1)
tidy(M1)
glance(M1)

#####Loop Model 4 ######

species_list <- df_abundance_re |> 
  filter(!Species == "Lota_lota") |> #with k = 10
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()
grid <- list()
pred_df <- list()
unique_lakes <- list()


df_abundance_re$fLake <- as.factor(df_abundance_re$Lake)

str(df_abundance_re)
#make new loop 
###predict.gam needs something else

for (i in species_list) {
  data <- df_abundance_re |> 
    filter(Species == i)
  unique_lakes <- distinct(data, fLake)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ), fLake = unique_lakes$fLake)
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're'),
                         family = ziP())
  viz[[i]] <- getViz(gam_output[[i]]) #needs to be in mgcviz class
  # print(plot(viz[[i]], allTerms = T), pages = 1)
  # print(qq(viz[[i]], rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2)))
  tiff_filename <- paste("model_4/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(check(viz[[i]],
              a.qq = list(method = "simul1"), 
              a.respoi = list(size = 0.5),
              a.hist = list(bins = 10)))
  dev.off()
  # print(gam.check(gam_output[[i]]))
  # print(summary(gam_output[[i]]))
  # print(tidy(gam_output[[i]]))
  # print(glance(gam_output[[i]]))
  # model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid, type = "response", se.fit = TRUE)
  # model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
  # pred_df <- model_bind |> 
  #   group_by(mean_last_7days) |> 
  #   mutate(fit = mean(fit)) |> 
  #   mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) |> 
  #   summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper)) |> 
  #   mutate(species = factor(i))
  # saveRDS(pred_df, paste0("model_4/predictions/predictions_",i,".rds"))
  # derivatives[[i]] <- derivatives(gam_output[[i]])
  # saveRDS(derivatives[[i]], paste0("model_4/derivatives/derivatives_", i, ".rds"))
}


#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap


s1 <- readRDS("model_4/predictions/predictions_Abramis_brama.rds")
s2 <- readRDS("model_4/predictions/predictions_Alburnus_alburnus.rds")
s3 <- readRDS("model_4/predictions/predictions_Alburnus_arborella.rds")
s4 <- readRDS("model_4/predictions/predictions_Barbatula_sp_Lineage_I.rds")
s5 <- readRDS("model_4/predictions/predictions_Blicca_bjoerkna.rds")
s6 <- readRDS("model_4/predictions/predictions_Coregonus_albellus.rds")
s7 <- readRDS("model_4/predictions/predictions_Coregonus_fatioi.rds")
s8 <- readRDS("model_4/predictions/predictions_Coregonus_sarnensis.rds")
s9 <- readRDS("model_4/predictions/predictions_Coregonus_sp.rds")
s10 <- readRDS("model_4/predictions/predictions_Cyprinus_carpio.rds")
s11 <- readRDS("model_4/predictions/predictions_Gasterosteus_aculeatus.rds")
s12 <- readRDS("model_4/predictions/predictions_Gobio_gobio.rds")
s13 <- readRDS("model_4/predictions/predictions_Gymnocephalus_cernua.rds")
s14 <- readRDS("model_4/predictions/predictions_Lepomis_gibbosus.rds")
s15 <- readRDS("model_4/predictions/predictions_Leuciscus_leuciscus.rds")
s16 <- readRDS("model_4/predictions/predictions_Lota_lota.rds")
s17 <- readRDS("model_4/predictions/predictions_Perca_fluviatilis.rds")
s18 <- readRDS("model_4/predictions/predictions_Phoxinus_csikii.rds")
s19 <- readRDS("model_4/predictions/predictions_Rutilus_rutilus.rds")
s20 <- readRDS("model_4/predictions/predictions_Salmo_trutta.rds")
s21 <- readRDS("model_4/predictions/predictions_Sander_lucioperca.rds")
s22 <- readRDS("model_4/predictions/predictions_Scardinius_erythrophthalmus.rds")
s23 <- readRDS("model_4/predictions/predictions_Scardinius_hesperidicus.rds")
s24 <- readRDS("model_4/predictions/predictions_Tinca_tinca.rds")


total_model_4_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
                                s13, s14, s15, s16,  s17, s18, s19, s20, s21, s22, s23,
                                s24) |> 
  rename(temp = mean_last_7days)




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


#model selection
#include 100%
"Abramis_brama" 
"Alburnus_alburnus" #very low pvalues, high AIC
"Barbatula_sp_Lineage_I" #flake ns  
"Coregonus_albellus" #both 0s
"Coregonus_fatioi" #temp 0
"Coregonus_sarnensis" #both 0s
"Coregonus_sp" #both 0s, very high AIC
"Gasterosteus_aculeatus" #both 0s
"Gymnocephalus_cernua" #both 0s
"Leuciscus_leuciscus" #both 0s
"Perca_fluviatilis" #both 0s, incredibly high AIC
"Rutilus_rutilus" #both 0s
"Sander_lucioperca" 
"Scardinius_erythrophthalmus" #both 0s
"Scardinius_hesperidicus"
"Tinca_tinca" 
"Gobio_gobio" #not 100 sure

#to decide 
# not good
# lota_lota redo
# 
# ***"Blicca_bjoerkna" #looking strange but all significant
# ***"Lepomis_gibbosus" #intercept ns, looks interesting 
# ***"Phoxinus_csikii" #significant, visuaaly special  
# ***"Salmo_trutta" #both 0s, looks interesting  
##***"Lota_lota"  #both 0s, looks interesting


#exclude
# "Alburnus_arborella" #flake ns, looks impossible
# "Cyprinus_carpio"#flake ns, looks impossible  

#to decide
total_model_4_pred |> 
  filter(species == "Blicca_bjoerkna") |>
  ggplot(aes(temp, fit, color = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)

total_model_4_pred |> 
  filter(species == "Gobio_gobio") |>
  ggplot(aes(temp, fit, color = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)

total_model_4_pred |> 
  filter(species == "Lepomis_gibbosus") |>
  ggplot(aes(temp, fit, color = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)

total_model_4_pred |> 
  filter(species == "Phoxinus_csikii") |>
  ggplot(aes(temp, fit, color = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)

total_model_4_pred |> 
  filter(species == "Salmo_trutta") |>
  ggplot(aes(temp, fit, color = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)


total_model_4_pred |> 
  filter(species == "Lota_lota") |>
  ggplot(aes(temp, fit)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5)
# facet_wrap(~species)


#solve lota_lota problem
#all except gobio gobio out
#decide which species can go in 
