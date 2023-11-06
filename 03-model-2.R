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

#second model for species with abundance data which only occur in one lake
#re-do with two additional species

#read df
df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")

table(df_abundance_gam$Abundance) 
str(df_abundance_gam)
head(df_abundance_gam)


#####Loop Model 2 ######

#ZIP not working in those three species
#all others work with k = 3


species_list <- df_abundance_gam |>
  filter(!Species %in% c("Coregonus_profundus",
                         "Phoxinus_sp", "Coregonus_zugensis")) |>
  distinct(Species) |> 
  pull(Species)

#less problems with comparing
species_list <- sort(species_list)


gam_output <- list()
model_prediction <- list()
derivatives <- list()
viz <- list()
pred_df <- list()

#make new loop 

for (i in species_list) {
  data <- df_abundance_gam |> 
    filter(Species == i)
  temp_gradient <- data.frame(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02))
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3), family = ziP())
  # viz[[i]] <- getViz(gam_output[[i]]) #needs to be in mgcviz class
  # print(plot(viz[[i]], allTerms = T), pages = 1)
  # print(qq(viz[[i]], rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2)))
  # tiff_filename <- paste("model_2/gam_check/gam_check_", i, ".tiff", sep = "")
  # tiff(tiff_filename, width = 800, height = 600)
  # print(check(viz[[i]],
  #             a.qq = list(method = "simul1"),
  #             a.respoi = list(size = 0.5),
  #             a.hist = list(bins = 10)))
  # dev.off()
  # print(gam.check(gam_output[[i]]))
  # print(summary(gam_output[[i]]))
  print(tidy(gam_output[[i]]))
  # print(glance(gam_output[[i]]))
  model_prediction[[i]] <- predict.gam(gam_output[[i]], temp_gradient, type = "response", se.fit = TRUE)
  model_bind <- cbind(model_prediction[[i]], temp_gradient)
  pred_df <- model_bind |>
    group_by(mean_last_7days) |>
    mutate(fit = mean(fit)) |>
    mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) |>
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_2/predictions/predictions_",i,".rds"))
  # derivatives[[i]] <- derivatives(gam_output[[i]])
  # saveRDS(derivatives[[i]], paste0("model_2/derivatives/derivatives_", i, ".rds"))
}

#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap



s1 <- readRDS("model_2/predictions/predictions_Barbatula_sp_Lineage_II.rds")
s2 <- readRDS("model_2/predictions/predictions_Coregonus_acrinasus.rds")
s3 <- readRDS("model_2/predictions/predictions_Coregonus_profundus.rds")
s4 <- readRDS("model_2/predictions/predictions_Coregonus_zugensis.rds")
s5 <- readRDS("model_2/predictions/predictions_Cottus_gobio_Profundal_Lucerne.rds")
s6 <- readRDS("model_2/predictions/predictions_Cottus_gobio_Profundal_Thun.rds")
s7 <- readRDS("model_2/predictions/predictions_Cottus_sp_Po_profundal.rds")
s8 <- readRDS("model_2/predictions/predictions_Phoxinus_sp.rds")
s9 <- readRDS("model_2/predictions/predictions_Telestes_muticellus.rds")
s10 <- readRDS("model_2/predictions/predictions_Alosa_agone.rds")
s11 <- readRDS("model_2/predictions/predictions_Cottus_sp_Po.rds")

total_model_2_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8,  s9, s10, s11) |> 
  rename(prediction = fit, temp = mean_last_7days) |> 
  select(-`model_prediction[[i]]`)

#save all predictions as RDS
# saveRDS(total_model_2_pred, "total_models/total_model_2_pred")



total_model_2_pred |> 
  ggplot(aes(temp, prediction)) +
  geom_line() +
  facet_wrap(~species)


#negbin checking 25.10.
#include
"Alosa_agone" 
"Coregonus_acrinasus" 
"Cottus_sp_Po"
"Coregonus_profundus" #tidy 0
"Coregonus_zugensis" #tidy 0

#exclude
# "Cottus_gobio_Profundal_Lucerne" definitily exclude looks crazy
# "Cottus_sp_Po_profundal" definitely exclude looks crazy
# "Phoxinus_sp" definitely exclude looks crazy

#ZIP checking
#visual horror




total_model_2_pred |> 
  filter(species %in% c("Alosa_agone", "Coregonus_acrinasus", "Cottus_sp_Po")) |> 
  ggplot(aes(temp, prediction)) +
  geom_line(aes(colour = species)) 

#to do
#work on zip

#Coregonus_acrinasus  k = 3 works
#coregonus profundus not working with k = 40, exclude from analysis I guess
#phoxinus sp not working with k = 20

#all working with k = 3 separately, except the two # ones
"Alosa_agone"
"Barbatula_sp_Lineage_II"       
"Coregonus_acrinasus"
# "Coregonus_profundus"  #not working           
"Coregonus_zugensis"
"Cottus_gobio_Profundal_Lucerne"
"Cottus_gobio_Profundal_Thun"
"Cottus_sp_Po"                  
"Cottus_sp_Po_profundal"
# "Phoxinus_sp" #not working          
"Telestes_muticellus"

#testing loop without those two species. stops at c-zugensis, works when excluding
#c.profundus, phoxinus and c.zugensis
#c.zugensis works with k = 7 but looks bad -> excldue


#significant?

"Alosa_agone" #s, visually ok
# "Barbatula_sp_Lineage_II" #s, no good
"Coregonus_acrinasus" #s, visually ok
# "Cottus_gobio_Profundal_Lucerne" #s 0, no
# "Cottus_gobio_Profundal_Thun" #ns, no
"Cottus_sp_Po" #s, ok         
# "Cottus_sp_Po_profundal"# s, no
# "Telestes_muticellus" #ns, no


#final: "Alosa_agone", "Coregonus_acrinasus", "Cottus_sp_Po"
#double-check residuals etc


mean_se_model_2 <- total_model_2_pred |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  # mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) |> 
  distinct(mean_se)


summarize(n_lake = n_distinct(Lake))


test2 <- df_abundance_gam |> 
  group_by(Species) |> 
  mutate(total_abundance = sum(Abundance), 
         observation_0 = sum(Abundance == 0)) |> 
  distinct(total_abundance, observation_0) |> 
  rename(species = Species) |> 
  # pivot_wider(names_from = Abundance, values_from = n) |> 
  # mutate(across(where(is.numeric), ~ ifelse(is.na(.), 0, .))) |> 
  # rename(species = Species, observation_0 = `0`, observation_1 = `1`) |> 
  mutate(n_lake = factor("1"))
  # mutate(sumrow = rowSums(pick(3:8), na.rm = T)) |> 
  # select(-`2`, -`3`, -`4`, -`5`, -`7`, -`15`) |> 
  # rename(species = Species, observation_0 = `0`, observation_1 = `1`, observations_abu = `sumrow`)
  # rename(species = Species, observation_0 = `0`, observation_1 = `1`)

#double check if all occur in one
# n_lake <- df_abundance_gam |> 
#   group_by(Lake, Species) |> 
#   summarize(TotalAbundance = sum(Abundance), .groups = 'drop') |> 
#   filter(TotalAbundance > 1) |> 
#   group_by(Species) |> 
#   summarize(n_lake = n_distinct(Lake)) |> 
#   rename(species = Species)

bind_2 <- merge(mean_se_model_2, test2)

saveRDS(bind_2, "model_2/bind_2.rds")

