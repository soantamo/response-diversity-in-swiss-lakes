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
library(readxl)

source(here("functions.R"))

# the filtering of the dfs is not working
# some problem with the type of df. when you change it to a tibble and innerjoin with
# success lsit -> works

#four interesting dharma links
# https://stats.stackexchange.com/questions/495823/residual-diagnostics-in-dharma-for-multilevel-logistic-regression
# https://stats.stackexchange.com/questions/531749/interpretation-of-dharma-residuals-for-gamma-glmm
# https://stats.stackexchange.com/questions/478369/generalised-linear-mixed-model-diagnostics-using-dharma
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html#general-remarks-on-interperting-residual-patterns-and-tests



#predictions for all species

mod_1_pred <- readRDS("total_models/pred_model_1_total")
mod_2_pred <- readRDS("total_models/pred_model_2_total")
mod_3_pred <- readRDS("total_models/pred_model_3_total")
mod_4_pred <- readRDS("total_models/pred_model_4_total")

model_predictions <- bind_rows(mod_1_pred, mod_2_pred, mod_3_pred, mod_4_pred) |> 
  select(-fProtocol)

# test1 <- mod_3_pred |> 
#   filter(species == "Coregonus_brienzii")
# 
# test <- model_predictions |> 
#   filter(species == "Coregonus_brienzii")
#   ggplot(aes(temp, fit)) +
#   geom_line()


#filter predictions in succesful models

successful_models <- read_excel("model_1/model_success_final.xlsx")
table(successful_models$model_success)


success_list <- successful_models |> 
  filter(model_success == 1) 

no_success_list <- successful_models |> 
  filter(model_success == 0)


success_model_predictions <- model_predictions |> 
  inner_join(success_list)

unsuccesful_model_predictions <- model_predictions |> 
  inner_join(no_success_list)


str(success_model_predictions)
success_model_predictions$species <- as.factor(success_model_predictions$species)
levels(success_model_predictions$species)

# plots: all models
model_predictions |> 
  mutate(up = fit + se.fit, low = fit - se.fit) |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = low, ymax = up), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL) 
  ylim(0,1)

# plot se, not working both
model_predictions |> 
  ggplot(aes(temp, se.fit, color = factor(species))) +
  geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) 
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL) +
  ylim(0,1)
    

# plots not succesfull models
  # with ylim(0,20) -> lepomis_gibbosus and phoxinus csiikii lok impossible

unsuccesful_model_predictions |> 
  filter(!species %in% c("Lepomis_gibbosus", "Phoxinus_csikii")) |>
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL) +
  ylim(0, 1)


unsuccesful_model_predictions |> 
  # filter(species %in% c("Alburnus_arborella")) |>
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)

test <- unsuccesful_model_predictions |> 
  filter(species %in% c("Salmo_trutta")) 
  

# plots successful models 

success_model_predictions |> 
  # filter(species == "Coregonus_brienzii") |> 
  mutate(upper_se = fit + se.fit, lower_se = fit - se.fit)  |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower_se, ymax = upper_se), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)


# next steps:
# make dfs of derivatives per lake
# 0. filter derivatives total into successful species
# 1. loop for (i in lakes_list){
# data <- total_derivatives |> 
# filter(fLake == i)
# }
# 2. we need to only use the temp data that is occuring in this lake ->
# min() and max() from e.g. df_final as second filter, filter temp
# 3. use Ross function to calculate response diversity for every lake
# 4. we need a overview table across all species and number of lakes


################################ derivatives for all species

mod_1_deriv <- readRDS("total_models/deriv_model_1_total")
mod_2_deriv <- readRDS("total_models/deriv_model_2_total")
mod_3_deriv <- readRDS("total_models/deriv_model_3_total")
mod_4_deriv <- readRDS("total_models/deriv_model_4_total")

all_models_derivatives <- bind_rows(mod_1_deriv, mod_2_deriv, mod_3_deriv, mod_4_deriv)


all_lakes_tib <- as_tibble(all_models_derivatives)
str(all_lakes_tib)
#filter predictions in succesful models
# something is very off with these dataframes, I cannnot filter them
# all species are coming
# it is not a tibble!!

######resp div with all models 

lakes_list <- all_lakes_tib |> 
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

all_lakes_tib$fLake <- as.character(all_lakes_tib$fLake)
# model_derivatives$fLake <- as.character(model_derivatives$fLake)

lakes_list <- all_lakes_tib |> 
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

lakes_list <- sort(lakes_list)

species_overview <- tibble()

all_lakes_tib |> 
  distinct(species)

str(all_lakes_tib)

# loop to get response diversity measures for each lake 
for (i in lakes_list){
  
  data <- all_lakes_tib |>
    select(temp, fLake, derivative, species) |> 
    filter(fLake == i)
  # would be nice to get a tibble for each Lake and number of species
  
  # number_species <- data |> 
  #   group_by(fLake) |>
  #   mutate(sum_species = n_distinct(species)) |>
  #   group_by(fLake, species, sum_species) |>
  #   distinct(species) |>
  #   relocate(sum_species, .after = species)
  # 
  # species_overview <- bind_rows(species_overview, number_species)
  
  
  df_resp_div <- data |>
    pivot_wider(
      names_from = species,
      values_from = derivative)
  
  
  df_resp_div$rdiv <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  saveRDS(df_resp_div, paste0("total_models/lakes_all_models/df_resp_div_", i, ".rds"))
  # 
}


resp_div_no_excl <- list.files(path = "total_models/lakes_all_models", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS) |> 
  relocate(rdiv, Med, sign, .after = temp)

# prepare for a tibble with the numvber of observations
# join species_overview from loop with number of observations of species per lake


df_median <- resp_div_no_excl |>
  group_by(fLake) |> 
  summarise(median_rdiv = median(rdiv))

resp_div_no_excl |> 
  ggplot(aes(x = temp, y = rdiv)) +
  geom_line(color = "#54008B") +
  geom_hline(data = df_median, aes(yintercept = median_rdiv), linewidth = 0.5,
             lty = "dashed")+
  facet_wrap(~fLake) +
  theme_bw()


resp_div_no_excl |> 
  ggplot(aes(x = temp, y = sign, color = fLake)) +
  geom_line(color = "#54008B") +
  facet_wrap(~fLake)  +
  theme_bw()


###################resp div for successful models



successful_models <- read_excel("model_1/model_success_final.xlsx")
table(successful_models$model_success)


success_list <- successful_models |> 
  filter(model_success == 1) 

success_model_deriv <- all_lakes_tib |> 
  inner_join(success_list)

success_model_deriv$species <- as.factor(success_model_deriv$species)
levels(success_model_deriv$species)

# plotting derivatives

# success_model_deriv |> 
#   filter(fLake %in% c("Biel", "Brienz", "Thun")) |> 
#   ggplot(aes(temp, derivative, color = factor(species))) +
#   geom_line() +
#   # geom_ribbon(aes(ymin = lower_se, ymax = upper_se), alpha = 0.3) +
#   theme_bw() +
#   facet_wrap(~fLake, scale = "free") +
#   theme(strip.background = element_rect(fill="lightgrey")) +
#   scale_color_viridis(discrete=TRUE)


# to be able to sort the pulled values fLake needs to be a character
success_model_deriv$fLake <- as.character(success_model_deriv$fLake)
# model_derivatives$fLake <- as.character(model_derivatives$fLake)


lakes_list <- success_model_deriv |> 
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

lakes_list <- sort(lakes_list)

species_overview <- tibble()

# loop to get response diversity measures for each lake 


for (i in lakes_list){
  
  data <- success_model_deriv |>
    select(temp, fLake, derivative, species) |> 
    filter(fLake == i)
  # would be nice to get a tibble for each Lake and number of species
  
  # number_species <- data |> 
  #   group_by(fLake) |>
  #   mutate(sum_species = n_distinct(species)) |>
  #   group_by(fLake, species, sum_species) |>
  #   distinct(species) |>
  #   relocate(sum_species, .after = species)
  # 
  # species_overview <- bind_rows(species_overview, number_species)


  df_resp_div <- data |>
    pivot_wider(
      names_from = species,
      values_from = derivative)


  df_resp_div$rdiv <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  saveRDS(df_resp_div, paste0("total_models/lakes/df_resp_div_", i, ".rds"))

}


resp_div_succ_models <- list.files(path = "total_models/lakes", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS) |> 
  relocate(rdiv, Med, sign, .after = temp)


##################################################### plotting
# all models

df_median_all <- resp_div_no_excl |>
  group_by(fLake) |> 
  summarise(median_rdiv = median(rdiv))

resp_div_no_excl |> 
  ggplot(aes(x = temp, y = rdiv)) +
  geom_line(color = "#54008B") +
  geom_hline(data = df_median_all, aes(yintercept = median_rdiv), linewidth = 0.5,
             lty = "dashed")+
  facet_wrap(~fLake) +
  theme_bw()


resp_div_no_excl |> 
  ggplot(aes(x = temp, y = sign, color = fLake)) +
  geom_line(color = "#54008B") +
  facet_wrap(~fLake)  +
  theme_bw()


# successful ones

df_median <- resp_div_succ_models |>
  group_by(fLake) |> 
  summarise(median_rdiv = median(rdiv))

resp_div_succ_models  |> 
  ggplot(aes(x = temp, y = rdiv)) +
  geom_line(color = "#54008B") +
  geom_hline(data = df_median, aes(yintercept = median_rdiv), linewidth = 0.5,
             lty = "dashed")+
  facet_wrap(~fLake) +
  theme_bw()

resp_div_succ_models  |> 
  ggplot(aes(x = temp, y = sign, color = fLake)) +
  geom_line(color = "#54008B") +
  facet_wrap(~fLake)  +
  theme_bw()

median <- df_median |> 
  group_by(fLake) |> 
  distinct(median_rdiv)


################ look at derivatives

# some loook weird, all weird ones are in this list, is something off with the derivatives???

all_lakes_tib |> 
  filter(species %in% c("perca"))

test <- all_lakes_tib |> 
  filter(species %in% c("Phoxinus_sp", "Chondrostoma_nasus", "Chondrostoma_soetta",
                        "Cottus_gobio_Profundal_Walen", "Cottus_gobio_Profundal_Thun",
                        "Cottus_gobio_Profundal_Lucerne", "Rutilus_aula", "Salmo_sp",
                        "Salvelinus_profundus", "Barbatula_sp_Lineage_II", "Cottus_sp_Po_profundal",
                        "Phoxinus_sp", "Telestes_muticellus")) |> 
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line() +
  theme_bw() +
  facet_wrap(~species) +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL) +
  ylim(20, 50)

test

# two look special -> Cottus_gobio_Profundal_Walen is at -50, Salmo sp 25,
# chondrostoma_soetta 40. salmo and chondrostoma are not included anyway

# all models

all_lakes_tib |> 
  filter(derivative > 200) |> 
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line() +
  scale_color_viridis(discrete=TRUE)

# one is veeery strange: gasterosteus gymnurus

# successful models

success_model_deriv |> 
  filter(derivative < -20) |> 
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line() +
  scale_color_viridis(discrete=TRUE)
# one very different: 

success_model_deriv |>
  filter(fLake %in% c("Biel", "Joux", "Morat"))  |> 
  filter(derivative > 3) |> 
  # filter(!species == "Phoxinus_csikii") |> 
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line() +
  facet_wrap(~fLake)

success_model_deriv

# squalius cephalus

# derivative has a crazy impact altough the predictions are basically 0 -> problem
# Cottus gobio profundal thun cant be included at -2500
# Gasterosteus gymnurus looks crazy
# alburnus_arborella, chondrostoma_soetta, cyprinus carpio and salmo sp -> no

#############################################################################3
# interpretation:

# work with the succesful ones
# changes
