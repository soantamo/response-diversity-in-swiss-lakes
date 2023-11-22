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
  # filter(species %in% c("Abramis_brama", "Alburnus_alburnus", "Alosa_agone",
  #                       "Ameiurus_melas")) |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)


# plots not succesfull models

unsuccesful_model_predictions |> 
  # filter(species %in% c("Abramis_brama", "Alburnus_alburnus", "Alosa_agone",
  #                       "Ameiurus_melas")) |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)

# plots successful models 

success_model_predictions |> 
  # filter(species %in% c("Abramis_brama", "Alburnus_alburnus", "Alosa_agone",
  #                       "Ameiurus_melas")) |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
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


# derivatives for all species

mod_1_deriv <- readRDS("total_models/deriv_model_1_total")
mod_2_deriv <- readRDS("total_models/deriv_model_2_total")
mod_3_deriv <- readRDS("total_models/deriv_model_3_total")
mod_4_deriv <- readRDS("total_models/deriv_model_4_total")


#filter predictions in succesful models
# something is very off with these dataframes, I cannnot filter them
# all species are coming
# it is not a tibble!!


successful_models <- read_excel("model_1/model_success_final.xlsx")
table(successful_models$model_success)


success_list <- successful_models |> 
  filter(model_success == 1) 

model_derivatives <- bind_rows(mod_1_deriv, mod_2_deriv, mod_3_deriv, mod_4_deriv)

# bind with successful models
total_model_derivatives <- model_derivatives |> 
  inner_join(success_list)


total_model_derivatives$species <- as.factor(total_model_derivatives$species)
levels(total_model_derivatives$species)


str(total_model_derivatives)


# list of all lakes

# to be able to sort the pulled values fLake needs to be a character
total_model_derivatives$fLake <- as.character(total_model_derivatives$fLake)


lakes_list <- total_model_derivatives |> 
  # filter(!fLake %in% c("Maggiore")) |>
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

lakes_list <- sort(lakes_list)

species_overview <- tibble()

# loop to get response diversity measures for each lake 


for (i in lakes_list){
  
  data <- total_model_derivatives |>
    select(temp, fLake, derivative, species) |> 
    filter(fLake == i)
  # would be nice to get a tibble for each Lake and number of species
  
  number_species <- data |> 
    group_by(fLake) |>
    mutate(sum_species = n_distinct(species)) |>
    group_by(fLake, species, sum_species) |>
    distinct(species) |>
    relocate(sum_species, .after = species)

  species_overview <- bind_rows(species_overview, number_species)

  
  # df_resp_div <- data |>
  #   pivot_wider(
  #     names_from = species,
  #     values_from = derivative)
  # 
  # 
  # df_resp_div$rdiv <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = F)
  # df_resp_div$sign <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = T)
  # df_resp_div$Med <- median(df_resp_div$rdiv)
  # saveRDS(df_resp_div, paste0("total_models/lakes/df_resp_div_", i, ".rds"))
  # 
}


resp_div_all_models <- list.files(path = "total_models/lakes", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS) |> 
  relocate(rdiv, Med, sign, .after = temp)

df_median <- resp_div_all_models |>
  group_by(fLake) |> 
  summarise(median_rdiv = median(rdiv))

resp_div_all_models |> 
  ggplot(aes(x = temp, y = rdiv, color = fLake)) +
  geom_line() +
  geom_hline(data = df_median, aes(yintercept = median_rdiv), size = 0.5,
             lty = "dashed")+
  facet_wrap(~fLake) +
  theme_bw()
  

resp_div_all_models |> 
  ggplot(aes(x = temp, y = sign, color = fLake)) +
  geom_line() +
  facet_wrap(~fLake)  +
  theme_bw()


# prepare for a tibble with the numvber of observations
# join species_overview from loop with number of observations of species per lake

df_final <- readRDS("df_final.rds")

df_success <- df_final |> 
  select(Presence, Species, Lake) |> 
  rename(fLake = Lake, species = Species) |> 
  inner_join(success_list)

str(df_success)
df_success |> distinct(species) #60 species

df_success_min_1 <- df_success |> 
  select(-model_success) |>  
  group_by(species, fLake) |>
  mutate(n_observations = sum(Presence)) |> 
  distinct(n_observations) |> 
  filter(!n_observations == 1)


# merge df_success and species_overview

overview_obs <- merge(df_success_min_1, species_overview)

library(gt)

overview_obs |> 
  select(species, mean_se, max_se, min_se, total_abundance, n_lake, success) |> 
  arrange(species) |> 
  gt() |>
  tab_header(title = "All models")


resp_div_all_models |> 
  ggplot(aes(x = temp, y = rdiv)) +
  geom_hline(yintercept = Med, lty = 2) +
  geom_line() +
  facet_wrap(~fLake)


resp_div_all_models |> 
  ggplot(aes(x = temp, y = sign)) +
  # geom_line(aes (y = Med))
  geom_line() +
  facet_wrap(~fLake)


#############################################################################3

