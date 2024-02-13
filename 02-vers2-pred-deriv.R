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

###############################################################################
#get model predictions 

source(here("functions_GAMs.R"))

df_1 <- readRDS("data_frame_models/df_binomial_gam")
df_2 <- readRDS("data_frame_models/df_abundance_gam")
df_3 <- readRDS("data_frame_models/df_binomial_re")
df_4 <- readRDS("data_frame_models/df_abundance_re")

predictions(df_1)
predictions(df_2)
predictions(df_3)
predictions(df_4)

depth_predictions(df_1)
depth_predictions(df_2)
depth_predictions(df_3)
depth_predictions(df_4)

df_predictions_all <- list.files(path = "total_models/predictions", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total predictions as RDS
saveRDS(df_predictions_all, "total_models/df_pred_all.rds")

###################################################################derivatives

df_1 <- readRDS("data_frame_models/df_binomial_gam")
df_2 <- readRDS("data_frame_models/df_abundance_gam")
df_3 <- readRDS("data_frame_models/df_binomial_re")
df_4 <- readRDS("data_frame_models/df_abundance_re")

species_lake <- read_xlsx("species_lake.xlsx") 


# model 1
species_list <- df_1 |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_1fLake <- as.factor(df_1$Lake)
df_1$fProtocol <- as.factor(df_1$Protocol)

str(df_1)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()

species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)

str(species_lake)

df_1$fLake <- as.factor(df_1$Lake)
df_1$fProtocol <- as.factor(df_1$Protocol)

for (i in species_list) {
  data <- df_1 |> 
    filter(Species == i)
  
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fProtocol, bs = 're'), family = binomial)
  
  lake_list <- distinct(data, Lake) |> 
    pull()
  
  for (j in lake_list){
    
    data_lake <- df_1 |> 
      filter(Species == i) |> 
      filter(Lake == j)
    
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
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

# model 2

species_list <- df_2 |> 
  # binomial one
  filter(Species == "Coregonus_sp_benthic_profundal") |>
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
df_abundance_gam$fLake <- as.factor(df_abundance_gam$Lake)

str(df_abundance_gam)


derivatives <- list()
gam_output <- list()

species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)

str(species_lake)

for (i in species_list) {
  
  data <- df_2 |> 
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
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


##model 2 zip
species_list <- df_2 |> 
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


species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)

str(species_lake)


for (i in species_list) {
  
  data <- df_2 |> 
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
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

###################model 3

species_list <- df_3 |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_binomial_re$fLake <- as.factor(df_binomial_re$Lake)
df_binomial_re$fProtocol <- as.factor(df_binomial_re$Protocol)

str(df_binomial_re)
derivatives <- list()
gam_output <- list()

species_lake <- read_xlsx("species_lake.xlsx") 

species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)

str(species_lake)

for (i in species_list) {
  data <- df_3 |> 
    filter(Species == i)
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = "re")
                         + s(fProtocol, bs = 're'), family = binomial)
  
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
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


# model 4

species_list <- df_4 |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


df_abundance_re$fLake <- as.factor(df_abundance_re$Lake)
df_abundance_re$fProtocol <- as.factor(df_abundance_re$Protocol)


derivatives <- list()
gam_output <- list()

species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)

for (i in species_list) {
  
  data <- df_4 |> 
    filter(Species == i)
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                         +  s(fProtocol, bs = 're'), family = ziP())
  
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
    saveRDS(derivatives, paste0("total_derivatives/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


df_deriv_all <- list.files(path = "total_models/derivatives", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
saveRDS(df_deriv_all, "total_models/df_deriv_all.rds")

###############################################################################
#response diversity

source(here("functions.R"))

model_predictions <- readRDS("total_models/df_pred_all") |> 
  select(-Protocol)

model_predictions$species <- as.factor(model_predictions$species)
levels(model_predictions$species)

# plots: all models
model_predictions |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)

all_models_derivatives <- readRDS("total_models/df_deriv_all")

all_deriv <- as_tibble(all_models_derivatives)
str(all_deriv)

levels(all_deriv$species)

######resp div with all models 

lakes_list <- all_deriv  |> 
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

all_deriv$fLake <- as.character(all_deriv $fLake)
# model_derivatives$fLake <- as.character(model_derivatives$fLake)

lakes_list <- all_deriv |> 
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

lakes_list <- sort(lakes_list)

species_overview <- tibble()


all_deriv$species <- as.character(all_deriv$species)
all_deriv$species <- as.factor(all_deriv$species)


str(all_deriv )

# loop to get response diversity measures for each lake 
for (i in lakes_list){
  
  data <- all_deriv |>
    select(temp, fLake, derivative, species) |> 
    filter(fLake == i)
  # would be nice to get a tibble for each Lake and number of species
  # 
  # number_species <- data |>
  #   group_by(fLake, species) |>
  #   distinct(species) |>
  #   mutate(num_species = 1) |>
  #   group_by(fLake) |>
  #   mutate(sum_species = sum(num_species))
  # 
  # species_overview <- bind_rows(species_overview, number_species)
  # saveRDS(species_overview, paste0("total_models/species_overview/species_overview_", i ,".rds"))
  # 
  df_resp_div <- data |>
    pivot_wider(
      names_from = species,
      values_from = derivative)
  df_resp_div$rdiv <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  saveRDS(df_resp_div, paste0("total_models/lakes_all_models/df_resp_div_", i, ".rds"))
  
}

resp_div_no_excl <- list.files(path = "total_models/lakes_all_models", pattern = ".rds", full.names = TRUE) |>
  map_dfr(readRDS) |>
  relocate(rdiv, Med, sign, .after = temp)

saveRDS(resp_div_no_excl,"total_models/resp_div_all.rds")

################################################################################
# rdiv overview plot

resp_div_all <- readRDS("total_models/resp_div_all.rds")

df_means <- resp_div_all |>
  group_by(fLake) |> 
  summarise(mean_rdiv = mean(rdiv), across(sign)) |> 
  summarise(mean_sign = mean(sign), across(mean_rdiv)) |> 
  rename(Lake = fLake) |> 
  distinct(Lake, mean_rdiv, mean_sign)


#plotting means

plot_means <- df_means |> 
  ggplot(aes(mean_rdiv, mean_sign)) +
  # geom_point(aes(color = Lake))
  # geom_label(aes(label = Lake))
  # geom_point() +
  geom_text(aes(label = Lake)) +
  # geom_text(aes(label = Lake), nudge_x = 0.01, nudge_y = 0.01,  check_overlap = T) +
  #   check_overlap = T) +
  theme_bw()

plot_means

p <- df_means |> 
  ggplot(aes(mean_rdiv, mean_sign)) +
  geom_point(color = "#007ED3")

plot_means <- p + geom_text_repel(aes(label = Lake),
                                  size = 3.5, 
                                  max.overlaps = 13) +
  labs(x = "mean dissimilarity", y = "mean divergence") +
  theme_bw()

plot_means

################################################################################
library(plotly)

data <- all_lakes_tib |> 
  mutate(species = factor(species)) |> 
  group_by(fLake, species) |> 
  mutate(max_derivative = max(derivative)) |> 
  mutate(mean_temp = mean(temp)) |> 
  distinct(species, fLake, mean_temp, max_derivative) |> 
  ungroup() |> 
  arrange(max_derivative)

max(data$max_derivative)
min(data$max_derivative)

data_new <- data                                      # Duplicate data
data_new$groups <- cut(data_new$max_derivative,               # Add group column
                       breaks = c(-7.634168, -3, 0, 1, 2, 3, 4, 10, 200))
head(data_new)   

data_new |> 
  # filter(!species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii",
  #                       "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II")) |>
  ggplot(aes(fLake, y = fct_reorder(species, max_derivative), fill= groups)) + 
  geom_tile() +
  # scale_fill_distiller(palette = "PRGn")
  # scale_fill_gradient(low = "#006FAB",
  #                     high = "#971B20",
  #                     guide = "colorbar") +
  scale_fill_manual(breaks = levels(data_new$groups),
                    values = c("#053061", "#2166AC","#92C5DE", "#FDDBC7",  "#F4A582", "#D6604D", "#B2182B", "#67001F"))


