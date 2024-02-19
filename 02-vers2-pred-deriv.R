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

# to do: add code to take one level of a lake where the fish occurs: strange warnings

###############################################################################
#get model predictions 

source(here("functions_GAMs.R"))

df_1 <- readRDS("data_frame_models/df_binomial_gam")
df_2 <- readRDS("data_frame_models/df_abundance_gam")
df_3 <- readRDS("data_frame_models/df_binomial_re")
df_4 <- readRDS("data_frame_models/df_abundance_re")

predictions(df_1) 
predictions(df_2)
predictions(df_3) #warnings are strange
predictions(df_4) #warnings are strange



predictions(data_df_2)

depth_predictions(df_1)
depth_predictions(df_2)
depth_predictions(df_3)
depth_predictions(df_4)


data_test <- df_3 |> 
  filter(Species == "Ameiurus_melas") |> 
  mutate(n_lake = n_distinct(Lake))
data_test$fLake <- as.factor(data_test$Lake)

unique_lakes <- unique(data_test$fLake)

random_lake <- sample(levels(unique_lakes), 1)

grid <- expand.grid(mean_last_7days = seq(
  from = min(data_test$mean_last_7days, na.rm = TRUE),
  to = max(data_test$mean_last_7days, na.rm = TRUE), by = 0.02),
  fProtocol = factor("VERT"), fLake = factor(random_lake))

grid(fLake = factor(random_lake))

# df_predictions_all <- list.files(path = "total_models/predictions", pattern = ".rds", full.names = TRUE) |> 
#   map_dfr(readRDS)

# save total predictions as RDS
# saveRDS(df_predictions_all, "total_models/df_pred_all.rds")


# load total predictions
model_predictions <- readRDS("total_models/df_pred_all.rds")

model_predictions$species <- as.factor(model_predictions$species)
levels(model_predictions$species)

# plots: all models
# second option to make se the same color as line
all_predictions <- model_predictions |> 
  ggplot(aes(temp, fit)) +
  # geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit), fill = factor(species)), alpha = 0.3) +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), alpha = 0.3) +
  geom_line(aes(color = factor(species))) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  # scale_color_viridis(discrete=TRUE, guide = NULL, aesthetics = c("color", "fill"))
  scale_color_viridis(discrete=TRUE, guide = NULL)


tiff(paste("total_models/plots/all_predictions_models.tiff", sep = ""), units="in", width = 12, height=8, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(all_predictions)

# Closing the graphical device
dev.off()

strange_predictions <- model_predictions |> 
  filter(fit > 1) |> 
  ggplot(aes(temp, fit)) +
  geom_line(aes(color = factor(species))) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL) +
  labs(title = "Fit > 1")

strange_predictions

tiff(paste("total_models/plots/strange_predictions.tiff", sep = ""), units="in", width = 12, height=8, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(strange_predictions)

# Closing the graphical device
dev.off()


four_predictions <- model_predictions |> 
  filter(species %in% c("Cyprinus_carpio", "Lepomis_gibbosus", "Phoxinus_csikii",
                        "Salmo_trutta")) |> 
  ggplot(aes(temp, fit)) +
  geom_line(aes(color = factor(species))) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  # scale_color_viridis(discrete=TRUE, guide = NULL, aesthetics = c("color", "fill"))
  scale_color_viridis(discrete=TRUE, guide = NULL)


four_predictions
tiff(paste("total_models/plots/strange_rpedictions_four.tiff", sep = ""), units="in", width = 12, height=8, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(four_predictions)

# Closing the graphical device
dev.off()


# look at abundance per temp of each species
df_models <- readRDS("df_models.rds")

species_list <- df_models |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

for (i in species_list) {
  
  data <- df_models |> 
    filter(Species == i)
  
  abu_plot <- data |> 
    ggplot(aes(mean_last_7days, Abundance, color = Protocol)) +
    geom_point() +
    labs(title = i) +
    scale_color_manual(values = c("seagreen", "skyblue"))
    
  tiff(paste("total_models/plot_abundance/abu_temp_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
  
  print(plot(abu_plot))
  
  dev.off()
  

}


############################################################################
#comparing depth and temp models

depth_temp <- read_xlsx("deviance_comparison.xlsx")

tab_1 <- depth_temp |> 
  filter(difference < 0) |> 
  arrange(difference) |> 
  gt()

tab_1

depth_temp1 <- depth_temp |> 
  filter(Species != "Coregonus_sp_large_pelagic") 

mean(depth_temp1$difference)
median(depth_temp1$difference)

lollipop_difference <- depth_temp|> 
  filter(Species != "Coregonus_sp_large_pelagic") |> 
    ggplot(aes(x = fct_reorder(Species, difference), y = difference)) +
    geom_segment(aes(x=fct_reorder(Species, difference), xend=fct_reorder(Species, difference), y=0, yend=difference), color ="darkgrey") +
    geom_point(size=2) +
    coord_flip() +
    xlab("") +
    ylab("Difference (dev. expl temp - dev. expl depth)") +
  labs(title = "mean: -0.02324794, median: -0.007589") +
  theme_classic()


lollipop_difference

tiff(paste("total_models/plots/lollipop_diff_depth_temp.tiff", sep = ""), units="in", width = 10, height=8, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(lollipop_difference)

# Closing the graphical device
dev.off()

###################################################################derivatives

# species that were recorded with electro, fishbase or eawag
species_lake <- read_xlsx("species_lake.xlsx") 
str(species_lake)

species_lake$fLake <- as.factor(species_lake$Lake)
species_lake$fProtocol <- as.factor(species_lake$Protocol)
species_lake$Species <- as.factor(species_lake$Species)

#loading all dfs 

df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")

df_binomial_gam$fLake <- as.factor(df_binomial_gam$Lake)
df_binomial_gam$fProtocol <- as.factor(df_binomial_gam$Protocol)

df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")
df_abundance_gam$fProtocol <- as.factor(df_abundance_gam$Protocol)
df_abundance_gam$fLake <- as.factor(df_abundance_gam$Lake)


df_binomial_re <- readRDS("data_frame_models/df_binomial_re")
df_binomial_re$fLake <- as.factor(df_binomial_re$Lake)
df_binomial_re$fProtocol <- as.factor(df_binomial_re$Protocol)

df_abundance_re <- readRDS("data_frame_models/df_abundance_re")
df_abundance_re$fLake <- as.factor(df_abundance_re$Lake)
df_abundance_re$fProtocol <- as.factor(df_abundance_re$Protocol)



# 
# 
# species_list <- df_binomial_gam |> 
#   distinct(Species) |> 
#   pull(Species)
# 
# species_list <- sort(species_list)
# 
# derivatives <- list()
# gam_output <- list()
# 
# # test 
# df_binomial_gam |> 
#   distinct(Species) |> 
#   pull(Species)
#   
# data_test <- df_binomial_gam |>
#   filter(Species == "Alosa_fallax")
# 
# lake_data <- species_lake |>
#   filter(Species == "Alosa_fallax")
# 
# gam_output <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fProtocol, bs = 're'), family = binomial)
# 
# lake_list <- distinct(lake_data, Lake) |>
#   pull()
# 
#   data_lake <- species_lake |>
#     filter(Species == "Alosa_fallax") |>
#     filter(Lake == "Maggiore")
#   
#   unique_lakes <- distinct(data_lake, fLake)
#   unique_protocol <- distinct(data_lake, fProtocol)
#   
#   newdata <- tibble(mean_last_7days = seq(
#     from = min(data_lake$temp, na.rm = TRUE),
#     to = max(data_lake$temp, na.rm = TRUE), length = 200), 
#     fProtocol = factor("VERT"))
#   
#   derivatives <- derivatives(gam_output, data = newdata, term = "s(mean_last_7days)", 
#                              partial_match = TRUE)
#     rename(temp = data)


species_list <- df_binomial_gam |>
  distinct(Species) |>
  pull(Species)

species_list <- sort(species_list)

derivatives <- list()
gam_output <- list()

species_lake |> 
  filter(Species == "Salvelinus_sp")

for (i in species_list) {
  data <- df_binomial_gam |>
    filter(Species == i)
  
  lake_data <- species_lake |>
    filter(Species == i)
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fProtocol, bs = 're'), family = binomial)
  
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
      fProtocol = factor("VERT"))
    
    derivatives <- derivatives(gam_output[[i]], data = newdata, term = "s(mean_last_7days)",
                               partial_match = TRUE) |>
      mutate(fLake = factor(j)) |>
      mutate(species = factor(i)) |>
      rename(temp = data)
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


test1 <- readRDS("total_models/derivatives/derivatives_Alosa_fallax_Maggiore.rds")

test1 |> 
  ggplot(aes(temp, derivative)) +
  geom_line()

# original
# for (i in species_list) {
#   data <- df_binomial_gam |>
#     filter(Species == i)
#   
#   lake_data <- species_lake |>
#     filter(Species == i)
#   
#   gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fProtocol, bs = 're'), family = binomial)
#   
#   lake_list <- distinct(lake_data, Lake) |>
#     pull()
#   
#   for (j in lake_list){
#     
#     data_lake <- species_lake |>
#       filter(Species == i) |>
#       filter(Lake == j)
#     
#     unique_lakes <- distinct(data_lake, fLake)
#     unique_protocol <- distinct(data_lake, fProtocol)
#     
#     newdata <- tibble(mean_last_7days = seq(
#       from = min(data_lake$temp, na.rm = TRUE),
#       to = max(data_lake$temp, na.rm = TRUE), length = 200),
#       fProtocol = sample(levels(unique_protocol$fProtocol), size = 200, replace = TRUE))
#     
#     derivatives <- derivatives(gam_output[[i]], data = newdata) |>
#       mutate(fLake = factor(j)) |>
#       mutate(species = factor(i)) |>
#       rename(temp = data)
#     saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
#   }
# }

# model2

species_list <- df_abundance_gam |> 
  # binomial one
  filter(Species == "Coregonus_sp_benthic_profundal") |>
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()


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
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

# model2 ZIP
species_list <- df_abundance_gam |>
  filter(!Species == "Coregonus_sp_benthic_profundal") |>
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()


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
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

# model3

species_list <- df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

derivatives <- list()
gam_output <- list()



for (i in species_list) {
  data <- df_binomial_re |> 
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


species_list <- df_abundance_re |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()


for (i in species_list) {
  
  data <- df_abundance_re |> 
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
    saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

#try again to make a function for derivatives
df_deriv_all <- list.files(path = "total_models/derivatives", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

# save total derivatives as RDS
saveRDS(df_deriv_all, "total_models/df_deriv_all.rds")

###############################################################################
#response diversity

source(here("functions.R"))

# derivatives

all_models_derivatives <- readRDS("total_models/df_deriv_all.rds")

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

data <- all_deriv |> 
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
                       breaks = c(-7.634168, -3, -1, 0, 1, 3, 4, 10, 30, 200, 14061010))
head(data_new)   

library(RColorBrewer)
# Define the number of colors you want

max_deriv_plot <- data_new |> 
  ggplot(aes(fLake, y = fct_reorder(species, max_derivative), fill= groups)) + 
  geom_tile() +
  # scale_fill_distiller(palette = "PRGn")
  # scale_fill_gradient(low = "#006FAB",
  #                     high = "#971B20",
  #                     guide = "colorbar") +
  scale_fill_manual(breaks = levels(data_new$groups),
                    values = rev(brewer.pal(11, "BrBG")))


                    # values = c("#313695", "#1A66FF", "#3399FF", "#66CCFF", "#99EEFF", "#CCFFFF",
                    #            "#FFFFCC", "#FFEE99", "#FFCC66", "#FF9933", "#FF661A", "#FF2B00"))

tiff(paste("total_models/plots/max_derivatives.tiff", sep = ""), units="in", width=10, height=8, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion

plot(max_deriv_plot)
# Closing the graphical device
dev.off()

