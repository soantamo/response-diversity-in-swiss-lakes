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
library(ggrepel)
library(ggpubr)

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
predictions(df_4) #lepomis also with binomial


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

all_predictions

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

# tiff(paste("total_models/plots/strange_predictions.tiff", sep = ""), units="in", width = 12, height=8, res=300)
# # plot(ggarrange(depth1, depth2, ncol = 2))
# # plot science discussion
# plot(strange_predictions)
# 
# # Closing the graphical device
# dev.off()


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
# tiff(paste("total_models/plots/strange_rpedictions_four.tiff", sep = ""), units="in", width = 12, height=8, res=300)
# # plot(ggarrange(depth1, depth2, ncol = 2))
# # plot science discussion
# plot(four_predictions)
# 
# # Closing the graphical device
# dev.off()


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


################################################################################
# categories and predictions


# prepare categorie in predictions 
# 5 categories;
# endemic: geographically constrained range
# non-native: from NA or Asia 
# non-endemic native: native to Switzerland but not endemic
# non-native region: native to Switzerland and surroundings but has been translocated
# to other Swiss lakes where the species was not native
# endemic translocated: endemic species that were translocated to other lakes

species_category <- read_excel("species-category.xlsx") |> 
  select(-notes) |> 
  mutate(category = ifelse(category == "native", "non_endemic_native", category)) |> 
  mutate(category = ifelse(category == "non_native_region", "non_endemic_native_and_translocated", category)) |> 
  mutate(category2 = factor("all"))


species_category$category <- as.factor(species_category$category)
levels(species_category$category)

model_pred_categories <- merge(model_predictions, species_category) 


category_names <- c(
  `endemic` = "endemic",
  `endemic_and_translocated` = "translocated endemic",
  `non_endemic_native` = "non-endemic native",
  `non_endemic_native_and_translocated` = "translocated non-endemic native",
  `non_native` = "non-native"
)


mycolors1 <-  c("endemic"= "#990F0F", "non_endemic_native"="#8F7EE5", "non_native" = "#260F99",
               "endemic_and_translocated" = "#85B22C", "non_endemic_native_and_translocated" = "#85B22C")


# plot_pred <- model_pred_categories |>
#   filter(species != "Lepomis_gibbosus") |> 
#   ggplot(aes(temp, fit, group = species, color = category)) +
#   # ggplot(aes(temp, fit, color = species)) +
#   geom_line(color = "#512DA8") +
#   # geom_line() +
#   theme_bw(base_size = 16) +
#   ylab("abundance") +
#   xlab("temperature")

# rescale the predictions

library(scales)

head(model_pred_categories)

model_pred_categories_rescaled <- model_pred_categories |> 
  group_by(species) |> 
  mutate(fit_rescaled = (fit - min(fit)) / (max(fit) - min(fit)))



# all four categories
plot_pred <- model_pred_categories_rescaled |>
  filter(species != "Lepomis_gibbosus") |> 
  ggplot(aes(temp, fit_rescaled, group = species, color = category)) +
  # ggplot(aes(temp, fit, color = species)) +
  geom_line() +
  # geom_line() +
  theme_bw(base_size = 16) +
  ylab("rescaled prediction") +
  xlab("temperature") +
  scale_color_manual(values = mycolors1, guide = NULL)

plot_category_predictions <- plot_pred + facet_grid(~category, labeller = as_labeller(category_names))

plot_category_predictions

tiff(paste("total_models/plots/plot_category_predictions.tiff", sep = ""), units="in", width=15, height=4, res=300)

plot(plot_category_predictions)

dev.off()

# plot g) for figure 2. 
# all species

category2_names <- c(
  `all` = "whole-lake community"
)

mycolors2 <-  c("all"= "#35978F")

part_1_plot <- model_pred_categories_rescaled |> 
  filter(species != "Lepomis_gibbosus") |> 
  ggplot(aes(temp, fit_rescaled, group = species, color = category2)) +
  # ggplot(aes(temp, fit, color = species)) +
  geom_line() +
  # geom_line() +
  theme_bw(base_size = 16) +
  ylab("rescaled prediction") +
  xlab("temperature") +
  scale_color_manual(values = mycolors2, guide = NULL) +
  facet_grid(~category2, labeller = as_labeller(category2_names))
part_1_plot

# non-endemic native
part_2_plot <- model_pred_categories_rescaled |>
  filter(species != "Lepomis_gibbosus") |> 
  filter(category %in% c("non_endemic_native")) |> 
  ggplot(aes(temp, fit_rescaled, group = species, color = category)) +
  # ggplot(aes(temp, fit, color = species)) +
  geom_line() +
  # geom_line() +
  theme_bw(base_size = 16) +
  ylab("rescaled prediction") +
  xlab("temperature") +
  scale_color_manual(values = mycolors1, guide = NULL) + 
  facet_grid(~category, labeller = as_labeller(category_names))

# endemic
part_3_plot <- model_pred_categories_rescaled |>
  filter(species != "Lepomis_gibbosus") |> 
  filter(category %in% c("endemic")) |> 
  ggplot(aes(temp, fit_rescaled, group = species, color = category)) +
  # ggplot(aes(temp, fit, color = species)) +
  geom_line() +
  # geom_line() +
  theme_bw(base_size = 16) +
  ylab("rescaled prediction") +
  xlab("temperature") +
  scale_color_manual(values = mycolors1, guide = NULL) + 
  facet_grid(~category, labeller = as_labeller(category_names))


plot_2g <- ggarrange(part_1_plot, part_2_plot, part_3_plot,  ncol = 3)


tiff(paste("total_models/plots/plot_2_g_categories.tiff", sep = ""), units="in", width=15, height=4, res=300)

plot(plot_2g)

dev.off()


# color guide 


myPalette <-  c("endemic" = "#990F0F", "non-endemic native" = "#8F7EE5"," whole-lake community" = "#35978F")

colorKey <- data.frame(colorName = names(myPalette))


plot_guide_2 <- ggplot(data = colorKey, aes(x = 1, y = 1:nrow(colorKey), fill = colorName, label = colorName)) +
  geom_tile() +
  scale_fill_manual(values = myPalette) +
  geom_text(color = "black", size = 4) +  # Corrected to use 'color' aesthetic
  theme_void() +
  theme(legend.position = "none")

tiff(paste("total_models/plots/plot_guide_2.tiff", sep = ""), units="in", width=6, height=4, res=300)

plot(plot_guide_2)

dev.off()


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
# derivatives() will ignore any random effect smooths it encounters in object.

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

# model1
species_list <- df_binomial_gam |>
  distinct(Species) |>
  pull(Species)

species_list <- sort(species_list)

derivatives <- list()
gam_output <- list()
model_prediction <- list()

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

    # newdata <- tibble(mean_last_7days = seq(
    #   from = min(data_lake$temp, na.rm = TRUE),
    #   to = max(data_lake$temp, na.rm = TRUE), length = 200),
    #   fProtocol = factor("VERT"))
    
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), by = 0.02),
      fProtocol = factor("VERT"))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid,
                                    exclude = "s(fProtocol)",
                                    type = "response", se.fit = TRUE)
    
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    
    pred_df <- model_bind |>
      rename(temp = mean_last_7days) |>
      mutate(species = factor(i)) |> 
      mutate(Lake = factor(j))
    
    saveRDS(pred_df, paste0("total_models/predictions_lake/predictions_", i, "_",  j, ".rds"))
    

    # derivatives <- derivatives(gam_output[[i]], data = newdata) |>
    #   mutate(fLake = factor(j)) |>
    #   mutate(species = factor(i)) |>
    #   rename(temp = data)
    # saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

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
model_prediction <- list()

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
    
    
    # newdata <- tibble(mean_last_7days = seq(
    #   from = min(data_lake$temp, na.rm = TRUE),
    #   to = max(data_lake$temp, na.rm = TRUE), length = 200),
    #   fProtocol = factor("VERT"))
    # 
    
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), by = 0.02),
      fProtocol = factor("VERT"))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid,
                                         exclude = "s(fProtocol)",
                                         type = "response", se.fit = TRUE)
    
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    
    pred_df <- model_bind |>
      rename(temp = mean_last_7days) |>
      mutate(species = factor(i)) |> 
      mutate(Lake = factor(j))
    
    saveRDS(pred_df, paste0("total_models/predictions_lake/predictions_", i, "_",  j, ".rds"))

    
    # derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
    #   mutate(fLake = factor(j)) |>
    #   mutate(species = factor(i)) |>
    #   rename(temp = data)
    # saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
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
model_prediction <- list()


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
    
    # newdata <- tibble(mean_last_7days = seq(
    #   from = min(data_lake$temp, na.rm = TRUE),
    #   to = max(data_lake$temp, na.rm = TRUE), length = 200),
    #   fProtocol = factor("VERT"))
    
    
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), by = 0.02),
      fProtocol = factor("VERT"))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid,
                                         exclude = "s(fProtocol)",
                                         type = "response", se.fit = TRUE)
    
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    
    pred_df <- model_bind |>
      rename(temp = mean_last_7days) |>
      mutate(species = factor(i)) |> 
      mutate(Lake = factor(j))
    
    saveRDS(pred_df, paste0("total_models/predictions_lake/predictions_", i, "_",  j, ".rds"))
    
    # derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
    #   mutate(fLake = factor(j)) |>
    #   mutate(species = factor(i)) |>
    #   rename(temp = data)
    # saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

# model3

species_list <- df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

derivatives <- list()
gam_output <- list()
model_prediction <- list()

for (i in species_list) {
  
  data <- df_binomial_re |> 
    filter(Species == i)
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = "re")
                         + s(fProtocol, bs = 're'), family = binomial)
  
  lake_data <- species_lake |>
    filter(Species == i)
  
  lake_list <- distinct(lake_data, Lake) |>
    pull()
  
  unique_lakes <- distinct(data, Lake) |> 
    pull()

  
  for (j in lake_list){
    
    data_lake <- species_lake |>
      filter(Species == i) |>
      filter(Lake == j)
    # take all lakes where the species really is and take random one
    
    random_lake <- sample(unique_lakes, 1)
  
    # 
    # newdata <- tibble(mean_last_7days = seq(
    #   from = min(data_lake$temp, na.rm = TRUE),
    #   to = max(data_lake$temp, na.rm = TRUE), lenqh = 200),
    #   fProtocol = factor("VERT"), fLake = factor(random_lake))
    
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), by = 0.02),
      fProtocol = factor("VERT"), fLake = factor(random_lake))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid,
                                         exclude = c("s(fProtocol)", "s(fLake)"),
                                         type = "response", se.fit = TRUE)
    
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    
    pred_df <- model_bind |>
      rename(temp = mean_last_7days) |>
      mutate(species = factor(i)) |> 
      mutate(Lake = factor(j))
    
    saveRDS(pred_df, paste0("total_models/predictions_lake/predictions_", i, "_",  j, ".rds"))
    
    # derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
    #   mutate(fLake = factor(j)) |>
    #   mutate(species = factor(i)) |>
    #   rename(temp = data)
    # saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


# model 4
# binomial models

species_list <- df_abundance_re |>
  filter(Species %in% c("Alburnus_arborella", "Barbatula_sp_Lineage_I",
                                "Cyprinus_carpio", "Phoxinus_csikii", "Salmo_trutta",
                        "Lepomis_gibbosus")) |>
  distinct(Species) |>
  pull(Species)

species_list <- sort(species_list)


derivatives <- list()
gam_output <- list()
model_prediction <- list()


for (i in species_list) {

  data <- df_abundance_re |>
    filter(Species == i)

  gam_output[[i]] <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                         +  s(fProtocol, bs = 're'), family = binomial)

  lake_data <- species_lake |>
    filter(Species == i)

  lake_list <- distinct(lake_data, Lake) |>
    pull()

  unique_lakes <- distinct(data, Lake) |>
    pull()

  for (j in lake_list){

    data_lake <- species_lake |>
      filter(Species == i) |>
      filter(Lake == j)

    random_lake <- sample(unique_lakes, 1)

    # newdata <- tibble(mean_last_7days = seq(
    #   from = min(data_lake$temp, na.rm = TRUE),
    #   to = max(data_lake$temp, na.rm = TRUE), length = 200),
    #   fLake = factor(random_lake), fProtocol = factor("VERT"))
    
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), by = 0.02),
      fProtocol = factor("VERT"), fLake = factor(random_lake))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid,
                                         exclude = c("s(fProtocol)", "s(fLake)"),
                                         type = "response", se.fit = TRUE)
    
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    
    pred_df <- model_bind |>
      rename(temp = mean_last_7days) |>
      mutate(species = factor(i)) |> 
      mutate(Lake = factor(j))
    
    saveRDS(pred_df, paste0("total_models/predictions_lake/predictions_", i, "_",  j, ".rds"))
    # derivatives <- derivatives(gam_output[[i]], data = newdata) |>
    #   mutate(fLake = factor(j)) |>
    #   mutate(species = factor(i)) |>
    #   rename(temp = data)
    # saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}


# rest
species_list <- df_abundance_re |> 
  filter(!Species %in% c("Alburnus_arborella", "Barbatula_sp_Lineage_I",
                        "Cyprinus_carpio", "Phoxinus_csikii", "Salmo_trutta", 
                        "Lepomis_gibbosus")) |>
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)


# we need to get the derivatives for every lake

derivatives <- list()
gam_output <- list()
model_prediction <- list()


for (i in species_list) {
  
  data <- df_abundance_re |> 
    filter(Species == i)
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                         +  s(fProtocol, bs = 're'), family = ziP())
  
  lake_data <- species_lake |>
    filter(Species == i)
  
  lake_list <- distinct(lake_data, Lake) |>
    pull()
  
  unique_lakes <- distinct(data, Lake) |> 
    pull()
  
  for (j in lake_list){
    
    data_lake <- species_lake |>
      filter(Species == i) |>
      filter(Lake == j)
   
    random_lake <- sample(unique_lakes, 1)
    # 
    # newdata <- tibble(mean_last_7days = seq(
    #   from = min(data_lake$temp, na.rm = TRUE),
    #   to = max(data_lake$temp, na.rm = TRUE), length = 200),
    #   fLake = factor(random_lake), fProtocol = factor("VERT"))
    # 
    
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data_lake$temp, na.rm = TRUE),
      to = max(data_lake$temp, na.rm = TRUE), by = 0.02),
      fProtocol = factor("VERT"), fLake = factor(random_lake))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid,
                                         exclude = c("s(fProtocol)", "s(fLake)"),
                                         type = "response", se.fit = TRUE)
    
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    
    pred_df <- model_bind |>
      rename(temp = mean_last_7days) |>
      mutate(species = factor(i)) |> 
      mutate(Lake = factor(j))
    
    saveRDS(pred_df, paste0("total_models/predictions_lake/predictions_", i, "_",  j, ".rds"))
    
    # derivatives <- derivatives(gam_output[[i]], data = newdata) |> 
    #   mutate(fLake = factor(j)) |>
    #   mutate(species = factor(i)) |>
    #   rename(temp = data)
    # saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
  }
}

#try again to make a function for derivatives
df_deriv <- list.files(path = "total_models/derivatives", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

df_predictions_lakes <- list.files(path = "total_models/predictions_lake", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS)

saveRDS(df_predictions_lakes, "total_models/df_predictions_lakes.rds")

# save total derivatives as RDS (no exclusions)
# saveRDS(df_deriv, "total_models/df_deriv_all_no_excl.rds")

saveRDS(df_deriv, "total_models/df_deriv_all.rds")

################################################################################
library(plotly)

# without exclusions
all_models_derivatives_no_excl <- readRDS("total_models/df_deriv_all_no_excl.rds")
df_deriv_no_excl <- as_tibble(all_models_derivatives_no_excl)

# with exclusions
all_models_derivatives <- readRDS("total_models/df_deriv_all.rds.rds")
all_derivatives <- as_tibble(all_models_derivatives)


# ############################
#look at outliers in histogram

df_new <- df_deriv_no_excl |> 
  group_by(species, fLake) |> 
  mutate(max_derivative = max(derivative)) |> 
  mutate(mean_derivative = mean(derivative)) |> 
  ungroup()


# transform(quantile(all_derivatives$derivative,
#                    c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.85,0.9,0.95,0.99,1)))
# 
# all_derivatives |> 
#   filter(derivative > 2.3) |> 
#   distinct(species)
transform(quantile(df_new$derivative,
                   c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.85,0.9,0.95,0.99,1)))



df_new$groups <- cut(df_new$derivative,              
                     breaks = c(-1.579943e+06, -2.342866e+00, -8.822087e-01,
                                -4.543748e-01, 3.454218e-04, 2.485142e-01,
                                5.527792e-01, 2.911987e+00, 2.018455e+01, 1.280898e+02,
                                1.231212e+07, 1.406101e+07),
                     labels = c("0-1%", "1-5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-85%", "85-90%", "90-95%",
                                "95-99%", "99-100%"))

library(RColorBrewer)

plot_percentiles <- df_new |> 
  filter(species != "Salmo_trutta") |> 
# filter(mean_derivative < 10) |>
  ggplot(aes(x = mean_derivative, fill = factor(species))) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 1.5) +
  # geom_histogram(aes())
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Cyprinus_carpio" = "purple",
                                "Alburnus_arborella" = "orange",
                                "Barbatula_sp_Lineage_I"="steelblue", 
                               "Phoxinus_csikii" = "#66C2A5", "Salmo_trutta" = "#F46D43"))

df_new |> 
  filter(species != "Salmo_trutta") |>
  # filter(mean_derivative < 10) |>
  ggplot(aes(x = mean_derivative, fill = factor(species))) + 
  geom_histogram(aes(y = after_stat(count / sum(count))), binwidth = 1.5) +
  # geom_histogram(aes())
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("Cyprinus_carpio" = "purple",
                               "Alburnus_arborella" = "orange",
                               "Barbatula_sp_Lineage_I"="steelblue", 
                               "Phoxinus_csikii" = "#66C2A5", "Salmo_trutta" = "#F46D43"))

# scale_fill_manual(breaks = levels(df_new$groups),
#                     values = c("#313695", "#313695", "#313695", "#313695", "#313695", "#313695", "#313695",
#                                "#F46D43", "#66C2A5", "#9E0142", "#9E0142"))

plot_percentiles

plot_percentiles_1 <- plot_percentiles +
  geom_vline(xintercept = 19.9, color = "red") +
  annotate("text", x= 18, y = 0.12, label="90th percentile", angle=90) +
  geom_vline(xintercept = 2.911987e+00, color = "red") +
  annotate("text", x= 1.5, y = 0.12, label="85th percentile", angle=90) +
  theme_bw(base_size = 13) +
  xlab("mean species derivative per lake") +
  ylab("percentage") +
 guides(fill = guide_legend(
    title = "species")) +
  ylim(0, 0.2)
  


tiff(paste("total_models/plots/plot_deriv_percentiles_1.tiff", sep = ""), units="in", width=12, height=6, res=300)

plot(plot_percentiles_1)

dev.off()

# above 90% = 2.018455e+01

df_new |> 
  filter(derivative > 2.018455e+01) |> 
  distinct(species)

# 5 species above 90% -> those will be binomial 
# 1 Alburnus_arborella    
# 2 Barbatula_sp_Lineage_I
# 3 Cyprinus_carpio       
# 4 Phoxinus_csikii       
# 5 Salmo_trutta 

# above 85% = 2.911987e+00

df_new |> 
  filter(derivative > 2.911987e+00) |> 
  distinct(species)

# 8 species above 85%

# 1 Alburnus_arborella     
# 2 Barbatula_sp_Lineage_I 
# 3 Barbatula_sp_Lineage_II
# 4 Cyprinus_carpio        
# 5 Lepomis_gibbosus       
# 6 Phoxinus_csikii        
# 7 Salmo_trutta           
# 8 Squalius_cephalus

df_new |> 
  ggplot(aes(x = ))

################tile graph
library(RColorBrewer)
# Define the number of colors you want

data_new <- df_new |> 
  distinct(mean_derivative, max_derivative, fLake, species)


data_new$percentiles <- cut(data_new$mean_derivative,              
                     breaks = c(-1.579943e+06, -2.342866e+00, -8.822087e-01,
                                -4.543748e-01, 3.454218e-04, 2.485142e-01,
                                5.527792e-01, 2.911987e+00, 2.018455e+01, 1.280898e+02,
                                1.231212e+07, 1.406101e+07),
                     labels = c("0-1%", "1-5%", "5-10%", "10-25%", "25-50%", "50-75%", "75-85%", "85-90%", "90-95%",
                                "95-99%", "99-100%"))

max_deriv_plot <- data_new |> 
  ggplot(aes(fLake, y = fct_reorder(species, mean_derivative), fill= percentiles)) + 
  geom_tile() +
  # scale_fill_distiller(palette = "PRGn")
  # scale_fill_gradient(low = "#006FAB",
  #                     high = "#971B20",
  #                     guide = "colorbar") +
  scale_fill_manual(breaks = levels(data_new$percentiles),
                    values = rev(brewer.pal(11, "BrBG")))


# values = c("#313695", "#1A66FF", "#3399FF", "#66CCFF", "#99EEFF", "#CCFFFF",
#            "#FFFFCC", "#FFEE99", "#FFCC66", "#FF9933", "#FF661A", "#FF2B00"))
# 
derivative_percentiles_2 <- max_deriv_plot +
  guides(fill = guide_legend(title = "percentiles (mean derivative)", reverse = TRUE)) +
  xlab("") +
  ylab("") +
  theme_bw(base_size = 16)

tiff(paste("total_models/plots/plot_deriv_percentiles_2.tiff", sep = ""), units="in", width=19, height=12, res=300)

plot(derivative_percentiles_2)

dev.off()
# tiff(paste("total_models/plots/max_derivatives.tiff", sep = ""), units="in", width=12, height=8, res=300)
# # plot(ggarrange(depth1, depth2, ncol = 2))
# # plot science discussion
# 
# plot(max_deriv_plot)
# # Closing the graphical device
# dev.off()

###################################################################################


# categories and predictions


# prepare categorie in predictions 
# 5 categories;
# endemic: geographically constrained range
# non-native: from NA or Asia 
# non-endemic native: native to Switzerland but not endemic
# non-native region: native to Switzerland and surroundings but has been translocated
# to other Swiss lakes where the species was not native
# endemic translocated: endemic species that were translocated to other lakes


predictions_lakes <- readRDS("total_models/df_predictions_lakes.rds") |> 
  select(-fLake)

species_endemism <- read_excel("species_endemism_richness.xlsx") |> 
  rename(endemism = detail_category) |> 
  rename(Lake = fLake) |> 
  select(-num_species, -sum_species)

# 
# species_category$category <- as.factor(species_category$category)
# levels(species_category$category)
# 
lake_pred_categories <- merge(predictions_lakes, species_endemism)

lake_pred_categories$endemism <- as.factor(lake_pred_categories$endemism)
str(lake_pred_categories)

lake_pred_categories <- lake_pred_categories |> 
  mutate(endemism = fct_recode(endemism,
                               "non_endemic_native" = "native")) |> 
  mutate(endemism = fct_recode(endemism,
                               "translocated" = "non_native_region"))

levels(lake_pred_categories$endemism)


mycolors <-  c("endemic"= "#990F0F", "non_endemic_native"="#8F7EE5", "non_native" = "#260F99",
               "translocated" = "#85B22C")

# mycolors <-  c("endemic"= "#303F9F", "native"="#CBC480", "non_native" = "#E74C3C",
#                "non_native_region" = "#CB8088")

lake_plot <- lake_pred_categories |> 
  # filter(Lake == "Maggiore") |> 
  filter(!species %in% c("Lepomis_gibbosus")) |>
  ggplot(aes(temp, fit, group = species, color = endemism)) +
  # ggplot(aes(temp, fit, color = species)) +
  geom_line() +
  # geom_line() +
  theme_bw(base_size = 16) +
  ylab("abundance") +
  xlab("temperature") +
  facet_wrap(~Lake, scale = "free") +
  scale_color_manual(values = mycolors) +
  ylim(0,1) 

lake_plot

tiff(paste("total_models/plots/plot_category_lakes.tiff", sep = ""), units="in", width=16, height=12, res=300)

plot(lake_plot)

dev.off()


##################################################################################
#response diversity

source(here("functions.R"))

# derivatives

all_models_derivatives <- readRDS("total_models/df_deriv_all.rds")

all_deriv <- as_tibble(all_models_derivatives)

str(all_deriv)

levels(all_deriv$species)

######resp div with all models but lepomis gibbosus

lakes_list <- all_deriv  |> 
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

# excluding lepomis
all_deriv <- all_deriv |>
  filter(!species %in% c("Lepomis_gibbosus"))

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

# rdiv_all <- resp_div_all |> 
#   group_by(fLake) |> 
#   mutate(mean_rdiv = mean(rdiv)) |> 
#   mutate(mean_sign = mean(sign))
#   
# create mean by group
# means<- resp_div_all |> 
#   group_by(fLake) |> 
#   summarise(mean_rdiv = mean(rdiv))

  
dMedian <- resp_div_all %>%
  group_by(fLake) %>%
  summarise(median = median(rdiv))


resp_div_all |> 
  ggplot(aes(temp, rdiv)) +
  geom_line() +
  geom_hline(data = dMedian, aes(yintercept = median), lty = 2) +
  facet_wrap(~fLake) +
  theme_bw()


resp_div_all |> 
  ggplot(aes(temp, sign)) +
  geom_line() +
  facet_wrap(~fLake) +
  theme_bw()

metrics_plots(resp_div_all)

metrics_plots <- function(df){
  require(ggpubr)
  lake_list <- df |> 
    distinct(fLake) |> 
    pull(fLake)
  
  for (i in lake_list){
    
    data <- df |> 
      filter(fLake == i)
    
    plot_rdiv <- data |> 
      ggplot(aes(temp, rdiv)) +
      geom_line() +
      theme_bw(base_size = 16) +
      ylab("dissimilarity") +
      # geom_hline(yintercept = data$Med, lty=2) +
      labs(title = paste(i))
    
    plot_sign <- data |> 
      ggplot(aes(temp, sign)) +
      geom_line() +
      theme_bw(base_size = 16) +
      ylab("divergence") +
      ylim(0,1)
    
    metrics_lake <- ggarrange(plot_rdiv, plot_sign, nrow = 2)
    
    tiff(paste("total_models/plot_metrics/metrics_resp_div_", i ,".tiff", sep = ""), units="in", width=8, height=9, res=300)
    
    plot(metrics_lake)

    dev.off()

  }
}

# why do we see these patterns?

# with exclusions
all_models_derivatives <- readRDS("total_models/df_deriv_all.rds")
all_derivatives <- as_tibble(all_models_derivatives)
# 
overview_derivatives <- all_derivatives |>
  # filter(derivative < 5) |>
  arrange(fLake) |> 
  ggplot(aes(temp, derivative, color = species)) +
  geom_line() +
  facet_wrap(~fLake) +
  # facet_wrap(~fLake, scale = "free") +
  theme_bw(base_size = 16) +
  scale_color_viridis(discrete = TRUE, guide = NULL)

overview_deriv <- overview_derivatives + guides(col = "none")

tiff(paste("total_models/plot_metrics/overview_derivatives_lake.tiff", sep = ""), units="in", width=11, height=9, res=300)

plot(overview_deriv)

dev.off()


# new overview plots with rdiv at minimum, mean  and maximum temp
minimum_rdiv <- resp_div_all |> 
  select(temp, rdiv, Med, sign, fLake) |> 
  group_by(fLake) |>  
  filter(temp %in% min(temp))


plot_a <- minimum_rdiv |>
  ggplot(aes(rdiv, sign)) +
  geom_point(color = "#02818A") + geom_text_repel(aes(label = fLake),
                                   size = 3.5,
                                   max.overlaps = 20) +
  labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
  theme_bw(base_size = 16) +
  ylim(0,1) +
  xlim(1,4) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  labs(title = "all species")

plot_a
  
  
plot_a2 <- minimum_rdiv |>
  ggplot(aes(rdiv, sign)) +
  geom_point(color = "#02818A") + 
  geom_text_repel(aes(label = fLake),
                                size = 3.5,
                                max.overlaps = 20) +
    labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  geom_vline(xintercept = 2.75, linetype = "dashed")  +
  labs(title = "all species")
  
  
plot_a2

maximum_rdiv <- resp_div_all |> 
  select(temp, rdiv, Med, sign, fLake) |> 
  group_by(fLake) |>  
  filter(temp %in% max(temp))

plot_b <- maximum_rdiv |>
  ggplot(aes(rdiv, sign)) +
  geom_point(color = "#02818A") + 
  geom_text_repel(aes(label = fLake),
                                    size = 3.5,
                                    max.overlaps = 20) +
  labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
  theme_bw(base_size = 16) +
  ylim(0,1) +
  xlim(1,4) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  geom_vline(xintercept = 2.5, linetype = "dashed")

plot_b

plot_b2 <-maximum_rdiv |>
  ggplot(aes(rdiv, sign)) +
  geom_point(color = "#02818A") +
  geom_text_repel(aes(label = fLake),
                                     size = 3.5,
                                     max.overlaps = 20) +
  labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
  theme_bw(base_size = 16) +
  ylim(0,1) + 
  geom_hline(yintercept = 0.5, linetype = "dashed") + 
  geom_vline(xintercept = 2, linetype = "dashed")



means_resp <- resp_div_all |> 
  select(temp, rdiv, Med, sign, fLake) |> 
  group_by(fLake) |>  
  mutate(mean_temp = mean(temp))

means_resp$temp <- round(means_resp$temp, 1)
means_resp$mean_temp <- round(means_resp$mean_temp, 1)

mean_temp <- means_resp |> 
  group_by(fLake) |>  
  filter(temp == mean_temp) |> 
  distinct(fLake, .keep_all = TRUE)

plot_c <- mean_temp |>
  ggplot(aes(rdiv, sign)) +
  geom_point(color = "#02818A") + 
  geom_text_repel(aes(label = fLake),
                                        size = 3.5,
                                        max.overlaps = 20) +
  labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
  theme_bw(base_size = 16) +
  ylim(0,1) +
  xlim(1,4) + geom_hline(yintercept = 0.5, linetype = "dashed") + 
  geom_vline(xintercept = 2.5, linetype = "dashed")
  
plot_c2 <- mean_temp |>
  ggplot(aes(rdiv, sign)) +
  geom_point(color = "#02818A") +
  geom_text_repel(aes(label = fLake),
                                      size = 3.5,
                                      max.overlaps = 20) +
  labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
  theme_bw(base_size = 16) +
  ylim(0,1)+ geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  xlim(1.5, 3.5)


plot_c2
library(ggpubr)
overview_temp_scaled <- ggarrange(plot_a, plot_c, plot_b, ncol = 3)
overview_temp <- ggarrange(plot_a2, plot_c2, plot_b2, ncol = 3)

tiff(paste("total_models/plot_metrics/overview_temp_scaled.tiff", sep = ""), units="in", width=15, height=5, res=300)

plot(overview_temp_scaled)

dev.off()


tiff(paste("total_models/plot_metrics/overview_temp.tiff", sep = ""), units="in", width=15, height=5, res=300)

plot(overview_temp)

dev.off()

 
#############################################################################
# response diversity endemic and non-endemic native

#response diversity

source(here("functions.R"))

# derivatives

all_models_derivatives <- readRDS("total_models/df_deriv_all.rds")

all_deriv <- as_tibble(all_models_derivatives) |> 
  select(temp, derivative, species, fLake) |> 
  rename(Lake = fLake)


species_endemism <- read_excel("species_endemism_richness.xlsx") |> 
  rename(endemism = detail_category) |> 
  rename(Lake = fLake) |> 
  select(-num_species, -sum_species)

derivatives_categories <- merge(all_deriv, species_endemism)

str(derivatives_categories)

levels(derivatives_categories$species)


endemic_deriv <- derivatives_categories |> 
  filter(endemism == "endemic")

endemic_deriv$Lake <- as.character(endemic_deriv$Lake)
######resp div with all models but lepomis gibbosus

lakes_list <- endemic_deriv |> 
  distinct(Lake) |> 
  pull(Lake)

str(lakes_list)

lakes_list <- sort(lakes_list)

species_overview <- tibble()


endemic_deriv$species <- as.character(endemic_deriv$species)
endemic_deriv$species <- as.factor(endemic_deriv$species)


str(derivatives_categories)

i <- "Thun"
# loop to get response diversity measures for each lake 
for (i in lakes_list){
  
  data <- endemic_deriv |>
    select(temp, Lake, derivative, species) |> 
    filter(Lake == i)
  
  df_resp_div <- data |>
    pivot_wider(
      names_from = species,
      values_from = derivative)
  
  df_resp_div$rdiv <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  saveRDS(df_resp_div, paste0("total_models/endemic_deriv/endemic_resp_div_", i, ".rds"))
  
}


endemic_resp_div <- list.files(path = "total_models/endemic_deriv", pattern = ".rds", full.names = TRUE) |>
  map_dfr(readRDS) |>
  relocate(rdiv, Med, sign, .after = temp)

saveRDS(endemic_resp_div,"total_models/endemic_resp_div.rds")

##############################

native_deriv <- derivatives_categories |> 
  filter(endemism == "native")

native_deriv$Lake <- as.character(native_deriv$Lake)
######resp div with all models but lepomis gibbosus

lakes_list <- native_deriv |> 
  distinct(Lake) |> 
  pull(Lake)

str(lakes_list)

lakes_list <- sort(lakes_list)

species_overview <- tibble()


native_deriv$species <- as.character(native_deriv$species)
native_deriv$species <- as.factor(native_deriv$species)


i <- "Biel"
# loop to get response diversity measures for each lake 
for (i in lakes_list){
  
  data <- native_deriv |>
    select(temp, Lake, derivative, species) |> 
    filter(Lake == i)
  
  df_resp_div <- data |>
    pivot_wider(
      names_from = species,
      values_from = derivative)
  
  df_resp_div$rdiv <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  saveRDS(df_resp_div, paste0("total_models/native_deriv/native_resp_div_", i, ".rds"))
  
}

native_resp_div <- list.files(path = "total_models/native_deriv", pattern = ".rds", full.names = TRUE) |>
  map_dfr(readRDS) |>
  relocate(rdiv, Med, sign, .after = temp)

saveRDS(native_resp_div,"total_models/native_resp_div.rds")


########################

trans_deriv <- derivatives_categories |> 
  filter(endemism == "non_native_region")

trans_deriv$Lake <- as.character(trans_deriv$Lake)
######resp div with all models but lepomis gibbosus

lakes_list <- trans_deriv |> 
  distinct(Lake) |> 
  pull(Lake)

str(lakes_list)

lakes_list <- sort(lakes_list)

species_overview <- tibble()


trans_deriv$species <- as.character(trans_deriv$species)
trans_deriv$species <- as.factor(trans_deriv$species)



# loop to get response diversity measures for each lake 
for (i in lakes_list){
  
  data <- trans_deriv |>
    select(temp, Lake, derivative, species) |> 
    filter(Lake == i)
  
  df_resp_div <- data |>
    pivot_wider(
      names_from = species,
      values_from = derivative)
  
  df_resp_div$rdiv <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-(1:2), drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  saveRDS(df_resp_div, paste0("total_models/translocated_deriv/trans_resp_div_", i, ".rds"))
  
}

translocated_resp_div <- list.files(path = "total_models/translocated_deriv", pattern = ".rds", full.names = TRUE) |>
  map_dfr(readRDS) |>
  relocate(rdiv, Med, sign, .after = temp)

saveRDS(translocated_resp_div,"total_models/translocated_resp_div.rds")


##############################################################################

endemic_resp_div <- readRDS("total_models/endemic_resp_div.rds")
native_resp_div <- readRDS("total_models/native_resp_div.rds")
trans_resp_div <- readRDS("total_models/translocated_resp_div.rds")
resp_div_all <- readRDS("total_models/resp_div_all.rds")

#   
  # new overview plots with rdiv at minimum, mean  and maximum temp

mycolors <-  c("endemic"= "#990F0F", "non_endemic_native"="#8F7EE5", "non_native" = "#260F99",
               "translocated" = "#85B22C", "all" = "#35978F")


minimum_rdiv <- resp_div_all |> 
  select(temp, rdiv, Med, sign, fLake) |> 
  group_by(fLake) |>  
  filter(temp %in% min(temp)) |> 
  mutate(level =  factor("minimum")) |>
  mutate(category =  factor("all")) |> 
  rename(Lake = fLake)

# mean

all_mean <- resp_div_all |> 
  select(temp, rdiv, Med, sign, fLake) |> 
  group_by(fLake) |>  
  mutate(mean_temp = mean(temp)) |> 
  mutate(category =  factor("all")) |> 
  mutate(level =  factor("mean")) |> 
  rename(Lake = fLake)

all_mean$temp <- round(all_mean$temp, 1)
all_mean$mean_temp <- round(all_mean$mean_temp, 1)

all_mean_temp <- all_mean |> 
  group_by(Lake) |>  
  filter(temp == mean_temp) |> 
  distinct(Lake, .keep_all = TRUE) |> 
  mutate(level = factor("mean")) |> 
  mutate(category =  factor("all"))

# maximum

maximum_rdiv <- resp_div_all |> 
  select(temp, rdiv, Med, sign, fLake) |> 
  group_by(fLake) |>  
  filter(temp %in% max(temp)) |> 
  mutate(level =  factor("maximum")) |> 
  mutate(category =  factor("all")) |> 
  rename(Lake = fLake)

rdiv_all <- rbind(minimum_rdiv, all_mean_temp, maximum_rdiv)

# plotting 2a-c


temp_names <- c(
  `minimum` = "minimum lake temperature",
  `mean` = "mean lake temperature",
  `maximum` = "maximum lake temperature"
)

rdiv_all |> 
  ggplot(aes(rdiv, sign, color = category)) +
  geom_point() +
  geom_text_repel(aes(label = Lake), size = 3.5,
                  max.overlaps = 40, color = "black") +
  labs(x = "dissimilarity", y = "divergence") +
  theme_bw(base_size = 16) +
  ylim(0,1) +
  # xlim(0,4.1) +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 2.5, linetype = "dashed") +
  scale_color_manual(values = mycolors, guide = NULL) +
  facet_wrap(~level, labeller = as_labeller(temp_names))

# 
# 
# plot_2a1 <- rdiv_all |> 
#   filter(level == "minimum") |> 
#   ggplot(aes(rdiv, sign, color = category)) +
#   geom_point() +
#   geom_text_repel(aes(label = Lake), size = 3.5,
#                     max.overlaps = 40, color = "black") +
#   labs(x = "dissimilarity", y = "divergence") +
#   theme_bw(base_size = 16) +
#   ylim(0,1) +
#   # xlim(0,4.1) +
#   geom_hline(yintercept = 0.5, linetype = "dashed") +
#   geom_vline(xintercept = 2.75, linetype = "dashed") +
#   scale_color_manual(values = mycolors, guide = NULL) +
#   facet_wrap(~level, labeller = as_labeller(temp_names))
# 
# plot_2a1
# 
# plot_2a2 <- rdiv_all |> 
#   filter(level == "mean") |> 
#   ggplot(aes(rdiv, sign, color = category)) +
#   geom_point() +
#   geom_text_repel(aes(label = Lake), size = 3.5,
#                   max.overlaps = 40, color = "black") +
#   labs(x = "dissimilarity", y = "divergence") +
#   theme_bw(base_size = 16) +
#   ylim(0,1) +
#   # xlim(0,4.1) +
#   geom_hline(yintercept = 0.5, linetype = "dashed") +
#   geom_vline(xintercept = 2.3, linetype = "dashed") +
#   scale_color_manual(values = mycolors, guide = NULL) +
#   facet_wrap(~level, labeller = as_labeller(temp_names))
# 
# plot_2a2
# 
# plot_2a3 <- rdiv_all |> 
#   filter(level == "maximum") |> 
#   ggplot(aes(rdiv, sign, color = category)) +
#   geom_point() +
#   geom_text_repel(aes(label = Lake), size = 3.5,
#                   max.overlaps = 40, color = "black") +
#   labs(x = "dissimilarity", y = "divergence") +
#   theme_bw(base_size = 16) +
#   ylim(0,1) +
#   # xlim(0,4.1) +
#   geom_hline(yintercept = 0.5, linetype = "dashed") +
#   geom_vline(xintercept = 2.75, linetype = "dashed") +
#   scale_color_manual(values = mycolors, guide = NULL) +
#   facet_wrap(~level, labeller = as_labeller(temp_names))
# 
# plot_2a3



# plot 2c-e

  endemic_minimum <- endemic_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    filter(temp %in% min(temp)) |> 
    mutate(level =  factor("minimum")) |> 
    mutate(category =  factor("endemic"))
  
  native_minimum <- native_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    filter(temp %in% min(temp)) |> 
    mutate(level =  factor("minimum")) |> 
    mutate(category =  factor("non_endemic_native"))
  
  # mean

  endemic_mean <- endemic_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    mutate(mean_temp = mean(temp))
  
  endemic_mean$temp <- round(endemic_mean$temp, 1)
  endemic_mean$mean_temp <- round(endemic_mean$mean_temp, 1)
  
  endemic_mean_temp <- endemic_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE) |> 
    mutate(level =  factor("mean")) |> 
    mutate(category = factor("endemic"))
  
  native_mean <- native_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    mutate(mean_temp = mean(temp))
  
  native_mean$temp <- round(native_mean$temp, 1)
  native_mean$mean_temp <- round(native_mean$mean_temp, 1)
  
  native_mean_temp <- native_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE)  |>
    mutate(level =  factor("mean")) |> 
    mutate(category = factor("non_endemic_native"))
  
  # maximum
  endemic_max <- endemic_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    filter(temp %in% max(temp)) |> 
    mutate(level =  factor("maximum")) |> 
    mutate(category =  factor("endemic"))
  
  native_max <- native_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    filter(temp %in% max(temp)) |> 
    mutate(level =  factor("maximum")) |> 
    mutate(category =  factor("non_endemic_native"))

  
  df_all <- rbind(rdiv_all, endemic_minimum, endemic_mean_temp, endemic_max, native_minimum,
                  native_mean_temp, native_max)
  

  
  mycolors <-  c("endemic"= "#990F0F", "non_endemic_native"="#8F7EE5", "non_native" = "#260F99",
                 "translocated" = "#85B22C", "all" = "#35978F")

  plot_2abc <- df_all |> 
    filter(category == "all") |> 
    ggplot(aes(rdiv, sign, color = category)) +
    geom_point() +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 40) +
    labs(x = "dissimilarity", y = "divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1) +
    xlim(1,4.1) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_vline(xintercept = 2.5, linetype = "dashed") +
    scale_color_manual(values = mycolors, guide = NULL) +
    facet_wrap(~level, labeller = as_labeller(temp_names))

  tiff(paste("total_models/plots/plot_2abc.tiff", sep = ""), units="in", width=15, height=5, res=300)
  
  plot(plot_2abc)
  
  dev.off()

  plot2def <- df_all |> 
    filter(category %in% c("all", "non_endemic_native")) |>
    ggplot(aes(rdiv, sign, color = category)) +
    geom_point() +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 45) +
    labs(x = "dissimilarity", y = "divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1) +
    xlim(0,4.1) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_vline(xintercept = 2, linetype = "dashed") +
    scale_color_manual(values = mycolors, guide = NULL) +
    facet_wrap(~level, labeller = as_labeller(temp_names))
  
  
  
  tiff(paste("total_models/plots/plot_2def.tiff", sep = ""), units="in", width=15, height=5, res=300)
  
  plot(plot2def)
  
  dev.off()
  
  
  plot2ghi <- df_all |> 
    filter(category %in% c("endemic", "non_endemic_native")) |>
    ggplot(aes(rdiv, sign, color = category)) +
    geom_point() +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 45) +
    labs(x = "dissimilarity", y = "divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1) +
    xlim(0,4.1) +
    geom_hline(yintercept = 0.5, linetype = "dashed") +
    geom_vline(xintercept = 2, linetype = "dashed") +
    scale_color_manual(values = mycolors, guide = NULL) +
    facet_wrap(~level, labeller = as_labeller(temp_names))
  
  tiff(paste("total_models/plots/plot_2ghi.tiff", sep = ""), units="in", width=15, height=5, res=300)
  
  plot(plot2ghi)
  
  dev.off()
# geom segment looks bad and not working how I want it


 a <- all_minimum |> 
    filter(category %in% c("all", "non_endemic_native")) |>
    ggplot(aes(rdiv, sign, color = category)) +
    geom_point() +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 40) +
    labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1) +
   xlim(0,4.1) +
   geom_hline(yintercept = 0.5, linetype = "dotted") +
   geom_vline(xintercept = 2.5, linetype = "dotted") +
   scale_color_manual(values = mycolors, guide = NULL) 
a
 
 # b <- all_minimum |> 
 #   filter(category %in% c("all", "endemic")) |>
 #   ggplot(aes(rdiv, sign, color = category)) +
 #   geom_point() +
 #   geom_text_repel(aes(label = Lake), size = 3.5,
 #                   max.overlaps = 40) +
 #   labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
 #   theme_bw(base_size = 16) +
 #   ylim(0,1) +
 #   geom_hline(yintercept = 0.5) +
 #   geom_vline(xintercept = 2) +
 #   scale_color_manual(values = mycolors, guide = NULL) 
 
 
 c <- all_minimum |> 
   filter(category %in% c("non_endemic_native", "endemic")) |> 
   ggplot(aes(rdiv, sign, color = category)) +
   geom_point() +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 40) +
   labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   xlim(0,4.1) +
   geom_hline(yintercept = 0.5, linetype = "dotted") +
   geom_vline(xintercept = 2, linetype = "dotted") +
   scale_color_manual(values = mycolors, guide = NULL)
 
 c
 tiff(paste("total_models/plots/minimum_temp_a.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(a)
 
 dev.off()
 
 tiff(paste("total_models/plots/minimum_temp_b.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(b)
 
 dev.off()
 
 tiff(paste("total_models/plots/minimum_temp_c.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(c)
 
 dev.off()
 
 # maximum
 
 all_mean <- resp_div_all |> 
   select(temp, rdiv, Med, sign, fLake) |> 
   group_by(fLake) |>  
   mutate(mean_temp = mean(temp)) |> 
   mutate(category =  factor("all")) |> 
   rename(Lake = fLake)
 
 all_mean$temp <- round(all_mean$temp, 1)
 all_mean$mean_temp <- round(all_mean$mean_temp, 1)
 
all_mean_temp <- all_mean |> 
   group_by(Lake) |>  
   filter(temp == mean_temp) |> 
   distinct(Lake, .keep_all = TRUE) |> 
   mutate(category = factor("all"))
 
  
endemic_mean <- endemic_resp_div |> 
  select(temp, rdiv, Med, sign, Lake) |> 
  group_by(Lake) |>  
  mutate(mean_temp = mean(temp))
  
  endemic_mean$temp <- round(endemic_mean$temp, 1)
  endemic_mean$mean_temp <- round(endemic_mean$mean_temp, 1)
  
  endemic_mean_temp <- endemic_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE) |> 
    mutate(category = factor("endemic"))
  
  native_mean <- native_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    mutate(mean_temp = mean(temp))
  
  native_mean$temp <- round(native_mean$temp, 1)
  native_mean$mean_temp <- round(native_mean$mean_temp, 1)
  
  native_mean_temp <- native_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE)  |> 
    mutate(category = factor("non_endemic_native"))
  
  trans_mean <- trans_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    mutate(mean_temp = mean(temp))
  
  trans_mean$temp <- round(trans_mean$temp, 1)
  trans_mean$mean_temp <- round(trans_mean$mean_temp, 1)
  
  trans_mean_temp <- trans_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE)  |> 
    mutate(category = factor("translocated"))
  
df_mean <- rbind(endemic_mean_temp, native_mean_temp, trans_mean_temp, all_mean_temp)

 
 mycolors <-  c("endemic"= "#990F0F", "non_endemic_native"="#8F7EE5", "non_native" = "#260F99",
                "translocated" = "#85B22C", "all" = "#35978F")
 
 a_mean <- df_mean |> 
   filter(category %in% c("all", "non_endemic_native")) |>
   ggplot(aes(rdiv, sign, color = category)) +
   geom_point() +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 40) +
   labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   xlim(0,4.1) +
   geom_hline(yintercept = 0.5, linetype = "dotted") +
   geom_vline(xintercept = 2.5, linetype = "dotted") +
   scale_color_manual(values = mycolors, guide = NULL) 
 a_mean
 
 # b_mean <- df_mean |> 
 #   filter(category %in% c("all", "endemic")) |>
 #   ggplot(aes(rdiv, sign, color = category)) +
 #   geom_point() +
 #   geom_text_repel(aes(label = Lake), size = 3.5,
 #                   max.overlaps = 40) +
 #   labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
 #   theme_bw(base_size = 16) +
 #   ylim(0,1) +
 #   scale_color_manual(values = mycolors, guide = NULL) 
 
 
 c_mean <- df_mean |> 
   filter(category %in% c("non_endemic_native", "endemic")) |> 
   ggplot(aes(rdiv, sign, color = category)) +
   geom_point() +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 40) +
   labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   xlim(0,4.1) +
   geom_hline(yintercept = 0.5, linetype = "dotted") +
   geom_vline(xintercept = 1.6, linetype = "dotted") +
   scale_color_manual(values = mycolors, guide = NULL)
 c_mean
 
 
 tiff(paste("total_models/plots/mean_temp_a.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(a_mean)
 
 dev.off()
 
 tiff(paste("total_models/plots/mean_temp_b.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(b_mean)
 
 dev.off()
 
 tiff(paste("total_models/plots/mean_temp_c.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(c_mean)
 
 dev.off()
 
 ################3
 # maximum
 
 maximum_rdiv <- resp_div_all |> 
   select(temp, rdiv, Med, sign, fLake) |> 
   group_by(fLake) |>  
   filter(temp %in% max(temp)) |> 
   mutate(category =  factor("all")) |> 
   rename(Lake = fLake)
 
 endemic_max <- endemic_resp_div |> 
   select(temp, rdiv, Med, sign, Lake) |> 
   group_by(Lake) |>  
   filter(temp %in% max(temp)) |> 
   # rename(end_rdiv = rdiv) |>
   # rename(end_sign = sign) |>
   mutate(category =  factor("endemic"))
 
 native_max <- native_resp_div |> 
   select(temp, rdiv, Med, sign, Lake) |> 
   group_by(Lake) |>  
   filter(temp %in% max(temp)) |> 
   # rename(nat_rdiv = rdiv) |>
   # rename(nat_sign = sign) |>
   mutate(category =  factor("non_endemic_native"))
 
 
 trans_max <- trans_resp_div |> 
   select(temp, rdiv, Med, sign, Lake) |> 
   group_by(Lake) |>  
   filter(temp %in% max(temp)) |> 
   # rename(trans_rdiv = rdiv) |> 
   # rename(trans_sign = sign) |> 
   mutate(category =  factor("translocated"))
 
 
 maximum_all <- rbind(maximum_rdiv, endemic_max, native_max, trans_max)
 
 
 mycolors <-  c("endemic"= "#990F0F", "non_endemic_native"="#8F7EE5", "non_native" = "#260F99",
                "translocated" = "#85B22C", "all" = "#35978F")
 
 a_max <- maximum_all |> 
   filter(category %in% c("all", "non_endemic_native")) |>
   ggplot(aes(rdiv, sign, color = category)) +
   geom_point() +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 40) +
   labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   xlim(0,4.1) +
   geom_hline(yintercept = 0.5, linetype = "dotted") +
   geom_vline(xintercept = 1.9, linetype = "dotted") +
   scale_color_manual(values = mycolors, guide = NULL) 
 
 a_max
 
 # b_max <- maximum_all |> 
 #   filter(category %in% c("all", "endemic")) |>
 #   ggplot(aes(rdiv, sign, color = category)) +
 #   geom_point() +
 #   geom_text_repel(aes(label = Lake), size = 3.5,
 #                   max.overlaps = 40) +
 #   labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
 #   theme_bw(base_size = 16) +
 #   ylim(0,1) +
 #   scale_color_manual(values = mycolors, guide = NULL) 
 
 
 c_max <- maximum_all |> 
   filter(category %in% c("non_endemic_native", "endemic")) |> 
   ggplot(aes(rdiv, sign, color = category)) +
   geom_point() +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 40) +
   labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   xlim(0,4.1) +
   geom_hline(yintercept = 0.5, linetype = "dotted") +
   geom_vline(xintercept = 1.5, linetype = "dotted") +
   scale_color_manual(values = mycolors, guide = NULL)
 
 c_max
 
 tiff(paste("total_models/plots/max_temp_a.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(a_max)
 
 dev.off()
 
 tiff(paste("total_models/plots/max_temp_b.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(b_max)
 
 dev.off()
 
 tiff(paste("total_models/plots/max_temp_c.tiff", sep = ""), units="in", width=7, height=5, res=300)
 
 plot(c_max)
 
 dev.off()
 
 
 
 
 ########################################################################33
 
 
 
 all_minimum |> 
   filter(category %in% c("non_endemic_native", "endemic")) |> 
   ggplot(aes(rdiv, sign, color = category)) +
   geom_point() +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 40) +
   labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   scale_color_manual(values = mycolors)
 
 # a +   geom_segment(aes(y=sign, yend= nat_sign,
 #                        x=rdiv, xend=nat_rdiv))
    
 #minimum 
  library(ggrepel)
  
  # 
  # E.e. geom_segment(aes(x = endemic$dissimilarity,
  #                       xend = nonendemic$dissimilarity, y = endemic$divergence, yend = nonendemic$divergence)…
 end_mini <- endemic_minimum |>
    ggplot(aes(rdiv, sign)) +
    geom_point(color = "#990F0F") +
   geom_text_repel(aes(label = Lake), size = 3.5,
                                       max.overlaps = 20) +
    labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1) +
   labs(title = "endemic")
 
 
native_short <- native_minimum |> 
  filter(!Lake %in% c("Geneva", "Joux", "Lugano", "Maggiore", "Poschiavo"))
 
nati_mini +
   geom_curve(aes(x = endemic_minimum$rdiv, y = endemic_minimum$sign, xend = native_short$rdiv, yend = native_short$sign, colour = "curve"))
 
 
   geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df)
  
 nat_mini <- native_minimum |>
   ggplot(aes(rdiv, sign)) +
   geom_point(color = "#8F7EE5") +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 20) +
   labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   labs(title = "native")
 
 t_mini <- trans_minimum |>
   ggplot(aes(rdiv, sign)) +
   geom_point(color = "#85B22C") +
   geom_text_repel(aes(label = Lake), size = 3.5,
                   max.overlaps = 20) +
   labs(x = "minimum temp dissimilarity", y = "minimum temp divergence") +
   theme_bw(base_size = 16) +
   ylim(0,1) +
   labs(title = "translocated")

  # maximum
  endemic_maximum <- endemic_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    filter(temp %in% max(temp))
  
  native_maximum <- native_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    filter(temp %in% max(temp))
  
  trans_maximum <- trans_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    filter(temp %in% max(temp))
  
  
 end_maxi <-  endemic_maximum |>
    ggplot(aes(rdiv, sign)) +
    geom_point(color = "#990F0F") +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 20) +
    labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1)
  
  
  nat_maxi <- native_maximum |>
    ggplot(aes(rdiv, sign)) +
    geom_point(color = "#8F7EE5") +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 20) +
    labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1)
  
  t_maxi <- trans_maximum |>
    ggplot(aes(rdiv, sign)) +
    geom_point(color = "#85B22C") +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 20) +
    labs(x = "maximum temp dissimilarity", y = "maximum temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1)
  
  
  
  endemic_mean <- endemic_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    mutate(mean_temp = mean(temp))
  
  endemic_mean$temp <- round(endemic_mean$temp, 1)
  endemic_mean$mean_temp <- round(endemic_mean$mean_temp, 1)
  
  endemic_mean_temp <- endemic_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE)
  
  
  end_mean <- endemic_mean_temp |>
    ggplot(aes(rdiv, sign)) +
    geom_point(color = "#990F0F") +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 20) +
    labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1)
  
  
  native_mean <- native_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    mutate(mean_temp = mean(temp))
  
  native_mean$temp <- round(native_mean$temp, 1)
  native_mean$mean_temp <- round(native_mean$mean_temp, 1)
  
  native_mean_temp <- native_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE)
  
  
  nat_mean <- native_mean_temp |>
    ggplot(aes(rdiv, sign)) +
    geom_point(color = "#8F7EE5") +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 20) +
    labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1)
  
  
  trans_mean <- trans_resp_div |> 
    select(temp, rdiv, Med, sign, Lake) |> 
    group_by(Lake) |>  
    mutate(mean_temp = mean(temp))
  
  trans_mean$temp <- round(trans_mean$temp, 1)
  trans_mean$mean_temp <- round(trans_mean$mean_temp, 1)
  
  trans_mean_temp <- trans_mean |> 
    group_by(Lake) |>  
    filter(temp == mean_temp) |> 
    distinct(Lake, .keep_all = TRUE)
  
  
  t_mean <- trans_mean_temp |>
    ggplot(aes(rdiv, sign)) +
    geom_point(color = "#85B22C") +
    geom_text_repel(aes(label = Lake), size = 3.5,
                    max.overlaps = 20) +
    labs(x = "mean temp dissimilarity", y = "mean temp divergence") +
    theme_bw(base_size = 16) +
    ylim(0,1)
  
  library(ggpubr)
endemic_overview <- ggarrange(end_mini, end_mean, end_maxi, ncol = 3)
native_overview <- ggarrange(nat_mini, nat_mean, nat_maxi, ncol = 3)
translocated_overview <- ggarrange(t_mini, t_mean, t_maxi, ncol = 3)
  
  tiff(paste("total_models/plots/overview_temp_endemic.tiff", sep = ""), units="in", width=15, height=5, res=300)
  
  plot(endemic_overview)
  
  dev.off()
  
  
  tiff(paste("total_models/plots/overview_temp_native.tiff", sep = ""), units="in", width=15, height=5, res=300)
  
  plot(native_overview)
  
  dev.off()
  
  
  tiff(paste("total_models/plots/overview_temp_translocated.tiff", sep = ""), units="in", width=15, height=5, res=300)
  
  plot(translocated_overview)
  
  dev.off()

