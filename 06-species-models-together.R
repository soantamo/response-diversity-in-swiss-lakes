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

model_predictions$species <- as.factor(model_predictions$species)
levels(model_predictions$species)

# test1 <- mod_3_pred |> 
#   filter(species == "Coregonus_brienzii")
# 
# test <- model_predictions |> 
#   filter(species == "Coregonus_brienzii")
#   ggplot(aes(temp, fit)) +
#   geom_line()


#filter predictions in succesful models

# successful_models <- read_excel("model_1/model_success_final.xlsx")
# table(successful_models$model_success)
# 
# 
# success_list <- successful_models |> 
#   filter(model_success == 1) 
# 
# no_success_list <- successful_models |> 
#   filter(model_success == 0)
# 
# 
# success_model_predictions <- model_predictions |> 
#   inner_join(success_list)
# 
# unsuccesful_model_predictions <- model_predictions |> 
#   inner_join(no_success_list)
# 
# 
# str(success_model_predictions)
# success_model_predictions$species <- as.factor(success_model_predictions$species)
# levels(success_model_predictions$species)

# plots: all models
model_predictions |> 
  # filter(species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii",
  #                        "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II",
  #                        "Cottus_sp_Profundal")) |>
  filter(str_detect(species, "Coregonus")) |>

  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)


# plot poster

library(RColorBrewer)
library(ggthemes)

tiff(paste("total_models/plots/poster_framwork.jpg", sep = ""), compression = "lzw",  units = "cm",
     width = 12, height = 8, pointsize = 18, res = 300)


model_predictions |> 
  filter(species %in% c("Leuciscus_leuciscus", "Rutilus_rutilus",
                        "Tinca_tinca")) |>
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_classic() +
  # theme(strip.background = element_rect(fill="lightgrey")) +
  # scale_color_viridis(discrete=TRUE, guide = NULL) +
  scale_color_manual(values = c("#3F007D", "#6A51A3", "#9E9AC8"), guide = NULL) +
  ylab("Abundance (Performance)") +
  xlab("Temperature (Environmental condition)") +
  ggtitle("Performance-environment relationship")

# Closing the graphical device
dev.off()


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
  # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species, scale = "free") +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = NULL)
  ylim(0,1)

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
  ylim(0, 1)

  
  # only joux 
  
  
  joux_prediction <- model_predictions |> 
    filter(species %in% c("Esox_lucius", "Thymallus_thymallus", "Coregonus_sp", 
                          "Leuciscus_leuciscus", "Perca_fluviatilis", "Rutilus_rutilus",
                          "Salmo_trutta", "Tinca_tinca")) |> 
    filter(temp > 11.80075 & temp < 18.13332) |> 
    mutate(upper_se = fit + se.fit, lower_se = fit - se.fit)  |> 
    ggplot(aes(temp, fit, color = factor(species))) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower_se, ymax = upper_se), alpha = 0.3) +
    theme_bw() +
    # facet_wrap(~species, scale = "free") +
    theme(strip.background = element_rect(fill="lightgrey")) +
    scale_color_viridis(discrete=TRUE) +
    ylab("Abundance") +
    ggtitle("Lac de Joux")
  
  joux_prediction

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

levels(all_lakes_tib$species)

# four species are excluded from the analysis

all_deriv <- all_lakes_tib |>
  filter(!species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii", "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II"))

######resp div with all models 

lakes_list <- all_deriv  |> 
  distinct(fLake) |> 
  pull(fLake)

str(lakes_list)

all_all_deriv $fLake <- as.character(all_all_deriv $fLake)
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
#   number_species <- data |>
#     group_by(fLake, species) |>
#     distinct(species) |>
#     mutate(num_species = 1) |>
#     group_by(fLake) |>
#     mutate(sum_species = sum(num_species))
# 
#   species_overview <- bind_rows(species_overview, number_species)
  # saveRDS(species_overview, paste0("total_models/lakes_all_models/species_overview_", i, ".rds"))
  

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

saveRDS(resp_div_no_excl,"total_models/lakes_all_models/resp_div_all.rds")

library(forcats)

species_overview$fLake <- as.factor(species_overview$fLake)

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 80
mycolors <- colorRampPalette(brewer.pal(11, "PuOr"))(nb.cols)

species_overview |> 
  # filter(fLake == "Biel") |>
  # mutate(Lake = fct_reorder(fLake, media_IMDB, .desc=FALSE))  %>%
  ggplot(aes(x = fct_reorder(fLake, sum_species), y = num_species, fill = factor(species))) +
  geom_col(show.legend = TRUE) +
  scale_fill_viridis(discrete = TRUE, option = "H") +
  # scale_fill_manual(values = mycolors) +
  xlab("Lake") +
  theme_bw()

# i need another solution for the species composition. no sense like this

# lollipop for better overview of number of species in model
# change colors

species_overview |> 
  ggplot(aes(x = fct_reorder(fLake, sum_species), y = sum_species)) +
  geom_segment(aes(x=fct_reorder(fLake, sum_species), xend=fct_reorder(fLake, sum_species), y=0, yend=sum_species), color ="darkgrey") +
  geom_point(size=2) +
  coord_flip() +
  xlab("") +
  ylab("Number of species") +
  theme_classic()

  scale_fill_viridis(discrete = TRUE, option = "H") +
  # scale_fill_manual(values = mycolors) +
  xlab("Lake") +
  theme_bw()

# what do I want to visualize?
# species per lake:lollipop
# derivatives of each species in lakes to see which species drives the differences
  # -> numerical derivatives, categorical species and lake
  # boxplot probably best
  
# derivative and temp -> either geom_line or geom_area. geom_line definitely
all_lakes_tib |> 
  filter(fLake == "Biel") |> 
  ggplot(aes(x = temp, y = derivative)) +
  # geom_area(aes(color = factor(species), alpha = 0.5))
  geom_line(aes(color = factor(species)), linewidth=1)
  # facet_wrap(~species) +
  scale_fill_viridis(discrete=TRUE, guide = NULL)
  
  # differences in derivatives per species
  # boxplot is a possibility
  
  library(RColorBrewer)
  # Define the number of colors you want
  nb.cols <- 80
  mycolors <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)
  
  
  all_lakes_tib |> 
    filter(species != "Gasterosteus_gymnurus") |> 
    filter(fLake == "Thun") |>
    # filter(fLake %in% c("Biel", "Thun", "Brienz", "Maggiore")) |>
    # filter(!species %in% c("Gasterosteus_gymnurus")) |> 
    ggplot(aes(x = fct_reorder(species, derivative), y = derivative, fill = factor(species))) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~fLake) +
    xlab("") +
    scale_fill_manual(values = mycolors, guide = NULL) 

    

  

  


resp_div_no_excl <- list.files(path = "total_models/lakes_all_models", pattern = ".rds", full.names = TRUE) |> 
  map_dfr(readRDS) |> 
  relocate(rdiv, Med, sign, .after = temp)

# saveRDS(resp_div_no_excl,"total_models/lakes_all_models/resp_div_all.rds")

# resp_div_long <- resp_div_no_excl |> 
#   pivot_longer(cols = Coregonus_confusus:Coregonus_zuerichensis,
#                names_to = "species", values_to = "derivative")
# 
# test <- resp_div_long |> 
#   drop_na() |> 
#   filter(fLake == "Poschiavo") |> 
#   distinct(species)
#  
# 
# 
# poschiavo_rd <- resp_div_no_excl |> 
#   filter(fLake == "Poschiavo") |> 
#   distinct(species)

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

median_non <- df_median_all |> 
  group_by(fLake) |> 
  distinct(median_rdiv)



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

median_succ <- df_median |> 
  group_by(fLake) |> 
  distinct(median_rdiv) |> 
  rename(median_success = median_rdiv)


sensitivitiy <- merge(median_succ, median_non)

# only changes rdiv in maggiore and poschiavo 
# 

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
  filter(fLake %in% c("Poschiavo"))  |> 
  # filter(derivative > 3) |> 
  # filter(!species == "Phoxinus_csikii") |> 
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line() +
  facet_wrap(~species)

success_model_deriv

all_lakes_tib |>
  filter(fLake %in% c("Poschiavo"))  |> 
  # filter(derivative > 3) |> 
  # filter(!species == "Phoxinus_csikii") |> 
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line() +
  facet_wrap(~species)

# squalius cephalus

# derivative has a crazy impact altough the predictions are basically 0 -> problem
# Cottus gobio profundal thun cant be included at -2500
# Gasterosteus gymnurus looks crazy
# alburnus_arborella, chondrostoma_soetta, cyprinus carpio and salmo sp -> no

#############################################################################3
# interpretation:

mod_1_pred <- readRDS("total_models/pred_model_1_total")
mod_2_pred <- readRDS("total_models/pred_model_2_total")
mod_3_pred <- readRDS("total_models/pred_model_3_total")
mod_4_pred <- readRDS("total_models/pred_model_4_total")

model_predictions <- bind_rows(mod_1_pred, mod_2_pred, mod_3_pred, mod_4_pred) |> 
  select(-fProtocol)

mod_1_deriv <- readRDS("total_models/deriv_model_1_total")
mod_2_deriv <- readRDS("total_models/deriv_model_2_total")
mod_3_deriv <- readRDS("total_models/deriv_model_3_total")
mod_4_deriv <- readRDS("total_models/deriv_model_4_total")

all_models_derivatives <- bind_rows(mod_1_deriv, mod_2_deriv, mod_3_deriv, mod_4_deriv)

all_lakes_tib <- as_tibble(all_models_derivatives)

resp_div_all <- readRDS("total_models/lakes_all_models/resp_div_all.rds")
# resp_div_succ <- readRDS("total_models/lakes/resp_div_succ.rds")
# all models

df_mean_all <- resp_div_all |>
  group_by(fLake) |> 
  summarise(mean_rdiv = mean(rdiv))

dissimilarity_all <- resp_div_all |> 
  ggplot(aes(x = temp, y = rdiv)) +
  geom_line(color = "#54008B") +
  # geom_hline(data = df_mean_all, aes(yintercept = mean_rdiv), linewidth = 0.5,
  #            lty = "dashed")+
  facet_wrap(~fLake) +
  theme_bw() +
  ggtitle("80 Species") +
  ylim(0,7.5)

dissimilarity_all

df_mean_divergence_all <- resp_div_all |>
  group_by(fLake) |> 
  summarise(mean_sign = mean(sign))

divergence_all <- resp_div_all|> 
  ggplot(aes(x = temp, y = sign, color = fLake)) +
  geom_line(color = "#54008B") +
  geom_hline(data = df_mean_divergence_all, aes(yintercept = mean_sign), linewidth = 0.5,
             lty = "dashed")+
  facet_wrap(~fLake)  +
  ggtitle("80 Species") +
  theme_bw()

mean_non <- df_mean_all |> 
  group_by(fLake) |> 
  distinct(mean_rdiv)

sign_all <- resp_div_all |> 
  group_by(fLake) |> 
  summarise(mean_sign = mean(sign))
 


df_means <- merge(df_mean_all, df_mean_divergence_all) |> 
  rename(Lake = fLake, mean_dissimilarity = mean_rdiv, mean_divergence = mean_sign)


library(gt)

df_means |>
  arrange(mean_divergence) |> 
  gt()

# save as excle
library(writexl)

write_xlsx(df_means, "total_models/response_diversity_overview.xlsx")

# successful ones

df_mean <- resp_div_succ |>
  group_by(fLake) |> 
  summarise(mean_rdiv = mean(rdiv))

dissimilarity_succ <- resp_div_succ |> 
  ggplot(aes(x = temp, y = rdiv)) +
  geom_line(color = "#54008B") +
  geom_hline(data = df_mean, aes(yintercept = mean_rdiv), linewidth = 0.5,
             lty = "dashed")+
  facet_wrap(~fLake) +
  theme_bw() +
  ggtitle("74 Species") +
  ylim(0, 7.5)

df_mean_divergence <- resp_div_succ |>
  group_by(fLake) |> 
  summarise(mean_sign = mean(sign))

divergence_succ <- resp_div_succ  |> 
  ggplot(aes(x = temp, y = sign, color = fLake)) +
  geom_line(color = "#54008B") +
  geom_hline(data = df_mean_divergence, aes(yintercept = mean_sign), linewidth = 0.5,
             lty = "dashed")+
  facet_wrap(~fLake)  +
  ggtitle("74 Species") +
  theme_bw()

mean_succ <- df_mean |> 
  group_by(fLake) |> 
  distinct(mean_rdiv) |> 
  rename(mean_success = mean_rdiv)

sign_succ <- resp_div_succ |> 
  group_by(fLake) |> 
  summarise(mean_succ = mean(sign)) 


sensitivity_divergence <- merge(sign_all, sign_succ)
sensitivity <- merge(mean_succ, mean_non)

diss_div <- merge(divergence_all, divergence_succ, dissimilarity_all, dissimilarity_succ)


# only changes rdiv in geneva, maggiore and poschiavo 

# compare plots:

library(gridExtra)

grid.arrange(dissimilarity_all, dissimilarity_succ, ncol=2)
grid.arrange(divergence_all, divergence_succ, ncol = 2)


mean_all_lakes <- merge(sensitivity, sensitivity_divergence) |> 
  select(fLake, mean_rdiv, mean_sign)

library(pals)

mean_all_lakes |> 
  ggplot(aes(mean_rdiv, mean_sign, color = fLake)) +
  geom_point() +
  theme_bw() +
  scale_colour_manual(values=unname(glasbey()))

# library(pals)
# 
# pal.bands(alphabet, alphabet2, cols25, glasbey, kelly, polychrome, 
#           stepped, tol, watlington,
#           show.names=FALSE)


mean_all_lakes |> 
  ggplot(aes(mean_rdiv, mean_sign)) +
  geom_text(aes(label = fLake)) +
  theme_bw()

mean_all_lakes |> 
  ggplot(aes(mean_rdiv, mean_sign)) +
  geom_label(aes(label = fLake)) +
  theme_bw()

# look at all derivatives: gasterosteus gymnurus is strangeeee
# maggiore: outlier is chondrostoma soetta
all_lakes_tib |> 
  filter(fLake == "Maggiore") |> 
  # filter(derivative > 30) |>
  ggplot(aes(temp, derivative, color = factor(species))) +
  geom_line()


species_lake <-  species_overview |> 
  group_by(fLake, species) |> 
  distinct() |> 
  pivot_wider(names_from = fLake, values_from = species)


lake_list <- resp_div_all |> 
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)



# looop plotting

library(gridExtra)
library(grid)


for (i in lake_list){
  
  species_list <- all_lakes_tib |> 
    filter(fLake == i) |> 
    distinct(species) |> 
    pull(species)
  
  data_deriv <- all_lakes_tib |> 
    filter(fLake == i)
  
  minimum <- min(data_deriv$temp)
  maximum <- max(data_deriv$temp)
  
  data_pred <- model_predictions |> 
    filter(species %in% species_list) |> 
    filter(temp > minimum & temp < maximum)
  
  data_resp_div <-resp_div_all |> 
    filter(fLake == i)
  
  lake_prediction <- data_pred |>
    mutate(upper_se = fit + se.fit, lower_se = fit - se.fit)  |>
    ggplot(aes(temp, fit, color = factor(species))) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower_se, ymax = upper_se), alpha = 0.3) +
    theme_bw() +
    theme(strip.background = element_rect(fill="lightgrey")) +
    scale_color_viridis(discrete=TRUE, guide = NULL) +
    ylab("Abundance") +
    ggtitle(i)
  
  deriv_plot <- data_deriv |> 
    ggplot(aes(temp, derivative, color = factor(species))) +
    geom_line() +
    theme_bw() +
    theme(strip.background = element_rect(fill="lightgrey")) +
    scale_color_viridis(discrete=TRUE, guide = NULL) +
    ylab("Derivative")
  
  
  df_mean_all <- data_resp_div |>
    group_by(fLake) |>
    summarise(mean_rdiv = mean(rdiv))

  dissimilarity_all <- data_resp_div |>
    ggplot(aes(x = temp, y = rdiv)) +
    geom_line(color = "#54008B") +
    geom_hline(data = df_mean_all, aes(yintercept = mean_rdiv), linewidth = 0.5,
               lty = "dashed")+
    # facet_wrap(~fLake) +
    theme_bw() +
    ylab("Dissimilarity") +
    ylim(1,7.5)

  df_mean_divergence_all <- data_resp_div |>
    group_by(fLake) |>
    summarise(mean_sign = mean(sign))

  divergence_all <- data_resp_div |>
    ggplot(aes(x = temp, y = sign, color = fLake)) +
    geom_line(color = "#54008B") +
    # geom_hline(data = df_mean_divergence_all, aes(yintercept = mean_sign), linewidth = 0.5,
    #            lty = "dashed") +
    # facet_wrap(~fLake)  +
    theme_bw() +
    ylab("Divergence") +
    ylim(0, 1)
  
# Opening the graphical device

  # pdf(paste("total_models/plots/plot_lake_", i, ".pdf", sep = ""), width = 4, height = 8)
  # tiff(paste("total_models/plots/plot_lake_", i, ".tiff", sep = ""), compression = "lzw",  units = "cm",
  #      width = 6, height = 13, pointsize = 18, res = 300)
  
  tiff(paste("total_models/plots/plot_predictions_no_guide_", i, ".tiff", sep = ""), compression = "lzw",  units = "cm",
       width = 12, height = 8, pointsize = 18, res = 300)


  grid_all <- grid.arrange(lake_prediction, deriv_plot,
                           dissimilarity_all, divergence_all, nrow = 4)

  grid_all <- grid.arrange(lake_prediction, nrow = 1)

  # Closing the graphical device
  dev.off()
# 
#   tiff(paste("total_models/plots/response_diversity_", i, ".tiff"), compression = "lzw",  units = "cm",
#        width = 8, height = 13, pointsize = 18, res = 300)
# 
#   grid_all <- grid.arrange(dissimilarity_all, divergence_all, nrow = 2,
#                            top = textGrob( i ,gp=gpar(fontsize=20,font=3)))
# 
#   # Closing the graphical device
#   dev.off()

  

}


lake_list <- all_lakes_tib |> 
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)

# boxplots of species and derivatives for each lake

# differences in derivatives per species
# boxplot is a possibility

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 80
mycolors <- colorRampPalette(brewer.pal(8, "Accent"))(nb.cols)


for (i in lake_list){
  data <- all_lakes_tib |> 
    filter(fLake == i)
  
  boxplot_deriv <- data |> 
    ggplot(aes(x = fct_reorder(species, derivative), y = derivative, fill = factor(species))) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~fLake) +
    xlab("") +
    scale_fill_manual(values = mycolors, guide = NULL) 
  
  
  tiff(paste("total_models/plots/plot_derivatives_species_", i, ".tiff", sep = ""), compression = "lzw",  units = "cm",
       width = 12, height = 8, pointsize = 18, res = 300)
  
  plot(boxplot_deriv)
  
  # Closing the graphical device
  dev.off()
}



data_test <- all_lakes_tib %>%
  mutate(species2=species) |> 
  select(-species)

all_lakes_tib |>
  filter(fLake %in% c("Biel")) |> 
  ggplot( aes(x=temp, y=derivative, group = species, color = species)) +
  # geom_line(data = data_test, aes(group=species2), color="grey", size=0.5, alpha=0.5)+
  geom_line(aes(color=species, group = species), color="#69b3a2", size=1.2 ) +
  scale_color_viridis(discrete = TRUE) +
  # theme_bw() +
  # theme(
  #   legend.position="none",
  #   plot.title = element_text(size=14),
  #   panel.grid = element_blank()
  # ) +
  # ggtitle("A spaghetti chart of baby names popularity") +
  facet_wrap(~species)


data_test <- all_lakes_tib %>%
  mutate(species2=species) |> 
  select(-species)


# highlight specific lines in one lake 
# not working https://www.data-to-viz.com/caveat/spaghetti.html
# https://www.datanovia.com/en/blog/gghighlight-easy-way-to-highlight-a-ggplot-in-r/
# https://yutannihilation.github.io/gghighlight/articles/gghighlight.html
library(gghighlight)

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 80
mycolors <- colorRampPalette(brewer.pal(8, "PiYG"))(nb.cols)

str(all_lakes_tib)

all_lakes_tib |> 
  filter(fLake == "Biel") |> 
  ggplot() +
  geom_line(aes(x=temp, y=derivative, color = species)) +
  gghighlight(use_direct_label = FALSE) +
  scale_color_viridis(discrete = TRUE) +
  # scale_color_manual(values = mycolors) +
  facet_wrap(~species)



lake_list <- all_lakes_tib |> 
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)

for (i in lake_list){
  data <- all_lakes_tib |> 
    arrange(species) |> 
    filter(fLake == i)
  
  highlight_plot <- data |> 
    ggplot() +
    geom_line(aes(x=temp, y=derivative, color = species)) +
    gghighlight(use_direct_label = FALSE) +
    scale_color_viridis(discrete = TRUE, guide = NULL) +
    # scale_color_manual(values = mycolors) +
    facet_wrap(~species)
  
  
  tiff(paste("total_models/plots/plot_highlight_line_", i, ".tiff", sep = ""), units="in", width=5, height=5, res=300)
  
  plot(highlight_plot)
  
  # Closing the graphical device
  dev.off()
  
  
}

# same with predictions 

lake_list <- all_lakes_tib |> 
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)

for (i in lake_list){
  
  species_list <- all_lakes_tib |> 
    filter(fLake == i) |> 
    distinct(species) |> 
    pull(species)
  
  data_deriv <- all_lakes_tib |> 
    filter(fLake == i)
  
  minimum <- min(data_deriv$temp)
  maximum <- max(data_deriv$temp)
  
  data_pred <- model_predictions |> 
    filter(species %in% species_list) |> 
    filter(temp > minimum & temp < maximum)
  
  highlight_plot <- data_pred |> 
    ggplot() +
    geom_line(aes(x=temp, y=fit, color = species)) +
    gghighlight(use_direct_label = FALSE) +
    scale_color_viridis(discrete = TRUE, guide = NULL) +
    # scale_color_manual(values = mycolors) +
    facet_wrap(~species)
  
  
  tiff(paste("total_models/plots/prediction_highlight_line_", i, ".tiff", sep = ""), units="in", width=5, height=5, res=300)
  
  plot(highlight_plot)
  
  # Closing the graphical device
  dev.off()
  
  
  
  
}


# heatmap testing for Lake vs Species and derivative to look for outliers
# https://www.data-to-viz.com/graph/heatmap.html
# heatmap of lake, species and mean_derivative 
library(plotly)

data <- all_lakes_tib |> 
  mutate(species = factor(species)) |> 
  group_by(fLake, species) |> 
  mutate(mean_derivative = mean(derivative)) |> 
  mutate(mean_temp = mean(temp)) |> 
  distinct(species, fLake, mean_temp, mean_derivative) |> 
  ungroup() |> 
  arrange(mean_derivative)
  
data |> 
  filter(!species %in% c("Gasterosteus_gymnurus")) |> 
  ggplot(aes(fLake, y = fct_reorder(species, mean_derivative), fill= mean_derivative)) + 
  geom_tile() +
  scale_fill_distiller(palette = "PRGn")

# heatmaps of response dievrsity, does not make sense

data <- all_lakes_tib |> 
  mutate(species = factor(species)) |> 
  group_by(fLake, species) |> 
  mutate(mean_derivative = mean(derivative)) |> 
  mutate(mean_temp = mean(temp)) |> 
  distinct(species, fLake, mean_temp, mean_derivative) |> 
  ungroup() |> 
  arrange(mean_derivative)

df_means |> 
  ggplot(aes(Lake, y = mean_dissimilarity, fill= mean_divergence)) +
  geom_tile() +
  scale_fill_distiller(palette = "BrBG")

# trying to do barplots or boxplots with all values not means

head(resp_div_all)
str(resp_div_all)
# needs to be long again

resp_div_long <- resp_div_all |> 
  pivot_longer(cols = Coregonus_confusus:Coregonus_zuerichensis, names_to = "species",
               values_to = "derivative") |> 
  drop_na()

# dissimilarity per lake
resp_div_long |> 
  ggplot(aes(x = fct_reorder(fLake, rdiv), rdiv)) +
  geom_boxplot() +
  coord_flip()

# # divergence per lake
resp_div_long |> 
  ggplot(aes(x = fct_reorder(fLake, sign), sign)) +
  geom_boxplot() +
  coord_flip()

# diss and divergence per lake

library(gghighlight)
library(forcats)

resp_div_long |> 
  ggplot(aes(temp, sign, color = fLake)) +
  geom_line(color = "#2F1163") +
  gghighlight(use_direct_label = FALSE) +
  # scale_color_manual(values = mycolors) +
  facet_wrap(facets = ~fct_reorder(fLake, sign, .desc = TRUE)) +
  theme_bw()


resp_div_long |> 
  ggplot(aes(temp, y = rdiv, color = fLake)) +
  geom_line(color = "#2F1163") +
  gghighlight(use_direct_label = FALSE) +
  # scale_color_viridis(discrete = TRUE, guide = NULL) +
  # scale_color_manual(values = mycolors) +
  facet_wrap(facets = ~fct_reorder(fLake, rdiv, .desc = TRUE)) +
  theme_bw()







