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


#model overview table


bind_1 <- readRDS("model_1/bind_1.rds")
bind_2 <- readRDS("model_2/bind_2.rds")
bind_3 <- readRDS("model_3/bind_3.rds")
bind_4 <- readRDS("model_4/bind_4.rds")

str(bind_1)
str(bind_2)
str(bind_3) 
str(bind_4)

bind_3$n_lake <- as.factor(bind_3$n_lake)
bind_4$n_lake <- as.factor(bind_4$n_lake)

total_summary <- bind_rows(bind_1, bind_2, bind_3, bind_4) |> 
  rename(mean_se_fit = mean_se, total_zeros = observation_0)

# library(writexl)
# dont redo
# write_xlsx(total_summary, "model_1/total_summary_table.xlsx")

total_summary_table <- read_excel("model_1/total_summary_table.xlsx")

#make summary of summary 
#sum total successful models and se for those

library(gt)

total_summary_table |> 
  filter(success == 1) |> 
  select(species, mean_se_fit, total_abundance, n_lake, model_type) |> 
  arrange(species) |> 
  gt() |>
  tab_header(title = "Successful models")

  

library("gtsummary")

lakes_models <- summary_success |> 
  select(n_lake, model_type)

tbl_summary_1 <- tbl_summary(lakes_models)


tbl_summary_1



#predictions for all species

mod_1_pred <- readRDS("total_models/total_model_1_pred")
mod_2_pred <- readRDS("total_models/total_model_2_pred")
mod_3_pred <- readRDS("total_models/total_model_3_pred")
mod_4_pred <- readRDS("total_models/total_model_4_pred")

total_model_predictions <- bind_rows(mod_1_pred, mod_2_pred, mod_3_pred, mod_4_pred)

#filter predictions in succesful models

success_list <- total_summary_table |> 
  filter(success == 1) |> 
  distinct(species) |> 
  pull(species)

success_model_predictions <- total_model_predictions |> 
  filter(species %in% success_list)

#double.check
# success_model_predictions |> 
#   distinct(species)


#plot
success_model_predictions |>
  # filter(species == "Lota_lota") |>
  ggplot(aes(temp, prediction, fill = species)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)


#original data 
df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")
df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")
df_binomial_re <- readRDS("data_frame_models/df_binomial_re")
df_abundance_re <- readRDS("data_frame_models/df_abundance_re")

