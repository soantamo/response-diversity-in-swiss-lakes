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

total_model_predictions <- bind_rows(mod_1_pred, mod_2_pred, mod_3_pred, mod_4_pred) |> 
  select(-fProtocol)

# derivatives for all species

mod_1_deriv <- readRDS("total_models/deriv_model_1_total")
mod_2_deriv <- readRDS("total_models/deriv_model_2_total")
mod_3_deriv <- readRDS("total_models/deriv_model_3_total")
mod_4_deriv <- readRDS("total_models/deriv_model_4_total")

total_model_derivatives <- bind_rows(mod_1_deriv, mod_2_deriv, mod_3_deriv, mod_4_deriv)



#filter predictions in succesful models

successful_models <- read_excel("model_1/model_success_final.xlsx")
table(successful_models$model_success)


success_list <- successful_models |> 
  filter(model_success == 1) |> 
  distinct(species) |> 
  pull(species)

success_model_predictions <- total_model_predictions |> 
  filter(species %in% success_list)


success_model_predictions |> 
  ggplot(aes(temp, fit, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~species) +
  theme(strip.background = element_rect(fill="lightgrey")) +
  scale_color_viridis(discrete=TRUE, guide = "none")


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



#############################################################################3

#model overview table
# 
# bind_1 <- readRDS("model_1/bind_1.rds")
# bind_2 <- readRDS("model_2/bind_2.rds")
# bind_3 <- readRDS("model_3/bind_3.rds")
# bind_4 <- readRDS("model_4/bind_4.rds")
# 
# str(bind_1)
# str(bind_2)
# str(bind_3) 
# str(bind_4)
# 
# bind_3$n_lake <- as.factor(bind_3$n_lake)
# bind_4$n_lake <- as.factor(bind_4$n_lake)
# 
# total_summary <- bind_rows(bind_1, bind_2, bind_3, bind_4) |> 
#   rename(total_zeros = observation_0)

# library(writexl)
# dont redo
# write_xlsx(total_summary, "model_1/total_summary_table_ver2.xlsx")

# total_summary_table <- read_excel("model_1/total_summary_table_ver2.xlsx")

#make summary of summary 
#sum total successful models and se for those

library(gt)

total_summary_table |> 
  select(species, mean_se, max_se, min_se, total_abundance, n_lake, success) |> 
  arrange(species) |> 
  gt() |>
  tab_header(title = "All models")


total_summary_table |> 
  filter(success == 1) |> 
  select(species, mean_se, max_se, min_se, total_abundance, n_lake, model_type) |> 
  arrange(max_se) |> 
  gt() |>
  tab_header(title = "Potentially successful models")
  
#model 1
total_summary_table |> 
  filter(model_type == 1) |> 
  filter(total_abundance > 5) |> 
  select(species, mean_se, max_se, min_se, total_abundance) |> 
  arrange(total_abundance) |> 
  gt() |>
  tab_header(title = "Model 1: binomial")

#model 2
total_summary_table |> 
  filter(model_type == 2) |> 
  # filter(total_abundance > 5) |> 
  select(species, mean_se, max_se, min_se, total_abundance) |> 
  arrange(total_abundance) |> 
  gt() |>
  tab_header(title = "Model 2: zero-inflated poisson")

#model 3
total_summary_table |> 
  filter(model_type == 3) |> 
  # filter(total_abundance > 5) |> 
  select(species, mean_se, max_se, min_se, total_abundance) |> 
  arrange(total_abundance) |> 
  gt() |>
  tab_header(title = "Model 3: binomial with re")

#model 4
total_summary_table |> 
  filter(model_type == 4) |> 
  # filter(total_abundance > 5) |> 
  select(species, mean_se, max_se, min_se, total_abundance) |> 
  arrange(total_abundance) |> 
  gt() |>
  tab_header(title = "Model 4: zero-inflated poisson with re")

library("gtsummary")

lakes_models <- summary_success |> 
  select(n_lake, model_type)


tbl_summary_1 <- tbl_summary(lakes_models)


tbl_summary_1

#table for percentages of models, number of species

percentages_model_types <- total_summary_table |> 
  select(model_type)

tbl_summary(percentages_model_types)

#table for percentages of number of lakes, number of species

percentages_lakes <- total_summary_table |> 
  select(n_lake)

tbl_summary(percentages_lakes)


#model validation
# see https://r.qcbs.ca/workshop08/book-en/gam-model-checking.html
# and https://r.qcbs.ca/workshop04/pres-en/workshop04-pres-en.html#38
# and https://r.qcbs.ca/workshop06/pres-en/workshop06-pres-en.html#1
# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
#reread https://stats.stackexchange.com/questions/405129/model-selection-for-gam-in-r


#predictions for all species

mod_1_pred <- readRDS("total_models/total_model_1_pred")
mod_2_pred <- readRDS("total_models/total_model_2_pred")
mod_3_pred <- readRDS("total_models/total_model_3_pred")
mod_4_pred <- readRDS("total_models/total_model_4_pred")

total_model_predictions <- bind_rows(mod_1_pred, mod_2_pred, mod_3_pred, mod_4_pred)

#filter predictions in succesful models

success_list <- total_summary_table |> 
  filter(success == 1) |> 
  filter(model_type %in% c(3, 4)) |> 
  #also filter the ones with abundance < 10 
  filter(total_abundance > 10) |>
  distinct(species) |> 
  pull(species)

success_model_predictions <- total_model_predictions |> 
  filter(species %in% success_list)

#double.check
# success_model_predictions |> 
#   distinct(species)


#plot, fixed scales

#model 1 

model_1 <- total_summary_table |> 
  filter(model_type == 1) |> 
  filter(total_abundance > 5) |> 
  distinct(species) |> 
  pull(species)


total_model_predictions |> 
  filter(species %in% model_1) |> 
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  facet_wrap(~species, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill="lightgrey"))+
  scale_color_viridis(discrete=TRUE) 
  
#model 2


model_2 <- total_summary_table |> 
  filter(model_type == 2) |> 
  distinct(species) |> 
  pull(species)


total_model_predictions |> 
  filter(species %in% model_2) |> 
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  facet_wrap(~species, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill="lightgrey"))+
  scale_color_viridis(discrete=TRUE) 

# model 3

model_3 <- total_summary_table |> 
  filter(model_type == 3) |> 
  distinct(species) |> 
  pull(species)


total_model_predictions |> 
  filter(species %in% model_3) |> 
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  facet_wrap(~species, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill="lightgrey"))+
  scale_color_viridis(discrete=TRUE) 

# model 4
model_4 <- total_summary_table |> 
  filter(model_type == 4) |> 
  distinct(species) |> 
  pull(species)


total_model_predictions |> 
  filter(species %in% model_4) |> 
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  facet_wrap(~species, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill="lightgrey"))+
  scale_color_viridis(discrete=TRUE) 

#plot only with good looking ones

# success_model_predictions |> 
#   filter(species %in% c("Salvelinus_umbla", "Coregonus_albellus",
#                         "Coregonus_sp", "Perca_fluviatilis", "Rutilus_rutilus",
#                         "Gasterosteus_aculeatus", "Gymnopcephalus_cernua")) |> 
#   ggplot(aes(temp, prediction, color = factor(species))) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
#   facet_wrap(~species, scales = "free") +
#   theme_bw() +
#   theme(strip.background = element_rect(fill="lightgrey")) + 
#   scale_color_viridis(discrete=TRUE) 
#   


# without confidence interval
q <- success_model_predictions |>
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  facet_wrap(~species, scales = "free") +
  theme_bw() +
  theme(strip.background = element_rect(fill="lightgrey"))

q + scale_color_viridis(discrete=TRUE) 

# with confidence interval,success species with abundance > 10
p <- success_model_predictions |>
  ggplot(aes(temp, prediction, color = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  facet_wrap(~species) +
  theme_bw() +
  theme(strip.background = element_rect(fill="lightgrey"))

p + scale_color_viridis(discrete=TRUE) 


total_summary_table |>
  filter(success == 1) |> 
  filter(total_abundance > 10) |> 
  filter(model_type %in% c(3, 4)) |> 
  select(species, mean_se, max_se, min_se, total_abundance, n_lake, model_type) |> 
  arrange(model_type) |> 
  gt()


t <- success_model_predictions |>
  # filter(species == "Lota_lota") |>
  ggplot(aes(temp, prediction, colour = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  facet_wrap(~species, scales = "free") +
  theme_bw()
  # theme(strip.background = element_rect(fill="orange"))

#13 might need to go

# change colors
t + scale_color_viridis(discrete=TRUE) 
t + scale_color_viridis(discrete = TRUE, option = "turbo") 

#total plot

success_model_predictions |>
  # filter(species == "Lota_lota") |>
  ggplot(aes(temp, prediction, fill = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw()


#13 might need to go

success_model_predictions |>
  filter(species == "Coregonus_wartmanni") |>
  ggplot(aes(temp, prediction, colour = factor(species))) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  theme_bw()



#original data 
df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")
df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")
df_binomial_re <- readRDS("data_frame_models/df_binomial_re")
df_abundance_re <- readRDS("data_frame_models/df_abundance_re")

