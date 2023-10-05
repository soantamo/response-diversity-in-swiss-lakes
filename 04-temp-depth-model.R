library(tidyverse)
library(patchwork)
library(ggtext)
library(mgcv)
library(gratia)
#library(hypervolume)
library(here)
library(readr)
library(lattice)


##very complex to do because see 
# https://stats.stackexchange.com/questions/495775/first-derivative-of-fitted-gam-changes-according-to-specified-model-distribution
#derivatives can only calculate derivative for one of the smooting functions

# function from Ross et al. and species colors are now seperate
source(here("functions.R"))
source(here("species_colors.R"))

# load final dataset
df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

nets_df_final <- df_final_no_electro |> 
  pivot_longer(
    cols = c("Abundance", "Presence"), 
    names_to = "Parameter", 
    values_to = "value"
  )

abu_nets_df_final <- nets_df_final |> 
  filter(Parameter == "Abundance")

#testing with subset

df_small <- abu_nets_df_final |>
  filter(Lake %in% c("Biel", "Brienz", "Walen"))

small_lakes <- df_small |>
  distinct(Lake) |>
  pull(Lake)


# make empty lists for the new data provided by the loop
gam_output <- list()
model_prediction <- list()
derivatives <- list()



#how can I store the summary outputs for all species?? sink() works with append
#loop for all lakes

for (i in small_lakes){
  presence_lake <- df_small |>
    filter(Lake == i)
  species_list <- presence_lake |>
    distinct(Species) |>
    pull(Species)
  temp_gradient <- data.frame(mean_last_7days = seq(from = min(presence_lake$mean_last_7days, na.rm = TRUE), to = max(presence_lake$mean_last_7days, na.rm = TRUE), length.out = 600))
  depth_gradient <- data.frame(Depth_sample = seq(from = min(presence_lake$Depth_sample, na.rm = TRUE), to = max(presence_lake$Depth_sample, na.rm = TRUE), length.out = 600))
  for (j in species_list){ #looping through all species occuring in this lake
    data <- presence_lake |> 
      filter(Species == j)
    gam_output[[j]] <- gam(data = data, value ~ s(mean_last_7days, k = 3, bs = "cs") + s(Depth_sample,  k = 3, bs = "cs"),  family = negbin(1), method = "REML") #added depth as explanatory variable
    # summary <- summary.gam(gam_output[[j]])
    # sink("summary_gam_test.txt", append = TRUE)
    # print.summary.gam(summary)
    # sink()
    newdata <- bind_cols(depth_gradient, temp_gradient)
    model_prediction[[j]] <- predict.gam(gam_output[[j]], newdata, type = "response", se.fit = TRUE)$fit
    model_bind <- cbind(model_prediction[[j]], newdata)
    saveRDS(model_bind, paste0("temp_and_depth/predictions/predictions_",i,"_",j,".rds"))
    derivatives[[j]] <- derivatives(gam_output[[j]], type = "central") #making derivatives and saving them in a RDS
    saveRDS(derivatives[[j]], paste0("temp_and_depth/derivatives/derivatives_",i,"_",j,".rds"))
  }
  
}

#one derivative per temp and one per depth
#why?
#solve this problem

# deriv <- readRDS("temp_and_depth/derivatives/derivatives_Biel_Abramis_brama.rds")


path_pred <- "/home/sophie/Dokumente/Master Thesis R/response-diversity-in-swiss-lakes/temp_and_depth/predictions"

predictions_list <-list.files(path_pred)

predictions_lakes <- list()

for (i in small_lakes){
  predictions_lakes <- predictions_list |> 
    as_tibble() |> 
    filter(str_detect(value, i)) |> 
    pull(value)
  pred <- list()
  lake_data <- df_small |>
    filter(Lake == i)
  temp_gradient <- data.frame(mean_last_7days = seq(from = min(presence_lake$mean_last_7days, na.rm = TRUE), to = max(presence_lake$mean_last_7days, na.rm = TRUE), length.out = 600))
  depth_gradient <- data.frame(Depth_sample = seq(from = min(presence_lake$Depth_sample, na.rm = TRUE), to = max(presence_lake$Depth_sample, na.rm = TRUE), length.out = 600))
  for (j in predictions_lakes){
    pred[[j]] <- readRDS(paste0(path_pred,"/", j))
    df_pred <- pred |>
      bind_rows(.id ="id") |>
      rename(Species = id) |> 
      rename(model_prediction = `model_prediction[[j]]`) |> #does not work
      rename(temp = mean_last_7days) |> 
      rename(depth = Depth_sample) |> 
      mutate(Species = sub("^[^_]*_", "", Species)) |> 
      mutate(Species = sub("^[^_]*_", "", Species)) |>
      mutate(Species = sub("\\.rds$", "", Species))
    saveRDS(df_pred, paste0("temp_and_depth/df_pred/df_pred_", i, ".rds"))
    
  }
}

df_biel <- readRDS("temp_and_depth/df_pred/df_pred_Biel.rds")
df_brienz <- readRDS("temp_and_depth/df_pred/df_pred_Brienz.rds")
df_walen <- readRDS("temp_and_depth/df_pred/df_pred_Walen.rds")

# df <- bind_rows(df_biel, df_brienz, df_walen)
# 
# fig_a <- ggplot() +
#   theme_classic(base_size = 14) +
#   labs(x = "Temperature", y = "Abundance", tag = "a)") +
#   geom_hline(
#     yintercept = 0,
#     lty = 2) +
#   geom_line(data = df, mapping = aes(x = depth, y = model_prediction, color = Species)) +
#   scale_colour_manual(values = lakeColors, guide = NULL)
# 
# fig_a
# 
# ggplot() +
#   theme_classic(base_size = 14) +
#   labs(x = "Temperature", y = "Abundance", tag = "a)") +
#   geom_tile(data = df, mapping = aes(x = depth, y = temp, fill = model_prediction)) +
#   scale_colour_manual(values = lakeColors, guide = NULL)



# derivatives
path_deriv <- "/home/sophie/Dokumente/Master Thesis R/response-diversity-in-swiss-lakes/temp_and_depth/derivatives"

deriv_list <-list.files(path_deriv)

deriv_lakes <- list()

for (i in small_lakes){
  deriv_lakes <- deriv_list |> 
    as_tibble() |> 
    filter(str_detect(value, i)) |> 
    pull(value)
  deriv <- list()
  for (j in deriv_lakes){
    # print(predictions_test)
    deriv[[j]] <- readRDS(paste0(path_deriv,"/", j))
    df_deriv <- deriv |>
      bind_rows(.id ="id") |>
      rename(Species = id) |> 
      rename(temp = data) |> 
      mutate(Species = sub("^[^_]*_", "", Species)) |> 
      mutate(Species = sub("^[^_]*_", "", Species)) |>
      mutate(Species = sub("\\.rds$", "", Species))
    saveRDS(df_deriv, paste0("test_df_deriv/df_deriv_", i, ".rds"))
  }
}

# test <- readRDS("df_derivatives/df_deriv_Biel.rds")
#response diversity

path_deriv <- "/home/sophie/Dokumente/Master Thesis R/response-diversity-in-swiss-lakes/temp_and_depth/derivatives"

deriv_list <-list.files(path_deriv)

deriv_lakes <- list()

for (i in small_lakes){
  deriv_lakes <- deriv_list |> 
    as_tibble() |> 
    filter(str_detect(value, i)) |> 
    pull(value)
  deriv <- list()
  for (j in deriv_lakes){
    # print(predictions_test)
    deriv[[j]] <- readRDS(paste0(path_deriv,"/", j))
    df_deriv <- deriv |>
      bind_rows(.id ="id") |>
      rename(Species = id) |> 
      rename(temp = data)
    df_resp_div <- df_deriv |> 
      select(Species, temp, derivative) |> 
      pivot_wider(
        names_from = Species,
        values_from = derivative) |> 
      rename_all(~stringr::str_replace(.,"^derivatives_","")) |> 
      rename_all(~stringr::str_replace(.,"^[^_]*_", "")) |>
      rename_with(~ sub(".rds$", "", .x), everything())
    #test <- df_resp_div |>
    #select (-temp) |>
    #mutate_all(rdiv = resp_div(1, sign_sens = F))
    df_resp_div$rdiv <- apply(df_resp_div[,-1, drop = FALSE], 1, resp_div, sign_sens = F)
    df_resp_div$sign <- apply(df_resp_div[,-1, drop = FALSE], 1, resp_div, sign_sens = T)
    df_resp_div$Med <- median(df_resp_div$rdiv)
    saveRDS(df_resp_div, paste0("test_df_resp/df_resp_div_", i, ".rds"))
  }
}
