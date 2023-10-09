library(tidyverse)
library(patchwork)
library(ggtext)
library(mgcv)
library(gratia)
library(gamm4)
#library(hypervolume)
library(here)
library(readr)
# function from Ross et al. and species colors are now seperate

# load final dataset
df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

##every species across all lakes

# species_list <- df_final_no_electro|>
#   distinct(Species) |>
#   pull(Species)


nets_df_final <- df_final_no_electro |> 
  pivot_longer(
    cols = c("Abundance", "Presence"), 
    names_to = "Parameter", 
    values_to = "value"
  )

abu_nets_df_final <- nets_df_final |> 
  filter(Parameter == "Abundance")


##testing GAMM

df_biel_perch <- abu_nets_df_final |> 
  filter(Lake  == "Biel") |> 
  filter(Species == "Perca_fluviatilis")

b <- gamm(data = df_biel_perch, value ~ s(mean_last_7days, k = 3, bs = "cs"),
                   family = negbin(1), method = "REML")

plot(b$gam, pages = 1)
summary(b$lme)
summary(b$gam)
anova(b$gam)
gam.check(b$gam)

#first use subset of lakes
df_small <- abu_nets_df_final |> 
  filter(Lake %in% c("Biel", "Brienz", "Walen"))

species_small <- df_small |> 
  distinct(Species) |> 
  pull(Species)


gam_output <- list()
model_prediction <- list()
derivatives <- list()

#make new loop 


for (i in species_small) {
  data <- df_small |> 
    filter(Species == i)
  temp_gradient <- data.frame(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ))
  gam_output[[i]] <- gamm(data = data, value ~ s(mean_last_7days, k = 3, bs = "re"), family = negbin(1), method = "REML")
  model_prediction[[i]] <- predict.gamm(gam_output[[i]], temp_gradient, type = "response", se.fit = TRUE)$fit
  model_bind <- cbind(model_prediction[[i]], temp_gradient)
  saveRDS(model_bind, paste0("test_all/predictions_",i,".rds"))
  # derivatives[[i]] <- derivatives(gam_output[[i]])
  # saveRDS(derivatives[[i]], paste0("test_all/derivatives_", i, ".rds"))
}

#we need response diversity calculation and GAMM

path_deriv <- "/home/sophie/Dokumente/Master Thesis R/response-diversity-in-swiss-lakes/Subset/derivatives_abundance"

deriv_list <-list.files(path_deriv)

deriv_lakes <- list()

for (i in sub_lakes){
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
    saveRDS(df_resp_div, paste0("Subset/df_response_diversity/df_resp_div_", i, ".rds"))
  }
}

##looking at plots

df_abramis_brama_pred <- readRDS("test_all/predictions_Abramis_brama.rds")

df_abramis_brama_pred <- df_abramis_brama_pred |> 
  as_tibble() |> 
  rename(model_prediction = `model_prediction[[i]]`) |> 
  rename(temp = mean_last_7days)

df_abramis_brama_derivative <- readRDS("test_all/derivatives_Abramis_brama.rds")  

df_abramis_brama_derivative <- df_abramis_brama_derivative |> 
  rename(temp = data)

fig_a <- ggplot() +
  theme_classic(base_size = 14) +
  labs(x = "Temperature", y = "Abundance") +
  geom_hline(
    yintercept = 0,
    lty = 2) +
  geom_line(data = df_abramis_brama_pred, mapping = aes(x = temp, y = model_prediction)) +
  scale_colour_manual(values = lakeColors, guide = NULL)
# lims(y = c(ymin_p, ymax_p))

fig_a

fig_b <- ggplot(data = df_abramis_brama_derivative, mapping = aes(x = temp, y = derivative)) +
  theme_classic(base_size = 14) +
  labs(x = "Temperature", y = "Derivative") +
  geom_hline(
    yintercept = 0,
    lty = 2) +
  geom_line(show.legend = FALSE) +
  scale_colour_manual(values = lakeColors, guide = NULL)
# lims(y = c(dmin, dmax))


fig_b

