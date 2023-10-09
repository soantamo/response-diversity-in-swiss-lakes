#GAMM testing
library(tidyverse)
library(patchwork)
library(ggtext)
library(mgcv)
library(gratia)
#library(hypervolume)
library(here)
library(readr)
library(viridis)

#make gams per species across all lakes

df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

nets_df_final <- df_final_no_electro |> 
  pivot_longer(
    cols = c("Abundance", "Presence"), 
    names_to = "Parameter", 
    values_to = "value"
  )

abu_nets_df_final <- nets_df_final |> 
  filter(Parameter == "Abundance")

#testing gam and gamm for one species across all lakes
#gam for perch across all lakes

df_perch <- abu_nets_df_final |> 
  filter(Species == "Perca_fluviatilis")

b <- gam(data = df_perch, value ~ s(mean_last_7days, k = 3),
          family = negbin(1), method = "REML")
gam.check(b)
summary(b)

temp_gradient <- data.frame(mean_last_7days = seq(from = min(df_perch$mean_last_7days, na.rm = TRUE), to = max(df_perch$mean_last_7days, na.rm = TRUE), by = 0.02))

# model_prediction<- predict.gam(b, temp_gradient, type = "response", se.fit = TRUE)$fit

derivatives <- derivatives(b)


#gamm

m <- gamm(data = df_perch, value ~ s(mean_last_7days, k = 3),
         family = negbin(1), method = "REML")

#parameters not checked. which distribution, value for k and also random effects
plot(m$gam, pages = 1)
summary(m$lme)
summary(m$gam)
anova(m$gam)
gam.check(m$gam)

temp_gradient <- data.frame(mean_last_7days = seq(from = min(df_perch$mean_last_7days, na.rm = TRUE), to = max(df_perch$mean_last_7days, na.rm = TRUE), by = 0.02))
prediction <- predict.gam(m$gam, temp_gradient, type = "response", se.fit = TRUE)$fit
df_prediction_perch <- cbind(prediction, temp_gradient)

deriv <- derivatives(m)



#prepare loop
#species subset

df_small <- abu_nets_df_final |> 
  filter(Species %in% c("Perca_fluviatilis", "Rutilus_rutilus", "Esox_lucius"))


species_list <- df_small |>
  distinct(Species) |>
  pull(Species)


# make empty lists for the new data provided by the loop
model <- list()
prediction <- list()
deriv <- list()

#loop for all all species


for (i in species_list){
  presence_species <- df_small |> 
    filter(Species == i)
  temp_gradient <- data.frame(mean_last_7days = seq(from = min(presence_species$mean_last_7days, na.rm = TRUE), to = max(presence_species$mean_last_7days, na.rm = TRUE), by = 0.02))
  model[[i]] <- gamm(data = presence_species, value ~ s(mean_last_7days, k = 3),
                     family = negbin(1), method = "REML")
  prediction[[i]] <- predict.gam(model[[i]]$gam, temp_gradient, type = "response", se.fit = TRUE)$fit
  model_bind <- cbind(prediction[[i]], temp_gradient)
  saveRDS(model_bind, paste0("test_all/pred/predictions_",i,".rds"))
  # deriv[[i]] <- derivatives(model[[i]])
  # saveRDS(deriv[[i]], paste0("test_all/deriv/derivatives_",i,".rds"))
  
}

#looking at predictions
pred_rutilus <- readRDS("test_all/pred/predictions_Rutilus_rutilus.rds") |> 
  as_tibble() |> 
  rename(model_prediction = `prediction[[i]]`) |> 
  rename(temp = mean_last_7days) |> 
  mutate(Species = factor("Rutilus_rutilus"))

pred_perca <- readRDS("test_all/pred/predictions_Perca_fluviatilis.rds") |> 
  as_tibble() |> 
  rename(model_prediction = `prediction[[i]]`) |> 
  rename(temp = mean_last_7days) |> 
  mutate(Species = factor("Perca_fluviatilis"))

pred_esox <- readRDS("test_all/pred/predictions_Esox_lucius.rds") |> 
  as_tibble() |> 
  rename(model_prediction = `prediction[[i]]`) |> 
  rename(temp = mean_last_7days) |> 
  mutate(Species = factor("Esox_lucius"))

df_pred <- bind_rows(pred_rutilus, pred_perca, pred_esox)

df_pred |> 
  ggplot(mapping = aes(x = temp, y = model_prediction, color = Species)) +
  theme_classic(base_size = 14) +
  labs(x = "Temperature", y = "Abundance") +
  geom_hline(
    yintercept = 0,
    lty = 2) +
  geom_line()


#derivatives
#can also be put in the loop

deriv_rutilus <- readRDS("test_all/deriv/derivatives_Rutilus_rutilus.rds") |> 
  mutate(Species = factor("Rutilus_rutilus"))

deriv_perch <- readRDS("test_all/deriv/derivatives_Perca_fluviatilis.rds") |> 
  mutate(Species = factor("Perca_fluviatilis"))

deriv_esox <- readRDS("test_all/deriv/derivatives_Esox_lucius.rds") |> 
  mutate(Species = factor("Esox_lucius"))

df <- bind_rows(deriv_rutilus, deriv_perch, deriv_esox) |> 
  rename(temp = data)

df |> 
  ggplot(mapping = aes(x = temp, y = derivative, color = Species)) +
  theme_classic(base_size = 14) +
  labs(x = "Temperature", y = "Derivative") +
  geom_hline(
    yintercept = 0,
    lty = 2) +
  geom_line(show.legend = TRUE)


#to do
#check GAMM, data distribution, random effects?
#look at summaries
#repeat for all species
#find faster way to make prediction and derivatives, one data frame with Species
#column in loop?
#add response diversity calculation
