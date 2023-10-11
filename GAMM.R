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
library(gamm4)

#make gams per species across all lakes

#overview, follotwing zuur GAMMs

df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

names(df_final_no_electro)
str(df_final_no_electro)

#only species present in lake get lakepresence 1
min(df_final_no_electro$LakePresence)

#look for outliers

par(mar=c(5,5,2,2), cex.lab = 1.5)
plot(y = 1:nrow(df_final_no_electro), 
     x = df_final_no_electro$Abundance, 
     xlab = "Values of the data",
     ylab = "Order of the data",
     pch = 16, 
     cex = 0.7)
#joux has extremely high perch abundances, strange. joux generally looks strange
#joux morat and neuchatel, check fishing date etc. 

#differences in sampling effort?

df_final_no_electro$Lake <- as.factor(df_final_no_electro$Lake)


df_final_no_electro |> 
  filter(Lake %in% c("Biel", "Brienz", "Walen")) |> 
  ggplot(aes(x = mean_last_7days, y = Abundance)) +
  geom_point() +
  facet_wrap(~Lake)

#joux, morat and neuchatel auffällig. poschiavo and zug also strange 


#make gam for one species, with poisson, see Zuur GAMMS page 155

df_perch <-  df_final_no_electro |> 
  filter(Species == "Perca_fluviatilis")

M1 <- gam(data = df_perch, Abundance ~ s(mean_last_7days),
         family = poisson)

summary(M1)

E1 <- resid(M1, type = "pearson")
sum(E1^2)/M1$df.residual #overdispersion with a factor 3.331914
#page 157 on top 

gam.check(M1)

#checking what happens if Lake is included as a factor
#do not run!!!! takes very long

# M2 <- gam(data = df_perch, Abundance ~ s(mean_last_7days, by = factor(Lake)),
#           family = poisson)
# 
# summary(M2)
# 
# E2 <- resid(M2, type = "pearson")
# sum(E1^2)/M2$df.residual
# 
# AIC(M1, M2)

#negbin testing

M3 <- gam(data = df_perch, Abundance ~ s(mean_last_7days),
          family = negbin(1))

summary(M3)

E3 <- resid(M3, type = "pearson")
sum(E3^2)/M1$df.residual #overdispersion with a factor 2.032133


AIC(M1, M2, M3)

#gamm: observations are not independent

df_perch$fLake <- as.factor(df_perch$Lake)

M4 <- gamm4(data = df_perch, Abundance ~ s(mean_last_7days),
           random =~(1 | fLake), family = poisson)

#overdispersion calculation


#how many percent of abundance data are 0?

table(df_perch$Abundance)

#is it overdispersion due to the many zeros? or is it because there is such a big
#variation in the data ? (page 160), biological choice





#this is not necessary, just take abundance column
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
  summary(model[[i]])
  saveRDS(model[[i]], paste0("test_all/model_",i,".rds"))
  # prediction[[i]] <- predict.gam(model[[i]]$gam, temp_gradient, type = "response", se.fit = TRUE)
  # model_bind <- cbind(prediction[[i]], temp_gradient)
  # saveRDS(model_bind, paste0("test_all/pred/predictions_",i,".rds"))
  # deriv[[i]] <- derivatives(model[[i]])
  # saveRDS(deriv[[i]], paste0("test_all/deriv/derivatives_",i,".rds"))
  
}

df <- readRDS("test_all/model_Esox_lucius.rds")
df$gam
summary_df <- summary(df$gam)
summary_df$

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
