library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)
library(broom)


#this model for species with binomial data and random effects

df_binomial_re <- readRDS("data_frame_models/df_binomial_re")

table(df_binomial_re$Abundance) 
str(df_binomial_re)
head(df_binomial_re)

df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)


#model testing for one species
df_one <- df_binomial_re |> 
  filter(Species == "Squalius_squalus")

df_one$fLake <- as.factor(df_one$Lake)
str(df_one)

sum(df_one$Abundance)
table(df_one$Abundance)

hist(df_one$Abundance)

M1 <- gam(Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're'),
          family = binomial, data = df_one)

summary.gam(M1)
gam.check(M1)
tidy(M1)
glance(M1)

#MODEL VALIDATION# M1
#Synthesis of pdf, GAMM book and GAM book

plot(M1)

#1.verifiy homogeneity: rsd vs fitted
fv <- fitted(M1) ##predicted values
rsd <- resid(M1) ##residuals
plot(x = fv, y = rsd, xlab = "Fitted values", ylab = "Residuals")

#ok?

#2.verify model misfit (or independence): rsd vs each covariate in the model and not in the model
par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)    
E1 <- resid(M1)
F1 <- fitted(M1)
plot(x=F1, y=E1, xlab = "Fitted values", ylab ="Residuals")
abline(h=0, lty=2)

plot(x=df_one$mean_last_7days, y = E1, xlab = "Temp", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_one$Protocol, y = E1, xlab = "Protocol", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_one$Depth_sample, y = E1, xlab = "Depth", ylab = "Residuals")
abline(h=0, lty=2)

#not bad I guess

#3. verifiy independence when multiple measurements were taken over time: auto-correlation
#not the case here

#4.verifiy normality: histogram of residuals

par(mfrow = c(1,2), mar = c(5, 5, 2, 2))
hist(E1, main = "", xlab = "Residuals", ylim = c(0,20))
qqnorm(E1, main = "")
qqline(E1)

#looks better

#5. check for influental observations: cook distance values

# par(mfrow = c(1,1), mar = c( 5, 5, 2, 2))
# plot(cooks.distance(M3), xlim = c(0,10000), 
#      ylim = c(0,1), xlab = "Observations", 
#      ylab = " Cooks distance values")
# abline(h = 1, lwd = 2, lty = 2)

#not working!

#6.if repeated measurements taken from the same site, check for patterns
#not the case

# #overdispersion

E1 <- resid(M1, type = "pearson")
sum(E1^2)/M1$df.residual

#####Loop Model 3 ######

species_list <- df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()

#make new loop 

for (i in species_list) {
  data <- df_binomial_re |> 
    filter(Species == i)
  temp_gradient <- data.frame(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ))
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3), family = binomial)
  # sink("summary.txt", append = TRUE) #double-check if gams are well fitted
  # print(summary(gam_output[[i]]))
  # gam.check(gam_output[[i]])
  # sink()
  print(gam.check(gam_output[[i]]))
  print(summary(gam_output[[i]]))
  print(tidy(gam_output[[i]]))
  print(glance(gam_output[[i]]))
  model_prediction[[i]] <- predict.gam(gam_output[[i]], temp_gradient, type = "response", se.fit = TRUE)$fit
  model_bind <- cbind(model_prediction[[i]], temp_gradient) |>
    mutate(species = factor(i))
  saveRDS(model_bind, paste0("model_3/predictions/predictions_",i,".rds"))
  derivatives[[i]] <- derivatives(gam_output[[i]])
  saveRDS(derivatives[[i]], paste0("model_3/derivatives/derivatives_", i, ".rds"))
}

#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap


s1 <- readRDS("model_3/predictions/predictions_Ameiurus_melas.rds")
s2 <- readRDS("model_3/predictions/predictions_Barbus_barbus.rds")
s3 <- readRDS("model_3/predictions/predictions_Carassius_gibelio.rds")
s4 <- readRDS("model_3/predictions/predictions_Cobitis_bilineata.rds")
s5 <- readRDS("model_3/predictions/predictions_Coregonus_alpinus.rds")
s6 <- readRDS("model_3/predictions/predictions_Coregonus_brienzii.rds")
s7 <- readRDS("model_3/predictions/predictions_Coregonus_duplex.rds")
s8 <- readRDS("model_3/predictions/predictions_Coregonus_palaea.rds")
s9 <- readRDS("model_3/predictions/predictions_Cottus_gobio_Aare_littoral.rds")
s10 <- readRDS("model_3/predictions/predictions_Cottus_gobio_Rhine.rds")
s11 <- readRDS("model_3/predictions/predictions_Cottus_gobio_unknownlineage.rds")
s12 <- readRDS("model_3/predictions/predictions_Esox_cisalpinus.rds")
s13 <- readRDS("model_3/predictions/predictions_Esox_lucius.rds")
s14 <- readRDS("model_3/predictions/predictions_Micropterus_salmoides.rds")
s15 <- readRDS("model_3/predictions/predictions_Salmo_marmoratus.rds")
s16 <- readRDS("model_3/predictions/predictions_Salvelinus_umbla.rds")
s17 <- readRDS("model_3/predictions/predictions_Silurus_glanis.rds")
s18 <- readRDS("model_3/predictions/predictions_Squalius_cephalus.rds")
s19 <- readRDS("model_3/predictions/predictions_Squalius_squalus.rds")
s20 <- readRDS("model_3/predictions/predictions_Thymallus_thymallus.rds")


total_model_3_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
                                s13, s14, s15, s16, s17, s18, s19, s20) |> 
  rename(prediction = `model_prediction[[i]]`, temp = mean_last_7days)


total_model_3_pred |> 
  ggplot(aes(temp, prediction)) +
  geom_line() +
  facet_wrap(~species)

#checking those models
"Ameiurus_melas" #include
"Carassius_gibelio" #include
"Cobitis_bilineata" #include
"Coregonus_brienzii" #include
# Coregonus_palae (tidy has 0 as p-value)
"Cottus_gobio_Aare_littoral" #include
"Cottus_gobio_Rhine" #include
"Esox_lucius" #include
# Salvelinus_umbla" tidy has a 0
"Silurus_glanis" #include
# "Squalius_cephalus" tidy has 0
"Squalius_squalus" #include

c("Ameiurus_melas", "Carassius_gibelio", "Cobitis_bilineata", "Coregonus_brienzii",
  "Cottus_gobio_Aare_littoral", "Cottus_gobio_Rhine", "Esox_lucius", "Silurus_glanis",
  "Squalius_squalus")


total_model_3_pred |> 
  filter(species %in% c("Ameiurus_melas", "Carassius_gibelio", "Cobitis_bilineata", "Coregonus_brienzii",
                        "Cottus_gobio_Aare_littoral", "Cottus_gobio_Rhine", "Esox_lucius", "Silurus_glanis",
                        "Squalius_squalus")) |> 
  ggplot(aes(temp, prediction)) + 
  geom_line() +
  facet_wrap(~species)


#9 out of 20 :/