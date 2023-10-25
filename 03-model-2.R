library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)
library(broom)

#second model for species with abundance data which only occur in one lake
#re-do with two additional species

#read df
df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")

table(df_abundance_gam$Abundance) 
str(df_abundance_gam)
head(df_abundance_gam)

#total of nine species
df_abundance_gam |> 
  distinct(Species) |> 
  pull(Species)


#model testing for one species


df_one <- df_abundance_gam |> 
  filter(Species == "Cottus_gobio_Profundal_Lucerne")

sum(df_one$Abundance)
table(df_one$Abundance)

hist(df_one$Abundance)

M1 <- gam(Abundance ~ s(mean_last_7days, k = 3), family = negbin(1),
          data = df_one)

summary.gam(M1)
gam.check(M1)
tidy(M1)

#MODEL VALIDATION# M1
#Synthesis of pdf, GAMM book and GAM book

plot(M1)
tidy(M1)
glance(M1)

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

#below 1

#general
gam.check(M1)
summary(M1)


#####Loop Model 2 ######

species_list <- df_abundance_gam |>
  distinct(Species) |> 
  pull(Species)

#less problems with comparing
species_list <- sort(species_list)


gam_output <- list()
model_prediction <- list()
derivatives <- list()

#make new loop 

for (i in species_list) {
  data <- df_abundance_gam |> 
    filter(Species == i)
  temp_gradient <- data.frame(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ))
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3), family = negbin(1))
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
  saveRDS(model_bind, paste0("model_2/predictions/predictions_",i,".rds"))
  derivatives[[i]] <- derivatives(gam_output[[i]])
  saveRDS(derivatives[[i]], paste0("model_2/derivatives/derivatives_", i, ".rds"))
}

#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap



s1 <- readRDS("model_2/predictions/predictions_Barbatula_sp_Lineage_II.rds")
s2 <- readRDS("model_2/predictions/predictions_Coregonus_acrinasus.rds")
s3 <- readRDS("model_2/predictions/predictions_Coregonus_profundus.rds")
s4 <- readRDS("model_2/predictions/predictions_Coregonus_zugensis.rds")
s5 <- readRDS("model_2/predictions/predictions_Cottus_gobio_Profundal_Lucerne.rds")
s6 <- readRDS("model_2/predictions/predictions_Cottus_gobio_Profundal_Thun.rds")
s7 <- readRDS("model_2/predictions/predictions_Cottus_sp_Po_profundal.rds")
s8 <- readRDS("model_2/predictions/predictions_Phoxinus_sp.rds")
s9 <- readRDS("model_2/predictions/predictions_Telestes_muticellus.rds")
s10 <- readRDS("model_2/predictions/predictions_Alosa_agone.rds")
s11 <- readRDS("model_2/predictions/predictions_Cottus_sp_Po.rds")

total_model_2_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11) |> 
  rename(prediction = `model_prediction[[i]]`, temp = mean_last_7days)



total_model_2_pred |> 
  ggplot(aes(temp, prediction)) +
  geom_line() +
  facet_wrap(~species)


#negbin checking 25.10.
#include
"Alosa_agone" 
"Coregonus_acrinasus" 
"Cottus_sp_Po"
"Coregonus_profundus" #tidy 0
"Coregonus_zugensis" #tidy 0

#exclude
# "Cottus_gobio_Profundal_Lucerne" definitily exclude looks crazy
# "Cottus_sp_Po_profundal" definitely exclude looks crazy
# "Phoxinus_sp" definitely exclude looks crazy



total_model_2_pred |> 
  filter(species %in% c("Alosa_agone", "Coregonus_acrinasus", "Cottus_sp_Po",
                        "Coregonus_profundus", "Coregonus_zugensis")) |> 
  ggplot(aes(temp, prediction)) +
  geom_line(aes(colour = species)) 


#done!!!

