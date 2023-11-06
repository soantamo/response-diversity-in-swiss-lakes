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

#loading subset of df for model 1
#added two species, need to be checked! Coregonus and Salmo

df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")

### model 1: GAM with binomial distribution, test with one species first, then loop
#double-check if all data is 1 or 0

table(df_binomial_gam$Presence) 
str(df_binomial_gam)
head(df_binomial_gam)

df_binomial_gam |> 
  distinct(Species) |> 
  pull(Species)

#correct

#model testing for one species

df_one <- df_binomial_gam |> 
  filter(Species == "Coregonus_intermundia")

M1 <- gam(Abundance ~ s(mean_last_7days, k = 3), family = binomial,
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

#predictions
# 
# temp_gradient <- data.frame(mean_last_7days = seq(
#   from = min(df_one$mean_last_7days, na.rm = TRUE),
#   to = max(df_one$mean_last_7days, na.rm = TRUE), by = 0.02
# ))
# 
# predictions_one <- predict.gam(M1, temp_gradient, type = "response", se.fit = TRUE)$fit
# 
# pred_bind <- cbind(predictions_one, temp_gradient)
# 
# pred_bind |> 
#   ggplot(aes(mean_last_7days, predictions_one)) +
#   geom_line()


#####Loop Model 1 ######

species_list <- df_binomial_gam |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()
viz <- list()
#make new loop
  

for (i in species_list) {
  data <- df_binomial_gam |> 
    filter(Species == i)
  temp_gradient <- data.frame(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02))
  
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3), family = binomial)
  # viz[[i]] <- getViz(gam_output[[i]]) #needs to be in mgcviz class
  # print(plot(viz[[i]], allTerms = T), pages = 1)
  # print(qq(viz[[i]], rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2)))
  # tiff_filename <- paste("model_1/gam_check/gam_check_", i, ".tiff", sep = "")
  # tiff(tiff_filename, width = 800, height = 600)
  # print(check(viz[[i]],
  #             a.qq = list(method = "simul1"), 
  #             a.respoi = list(size = 0.5),
  #             a.hist = list(bins = 10)))
  # dev.off()
  # print(gam.check(gam_output[[i]]))
  # print(summary(gam_output[[i]]))
  # print(tidy(gam_output[[i]]))
  # print(glance(gam_output[[i]]))
  model_prediction[[i]] <- predict.gam(gam_output[[i]], temp_gradient, type = "response", se.fit = TRUE) #adding se, $fit 
  model_bind <- cbind(model_prediction[[i]], temp_gradient) |> 
    mutate(species = factor(i))
  saveRDS(model_bind, paste0("model_1/predictions/predictions_",i,".rds"))
  # derivatives[[i]] <- derivatives(gam_output[[i]])
  # saveRDS(derivatives[[i]], paste0("model_1/derivatives/derivatives_", i, ".rds"))
}

# Warnmeldungen:
#   1: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                  Iterationsgrenze erreicht ohne volle Konvergenz -- sorgfältig prüfen
#                2: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                               Anpassung beendet mit Schrittweitenfehler - Ergebnisse sorgfältig prüfen

#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap


s1 <- readRDS("model_1/predictions/predictions_Alosa_fallax.rds")
s2 <- readRDS("model_1/predictions/predictions_Chondrostoma_nasus.rds")
s3 <- readRDS("model_1/predictions/predictions_Chondrostoma_soetta.rds")
s4 <- readRDS("model_1/predictions/predictions_Coregonus_arenicolus.rds")
s5 <- readRDS("model_1/predictions/predictions_Coregonus_candidus.rds")
s6 <- readRDS("model_1/predictions/predictions_Coregonus_confusus.rds")
s7 <- readRDS("model_1/predictions/predictions_Coregonus_heglingus.rds")
s8 <- readRDS("model_1/predictions/predictions_Coregonus_helveticus.rds")
s9 <- readRDS("model_1/predictions/predictions_Coregonus_intermundia.rds")
s10 <- readRDS("model_1/predictions/predictions_Coregonus_litoralis.rds")
s11 <- readRDS("model_1/predictions/predictions_Coregonus_macrophthalmus.rds")
s12 <- readRDS("model_1/predictions/predictions_Coregonus_wartmanni.rds")
s13 <- readRDS("model_1/predictions/predictions_Coregonus_zuerichensis.rds")
s14 <- readRDS("model_1/predictions/predictions_Cottus_gobio_Profundal_Walen.rds")
s15 <- readRDS("model_1/predictions/predictions_Gasterosteus_gymnurus.rds")
s16 <- readRDS("model_1/predictions/predictions_Rutilus_aula.rds")
s17 <- readRDS("model_1/predictions/predictions_Salaria_fluviatilis_French.rds")
s18 <- readRDS("model_1/predictions/predictions_Salmo_labrax.rds")
s19 <- readRDS("model_1/predictions/predictions_Salmo_sp_Blackspot.rds")
s20 <- readRDS("model_1/predictions/predictions_Salmo_sp.rds")
s21 <- readRDS("model_1/predictions/predictions_Salvelinus_namaycush.rds")
s22 <- readRDS("model_1/predictions/predictions_Salvelinus_profundus.rds")
s23 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Limnetic_Thun.rds")
s24 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_dwarf_Thun.rds")
s25 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_dwarf_VWS.rds")
s26 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_extreme_Thun.rds")
s27 <- readRDS("model_1/predictions/predictions_Salvelinus_sp_Profundal_Walen_I.rds")
s28 <- readRDS("model_1/predictions/predictions_Coregonus_duplex.rds")
s29 <- readRDS("model_1/predictions/predictions_Salmo_marmoratus.rds")

total_model_1_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s28,  s7, s8, s9, s10, s11, s12,
                                s13, s14, s15, s16, s17, s18, s29, s20, s19, s21, s22, s23,
                                s24, s25, s26, s27) |> 
  rename(prediction = fit, temp = mean_last_7days)


total_model_1_pred |> 
  ggplot(aes(temp, prediction)) +
  geom_line() +
  facet_wrap(~species)

#some look very strange :I
#I can for sure exclude some of the species
#looking strange and not significant:

#1. exclude species that look too strange (Visually, tidy, glance, summary, and gam.check)
#species that stay
#species_list shows the numbers, check again now

# 25.10
#include 
# "Coregonus_confusus"
# "Coregonus_litoralis"
# "Coregonus_macrophthalmus"
# "Coregonus_wartmanni"
# "Coregonus_zuerichensis"
# "Salmo_sp_Blackspot"
# "Salvelinus_sp_Profundal_Walen_I"
# "Coregonus_candidus" #see below
# "Coregonus_helveticus" #see below

# double-check: 5, 8, 9, 10
#include those too
# "Coregonus_candidus" #temp 0
# ***"Coregonus_heglingus" #0.07
# "Coregonus_helveticus" #negative p-value?? problem with tidy, summary normal
# ***"Coregonus_intermundia" #0.06


total_model_1_pred |> 
  filter(species %in% c("Coregonus_confusus", "Coregonus_litoralis",
                        "Coregonus_macrophthalmus", "Coregonus_wartmanni",
                        "Coregonus_zuerichensis", "Salmo_sp_Blackspot",
                        "Salvelinus_sp_Profundal_Walen_I", "Coregonus_candidus",
                        "Coregonus_helveticus"
                        # "Coregonus_heglingus",
                        # "Coregonus_intermundia"
                        )) |> 
  ggplot(aes(temp, prediction)) +
  geom_line() +
  facet_wrap(~species)
  
# to do
#can i include almost significant temp species? no

#final list: "Coregonus_confusus", "Coregonus_litoralis",
# "Coregonus_macrophthalmus", "Coregonus_wartmanni",
# "Coregonus_zuerichensis", "Salmo_sp_Blackspot",
# "Salvelinus_sp_Profundal_Walen_I", "Coregonus_candidus",
# "Coregonus_helveticus"
#double check residuals

# prepare mean values of se.fit 

mean_se_model_1 <- total_model_1_pred |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  distinct(mean_se)


test1 <- df_binomial_gam |> 
  group_by(Species) |> 
  count(Abundance) |> 
  pivot_wider(names_from = Abundance, values_from = n) |> 
  rename(species = Species, observation_0 = `0`, observation_1 = `1`)

test_bind_1 <- merge(mean_se_model_1, test1)

library(writexl)

print(mean_se_model_1, n = 29)
write_xlsx(test_bind, "model_1/mean_se_model_1.xlsx")
