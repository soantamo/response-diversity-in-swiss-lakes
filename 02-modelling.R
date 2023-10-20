library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)
library(broom)

# function from Ross et al. and species colors are now seperate
source(here("functions.R"))
source(here("species_colors.R"))

#loading subsets of dfs 
#rename, names too long

df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")

df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")

df_binomial_re <- readRDS("data_frame_models/df_binomial_re")

df_abundance_re <- readRDS("data_frame_models/df_abundance_re")


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


gam_output <- list()
model_prediction <- list()
derivatives <- list()

#make new loop 

for (i in species_list) {
  data <- df_binomial_gam |> 
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
  saveRDS(model_bind, paste0("model_1/predictions/predictions_",i,".rds"))
  derivatives[[i]] <- derivatives(gam_output[[i]])
  saveRDS(derivatives[[i]], paste0("model_1/derivatives/derivatives_", i, ".rds"))
}

# Warnmeldungen:
#   1: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                  Iterationsgrenze erreicht ohne volle Konvergenz -- sorgfältig prüfen
#                2: In newton(lsp = lsp, X = G$X, y = G$y, Eb = G$Eb, UrS = G$UrS, L = G$L,  :
#                               Anpassung beendet mit Schrittweitenfehler - Ergebnisse sorgfältig prüfen

#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap

df_binomial_gam |> 
  distinct(Species) |> 
  pull(Species)

s1 <- readRDS("model_1/predictions_Alosa_fallax.rds")
s2 <- readRDS("model_1/predictions_Chondrostoma_nasus.rds")
s3 <- readRDS("model_1/predictions_Chondrostoma_soetta.rds")
s4 <- readRDS("model_1/predictions_Coregonus_arenicolus.rds")
s5 <- readRDS("model_1/predictions_Coregonus_candidus.rds")
s6 <- readRDS("model_1/predictions_Coregonus_confusus.rds")
s7 <- readRDS("model_1/predictions_Coregonus_heglingus.rds")
s8 <- readRDS("model_1/predictions_Coregonus_helveticus.rds")
s9 <- readRDS("model_1/predictions_Coregonus_intermundia.rds")
s10 <- readRDS("model_1/predictions_Coregonus_litoralis.rds")
s11 <- readRDS("model_1/predictions_Coregonus_macrophthalmus.rds")
s12 <- readRDS("model_1/predictions_Coregonus_wartmanni.rds")
s13 <- readRDS("model_1/predictions_Coregonus_zuerichensis.rds")
s14 <- readRDS("model_1/predictions_Cottus_gobio_Profundal_Walen.rds")
s15 <- readRDS("model_1/predictions_Gasterosteus_gymnurus.rds")
s16 <- readRDS("model_1/predictions_Rutilus_aula.rds")
s17 <- readRDS("model_1/predictions_Salaria_fluviatilis_French.rds")
s18 <- readRDS("model_1/predictions_Salmo_labrax.rds")
s19 <- readRDS("model_1/predictions_Salmo_sp_Blackspot.rds")
s20 <- readRDS("model_1/predictions_Salmo_sp.rds")
s21 <- readRDS("model_1/predictions_Salvelinus_namaycush.rds")
s22 <- readRDS("model_1/predictions_Salvelinus_profundus.rds")
s23 <- readRDS("model_1/predictions_Salvelinus_sp_Limnetic_Thun.rds")
s24 <- readRDS("model_1/predictions_Salvelinus_sp_Profundal_dwarf_Thun.rds")
s25 <- readRDS("model_1/predictions_Salvelinus_sp_Profundal_dwarf_VWS.rds")
s26 <- readRDS("model_1/predictions_Salvelinus_sp_Profundal_extreme_Thun.rds")
s27 <- readRDS("model_1/predictions_Salvelinus_sp_Profundal_Walen_I.rds")

total_model_1_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12,
                                s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23,
                                s24, s25, s26, s27) |> 
  rename(prediction = `model_prediction[[i]]`, temp = mean_last_7days)


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


# "Coregonus_confusus" #included
# "Coregonus_macrophthalmus" #grenzwertig, included
# "Coregonus_wartmanni" #included
# "Coregonus_intermundia" #knapp ns, model looks okay
# "Coregonus_helveticus"#included
# "Salmo_sp_Blackspot" #included
# "Coregonus_litoralis" #included
# "Salvelinus_sp_Profundal_Walen_I" #included
# "Coregonus_zuerichensis" #included

#grenzfälle

# "Coregonus_heglingus" #not goood enough
# "Coregonus_candidus" #check separately, because tidy looks strange, AIC incredibly high
#included, looks too strange
#coregonus_intermundia separately modelled -> looks okay


#final list
c("Coregonus_confusus", "Coregonus_wartmanni", "Coregonus_macrophthalmus",
  "Coregonus_intermundia", "Coregonus_helveticus", "Salmo_sp_Blackspot", "Coregonus_litoralis", 
  "Salvelinus_sp_Profundal_Walen_I", "Coregonus_zuerichensis")
  
  
# 10 Species can be included

#2. calculate response diversity only for species that stay

response_diversity_list <- df_binomial_gam |> 
  filter(Species %in% c("Coregonus_confusus", "Coregonus_wartmanni", "Coregonus_macrophthalmus",
                        "Coregonus_intermundia", "Coregonus_helveticus", "Salmo_sp_Blackspot", 
                        "Coregonus_litoralis", "Salvelinus_sp_Profundal_Walen_I", "Coregonus_zuerichensis")) |> 
  distinct(Species) |> 
  pull(Species)


path_deriv <- "/home/sophie/Dokumente/Master Thesis R/response-diversity-in-swiss-lakes/model_1/derivatives"

# deriv_list <-list.files(path_deriv)

# deriv_lakes <- list()

deriv <- list()
# model_prediction <- list()
# derivatives <- list()

for (i in response_diversity_list){
  deriv[[i]] <- readRDS(paste0(path_deriv, "/derivatives_", i, ".rds"))
  df_deriv <- deriv |>
    mutate(Species = factor(i)) |> 
    rename(temp = data)
  df_resp_div <- df_deriv |> 
    select(Species, temp, derivative) |> 
    pivot_wider(
      names_from = Species,
      values_from = derivative)
  df_resp_div$rdiv <- apply(df_resp_div[,-1, drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-1, drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  saveRDS(df_resp_div, paste0("model_1/response_diversity/df_resp_div_", i, ".rds"))
  
}

deriv <- readRDS("model_1/derivatives/derivatives_Coregonus_confusus.rds")
df_deriv <- deriv |>
  mutate(Species = factor("Coregonus_confusus")) |> 
  rename(temp = data) 
df_resp_div <- df_deriv |> 
  select(Species, temp, derivative) |> 
  pivot_wider(
    names_from = Species,
    values_from = derivative)
df_resp_div$rdiv <- apply(df_resp_div, 2, resp_div)
#solve this problem, compare with ross code
df_resp_div$sign <- apply(df_resp_div[,-1, drop = FALSE], 1, resp_div, sign_sens = T)
df_resp_div$Med <- median(df_resp_div$rdiv)

