library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(lattice)
library(broom)

#####continue with df_one prediction
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
  filter(Species == "Esox_lucius")

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

##################################prediction#######################################3
#problem: we want one pseudo lake in our predictions
#lme4 or jtools with predict.merMod or predict_mer.Mod using re.form = NA

#calculating percentages of every lake in df
percentages <- table(df_one$fLake) / length(df_one$fLake)

#new_data for prediction with fLake added as random iteration of levels of fLake based
#on their percentages in original df
#which length is reasonable? 

new_data <- data.frame(mean_last_7days = seq(
  from = min(df_one$mean_last_7days, na.rm = TRUE),
  to = max(df_one$mean_last_7days, na.rm = TRUE), length.out = 850
), fLake = sample(levels(df_one$fLake), size = 850, replace = TRUE, prob = percentages))


table(new_data$fLake)

#double check percentages, not exactly the same, why?
table(new_data$fLake) / length(new_data$fLake) #prob_new_data 
table(df_one$fLake) / length(df_one$fLake) #prob_df_one

#prediction with new_data
#adding that marginal to zero
# https://stackoverflow.com/questions/67098467/on-the-predict-mermod-function-arguments

prediction <- prediction(M1, new_data, type = "response")

# https://github.com/strengejacke/ggeffects/issues/97
### manually  

## Create a grid of `x0` values at the average value of x1 and x2 at each level of the categorical variable.  

## Make predictions on this and average over the categorical variable `fac` to get the mean prediction and the associated interval   

grid <- expand.grid(mean_last_7days = seq(
  from = min(df_one$mean_last_7days, na.rm = TRUE),
  to = max(df_one$mean_last_7days, na.rm = TRUE), by = 0.02
), fLake = levels(df_one$fLake))

levels(grid$fLake)
table(grid$fLake)

x0_preds <- predict.gam(M1, newdata = grid, type = "response", se.fit = TRUE)
pred_df <- cbind(grid, as.data.frame(x0_preds))

pred_df %>%
  group_by(mean_last_7days) %>%
  mutate(species = factor("Squalius_squalus")) |> 
  mutate(fit = mean(fit)) |> 
  mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) %>%
  summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper)) %>% ## by grouping x0, average over the levels of fac
  ggplot(.,aes(mean_last_7days, fit)) + geom_line() + geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) 

########################################################
#new idea:
#package marginaleffects
# https://marginaleffects.com/
#not working, interesting package though

library(marginaleffects)

grid <- expand.grid(mean_last_7days = seq(
  from = min(df_one$mean_last_7days, na.rm = TRUE),
  to = max(df_one$mean_last_7days, na.rm = TRUE), by = 0.02
), fLake = levels(df_one$fLake))


prediction <- predictions(M1, by = "fLake")

plot_predictions(M1, grid)

plot_predictions(M1, condition = c("mean_last_7days"))
plot_predictions(M1, condition = "mean_last_7days") +
  facet_wrap(~fLake)

test <- slopes(M1)
  # select(estimate, mean_last_7days, fLake)

plot_predictions(prediction)

predictions_marginaleffects |> 
  ggplot(aes(mean_last_7days, estimate)) +
  geom_line()


##########################################################################3


#derivative calculation and plotting works

deriv <- derivatives(M1)

deriv |> 
  ggplot(aes(data, derivative)) +
  geom_line()


####LOOOP#######################################################################

#adding the manual margin effect calculation

species_list <- df_binomial_re |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()
grid <- list()
pred_df <- list()
unique_lakes <- list()


df_binomial_re$fLake <- as.factor(df_binomial_re$Lake)


#make new loop 
###predict.gam needs something else

for (i in species_list) {
  data <- df_binomial_re |> 
    filter(Species == i)
  unique_lakes <- distinct(data, fLake)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ), fLake = unique_lakes$fLake)
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're'),
                         family = binomial)
  print(gam.check(gam_output[[i]]))
  print(summary(gam_output[[i]]))
  print(tidy(gam_output[[i]]))
  print(glance(gam_output[[i]]))
  model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid, type = "response", se.fit = TRUE)
  model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
  pred_df <- model_bind |> 
    group_by(mean_last_7days) |> 
    mutate(fit = mean(fit)) |> 
    mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) |> 
    summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper)) |> 
    mutate(species = factor(i))
  saveRDS(pred_df, paste0("model_3/predictions/predictions_",i,".rds"))
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

s8 <- readRDS("model_3/predictions/predictions_Coregonus_palaea.rds")
s9 <- readRDS("model_3/predictions/predictions_Cottus_gobio_Aare_littoral.rds")
s10 <- readRDS("model_3/predictions/predictions_Cottus_gobio_Rhine.rds")
s11 <- readRDS("model_3/predictions/predictions_Cottus_gobio_unknownlineage.rds")
s12 <- readRDS("model_3/predictions/predictions_Esox_cisalpinus.rds")
s13 <- readRDS("model_3/predictions/predictions_Esox_lucius.rds")
s14 <- readRDS("model_3/predictions/predictions_Micropterus_salmoides.rds")

s16 <- readRDS("model_3/predictions/predictions_Salvelinus_umbla.rds")
s17 <- readRDS("model_3/predictions/predictions_Silurus_glanis.rds")
s18 <- readRDS("model_3/predictions/predictions_Squalius_cephalus.rds")
s19 <- readRDS("model_3/predictions/predictions_Squalius_squalus.rds")
s20 <- readRDS("model_3/predictions/predictions_Thymallus_thymallus.rds")


total_model_3_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s8, s9, s10, s11, s12,
                                s13, s14, s16, s17, s18, s19, s20) |> 
  rename(temp = mean_last_7days)



total_model_3_pred |> 
  ggplot(aes(temp, fit, fill = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)


total_model_3_pred |> 
  ggplot(aes(temp, fit, fill = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5)
  # facet_wrap(~species)

#not included 
# "Carassius_gibelio" #really ns
#coregonus_alpinus
#coregonus_palaea
#cottus_gobio_unknownlineage
#esox_cisalpinus
#"Micropterus_salmoides" 
#thymallus_thymallus
# Salvelinus_umbla" tidy has a 0 for flake and temp
# "Squalius_cephalus" tidy has 0 for flake and temp

#checking those models, include
"Ameiurus_melas"
"Cobitis_bilineata"
"Cottus_gobio_Aare_littoral"
"Esox_lucius" 

# unsure
"Barbus_barbus" #temp not significant, but 0.07
"Coregonus_brienzii" #fLake ns, is this relevant?
"Cottus_gobio_Rhine" #fLake ns
# "Silurus_glanis" #fLake ns, temp 0.06
# "Squalius_squalus" #temp 0.08


total_model_3_pred |> 
  filter(species %in% c("Ameiurus_melas", "Cobitis_bilineata","Cottus_gobio_Aare_littoral",
                        "Esox_lucius", "Barbus_barbus", "Coregonus_brienzii", "Cottus_gobio_Rhine", 
                        "Silurus_glanis", "Squalius_squalus")) |> 
  ggplot(aes(temp, fit, fill = species)) +
  geom_line() + 
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)



#9 out of 18
