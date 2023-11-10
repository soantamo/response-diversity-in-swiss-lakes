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
library(DHARMa)

#this model is for species with abundance data and random effects

df_abundance_re <- readRDS("data_frame_models/df_abundance_re")

table(df_abundance_re$Abundance) 
str(df_abundance_re)
head(df_abundance_re)

df_abundance_re |> 
  distinct(Species) |> 
  pull(Species)

###CONTINUE here

####################3#model testing for one species

#lota_lota: k = 3 not working
# "Lota_lota" not running with k = 3, also not k = 5
#k = 10 works
#k = 9 works
#k = 8 works
# k = 7 works
# k = 6 works
# k = 5 not working 
#testing backwards to 5

df_one <- df_abundance_re |>
  filter(Species == "Lota_lota")

df_one$fLake <- as.factor(df_one$Lake)


M1 <- gam(Abundance ~ s(mean_last_7days, k = 5) + s(fLake, bs = 're'),
          re.test = FALSE, #nothing is calculated for re, might make calc faster
          family = ziP(), data = df_one)


summary.gam(M1)
gam.check(M1)
tidy(M1)
glance(M1)

#####Loop Model 4 ######
#lota_lota needs to run separately

species_list <- df_abundance_re |> 
  # filter(!Species == "Lota_lota") |> #with k = 6
  filter(!Species %in% c("Alburnus_arborella",
           "Cyprinus_carpio", "Blicca_bjoerkna", "Lepomis_gibbosus", "Phoxinus_csikii",
           "Salmo_trutta", "Lota_lota")) |> 
  distinct(Species) |> 
  pull(Species)

species_list <- sort(species_list)

gam_output <- list()
model_prediction <- list()
derivatives <- list()
grid <- list()
pred_df <- list()
unique_lakes <- list()
viz <- list()


df_abundance_re$fLake <- as.factor(df_abundance_re$Lake)

str(df_abundance_re)
#make new loop 
###predict.gam needs something else

for (i in species_list) {
  data <- df_abundance_re |> 
    filter(Species == i)
  unique_lakes <- distinct(data, fLake)
  grid <- expand.grid(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
  ), fLake = unique_lakes$fLake)
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're'),
                         family = ziP())
  #lota_lota
  # gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 6) + s(fLake, bs = 're'),
  #                        family = ziP())
  # viz[[i]] <- getViz(gam_output[[i]]) #needs to be in mgcviz class
  # print(plot(viz[[i]], allTerms = T), pages = 1)
  # print(qq(viz[[i]], rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2)))
  # tiff_filename <- paste("model_4/gam_check/gam_check_", i, ".tiff", sep = "")
  # tiff(tiff_filename, width = 800, height = 600)
  # print(check(viz[[i]],
  #             a.qq = list(method = "simul1"),
  #             a.respoi = list(size = 0.5),
  #             a.hist = list(bins = 10)))
  # dev.off()
  # print(gam.check(gam_output[[i]]))
  # print(summary(gam_output[[i]]))
  print(tidy(gam_output[[i]]))
  # print(glance(gam_output[[i]]))
  # model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid, type = "response", se.fit = TRUE)
  # model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
  # pred_df <- model_bind |>
  #   group_by(mean_last_7days) |>
  #   mutate(fit = mean(fit)) |>
  #   mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) |>
  #   summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
  #   mutate(species = factor(i))
  # saveRDS(pred_df, paste0("model_4/predictions/predictions_",i,".rds"))
  # derivatives[[i]] <- derivatives(gam_output[[i]])
  # saveRDS(derivatives[[i]], paste0("model_4/derivatives/derivatives_", i, ".rds"))
}


#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap


s1 <- readRDS("model_4/predictions/predictions_Abramis_brama.rds")
s2 <- readRDS("model_4/predictions/predictions_Alburnus_alburnus.rds")
s3 <- readRDS("model_4/predictions/predictions_Alburnus_arborella.rds")
s4 <- readRDS("model_4/predictions/predictions_Barbatula_sp_Lineage_I.rds")
s5 <- readRDS("model_4/predictions/predictions_Blicca_bjoerkna.rds")
s6 <- readRDS("model_4/predictions/predictions_Coregonus_albellus.rds")
s7 <- readRDS("model_4/predictions/predictions_Coregonus_fatioi.rds")
s8 <- readRDS("model_4/predictions/predictions_Coregonus_sarnensis.rds")
s9 <- readRDS("model_4/predictions/predictions_Coregonus_sp.rds")
s10 <- readRDS("model_4/predictions/predictions_Cyprinus_carpio.rds")
s11 <- readRDS("model_4/predictions/predictions_Gasterosteus_aculeatus.rds")
s12 <- readRDS("model_4/predictions/predictions_Gobio_gobio.rds")
s13 <- readRDS("model_4/predictions/predictions_Gymnocephalus_cernua.rds")
s14 <- readRDS("model_4/predictions/predictions_Lepomis_gibbosus.rds")
s15 <- readRDS("model_4/predictions/predictions_Leuciscus_leuciscus.rds")
s16 <- readRDS("model_4/predictions/predictions_Lota_lota.rds")
s17 <- readRDS("model_4/predictions/predictions_Perca_fluviatilis.rds")
s18 <- readRDS("model_4/predictions/predictions_Phoxinus_csikii.rds")
s19 <- readRDS("model_4/predictions/predictions_Rutilus_rutilus.rds")
s20 <- readRDS("model_4/predictions/predictions_Salmo_trutta.rds")
s21 <- readRDS("model_4/predictions/predictions_Sander_lucioperca.rds")
s22 <- readRDS("model_4/predictions/predictions_Scardinius_erythrophthalmus.rds")
s23 <- readRDS("model_4/predictions/predictions_Scardinius_hesperidicus.rds")
s24 <- readRDS("model_4/predictions/predictions_Tinca_tinca.rds")


total_model_4_pred <- bind_rows(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10,  s11, s12,
                                s13, s14, s15, s16, s17, s18, s19, s20, s21, s22, s23,
                                s24) |> 
  rename(temp = mean_last_7days, prediction = fit)

#save all predictions as RDS
# saveRDS(total_model_4_pred, "total_models/total_model_4_pred")


# The reason we can't keep p-value < 2.2e-16 in the output is that that
# would mean we'd need a character output rather than a numeric one, which 
# prevents any further programming with it. For example, suppose your next 
# step were to filter for tests with p.value < .05, or to sort in ascending 
# order of p-values. Having a character output would break that functionality.
#https://github.com/tidymodels/broom/issues/227

#and 


# 2.2e-16 is the scientific notation of 0.00000000000000022, 
# meaning it is very close to zero. Your statistical 
# software probably uses this notation automatically for very small numbers. 
# You may be able to change this in the settings.
# The notation alone is no reason to be suspicious. The result itself might be, 
# but you will have to be the judge of that.
# < 2.2e-16 as the p value would indicate a significant result, 
# meaning that the actual p value is even smaller than 2.2e-16 
# (a typical threshold is 0.05, anything smaller counts as statistically significant).
# https://stats.stackexchange.com/questions/403343/what-is-the-interpretation-of-the-p-value-of-2-2e-16


#model selection
#include 100%
"Abramis_brama" 
"Alburnus_alburnus" #very low pvalues, high AIC
"Barbatula_sp_Lineage_I" #flake ns  
"Coregonus_albellus" #both 0s
"Coregonus_fatioi" #temp 0
"Coregonus_sarnensis" #both 0s
"Coregonus_sp" #both 0s, very high AIC
"Gasterosteus_aculeatus" #both 0s
"Gymnocephalus_cernua" #both 0s
"Leuciscus_leuciscus" #both 0s
"Perca_fluviatilis" #both 0s, incredibly high AIC
"Rutilus_rutilus" #both 0s
"Sander_lucioperca" 
"Scardinius_erythrophthalmus" #both 0s
"Scardinius_hesperidicus"
"Tinca_tinca" 
"Gobio_gobio" #include

#exclude
# "Alburnus_arborella" #flake ns, looks impossible
# "Cyprinus_carpio"#flake ns, looks impossible  
#"Blicca_bjoerkna" # 0.00000376, visually not good
# "Lepomis_gibbosus" # 0.00000342, visually not good
#"Phoxinus_csikii" #0.0362, visually not good
#"Salmo_trutta" #0s, visually not good
#lota_lota


#to decide
# 
total_model_4_pred |>
  ggplot(aes(temp, fit, color = species)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
  facet_wrap(~species)

# total_model_4_pred |> 
#   filter(species == "Gobio_gobio") |>
#   ggplot(aes(temp, fit, color = species)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
#   facet_wrap(~species)
# 
# total_model_4_pred |> 
#   filter(species == "Lepomis_gibbosus") |>
#   ggplot(aes(temp, fit, color = species)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
#   facet_wrap(~species)
# 
# total_model_4_pred |> 
#   filter(species == "Phoxinus_csikii") |>
#   ggplot(aes(temp, fit, color = species)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
#   facet_wrap(~species)
# 
# total_model_4_pred |> 
#   filter(species == "Salmo_trutta") |>
#   ggplot(aes(temp, fit, color = species)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5) +
#   facet_wrap(~species)
# 
# 
# total_model_4_pred |> 
#   filter(species == "Lota_lota") |>
#   ggplot(aes(temp, fit)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.5)
# # facet_wrap(~species)


#decide which species can go in: done!
#residual checking

mean_se_model_4 <- total_model_4_pred |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  mutate(max_se = max(se.fit)) |> 
  mutate(min_se = min(se.fit)) |> 
  distinct(mean_se, max_se, min_se)



test4 <- df_abundance_re |> 
  group_by(Species) |> 
  mutate(total_abundance = sum(Abundance), 
         observation_0 = sum(Abundance == 0)) |> 
  distinct(total_abundance, observation_0) |> 
  rename(species = Species)


n_lake4 <- df_abundance_re |>
  group_by(Species) |>
  summarize(n_lake = n_distinct(Lake)) |>
  rename(species = Species)

two_bind4 <- merge(test4, n_lake4, by.x = "species")

bind_4 <- merge(two_bind4, mean_se_model_4)

saveRDS(bind_4, "model_4/bind_4.rds")


##testing bs = "cs" -> no difference for predictions
##changing K -> no difference
##add dharma
###add random effects for method


df_one <- df_abundance_re |> 
  filter(Species == "Alburnus_alburnus")

df_one$fLake <- as.factor(df_one$Lake)
df_one$fProtocol <- as.factor(df_one$Protocol)

str(df_one)

gam_output <- gam(data = df_one, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                  + s(fProtocol, bs = 're') , family = ziP())



##dharma
#dispersion problems?
testDispersion(gam_output)
#simulate residuals
simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
residuals(simulationOutput)
residuals(simulationOutput, quantileFunction = qnorm, outlierValues = c(-7,7))
plot(simulationOutput)

#undertsand
plotResiduals(simulationOutput, form = fLake)




viz <- getViz(gam_output) #needs to be in mgcviz class
print(plot(viz, allTerms = T), pages = 1)
print(qq(viz, rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2)))
print(check(viz,
            a.qq = list(method = "simul1"),
            a.respoi = list(size = 0.5),
            a.hist = list(bins = 10)))

print(tidy(gam_output))


###add weights in predictions

#maybe do a grid instead of data frame -> needs to be grid for the plotting
#dont forget to add method

percentages <- table(df_one$fLake) / length(df_one$fLake) * 100


unique_method <- distinct(df_one, fProtocol)

#predictions weighted not working yet 

grid <- expand.grid(mean_last_7days = seq(
  from = min(df_one$mean_last_7days, na.rm = TRUE),
  to = max(df_one$mean_last_7days, na.rm = TRUE), length.out = 100), 
  fLake = sample(levels(df_one$fLake), size = 100, replace = TRUE, prob = percentages),
  fProtocol = unique_method$fProtocol)

grid <- expand.grid(mean_last_7days = seq(
  from = min(df_one$mean_last_7days, na.rm = TRUE),
  to = max(df_one$mean_last_7days, na.rm = TRUE), by = 0.02), 
  fLake = sample(levels(unique_lakes$fLake), replace = TRUE, prob = percentages),
  fProtocol = unique_method$fProtocol)


table(grid$fLake) / length(grid$fLake) *100
# 
# fProtocol = unique_method$fProtocol
##no weighting

unique_lakes <- distinct(df_one, fLake)

grid <- expand.grid(mean_last_7days = seq(
  from = min(df_one$mean_last_7days, na.rm = TRUE),
  to = max(df_one$mean_last_7days, na.rm = TRUE), by = 0.02
), fLake = unique_lakes$fLake)

#grid
model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
model_bind <- cbind(grid, as.data.frame(model_prediction))

# try to include fLake and fProtocol

pred_df <- model_bind |>
  group_by(mean_last_7days) |>
  mutate(fit = mean(fit)) |> 
  mutate(lower = fit - 2*se.fit, upper = fit + 2*se.fit) |>
  summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
            across(se.fit), across(fLake), across(fProtocol)) |> 
  rename(temp = mean_last_7days)

pred_df |> 
  ggplot(aes(temp, fit)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
  theme_bw() +
  facet_wrap(~fLake) +
  theme(strip.background = element_rect(fill="lightgrey"))

table(pred_df$fLake) / length(pred_df$fLake) * 100

#weight in GAM 
#https://stats.stackexchange.com/questions/598261/what-does-the-weights-do-in-mgcvgam-and-how-to-put-weights-in-gam
# https://stats.stackexchange.com/questions/598261/what-does-the-weights-do-in-mgcvgam-and-how-to-put-weights-in-gam
# 

# weights works in the same way for gam() and it does for glm() - GAMs fitted by 
# gam() and bam() are really just fancy GLMs, and when the models contain smooths 
# then they are fitted with a penalised version of the algorithm used to fit GLMs.
# 
# Because changing the weights changes the overall magnitude of the likelihood.
# You will likely want to actually used weights = my_weights / mean(my_weights),
# which is given on the help page for ?gam in the description of the weights argument.
# 
# I wouldn't recommend using bootstrapping with GAMs however; non-parametric 
# bootstrapping can lead to over smoothing because of the repeated observations of
# some samples in each bootstrap sample, and parametric bootstrapping of models is
# incredibly time consuming for GAMs and we can usually do just as well with posterior
# simulation.



  
