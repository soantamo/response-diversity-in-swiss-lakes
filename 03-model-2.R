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
  filter(Species ==  "Coregonus_zugensis" )

sum(df_one$Abundance)
table(df_one$Abundance)

hist(df_one$Abundance)

M1 <- gam(Abundance ~ s(mean_last_7days, k = 7), family = ziP(),
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

#ZIP not working in those three species
#all others work with k = 3


species_list <- df_abundance_gam |>
  filter(!Species %in% c("Coregonus_profundus",
                         "Phoxinus_sp", "Coregonus_zugensis")) |>
  distinct(Species) |> 
  pull(Species)

#less problems with comparing
species_list <- sort(species_list)


gam_output <- list()
model_prediction <- list()
derivatives <- list()
viz <- list()

#make new loop 

for (i in species_list) {
  data <- df_abundance_gam |> 
    filter(Species == i)
  temp_gradient <- data.frame(mean_last_7days = seq(
    from = min(data$mean_last_7days, na.rm = TRUE),
    to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02))
  gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3), family = ziP())
  viz[[i]] <- getViz(gam_output[[i]]) #needs to be in mgcviz class
  print(plot(viz[[i]], allTerms = T), pages = 1)
  print(qq(viz[[i]], rep = 20, showReps = T, CI = "none", a.qqpoi = list("shape" = 19), a.replin = list("alpha" = 0.2)))
  tiff_filename <- paste("model_2/gam_check/gam_check_", i, ".tiff", sep = "")
  tiff(tiff_filename, width = 800, height = 600)
  print(check(viz[[i]],
              a.qq = list(method = "simul1"),
              a.respoi = list(size = 0.5),
              a.hist = list(bins = 10)))
  dev.off()
  # print(gam.check(gam_output[[i]]))
  # print(summary(gam_output[[i]]))
  print(tidy(gam_output[[i]]))
  # print(glance(gam_output[[i]]))
  model_prediction[[i]] <- predict.gam(gam_output[[i]], temp_gradient, type = "response", se.fit = TRUE)
  model_bind <- cbind(model_prediction[[i]], temp_gradient) |>
    mutate(species = factor(i))
  saveRDS(model_bind, paste0("model_2/predictions/predictions_",i,".rds"))
  # derivatives[[i]] <- derivatives(gam_output[[i]])
  # saveRDS(derivatives[[i]], paste0("model_2/derivatives/derivatives_", i, ".rds"))
}

#how can I check all the models easily??? some do not look good
# checking out all plots
#one big data frame and using facet_wrap



s1 <- readRDS("model_2/predictions/predictions_Barbatula_sp_Lineage_II.rds")
s2 <- readRDS("model_2/predictions/predictions_Coregonus_acrinasus.rds")
#s3 <- readRDS("model_2/predictions/predictions_Coregonus_profundus.rds")
#s4 <- readRDS("model_2/predictions/predictions_Coregonus_zugensis.rds")
s5 <- readRDS("model_2/predictions/predictions_Cottus_gobio_Profundal_Lucerne.rds")
s6 <- readRDS("model_2/predictions/predictions_Cottus_gobio_Profundal_Thun.rds")
s7 <- readRDS("model_2/predictions/predictions_Cottus_sp_Po_profundal.rds")
#s8 <- readRDS("model_2/predictions/predictions_Phoxinus_sp.rds")
s9 <- readRDS("model_2/predictions/predictions_Telestes_muticellus.rds")
s10 <- readRDS("model_2/predictions/predictions_Alosa_agone.rds")
s11 <- readRDS("model_2/predictions/predictions_Cottus_sp_Po.rds")

total_model_2_pred <- bind_rows(s1, s2, s5, s6, s7, s9, s10, s11) |> 
  rename(prediction = fit, temp = mean_last_7days)



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

#ZIP checking
#visual horror




total_model_2_pred |> 
  filter(species %in% c("Alosa_agone", "Coregonus_acrinasus", "Cottus_sp_Po")) |> 
  ggplot(aes(temp, prediction)) +
  geom_line(aes(colour = species)) 

#to do
#work on zip

#Coregonus_acrinasus  k = 3 works
#coregonus profundus not working with k = 40, exclude from analysis I guess
#phoxinus sp not working with k = 20

#all working with k = 3 separately, except the two # ones
"Alosa_agone"
"Barbatula_sp_Lineage_II"       
"Coregonus_acrinasus"
# "Coregonus_profundus"  #not working           
"Coregonus_zugensis"
"Cottus_gobio_Profundal_Lucerne"
"Cottus_gobio_Profundal_Thun"
"Cottus_sp_Po"                  
"Cottus_sp_Po_profundal"
# "Phoxinus_sp" #not working          
"Telestes_muticellus"

#testing loop without those two species. stops at c-zugensis, works when excluding
#c.profundus, phoxinus and c.zugensis
#c.zugensis works with k = 7 but looks bad -> excldue


#significant?

"Alosa_agone" #s, visually ok
# "Barbatula_sp_Lineage_II" #s, no good
"Coregonus_acrinasus" #s, visually ok
# "Cottus_gobio_Profundal_Lucerne" #s 0, no
# "Cottus_gobio_Profundal_Thun" #ns, no
"Cottus_sp_Po" #s, ok         
# "Cottus_sp_Po_profundal"# s, no
# "Telestes_muticellus" #ns, no


#final: "Alosa_agone", "Coregonus_acrinasus", "Cottus_sp_Po"
#double-check residuals etc


mean_se_model_2 <- total_model_2_pred |> 
  group_by(species) |> 
  mutate(mean_se = mean(se.fit)) |> 
  distinct(mean_se)


test2 <- df_abundance_gam |> 
  group_by(Species) |> 
  count(Abundance) |> 
  pivot_wider(names_from = Abundance, values_from = n) |> 
  mutate(sumrow = rowSums(pick(3:8), na.rm = T)) |> 
  select(-`2`, -`3`, -`4`, -`5`, -`7`, -`15`) |> 
  rename(species = Species, observation_0 = `0`, observation_1 = `1`, observations_abu = `sumrow`)
  # rename(species = Species, observation_0 = `0`, observation_1 = `1`)
