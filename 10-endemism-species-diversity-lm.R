library(tidyverse)
library(here)
library(readr)
library(viridis)
library(lattice)
library(broom)
library(readxl)
library(ggpubr)
library(ggfortify)
library(car)
library(writexl)
library(readxl)


# lake properties df
lake_info <- read_xlsx("lake_info.xlsx")

# species overview 


species_endemism <- read_excel("species_endemism_richness.xlsx") |> 
  select(-6) |> 
  rename(endemism = details)

species_endemism$endemism <- as.factor(species_endemism$endemism)

str(species_endemism)

# adding number of endemic and native etc species.
df_species_endemism <- species_endemism |> 
  select(-species, - num_species) |> 
  add_count(fLake, endemism) |> 
  rename(count = n) |> 
  distinct(fLake, count, sum_species, endemism)

str(df_species_endemism)

df_species_endemism$count <- as.numeric(df_species_endemism$count)

df_species_endemism_long <- df_species_endemism |> 
  pivot_wider(names_from = endemism, values_from = count) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |> 
  rename(Lake = fLake)

str(df_species_endemism_long)
  
# ready df !!!!

# resp diversity per lake df
resp_div_all <- readRDS("total_models/resp_div_all.rds") |> 
  select(temp:fLake)

# no info for poschiavo in phos max, needs to go
rdiv <- resp_div_all |>  
  select(temp, fLake, rdiv, sign) |>
  drop_na()

df_rdiv <- rdiv |> 
  group_by(fLake) |> 
  mutate(mean_rdiv = mean(rdiv)) |> 
  mutate(max_rdiv = max(rdiv)) |> 
  mutate(mean_sign = mean(sign)) |> 
  mutate(max_sign = max(sign)) |> 
  distinct(fLake, mean_rdiv, max_rdiv, mean_sign, max_sign) |> 
  rename(Lake = fLake)

df_endemism_rdiv <- merge(df_species_endemism_long, df_rdiv) 
str(df_endemism_rdiv)

# plot lm

ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "#FF3030", fill = "#CDC9C9") +
    labs(title = paste("R^2 = ",signif(summary(fit)$r.squared, 2),
                       # "adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       # "Intercept =",signif(fit$coef[[1]],5 ),
                       # " Slope =",signif(fit$coef[[2]], 5),
                       " p-value =",signif(summary(fit)$coef[2,4], 2))) +
    theme_bw()
}

# #############################################################################

srdiv <- lm(mean_rdiv ~ sum_species, data = species_richness)
summary(srdiv)
plot(srdiv)

par(mfrow=c(1,1))
plot(species_richness$sum_species, species_richness$mean_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(srdiv)

summary(srdiv)
anova(srdiv)


# analysis 
# dissimilarity 
ggplot(mapping = aes(x = sum_species, y = mean_rdiv), data = species_richness) +
  geom_point()
# suggests a very positive relationship

hist(species_richness$mean_rdiv)


# check assumptions

# overview

par(mfrow=c(2,2))
plot(srdiv, pch = 16, col = "blue")

autoplot(srdiv, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
par(mfrow=c(1,1))
plot(srdiv,1)

# ok
# equal variance: no patterns should be there

phos_res <- resid(srdiv)
phos_fit <- fitted(srdiv)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# 2
# histogram of residuals
hist(srdiv$residuals)

# qqplot
plot(srdiv, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(srdiv))
# normality is okay

# 3: Testing the Homoscedasticity Assumption
plot(srdiv, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(srdiv) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(srdiv))

# Print leverage for each observation
leverage

# look at highest values, 11 and 5: poschiavo and joux

par(mfrow=c(1,1))
barplot(hatvalues(srdiv), 
        col = "aquamarine3")


leveragePlots(srdiv)

# look for points with high leverage
sum(hatvalues(srdiv) > 2 * mean(hatvalues(srdiv)))
# 1 point of high leverage

sum(abs(rstandard(srdiv)) > 2)
# 0 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

srdiv_add = cooks.distance(srdiv)
sum(srdiv_add  > 3 / length(srdiv_add ))

large_srdiv_add  = srdiv_add  > 3 / length(srdiv_add )
srdiv_add[large_srdiv_add]

# lposchiavo  is different

coef(srdiv)

# what happens if we remove this point? removing both poschiavo and joux
srdiv_fix = lm(mean_rdiv ~ sum_species,
                data = species_richness,
                subset = srdiv_add  <= 3 / length(srdiv_add ))
coef(srdiv_fix)
# different

ggplotRegression(srdiv)
ggplotRegression(srdiv_fix)

# summary:
# 1: okay
# 2: okay
# 3: okay
# 4: okay

# number of species can explain 60 %, without joux and poschiavo still 36%
# is endemism etc relevant?

#################################################################################3
endemism_groupings <- df_endemism_rdiv |> 
  group_by(Lake) |> 
  mutate(non = sum(non_native + exotic))

native <- lm(mean_rdiv ~ native, data = endemism_groupings)
summary(native)
plot(native)
shapiro.test(resid(native)) #ok
lmtest::bptest(native) #ok
# 1: not fully horizontal, 2: ok, 3: ok, 4: some leverage

p_native <- ggplotRegression(native)


exotic <- lm(mean_rdiv ~ exotic, data = endemism_groupings)
summary(exotic)
plot(exotic)
shapiro.test(resid(exotic)) #ok
lmtest::bptest(exotic) #knapp, evtl homodeicasicisty 
# 1: ok, 2: ok, 3: ok, 4: some leverage

p_exotic <- ggplotRegression(exotic)


non_native <- lm(mean_rdiv ~ non_native, data = endemism_groupings)
summary(non_native)
plot(non_native)
shapiro.test(resid(non_native)) #ok
lmtest::bptest(non_native) #ok
# 1: ok, 2: ok, 3: ok, 4: some leverage

p_non_native <- ggplotRegression(non_native)


endemic <- lm(mean_rdiv ~ endemic, data = endemism_groupings)
summary(endemic)
plot(endemic)
shapiro.test(resid(endemic)) #ok
lmtest::bptest(endemic) #ok
# 1: not fully horizontal, 2: ok, 3: not horizontal, has a shape, residuals bigger
# with low endemism and high endemism
# , 4: some leverage

p_endemic <- ggplotRegression(endemic)

ggarrange(p_native, p_exotic, p_non_native, p_endemic, ncol = 2, nrow = 2)

################################################################################

# Regional endemism is often characterised by the proportion of endemics: 
#   the number of endemic taxa (E) divided by the total number 
# of taxa (S) [as a percentage: (E × 100 %)/S]. 
# Endemism in Vascular Plants pp 11–48

endemism_percents <- endemism_groupings|> 
  group_by(Lake) |> 
  mutate(perc_endemism = sum(endemic * 100 / sum_species)) |> 
  mutate(perc_exotic = sum(exotic * 100 / sum_species)) |> 
  mutate(perc_non_native = sum(non_native * 100 / sum_species)) |> 
  mutate(perc_native = sum(native * 100 / sum_species))

end_percent <- lm(mean_rdiv ~ perc_endemism, data = endemism_percents)
summary(end_percent)
plot(end_percent)
shapiro.test(resid(end_percent)) #ok
lmtest::bptest(end_percent) #ok
# 1: ok, 2: ok, 3: naja, 4: 5 , 8 and 13

ggplotRegression(end_percent)


end_exotic <- lm(mean_rdiv ~ perc_exotic, data = endemism_percents)
summary(end_exotic)
plot(end_exotic)
shapiro.test(resid(end_exotic)) #ok
lmtest::bptest(end_exotic) #ok
# 1: ok, 2: ok, 3: naja, 4: 5 , 8 and 13

ggplotRegression(end_exotic)


end_nonnative <- lm(mean_rdiv ~ perc_non_native, data = endemism_percents)
summary(end_nonnative)
plot(end_nonnative)
shapiro.test(resid(end_nonnative)) #ok
lmtest::bptest(end_nonnative) #ok
# 1: ok, 2: ok, 3: naja, 4: 5 , 8 and 13

ggplotRegression(end_nonnative)



lm_analysis <- function(y, x, df) {
  
  mod <- lm(y ~ x, data = df)
  print(summary(mod))
  print(shapiro.test(resid(mod)))
  print(lmtest::bptest(mod))
  par(mfrow=c(2,2))
  print(plot(mod))
  
  ggplot(mod$model, aes_string(x = names(mod$model)[2], y = names(mod$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "#FF3030", fill = "#CDC9C9") +
    labs(title = paste("R^2 = ",signif(summary(mod)$r.squared, 2),
                       # "adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       # "Intercept =",signif(fit$coef[[1]],5 ),
                       # " Slope =",signif(fit$coef[[2]], 5),
                       " p-value =",signif(summary(mod)$coef[2,4], 2))) +
    theme_bw()
}



lm_analysis(endemism_percents$mean_rdiv, endemism_percents$non_native, endemism_percents)
lm_analysis(endemism_percents$perc_native)
lm_analysis(endemism_percents$perc_exotic)
lm_analysis(endemism_percents$perc_endemism)



