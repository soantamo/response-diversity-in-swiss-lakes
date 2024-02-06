library(tidyverse)
library(here)
library(readr)
library(viridis)
library(lattice)
library(readxl)
library(ggpubr)
library(ggfortify)
library(car)

# lake properties df
lake_info <- read_xlsx("lake_info.xlsx")

eutroph <- lake_info |> 
  select(Lake, Phos_max, Max_depth)

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

eutroph_dissimilarity <- merge(eutroph, df_rdiv)

eutroph_dissimilarity$Phos_max <- as.numeric(eutroph_dissimilarity$Phos_max)
eutroph_dissimilarity$fPhos <- as.factor(eutroph_dissimilarity$Phos_max)

str(eutroph_dissimilarity)

eutroph_dissimilarity |> 
  distinct(Lake, Max_depth)


# function for regression plot
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


#################### regression: eutrophication and mean_rdiv

drdiv1 <- lm(mean_rdiv ~ Max_depth, data = eutroph_dissimilarity)
summary(drdiv1)
plot(drdiv1)

plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$mean_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(drdiv1)

summary(drdiv1)
anova(drdiv1)

# dissimilarity 
ggplot(mapping = aes(x = Max_depth, y = mean_rdiv), data = eutroph_dissimilarity) +
  geom_point()
#might be positive
# check assumptions

# overview

par(mfrow=c(2,2))
plot(drdiv1, pch = 16, col = "blue")

autoplot(drdiv1, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
plot(drdiv1,1)

# equal variance: no patterns should be there

phos_res <- resid(drdiv1)
phos_fit <- fitted(drdiv1)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# okay, not great

# 2
# histogram of residuals
hist(drdiv1$residuals)

# qqplot
plot(drdiv1, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(drdiv1))
# normality is okay

# 3: Testing the Homoscedasticity Assumption
plot(drdiv1, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(drdiv1) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(drdiv1))

# Print leverage for each observation
leverage

# look at highest values, 8, maggiore

par(mfrow=c(2,1))
barplot(hatvalues(drdiv1), 
        col = "aquamarine3")


leveragePlots(drdiv1)

# look for points with high leverage
sum(hatvalues(drdiv1) > 2 * mean(hatvalues(drdiv1)))
# 1 point of high leverage

sum(abs(rstandard(drdiv1)) > 2)
# also 0 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

drdv1_add = cooks.distance(drdiv1)
sum(drdv1_add > 4 / length(drdv1_add))

large_drdv1_add = drdv1_add > 4 / length(drdv1_add)
drdv1_add[large_drdv1_add]

# joux is very different

coef(drdiv1)

# what happens if we remove this point?
drdiv1_fix = lm(mean_rdiv ~ Max_depth,
                data = eutroph_dissimilarity,
                subset = drdv1_add <= 4 / length(drdv1_add))
coef(drdiv1_fix)
# no real difference, let joux inside the model 

# summary:
# 1: semi okay
# 2: okay
# 3: okay
# 4: okay

# plot lm
ggplotRegression(drdiv1)

###########################################################################
drdiv2 <- lm(max_rdiv ~ Max_depth, data = eutroph_dissimilarity)
summary(drdiv2)
plot(drdiv2)

plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$max_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(drdiv2)

summary(drdiv2)
anova(drdiv2)

# dissimilarity 
ggplot(mapping = aes(x = Max_depth, y = max_rdiv), data = eutroph_dissimilarity) +
  geom_point()
#might be positive
# check assumptions

# overview

par(mfrow=c(2,2))
plot(drdiv2, pch = 16, col = "blue")

autoplot(drdiv2, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
par(mfrow=c(1,1))
plot(drdiv2,1)

# equal variance: no patterns should be there

phos_res <- resid(drdiv2)
phos_fit <- fitted(drdiv2)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# no patterns, but not very horizontal 

# 2
# histogram of residuals
hist(drdiv2$residuals)

# qqplot
plot(drdiv2, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(drdiv2))
# normality is okay

# 3: Testing the Homoscedasticity Assumption
plot(drdiv2, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(drdiv2) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(drdiv2))

# Print leverage for each observation
leverage

# look at highest values, 8, maggiore

par(mfrow=c(2,1))
barplot(hatvalues(drdiv2), 
        col = "aquamarine3")


leveragePlots(drdiv2)

# look for points with high leverage
sum(hatvalues(drdiv1) > 2 * mean(hatvalues(drdiv1)))
# 1 point of high leverage

sum(abs(rstandard(drdiv2)) > 2)
# also 0 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

drdv2_add = cooks.distance(drdiv2)
sum(drdv2_add > 4 / length(drdv2_add))

large_drdv2_add = drdv2_add > 4 / length(drdv2_add)
drdv2_add[large_drdv2_add]

# lugano is very different

coef(drdiv2)

# what happens if we remove this point?
drdiv2_fix = lm(mean_rdiv ~ Max_depth,
                data = eutroph_dissimilarity,
                subset = drdv2_add <= 4 / length(drdv2_add))
coef(drdiv2_fix)

# summary:
# 1: semi okay
# 2: okay
# 3: okay
# 4: okay

# plot lm
ggplotRegression(drdiv2)

#################################################################################
dsign1 <- lm(mean_sign ~ Max_depth, data = eutroph_dissimilarity)
summary(dsign1)
plot(dsign1)

plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$mean_sign, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(dsign1)

summary(dsign1)
anova(dsign1)

# dissimilarity 
ggplot(mapping = aes(x = Max_depth, y = mean_sign), data = eutroph_dissimilarity) +
  geom_point()
#might be positive
# check assumptions

# overview

par(mfrow=c(2,2))
plot(dsign1, pch = 16, col = "blue")

autoplot(dsign1, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
par(mfrow=c(1,1))
plot(dsign1,1)

# not very horizontal

# equal variance: no patterns should be there

phos_res <- resid(dsign1)
phos_fit <- fitted(dsign1)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# no patterns, but not very horizontal 

# 2
# histogram of residuals
hist(dsign1$residuals)

# qqplot
plot(dsign1, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(dsign1))
# normality is okay, but looks baaad

# 3: Testing the Homoscedasticity Assumption
plot(dsign1, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(dsign1) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(dsign1))

# Print leverage for each observation
leverage

# look at highest values, 8, maggiore

par(mfrow=c(2,1))
barplot(hatvalues(dsign1), 
        col = "aquamarine3")


leveragePlots(dsign1)

# look for points with high leverage
sum(hatvalues(dsign1) > 2 * mean(hatvalues(dsign1)))
# 1 point of high leverage

sum(abs(rstandard(dsign1)) > 2)
# also 1 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

dsign1_add = cooks.distance(dsign1)
sum(dsign1_add > 4 / length(dsign1_add))

large_dsign1_add = dsign1_add > 4 / length(dsign1_add)
dsign1_add[large_dsign1_add]

# joux and  lugano is very different

coef(dsign1)

# what happens if we remove this point?
dsign1_fix = lm(mean_sign ~ Max_depth,
                data = eutroph_dissimilarity,
                subset = dsign1_add <= 4 / length(dsign1_add))
coef(dsign1_fix)

# not very different

# summary:
# 1: semi okay
# 2: okay
# 3: okay
# 4: okay

# plot lm
ggplotRegression(dsign1)

##################################################################################

dsign2 <- lm(max_sign ~ Max_depth, data = eutroph_dissimilarity)
summary(dsign2)
par(mfrow=c(2,2))
plot(dsign2)

plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$max_sign, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(dsign2)

summary(dsign2)
anova(dsign2)

# dissimilarity 
ggplot(mapping = aes(x = Max_depth, y = max_sign), data = eutroph_dissimilarity) +
  geom_point()
#might be positive
# check assumptions

# overview

par(mfrow=c(2,2))
plot(dsign2, pch = 16, col = "blue")

autoplot(dsign2, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
par(mfrow=c(1,1))
plot(dsign2,1)

# not very horizontal

# equal variance: no patterns should be there

phos_res <- resid(dsign2)
phos_fit <- fitted(dsign2)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# no patterns, but not very horizontal 

# 2
# histogram of residuals
hist(dsign2$residuals)

# qqplot
plot(dsign2, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(dsign2))
# normality not okay

# 3: Testing the Homoscedasticity Assumption
plot(dsign2, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(dsign2) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(dsign2))

# Print leverage for each observation
leverage

# look at highest values, 8, maggiore

par(mfrow=c(2,1))
barplot(hatvalues(dsign2), 
        col = "aquamarine3")


leveragePlots(dsign2)

# look for points with high leverage
sum(hatvalues(dsign2) > 2 * mean(hatvalues(dsign2)))
# 1 point of high leverage

sum(abs(rstandard(dsign2)) > 2)
# also 0 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

dsign2_add = cooks.distance(dsign2)
sum(dsign2_add > 4 / length(dsign2_add))
0

large_dsign2_add = dsign2_add > 4 / length(dsign2_add)
dsign2_add[large_dsign2_add]

# none

# summary:
# 1: not okay
# 2: not normally distributed
# 3: okay
# 4: okay

################################################################################lm
lm_analysis <- function(y, x, df) {
  
  mod <- lm(y ~ x, data = df)
  print(summary(mod))
  print(shapiro.test(resid(mod)))
  print(lmtest::bptest(mod))
  par(mfrow=c(2,2))
  print(plot(mod))
  
  ggplot(mod$model, aes_string(x = names(mod$model)[2], y = names(mod$model)[1])) + 
    geom_point() +
    geom_smooth(method = "lm", col = "red",  fill = "#CDC9C9") +
    # stat_smooth(method = "lm", col = "#FF3030", fill = "#CDC9C9") +
    labs(title = paste("R^2 = ",signif(summary(mod)$r.squared, 2),
                       # "adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       # "Intercept =",signif(fit$coef[[1]],5 ),
                       # " Slope =",signif(fit$coef[[2]], 5),
                       " p-value =", signif(summary(mod)$coef[2,4], 2))) +
    theme_bw()
}

# maximum_depth:

depth1 <- lm_analysis(eutroph_dissimilarity$mean_rdiv, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity)  +
  geom_smooth(method = "lm", col = "#66CD00",  fill = "#CDC9C9") +
  labs(x = "maximum_depth", y = "mean_dissimilarity")
# 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
depth2 <- lm_analysis(eutroph_dissimilarity$max_rdiv, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity) +
  geom_smooth(method = "lm", col = "#66CD00",  fill = "#CDC9C9") +
  labs(x = "maximum_depth", y = "max_dissimilarity")
# 1: naja, 2: ok, 3: naja mit pattern, test ok, 4: ok -> ?
# depth3 <-lm_analysis(eutroph_dissimilarity$mean_sign, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity) +
#   geom_smooth(method = "lm", col = "#EE1289",  fill = "#CDC9C9") +
#   labs(x = "maximum_depth", y = "mean_divergence")
# 1: naja, 2: no, 3: naja mit pattern, test ok, 4: ok -> 0
# depth4 <- lm_analysis(eutroph_dissimilarity$max_sign, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity) +
#   geom_smooth(method = "lm", col = "#EE1289",  fill = "#CDC9C9") +
#   labs(x = "maximum_depth", y = "mean_divergence")
# 1: naja, 2: no, 3: naja mit pattern, test ok, 4: ok -> 0

ggarrange(depth1, depth2, depth3, depth4, ncol = 2, nrow = 2)

tiff(paste("total_models/plots/lm_depth_max.tiff", sep = ""), units="in", width=8, height=4, res=300)

plot(ggarrange(depth1, depth2, ncol = 2))

# Closing the graphical device
dev.off()

# eutrophication

phos1 <- lm_analysis(eutroph_dissimilarity$mean_rdiv, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)  +
  labs(x = "maximum_phosphorus", y = "mean_dissimilarity")
# 1: ok, 2: ok, 3: ok, 4: ok -> 1
phos2 <- lm_analysis(eutroph_dissimilarity$max_rdiv, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity) +
  labs(x = "maximum_phosphorus", y = "max_dissimilarity")
# 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
# phos3 <- lm_analysis(eutroph_dissimilarity$mean_sign, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)
# # 1: ok, 2: no, 3: naja, test ok, 4: ok -> 0
# phos4 <- lm_analysis(eutroph_dissimilarity$max_sign, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)
# # 1: no, 2: no, 3: naja, test ok, 4: ok -> 0

ggarrange(phos1, phos2, ncol = 2)

tiff(paste("total_models/plots/lm_max_phos.tiff", sep = ""), units="in", width=8, height=4, res=300)

plot(ggarrange(phos1, phos2, ncol = 2))

# Closing the graphical device
dev.off()