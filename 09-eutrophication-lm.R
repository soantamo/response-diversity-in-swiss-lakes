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
library(readxl)
library(ggpubr)
library(ggfortify)
library(car)

# You should think long and hard about removing any data point(s) 
# and if you do you should always report this and justify your reasoning.

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
  select(Lake, fPhos) |> 
  distinct(Lake, fPhos)


#################################################################################3
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
                       " p-value =",signif(summary(mod)$coef[2,4], 2))) +
    theme_bw()
}

lm_analysis(eutroph_dissimilarity$mean_rdiv, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity) +
  geom_smooth(method = "lm", col = "#4876FF",  fill = "#CDC9C9")

lm_analysis(eutroph_dissimilarity$max_rdiv, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)

lm_analysis(eutroph_dissimilarity$mean_sign, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)
a <- lm_analysis(eutroph_dissimilarity$max_sign, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)
a

#################### regression: eutrophication and mean_rdiv

mrdiv1 <- lm(mean_rdiv ~ Phos_max, data = eutroph_dissimilarity)
summary(mrdiv1)
plot(mrdiv1)

plot(eutroph_dissimilarity$Phos_max, eutroph_dissimilarity$mean_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(mrdiv1)

summary(mrdiv1)
anova(mrdiv1)

# analysis 
# dissimilarity 
ggplot(mapping = aes(x = Phos_max, y = mean_rdiv), data = eutroph_dissimilarity) +
  geom_point()
# suggests a negative relationship

hist(eutroph_dissimilarity$mean_rdiv)


# check assumptions
# Two of the most important assumption are equal variances 
# (homogeneity of variance) and normality of residuals. 
# To check for equal variances we can construct a graph of 
# residuals versus fitted values. We can do this by first extracting 
# the residuals and fitted values from our model object using the resid() 
# and fitted() functions.
# residuals

# https://bookdown.org/jimr1603/Intermediate_R_-_R_for_Survey_Analysis/testing-regression-assumptions.html
# https://book.stat420.org/transformations.html
# https://uw-statistics.github.io/Stat311Tutorial/checking-model-assumptions.html
# 1:  Residuals vs Fitted: is used to check the assumptions of linearity. 
# If the residuals are spread equally around a horizontal line without
# distinct patterns (red line is approximately horizontal at zero),
# that is a good indication of having a linear relationship.

# 2:  Normal Q-Q: is used to check the normality of residuals assumption. 
# If the majority of the residuals follow the 
# straight dashed line, then the assumption is fulfilled.

# 3: Scale-Location: is used to check the homoscedasticity of residuals 
# (equal variance of residuals). If the residuals are spread randomly
# and the see a horizontal line with equally (randomly) spread points, 
# then the assumption is fulfilled.

# 4: Residuals vs Leverage: is used to identify any influential
# value in our dataset. Influential values are extreme values 
# that might influence the regression results when included or 
# excluded from the analysis. Look for cases outside of a dashed line.

# overview

par(mfrow=c(2,2))
plot(mrdiv1, pch = 16, col = "blue")

library(ggfortify)
autoplot(mrdiv1, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
plot(mrdiv1,1)

# equal variance: no patterns should be there

phos_res <- resid(mrdiv1)
phos_fit <- fitted(mrdiv1)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# 2
# histogram of residuals
hist(mrdiv1$residuals)

# qqplot
plot(mrdiv1, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(mrdiv1))
# normality is okay

# 3: Testing the Homoscedasticity Assumption
plot(mrdiv1, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(mrdiv1) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(mrdiv1))

# Print leverage for each observation
leverage

# look at highest values

par(mfrow=c(2,1))
barplot(hatvalues(mrdiv1), 
        col = "aquamarine3")

library(car)
leveragePlots(mrdiv1)

# look for points with high leverage
sum(hatvalues(mrdiv1) > 2 * mean(hatvalues(mrdiv1)))
# 1 point of high leverage

sum(abs(rstandard(mrdiv1)) > 2)
# also 1 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

cd_mpg_hp_add = cooks.distance(mrdiv1)
sum(cd_mpg_hp_add > 4 / length(cd_mpg_hp_add))

large_cd_mpg = cd_mpg_hp_add > 4 / length(cd_mpg_hp_add)
cd_mpg_hp_add[large_cd_mpg]

# joux is very different

coef(mrdiv1)

# what happens if we remove this point?
mrdiv1_fix = lm(mean_rdiv ~ Phos_max,
                    data = eutroph_dissimilarity,
                    subset = cd_mpg_hp_add <= 4 / length(cd_mpg_hp_add))
coef(mrdiv1_fix)
# no real difference, let joux inside the model 

# summary:
# 1: okay
# 2: okay
# 3: okay
# 4: okay

# plot lm

rdiv_mean <- ggplot(mapping = aes(x = Phos_max, y = mean_rdiv), data = eutroph_dissimilarity) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "#66CD00", fill = "#66CD00") +
  stat_regline_equation(label.y = 4, label.x = 180, aes(label = after_stat(eq.label))) +
  stat_regline_equation(label.y = 3.8, label.x = 180, aes(label = ..rr.label..)) +
  theme_bw()

################################################################################

mrdiv2 <- lm(max_rdiv ~ Phos_max, data = eutroph_dissimilarity)
summary(mrdiv2)
plot(mrdiv2)

par(mfrow=c(1,1))
plot(eutroph_dissimilarity$Phos_max, eutroph_dissimilarity$max_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(mrdiv2)

summary(mrdiv2)
anova(mrdiv2)


# analysis 
# dissimilarity 
ggplot(mapping = aes(x = Phos_max, y = max_rdiv), data = eutroph_dissimilarity) +
  geom_point()
# suggests a negative relationship

hist(eutroph_dissimilarity$max_rdiv)


# check assumptions

# 1:  Residuals vs Fitted: is used to check the assumptions of linearity. 
# 2:  Normal Q-Q: is used to check the normality of residuals assumption. 
# 3: Scale-Location: is used to check the homoscedasticity of residuals 
# (equal variance of residuals).
# 4: Residuals vs Leverage: is used to identify any influential
# value in our dataset. Influential values are extreme values 
# that might influence the regression results when included or 
# excluded from the analysis. Look for cases outside of a dashed line.

# overview

par(mfrow=c(2,2))
plot(mrdiv2, pch = 16, col = "blue")

autoplot(mrdiv2, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
par(mfrow=c(1,1))
plot(mrdiv2,1)

# equal variance: no patterns should be there

phos_res <- resid(mrdiv2)
phos_fit <- fitted(mrdiv2)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# 2
# histogram of residuals
hist(mrdiv2$residuals)

# qqplot
plot(mrdiv2, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(mrdiv2))
# normality is okay

# 3: Testing the Homoscedasticity Assumption
plot(mrdiv2, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(mrdiv2) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(mrdiv2))

# Print leverage for each observation
leverage

# look at highest values

par(mfrow=c(1,1))
barplot(hatvalues(mrdiv2), 
        col = "aquamarine3")


leveragePlots(mrdiv2)

# look for points with high leverage
sum(hatvalues(mrdiv2) > 2 * mean(hatvalues(mrdiv2)))
# 1 point of high leverage

sum(abs(rstandard(mrdiv2)) > 2)
# 0 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

mrdiv2_add = cooks.distance(mrdiv2)
sum(mrdiv2_add  > 4 / length(mrdiv2_add ))

large_mrdiv2_add  = mrdiv2_add  > 4 / length(mrdiv2_add )
mrdiv2_add[large_mrdiv2_add]

# lugano is different

coef(mrdiv2)

# what happens if we remove this point?
mrdiv2_fix = lm(max_rdiv ~ Phos_max,
                data = eutroph_dissimilarity,
                subset = mrdiv2_add  <= 4 / length(mrdiv2_add ))
coef(mrdiv2_fix)
# no real difference, let lugano inside the model 

# summary:
# 1: okay
# 2: okay
# 3: okay
# 4: okay

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


a <- ggplotRegression(mrdiv1)
a

b <- ggplotRegression(mrdiv2) +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")

b

# plot both 
plot_eutrophication_rdiv <- ggarrange(a, b , ncol = 2)

##################################################################################
#divergence
msign1 <- lm(mean_sign ~ Phos_max, data = eutroph_dissimilarity)
summary(msign1)
par(mfrow=c(2,2))
plot(msign1)

par(mfrow=c(1,1))
plot(eutroph_dissimilarity$Phos_max, eutroph_dissimilarity$mean_sign, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(msign1)

anova(msign1)

# dissimilarity 
ggplot(mapping = aes(x = Phos_max, y = mean_sign), data = eutroph_dissimilarity) +
  geom_point()
# suggests random

hist(eutroph_dissimilarity$mean_sign)


# check assumptions
# overview

par(mfrow=c(2,2))
plot(msign1, pch = 16, col = "blue")

autoplot(msign1, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
par(mfrow=c(1,1))
plot(msign1,1)

# equal variance: no patterns should be there

phos_res <- resid(msign1)
phos_fit <- fitted(msign1)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# 2
# histogram of residuals
hist(msign1$residuals)

# qqplot
plot(msign1, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(msign1))
# normality not given

# 3: Testing the Homoscedasticity Assumption
plot(msign1, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(msign1) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(msign1))

# Print leverage for each observation
leverage

# look at highest values

par(mfrow=c(1,1))
barplot(hatvalues(msign1), 
        col = "aquamarine3")


leveragePlots(msign1)

# look for points with high leverage
sum(hatvalues(msign1) > 2 * mean(hatvalues(msign1)))
# 1 point of high leverage

sum(abs(rstandard(msign1)) > 2)
# 1 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

msign1_add = cooks.distance(msign1)
sum(msign1_add  > 4 / length(msign1_add))

large_msign1_add = msign1_add  > 4 / length(msign1_add)
msign1_add[large_msign1_add]

# lmaggiore and zug

coef(msign1)

# what happens if we remove this point?
msign1_fix = lm(mean_sign ~ Phos_max,
                data = eutroph_dissimilarity,
                subset = msign1_add  <= 4 / length(msign1_add ))
coef(msign1_fix)
# no real difference, let lugano inside the model 

# summary:
# 1: okay
# 2: not normally distirbuted
# 3: okay
# 4: okay

# plot lm

c <- ggplotRegression(msign1)  +
  geom_smooth(method = "lm", col = "#1C86EE",  fill = "#CDC9C9")
c

################################################################################
msign2 <- lm(max_sign ~ Phos_max, data = eutroph_dissimilarity)
summary(msign2)
par(mfrow=c(2,2))
plot(msign2)

par(mfrow=c(1,1))
plot(eutroph_dissimilarity$Phos_max, eutroph_dissimilarity$max_sign, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(msign2)

anova(msign2)

# dissimilarity 
ggplot(mapping = aes(x = Phos_max, y = max_sign), data = eutroph_dissimilarity) +
  geom_point()
# suggests random

hist(eutroph_dissimilarity$max_sign)


# check assumptions
# overview

par(mfrow=c(2,2))
plot(msign2, pch = 16, col = "blue")

autoplot(msign2, which = 1:6, ncol = 2, label.size = 3)

# 1
# ideally a horizontal line across zero
par(mfrow=c(1,1))
plot(msign2,1)

# not very horizontal

# equal variance: no patterns should be there

phos_res <- resid(msign2)
phos_fit <- fitted(msign2)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")

# 2
# histogram of residuals
hist(msign2$residuals)

# qqplot
plot(msign2, 2)

# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()


shapiro.test(resid(msign2))
# normality not given

# 3: Testing the Homoscedasticity Assumption
plot(msign2, 3)
# some deviation from horizontal line

# breusch pagan test for Homoscedasticity 
lmtest::bptest(msign2) 
# p value > 0.05 Thus, we assume that homoscedasticity is present.

# 4: leverage

# Get leverage for each observation in the data set
leverage <- as.data.frame(hatvalues(msign2))

# Print leverage for each observation
leverage

# look at highest values

par(mfrow=c(1,1))
barplot(hatvalues(msign2), 
        col = "aquamarine3")


leveragePlots(msign2)

# look for points with high leverage
sum(hatvalues(msign2) > 2 * mean(hatvalues(msign2)))
# 1 point of high leverage

sum(abs(rstandard(msign2)) > 2)
# 0 point with big residual
# There is also one point with a large residual. Do these result in any
# points that are considered influential?

msign2_add = cooks.distance(msign2)
sum(msign2_add  > 4 / length(msign2_add))

large_msign2_add = msign2_add  > 4 / length(msign2_add)
msign1_add[large_msign2_add]

# zug

coef(msign2)

# what happens if we remove this point?
msign2_fix = lm(max_sign ~ Phos_max,
                data = eutroph_dissimilarity,
                subset = msign2_add  <= 4 / length(msign2_add ))
coef(msign2_fix)
# no real difference

# summary:
# 1: not okay
# 2: not normally distirbuted
# 3: okay
# 4: okay

# plot lm

##################################### overview

a <- ggplotRegression(mrdiv1) #model ok
b <- ggplotRegression(mrdiv2) #model ok
c <- ggplotRegression(msign1)  +
  geom_smooth(method = "lm", col = "#4876FF",  fill = "#CDC9C9")
#model not ok
d <- ggplotRegression(msign2) +
  geom_smooth(method = "lm", col = "#4876FF",  fill = "#CDC9C9")
#model not ok



max_phos <- ggarrange(a, b, c, d, nrow = 2, ncol = 2)
max_phos

# tiff(paste("total_models/plots/lm_phos_max.tiff", sep = ""), units="in", width=9, height=6, res=300)
# 
# plot(max_phos)
# 
# # Closing the graphical device
# dev.off()


