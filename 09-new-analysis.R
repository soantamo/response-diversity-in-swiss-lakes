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

ggplot(mapping = aes(x = Phos_max, y = mean_rdiv), data = eutroph_dissimilarity) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)



# check assumptions
# Two of the most important assumption are equal variances 
# (homogeneity of variance) and normality of residuals. 
# To check for equal variances we can construct a graph of 
# residuals versus fitted values. We can do this by first extracting 
# the residuals and fitted values from our model object using the resid() 
# and fitted() functions.
# residuals

# equal variance

phos_res <- resid(mrdiv1)
phos_fit <- fitted(mrdiv1)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()

# 
library(ggfortify)
autoplot(mrdiv1, which = 1:6, ncol = 2, label.size = 3)

# homogeneity of variance not given at all. it is depending on the Lake, Phos_max
# is not independent.

################################################################################

# phos_max: anova
# depth_max: lm

# anova phos_max
# https://statsandr.com/blog/anova-in-r/

ggplot(eutroph_dissimilarity) +
  aes(x = fPhos, y = rdiv, color = Lake) +
  geom_jitter() +
  theme(legend.position = "none")

# anova 
res_aov <- aov(rdiv ~ fPhos,
               data = eutroph_dissimilarity)

# check normality 

par(mfrow = c(1, 2)) # combine plots

# histogram
hist(res_aov$residuals)

# QQ-plot
library(car)
qqPlot(res_aov$residuals,
       id = FALSE)

shapiro.test(res_aov$residuals) #not normally distributed
# -> kruskal wallis test


# check equality of variances
# Equality of variances - homogeneity

library("lattice")

dotplot(rdiv ~ fPhos,
        data = eutroph_dissimilarity)

# Levene's test for homogeneity
library(car)

leveneTest(rdiv ~ fPhos,
           data = eutroph_dissimilarity)

# visual interpretation of normality and homogenetiy

par(mfrow = c(1, 2)) # combine plots

# 1. Homogeneity of variances
plot(res_aov, which = 3)

# 2. Normality
plot(res_aov, which = 2)


kruskal.test(rdiv ~ fPhos, data = eutroph_dissimilarity)

