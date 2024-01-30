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
  filter(!fLake %in% c("Poschiavo")) |> 
  select(temp, fLake, rdiv, sign) |>
  drop_na() |> 
  rename(Lake = fLake)

eutroph_dissimilarity <- merge(eutroph, rdiv)

eutroph_dissimilarity$Phos_max <- as.numeric(eutroph_dissimilarity$Phos_max)
eutroph_dissimilarity$fPhos <- as.factor(eutroph_dissimilarity$Phos_max)

str(eutroph_dissimilarity)
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

