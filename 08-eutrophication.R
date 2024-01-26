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

# lake properties df
lake_info <- read_xlsx("lake_info.xlsx")

eutroph <- lake_info |> 
  select(Lake, Phos_max)

# resp diversity per lake df
resp_div_all <- readRDS("total_models/resp_div_all.rds") |> 
  select(temp:fLake)


# dissimilarity and max phosporus
# no info for poschiavo in phos max, needs to go
rdiv <- resp_div_all |>  
  filter(fLake != "Poschiavo") |> 
  select(temp, fLake, rdiv, sign) |>
  rename(Lake = fLake)

eutroph_dissimilarity <- merge(eutroph, rdiv)

str(eutroph_dissimilarity)

eutroph_dissimilarity$Phos_max <- as.numeric(eutroph_dissimilarity$Phos_max)

levels(eutroph_dissimilarity$fPhos)

# eutroph_dissimilarity$fPhos <- factor(eutroph_dissimilarity$fPhos, levels=c("21", "23", '25', '29', "35.3", '37', 
#                                         "53", "59", "89.6", "140", "147", "176",
#                                         "210"))

#display factor levels for region
# levels(eutroph_dissimilarity$fPhos)

# categorize oligrotrophy

eutroph_dissimilarity <- eutroph_dissimilarity |> 
  mutate(eutroph = case_when(Phos_max < 38 ~ "never",
                                     Phos_max < 60 ~ 'moderately',
                           Phos_max < 105 ~ "strongly",
                                     Phos_max < 220 ~ 'hyper'))

str(eutroph_dissimilarity)

eutroph_dissimilarity$eutroph <- as.factor(eutroph_dissimilarity$eutroph)
eutroph_dissimilarity$eutroph <- factor(eutroph_dissimilarity$eutrop, levels=c("never", "moderately",
                                                                               "strongly", "hyper"))
# aanova

one.way <- aov(rdiv ~ eutroph, data = eutroph_dissimilarity)

TukeyHSD(one.way)

summary(one.way)

# par(mfrow=c(2,2))
# plot(one.way)
# par(mfrow=c(1,1))

two.way.plot <- ggplot(eutroph_dissimilarity, aes(x = eutroph , y = rdiv)) +
  geom_boxplot()

two.way.plot


mean.data <- eutroph_diss
imilarity %>%
  group_by(eutroph) %>%
  summarise(
    rdiv = mean(rdiv)
  )

plot <- two.way.plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2, color = "red") +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange', color = "red") +
  geom_boxplot(data=mean.data, aes(x=eutroph, y=rdiv))

plot


# anova divergence

sign <- aov(sign ~ eutroph, data = eutroph_dissimilarity)

summary(sign)

# par(mfrow=c(2,2))
# plot(one.way)
# par(mfrow=c(1,1))

two.way.plot2 <- ggplot(eutroph_dissimilarity, aes(x = eutroph , y = sign)) +
  geom_boxplot()

two.way.plot2

mean.data2 <- eutroph_dissimilarity %>%
  group_by(eutroph) %>%
  summarise(
    sign = mean(sign)
  )

plot2 <- two.way.plot2 + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2, color = "red") +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange', color = "red") +
  geom_boxplot(data=mean.data2, aes(x=eutroph, y=sign))

plot2


# regression

# lm with rdiv along phos_max
# model <- lm(rdiv ~ eutroph , data=eutroph_dissimilarity)
# #view model summary
# summary(model)
# plot(model)
# plot(eutroph_dissimilarity$eutroph, eutroph_dissimilarity$rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
# #add fitted regression line
# abline(model)
# # impossible, not working like this. forget lm
# model <- lm(rdiv ~ Phos_max, data=eutroph_dissimilarity)
# summary(model)$coef
# plot(eutroph_dissimilarity$Phos_max, eutroph_dissimilarity$rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
# abline(model)
# # i dont know how to do this shit
# model2 <- gam(rdiv ~ Phos_max, data = eutroph_dissimilarity)
# plot(model2)
