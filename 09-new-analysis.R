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

par(mfrow=c(2,2))
plot(mrdiv1, pch = 16, col = "blue")
# equal variance: no patterns should be there

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

# 1:
# 2: 
# 3: 
# 4: 



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

# 1
# ideally a horizontal line across zero
plot(mrdiv1,1)

# 2
# histogram of residuals
hist(mrdiv1$residuals)

# qqplot
plot(mrdiv1, 2)

# Using pre-installed library(MASS)
# get distribution of studentized residuals (i.e. transform residuals for test)
sresid <- MASS::studres(mrdiv1) #using MASS package function to transform data easily
shapiro.test(sample(sresid)) # p value non-sign: normal distribution of residuals

# 3
plot(mrdiv1, 3)
# some deviation there

################################################################################
mrdiv2 <- lm(max_rdiv ~ Phos_max, data = eutroph_dissimilarity)
summary(mrdiv2)
plot(mrdiv2)

plot(eutroph_dissimilarity$Phos_max, eutroph_dissimilarity$max_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(mrdiv2)

summary(mrdiv2)
anova(mrdiv2)

# analysis 
par(mfrow=c(2,2))
plot(mrdiv2, pch = 16, col = "blue")
# dissimilarity 
ggplot(mapping = aes(x = Phos_max, y = max_rdiv), data = eutroph_dissimilarity) +
  geom_point()
# suggests a negative relationship

hist(eutroph_dissimilarity$max_rdiv)

ggplot(mapping = aes(x = Phos_max, y = max_rdiv), data = eutroph_dissimilarity) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE)

# equal variance

phos_res <- resid(mrdiv2)
phos_fit <- fitted(mrdiv2)

ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
  geom_point() +
  geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
# qq

ggplot(mapping = aes(sample = phos_res)) +
  stat_qq() + 
  stat_qq_line()

# 
library(ggfortify)
autoplot(mrdiv2, which = 1:6, ncol = 2, label.size = 3)


