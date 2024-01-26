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
  select(Lake, Phos_max, Max_depth)

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

one.way <- aov(rdiv ~ Phos_max, data = eutroph_dissimilarity)


summary(one.way)
plot(one.way)


# par(mfrow=c(2,2))
# plot(one.way)
# par(mfrow=c(1,1))

two.way.plot <- ggplot(eutroph_dissimilarity, aes(x = Phos_max , y = rdiv)) +
  geom_boxplot()

two.way.plot


mean.data <- eutroph_dissimilarity %>%
  group_by(Phos_max) %>%
  summarise(
    rdiv = mean(rdiv)
  )

plot <- two.way.plot + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2, color = "red") +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange', color = "red") +
  geom_boxplot(data=mean.data, aes(x=Phos_max, y=rdiv))

plot

# anova depth

eutroph_dissimilarity$fDepth <- as.factor(eutroph_dissimilarity$Max_depth)

depth_aov <- aov(rdiv ~ fDepth, data = eutroph_dissimilarity)


summary(depth_aov)
plot(depth_aov)


plot_depth <- ggplot(eutroph_dissimilarity, aes(x = fDepth , y = rdiv)) +
  geom_boxplot()

plot_depth


mean.data <- eutroph_dissimilarity %>%
  group_by(fDepth) %>%
  summarise(
    rdiv = mean(rdiv)
  )

p <- plot_depth + stat_summary(fun.data = 'mean_se', geom = 'errorbar', width = 0.2, color = "red") +
  stat_summary(fun.data = 'mean_se', geom = 'pointrange', color = "red") +
  geom_boxplot(data=mean.data, aes(x=fDepth, y=rdiv))

p

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

model <- lm(rdiv ~ Max_depth, data = eutroph_dissimilarity)
summary(model)
plot(model)

plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(model)

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


model1 <- lm(rdiv ~ Max_depth, data = eutroph_dissimilarity)
plot(model1)

plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(model1)


model2 <- glm(sign ~ Max_depth, data = eutroph_dissimilarity)
plot(model2)

plot.gam(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
#add fitted regression line
abline(model2)


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

levels(eutroph_dissimilarity$eutroph)

boxplot(rdiv ~ eutroph, data = eutroph_dissimilarity,
        xlab = "Past Eutrophication", ylab = "Dissimilarity",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07"))


res.aov <- aov(rdiv ~ eutroph, data = eutroph_dissimilarity)
# Summary of the analysis
summary(res.aov)

# In one-way ANOVA test, a significant p-value indicates that some of t
# he group means are different, but we don’t know which pairs of groups are different.
# It’s possible to perform multiple pairwise-comparison,
# to determine if the mean difference between specific pairs of group 
# are statistically significant.


# As the ANOVA test is significant, we can compute 
# Tukey HSD (Tukey Honest Significant Differences, R function: TukeyHSD())
# for performing multiple pairwise-comparison between the means of groups.

TukeyHSD(res.aov)

# diff: difference between means of the two groups
# lwr, upr: the lower and the upper end point of the confidence interval at 95% (default)
# p adj: p-value after adjustment for the multiple comparisons.

# all but strongly-never are significantly different.


pairwise.t.test(eutroph_dissimilarity$eutroph, eutroph_dissimilarity$rdiv)

plot(res.aov, 1) 


# test validity: 
# homogeneity
# not homogeonus -> outlier 

library(car)
leveneTest(rdiv ~ eutroph, data = eutroph_dissimilarity)

# really significant , not homogeonus. cant be used 


oneway.test(rdiv ~ eutroph, data = eutroph_dissimilarity)

# 2. Normality
plot(res.aov, 2)
# Extract the residuals
aov_residuals <- residuals(object = res.aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals )
 # everything for anova is violated

# kruskal for non normal data
kruskal.test(rdiv ~ Max_depth, data = eutroph_dissimilarity)



###################################################### new start
# based on eth statistics
# https://boostedml.com/2019/03/linear-regression-plots-how-to-read-a-qq-plot.html
# https://towardsdatascience.com/understanding-linear-regression-output-in-r-7a9cbda948b3
# y is rdiv 
# y is continous
# x as phos_max is discrete, not a factor. use it as numerical
# y is dependent?? LMM?

hist(eutroph_dissimilarity$rdiv)

eutroph_dissimilarity$Phos_max <- as.numeric(eutroph_dissimilarity$Phos_max)

fit <- lm(rdiv ~ as.numeric(as.character(Phos_max)), data = eutroph_dissimilarity)
plot(rdiv ~ as.numeric(as.character(Phos_max)), data = eutroph_dissimilarity)
abline(fit)
     
summary(fit)

# residuals
summary(eutroph_dissimilarity$rdiv - fit$fitted.values)
# our median is close to zero -> good
# 7 percent of variation are explained by phos max


plot(fit)
# check for normality etc.

d<-density(fit[['residuals']])
plot(d,main='Residual KDE Plot',xlab='Residual value')


plot(ecdf(fit[['residuals']]))

shapiro.test(fit[['residuals']]) 

# very small p valuem, check density plots in each group

eutroph_dissimilarity |> 
  ggplot(aes(rdiv, colour = factor(Phos_max))) +
  geom_density() +
  scale_fill_continuous()

# plot LM rdiv Phos_max
library(ggpubr)
f <- eutroph_dissimilarity |> 
  ggplot(aes(Phos_max, rdiv)) +
  stat_summary(fun.data=mean_cl_normal, geom = "pointrange") + 
  geom_smooth(method='lm') +
  theme_bw() +
  stat_regline_equation(label.y = 3.5, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = 3.2, aes(label = ..rr.label..))

f

# r square is very bad
# or 

ggplot(eutroph_dissimilarity, aes(x = as.character(Phos_max), y = rdiv)) + 
  geom_point() + 
  geom_abline(slope = coef(fit)[["Phos_max"]], 
             intercept = coef(fit)[["(Intercept)"]])

eutroph_dissimilarity |> 
  ggplot(aes(Phos_max, sign)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm')


eutroph_dissimilarity |> 
  ggplot(aes(Max_depth, rdiv)) +
  stat_summary(fun.data=mean_cl_normal) + 
  geom_smooth(method='lm')


  
           
# it probably is okay

# transforming data 

fit_trans <-lm(log(rdiv) ~ as.numeric(as.character(Phos_max)), data = eutroph_dissimilarity)
summary(fit_trans)
plot(fit_trans)
plot(log(rdiv) ~ as.numeric(as.character(Phos_max)), data = eutroph_dissimilarity)
abline(fit_trans)


# try glm

glm_fit <- glm(rdiv ~ as.numeric(as.character(Phos_max)), data = eutroph_dissimilarity,
               family = gaussian)

summary(glm_fit)

plot(rdiv ~ as.numeric(as.character(Phos_max)), data = eutroph_dissimilarity)
abline(glm_fit)

hist(eutroph_dissimilarity$rdiv)

