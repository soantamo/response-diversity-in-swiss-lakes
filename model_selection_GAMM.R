##model selection
library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)
#data
df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

names(df_final_no_electro)
str(df_final_no_electro)

#only species present in lake get lakepresence 1
min(df_final_no_electro$LakePresence)

#look for outliers
par(mar=c(5,5,2,2), cex.lab = 1.5)
plot(y = 1:nrow(df_final_no_electro), 
     x = df_final_no_electro$Abundance, 
     xlab = "Values of the data",
     ylab = "Order of the data",
     pch = 16, 
     cex = 0.7)
#joux has extremely high perch abundances, strange. joux generally looks strange
#joux morat and neuchatel, check fishing date etc. 

#differences in sampling effort?

# df_final_no_electro$Lake <- as.factor(df_final_no_electro$Lake)

df_final_no_electro |> 
  filter(Lake %in% c("Biel", "Brienz", "Walen")) |> 
  ggplot(aes(x = mean_last_7days, y = Abundance)) +
  geom_point() +
  facet_wrap(~Lake)

#joux, morat and neuchatel strange. poschiavo and zug also strange 

########################## models, one species across all lakes

df_perch <-  df_final_no_electro |> 
  filter(Species == "Perca_fluviatilis")

#add lake as factor
df_perch$fLake <- as.factor(df_perch$Lake)

#fitting model, poisson first, Lakes as random effect
#k is 10, method is default ML
M1 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days), 
           random = list(fLake =~ 1), family = poisson)

plot(M1$gam)
summary(M1$gam) #R² can be used as model selection (as AIC), 0.00462 
anova(M1$gam)
summary(M1$lme)

fv <- exp(fitted(M1$lme)) ## predicted values (including re)
rsd <- (M1$gam$y - fv)/sqrt(fv) ## Pearson residuals (Poisson case)
op <- par(mfrow=c(1,2))
qqnorm(rsd);plot(fv^.5,rsd)
par(op) 
#MISSING! validaiton following p. 52

#overdispersion
E1 <- resid(M1$gam, type = "pearson")
sum(E1^2)/M1$gam$df.residual 
#overdispersion: 4.013684

#how many percent of abundance data are 0?

table(df_perch$Abundance)
length(df_perch$Abundance)
#percent of zeros: 59.32324 %
100 /26627 * 15796

#try negbin and zero-inflated
#is it overdispersion due to the many zeros? or is it because there is such a big
#variation in the data ? (page 160), biological choice

M2 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days), 
           random = list(fLake =~ 1), family = negbin(1))

plot(M2$gam)
summary(M2$gam) #R² can be used as model selection (as AIC): 0.00527 
anova(M2$gam)
summary(M2$lme)

fv <- exp(fitted(M2$lme)) ## predicted values (including re)
rsd <- (M2$gam$y - fv)/sqrt(fv) ## Pearson residuals (Poisson case)
op <- par(mfrow=c(1,2))
qqnorm(rsd);plot(fv^.5,rsd)
par(op) 

E2 <- resid(M2$gam, type = "pearson")
sum(E2^2)/M2$gam$df.residual #less overdispersion: 2.600412

AIC(M1$lme, M2$lme) #model 2 seems to be better

#zero-inflated model
M3 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days), 
           random = list(fLake =~ 1), family = ziP())
