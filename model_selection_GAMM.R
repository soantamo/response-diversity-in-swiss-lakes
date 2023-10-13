##model selection
library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)

#reading in data
df_final <- read_rds("df_final.rds")

names(df_final)
str(df_final)

#only species present in lake get lakepresence 1
min(df_final$LakePresence)

##############################################################################
#look for outliers
par(mar=c(5,5,2,2), cex.lab = 1.5)
plot(y = 1:nrow(df_final), 
     x = df_final$Abundance, 
     xlab = "Values of the data",
     ylab = "Order of the data",
     pch = 16, 
     cex = 0.7)

test <- df_final |> 
  filter(Lake == "Joux")
#joux has extremely high abundances, strange. joux generally looks strange
#joux morat and neuchatel, check fishing date etc. 

table(df_final$Abundance)

test <- df_final |> 
  filter(Abundance >= 20)

table(test$Abundance)

b <- test |> 
  filter(Lake == "Joux")

#differences in sampling effort?
#takes long

# df_final |> 
#   filter(Lake %in% c("Biel", "Brienz", "Walen")) |> 
#   ggplot(aes(x = mean_last_7days, y = Abundance)) +
#   geom_point() +
#   facet_wrap(~Lake)

#joux, morat and neuchatel strange. poschiavo and zug also strange 

########################## models, one species across all lakes

df_perch <-  df_final |> 
  filter(Species == "Perca_fluviatilis") |> 
  drop_na(mean_last_7days) #dropping those NAs

#missing values in df, thats why plotting response vs. residuals is not working

#adding column with lake as factor
df_perch$fLake <- as.factor(df_perch$Lake)
df_perch$fProtocol <- as.factor(df_perch$Protocol)

#which are our potential explanatory variables?
names(df_perch)
#Abundance: continous response
#mean_last_7days: covariate/explanatory continous
#Protocol: categorical covariate
#Lake: categorical covariate
#Depth_sample: continous covariate

#fitting gamm with gaussian
M0 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days),
           random = list(fLake =~ 1), family = gaussian)

plot(M0$gam) #wiggly af, k must be lower
summary(M0$gam) #R² can be used as model selection (as AIC), 0.00462
anova(M0$gam)
summary(M0$lme)
gam.check(M0$gam) #not precise enough for GAMMs

#MODEL VALIDATION#
#Synthesis of pdf, GAMM book and GAM book
#1.verifiy homogeneity: rsd vs fitted
fv <- fitted(M0$lme, type = "n") ##predicted values
rsd <- resid(M0$lme) ##residuals
plot(x = fv, y = rsd, xlab = "Fitted values", ylab = "Residuals")

#no

#2.verify model misfit (or independence): rsd vs each covariate in the model and not in the model
par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)    
E1 <- resid(M0$lme, type ="n")
F1 <- fitted(M0$lme)
plot(x=F1, y=E1, xlab = "Fitted values", ylab ="Residuals")
abline(h=0, lty=2)

plot(x=df_perch$mean_last_7days, y = E1, xlab = "Temp", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$fProtocol, y = E1, xlab = "Protocol", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$Depth_sample, y = E1, xlab = "Depth", ylab = "Residuals")
abline(h=0, lty=2)

#3. verifiy independence when multiple measurements were taken over time: auto-correlation
#not the case here

#4.verifiy normality: histogram of residuals

par(mfrow = c(1,2), mar = c(5, 5, 2, 2))
hist(E1, main = "", xlab = "Residuals")
qqnorm(E1, main = "")
qqline(E1)

#no

#5. check for influental observations: cook distance values
par(mfrow = c(1,1), mar = c( 5, 5, 2, 2))
plot(cooks.distance(M0$gam), xlim = c(0,10000), 
     ylim = c(0,1), xlab = "Observations", 
     ylab = " Cooks distance values")
abline(h = 1, lwd = 2, lty = 2)

#not working!

#6.if repeated measurements taken from the same site, check for patterns
#not the case

# #overdispersion

E1 <- resid(M0$gam, type = "pearson")
sum(E1^2)/M0$gam$df.residual 

#this is not a good model

#model validation 
# Residuals should be plotted against
# 1. fitted values.
# 2. predictor variables (those included and those dropped)

# The key assumptions are: https://statistique.cuso.ch/fileadmin/statistique/part-3.pdf
# 1. The assumed mean variance relationship is correct, so that
# scaled residuals have constant variance.
# 2. The response data are independent, so that the residuals
# appear approximately so.

#1. homogeneity of residuals: plot residuals vs. fitted values, based on gamm book
fv <- fitted(M0$lme, type = "n") ##predicted values
rsd <- resid(M0$lme) ##residuals
plot(x = fv, y = rsd, xlab = "Fitted values", ylab = "Residuals")

#2 
plot(x = df_perch$mean_last_7days, y = rsd, xlab = "Temp", ylab = "Residuals")
plot(x = df_perch$fLake, y = rsd, xlab = "Lake", ylab = "Residuals")
boxplot(rsd ~ fLake, data = df_perch)

#distribution checking
rsd <- residuals(M0$lme)
qq.gam(M0$gam,rep=100); plot(fitted(M0$lme),rsd)
plot(df_perch$mean_last_7days,rsd); plot(df_perch$mean_last_7days,rsd)

#code from GAMM book
par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)    
E1 <- resid(M0$lme, type ="n")
F1 <- fitted(M0$lme)
plot(x=F1, y=E1, xlab = "Fitted values", ylab ="Residuals")
abline(h=0, lty=2)

plot(x=df_perch$mean_last_7days, y = E1, xlab = "Temp", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$fLake, y = E1, xlab = "Lake", ylab = "Residuals")
abline(h=0, lty=2)

boxplot(E1 ~ fLake, data = df_perch)





###########################################################################



############################################

#2.1 overdispersion

#fitting model, poisson first, Lakes as random effect
#k is 10, method is default ML
M1 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days, bs = "re"), 
           random = list(fLake =~ 1), family = poisson)

plot(M1$gam)
summary(M1$gam) #R² can be used as model selection (as AIC), 0.00462 
anova(M1$gam)
summary(M1$lme)

gam.check(M1$gam)

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
AIC(M0$lme, M1$lme)
#M0 is better
#how many percent of abundance data are 0?

table(df_perch$Abundance)
length(df_perch$Abundance)
#percent of zeros: 59.32324 %
100 /26627 * 15796

#try negbin and zero-inflated
#is it overdispersion due to the many zeros? or is it because there is such a big
#variation in the data ? (page 160), biological choice

M2 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days, k = 3), 
           random = list(fLake =~ 1), family = negbin(1))

plot(M2$gam)
summary(M2$gam) #R² can be used as model selection (as AIC): 0.00527 
anova(M2$gam)
summary(M2$lme)

#gamm help from R
fv <- exp(fitted(M2$lme)) ## predicted values (including re)
rsd <- (M2$gam$y - fv)/sqrt(fv) ## Pearson residuals (Poisson case)
op <- par(mfrow=c(1,2))
qqnorm(rsd);plot(fv^.5,rsd)
par(op) 

rsd <- residuals(M2$gam)
qq.gam(M2$gam,rep=100); plot(fitted(M2$lme),rsd)

E2 <- resid(M2$gam, type = "pearson")
sum(E2^2)/M2$gam$df.residual #less overdispersion: 2.600412

AIC(M0$lme, M1$lme, M2$lme) #model 0 seems to be best based on AIC
#probably different for other species?

anova(M0$lme, M1$lme, M2$lme)

#zero-inflated model, not working with gamm() or gamm4()
#error
# You can't use these extended families outside of gam() or bam() from the mgcv package.
# With gamm4 you're stuck with the families that lmer() and glmer() 
# from the lme4 package supports.
# Options would be to use the gamlss package for ZIP models 
# as I don't think it has a Tweedie family, or use the glmmTMB package 
# which has lots of options, including tw() for a Tweedie family, however 
# I don't believe it understands smooths so you'd need to convert your 
# model into a form that can be fitted as a mixed effects model.
# # Another option, which is likely to be inefficient but which you 
# can use to get started with immediately is to use the "re" basis spline 
# with gam() or bam() and create a factor or name nested in party to use
# as the random effect, or even just s(name, party, bs = 're') might do it, 
# assuming both are factors, but do the help for this basis to make sure 
# I'm interpreting what you intend for the random effect.
#https://stats.stackexchange.com/questions/400444/using-gamm4-on-zero-inflated-count-data-with-tweedie-or-zero-inflated-poisson-di

#With gamm4::gamm4() you are limited to the families supported by lme4::glmer()
#https://stats.stackexchange.com/questions/550849/gamm-with-betarlink-logit

#dropping zero-inflated ones

# M3 <- gamm4(data = df_perch, Abundance ~ s(mean_last_7days), random =~ (1 | fLake), 
#             family = ziP())

# library(gamlss) #probably not useful because GAM only, but lots of distributions
# library(glmmTMB) #GLMM only 

#textbook uses MCMC

##should random effects look like this in code?
#k-value?
#method for smoothing
#spline?

#a lot of zeros, zero-inflated model not possible with frequentist approach
#lets use negbin

######################################## negbin party
N1 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days), 
           random = list(fLake =~ 1), family = negbin(1))

plot(N1$gam)
summary(N1$gam) #R² can be used as model selection (as AIC): 0.00527 
anova(N1$gam)
summary(N1$lme)

###model validation
#from online https://statistique.cuso.ch/fileadmin/statistique/part-3.pdf
# and https://www.maths.ed.ac.uk/~swood34/mgcv/check-select.pdf
#double check
#how can I interpret these residuals?
#Residuals should be plotted against
# 1. fitted values.
# 2. predictor variables (those included and those dropped).
# 3. time, if the data are temporal.
gam.check(N1$gam) #not precise, care required

rsd <- residuals(N1$gam)
fit <- 

qq.gam(N1$gam,rep=100); plot(fitted(N1$lme),rsd)

#lower k
N2 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days, k = 3), 
           random = list(fLake =~ 1), family = negbin(1))

b <- gam(data = df_perch, Abundance ~ s(mean_last_7days, k = 3), family = ziP())
gam.check(b)
anova(b)
E1 <- residuals(b)
F1 <- fitted(b)
plot(x = F1, y = E1)
hist(E1)
ggnorm(E1)

#comparing models
AIC(N1$lme, N2$lme, b)
anova(N1$lme, N2$lme)


######################################### poisson party

#fitting model, poisson first, Lakes as random effect
P1 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days), 
           random = list(fLake =~ 1), family = poisson)

plot(P1$gam)
summary(P1$gam) #R² can be used as model selection (as AIC), 0.00462 
anova(P1$gam)
summary(P1$lme)

gam.check(P1$gam)


##model validation following  Zuur A beginners guide to GAMs p.22 and GAMMs p. 52

E1 <- resid(P1$lme, type ="n")
F1 <- fitted(P1$lme)

plot(x = F1, y = E1, xlab = "fitted values", ylab = "residuals")
abline(h = 0)

hist(E1)

# fv <- exp(fitted(P1$lme)) ## predicted values (including re)
# rsd <- (P1$gam$y - fv)/sqrt(fv) ## Pearson residuals (Poisson case)
# op <- par(mfrow=c(1,2))
# qqnorm(rsd);plot(fv^.5,rsd)
# par(op) 
# #MISSING! validaiton following p. 52
# 
# # par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)    
# E1 <- resid(P1$lme, type ="n")
# F1 <- fitted(P1$lme)
# plot(x=F1, y=E1, xlab = "Fitted values", ylab ="Residuals")
# abline(h=0, lty=2)
# 
# plot(x=df_perch$mean_last_7days, y = E1, xlab = "Year", ylab = "Residuals")
# abline(h=0, lty=2)
# 
# plot(x=PB$DayInYear, y = E1, xlab = "Day in Year", ylab = "Residuals")
# abline(h=0, lty=2)
# 
# boxplot(E1 ~ Season, data = PB)


#from online https://statistique.cuso.ch/fileadmin/statistique/part-3.pdf
#double check
#how can I interpret these residuals?
#Residuals should be plotted against
# 1. fitted values.
# 2. predictor variables (those included and those dropped).
# 3. time, if the data are temporal.

rsd <- residuals(P1$gam)
qq.gam(P1$gam,rep=100); plot(fitted(P1$lme),rsd)
plot(df_perch$mean_last_7days,rsd)

#overdispersion
E1 <- resid(M1$gam, type = "pearson")
sum(E1^2)/M1$gam$df.residual 


#Second model for poisson with bs = "re", as suggested by gamm()
P2 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days, bs = "re"), 
           random = list(fLake =~ 1), family = poisson)

plot(P2$gam)
summary(P2$gam) #R² can be used as model selection (as AIC), 0.00462 
anova(P2$gam)
summary(P2$lme)

gam.check(P2$gam)

fv_p2 <- exp(fitted(P2$lme)) ## predicted values (including re)
rsd_p2 <- (P2$gam$y - fv)/sqrt(fv_p2) ## Pearson residuals (Poisson case)
op_p2 <- par(mfrow=c(1,2))
qqnorm(rsd_p2);plot(fv_p2^.5,rsd_p2)
par(op_p2)




#also try gamm4()

