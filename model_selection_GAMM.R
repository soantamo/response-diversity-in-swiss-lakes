##model selection
library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)

#do not forget
##should random effects look like this in code?
#k-value?
#method for smoothing
#spline?

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

plot(M0$gam) #wiggly af, k must be lower than 10
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

#this is not a good model! next poisson

###########################################################################
M1 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days), 
           random = list(fLake =~ 1), family = poisson)

plot(M1$gam) 
summary(M1$gam) 
anova(M1$gam)
summary(M1$lme)
gam.check(M1$gam)

anova(M0$lme, M1$lme)
AIC(M0$lme, M1$lme) #better AIC for model one, but residuals look bad there


#MODEL VALIDATION#
#Synthesis of pdf, GAMM book and GAM book
#1.verifiy homogeneity: rsd vs fitted
fv <- fitted(M1$lme, type = "n") ##predicted values
rsd <- resid(M1$lme) ##residuals
plot(x = fv, y = rsd, xlab = "Fitted values", ylab = "Residuals")

#no

#2.verify model misfit (or independence): rsd vs each covariate in the model and not in the model
par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)    
E1 <- resid(M1$lme, type ="n")
F1 <- fitted(M1$lme)
plot(x=F1, y=E1, xlab = "Fitted values", ylab ="Residuals")
abline(h=0, lty=2)

plot(x=df_perch$mean_last_7days, y = E1, xlab = "Temp", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$fProtocol, y = E1, xlab = "Protocol", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$Depth_sample, y = E1, xlab = "Depth", ylab = "Residuals")
abline(h=0, lty=2)

#looks a bit better

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
plot(cooks.distance(M1$gam), xlim = c(0,10000), 
     ylim = c(0,1), xlab = "Observations", 
     ylab = " Cooks distance values")
abline(h = 1, lwd = 2, lty = 2)

#not working!

#6.if repeated measurements taken from the same site, check for patterns
#not the case

# #overdispersion

E1 <- resid(M1$gam, type = "pearson")
sum(E1^2)/M1$gam$df.residual 

#higher compared to gaussian

#how many percent of abundance data are 0?
table(df_perch$Abundance)
length(df_perch$Abundance)
#percent of zeros: 59.32324 %
100 /26627 * 15796

#try negbin and zero-inflated
#is it overdispersion due to the many zeros? or is it because there is such a big
#variation in the data ? (page 160), biological choice

############################################

#try negbin with GAMM, theta?

M2 <- gamm(data = df_perch, Abundance ~ s(mean_last_7days, k = 3), 
           random = list(fLake =~ 1), family = negbin(1))

plot(M2$gam)
summary(M2$gam) 
anova(M2$gam)
summary(M2$lme)

anova(M0$lme, M1$lme, M2$lme)
AIC(M0$lme, M1$lme, M2$lme) #AIC at least better than model 1, still worse than M0


#MODEL VALIDATION#
#Synthesis of pdf, GAMM book and GAM book
#1.verifiy homogeneity: rsd vs fitted
fv <- fitted(M2$lme, type = "n") ##predicted values
rsd <- resid(M2$lme) ##residuals
plot(x = fv, y = rsd, xlab = "Fitted values", ylab = "Residuals")

#no

#2.verify model misfit (or independence): rsd vs each covariate in the model and not in the model
par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)    
E1 <- resid(M2$lme, type ="n")
F1 <- fitted(M2$lme)
plot(x=F1, y=E1, xlab = "Fitted values", ylab ="Residuals")
abline(h=0, lty=2)

plot(x=df_perch$mean_last_7days, y = E1, xlab = "Temp", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$fProtocol, y = E1, xlab = "Protocol", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$Depth_sample, y = E1, xlab = "Depth", ylab = "Residuals")
abline(h=0, lty=2)

#looks a bit better

#3. verifiy independence when multiple measurements were taken over time: auto-correlation
#not the case here

#4.verifiy normality: histogram of residuals

par(mfrow = c(1,2), mar = c(5, 5, 2, 2))
hist(E1, main = "", xlab = "Residuals", ylim = c(0,30))
qqnorm(E1, main = "")
qqline(E1)

#no

#5. check for influental observations: cook distance values
par(mfrow = c(1,1), mar = c( 5, 5, 2, 2))
plot(cooks.distance(M2$gam), xlim = c(0,10000), 
     ylim = c(0,1), xlab = "Observations", 
     ylab = " Cooks distance values")
abline(h = 1, lwd = 2, lty = 2)

#not working!

#6.if repeated measurements taken from the same site, check for patterns
#not the case

# #overdispersion

E1 <- resid(M2$gam, type = "pearson")
sum(E1^2)/M2$gam$df.residual 

#lower compared to poisson



#also try gamm4() for negbin

#idee: either do negbin GAMM and find good model or try gam with ziP()
###############################################################################
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
# library(gamlss) #probably not useful because GAM only, but lots of distributions
# library(glmmTMB) #GLMM only 
#textbook uses MCMC

#option to do GAM and use fLake as second smoothing factor with "re"
#negbin zero-inflated would also be possible here

M3 <- gam(Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're'),
          family = ziP(), data = df_perch)


#https://stats.stackexchange.com/questions/495775/first-derivative-of-fitted-gam-changes-according-to-specified-model-distribution

plot.gam(M3)

derivatives <- derivatives(M3) #derivatives works!!

plot(M3)
summary(M3) 
anova(M3)

anova(M0$lme, M1$lme, M2$lme, M3)
AIC(M0$lme, M1$lme, M2$lme, M3) #M3 lowest


#MODEL VALIDATION#
#Synthesis of pdf, GAMM book and GAM book
#1.verifiy homogeneity: rsd vs fitted
fv <- fitted(M3) ##predicted values
rsd <- resid(M3) ##residuals
plot(x = fv, y = rsd, xlab = "Fitted values", ylab = "Residuals")

#naja

#2.verify model misfit (or independence): rsd vs each covariate in the model and not in the model
par(mfrow = c(2,2), mar = c(5,5,2,2), cex.lab = 1.5)    
E1 <- resid(M3)
F1 <- fitted(M3)
plot(x=F1, y=E1, xlab = "Fitted values", ylab ="Residuals")
abline(h=0, lty=2)

plot(x=df_perch$mean_last_7days, y = E1, xlab = "Temp", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$fProtocol, y = E1, xlab = "Protocol", ylab = "Residuals")
abline(h=0, lty=2)

plot(x=df_perch$Depth_sample, y = E1, xlab = "Depth", ylab = "Residuals")
abline(h=0, lty=2)

#not bad I guess

#3. verifiy independence when multiple measurements were taken over time: auto-correlation
#not the case here

#4.verifiy normality: histogram of residuals

par(mfrow = c(1,2), mar = c(5, 5, 2, 2))
hist(E1, main = "", xlab = "Residuals", ylim = c(0,20))
qqnorm(E1, main = "")
qqline(E1)

#looks better

#5. check for influental observations: cook distance values

# par(mfrow = c(1,1), mar = c( 5, 5, 2, 2))
# plot(cooks.distance(M3), xlim = c(0,10000), 
#      ylim = c(0,1), xlab = "Observations", 
#      ylab = " Cooks distance values")
# abline(h = 1, lwd = 2, lty = 2)

#not working!

#6.if repeated measurements taken from the same site, check for patterns
#not the case

# #overdispersion

E1 <- resid(M3, type = "pearson")
sum(E1^2)/M3$df.residual

#lowest! 1.06

#derivatives work -> use M3 as basis for the model

#single occurrences is a problem though

single_occurrences <- df_final |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(Lakes == 1)

one_fish_one_lake <- df_final %>%
  group_by(Lake, Species) %>%
  summarize(TotalAbundance = sum(Abundance)) |> 
  filter(TotalAbundance == 1)

species <- df_final |> 
  distinct(Species)

#how many species do only have binary data?

test <- df_final |> 
  filter(Abundance > 1) |> 
  group_by(Species) |>
  count()

#this will get complicated, yey


