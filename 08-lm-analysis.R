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

# select the ones we use
lake_selection <- lake_info |> 
  select(Lake, Phos_max, Max_depth)

# information about endemism
species_endemism <- read_excel("species_endemism_richness.xlsx") |> 
  rename(endemism = detail_category)

# exclude lepomis gibbosus
# information about endemism
species_endemism_short <- species_endemism |> 
  filter(species != "Lepomis_gibbosus") |> 
  select(-sum_species) |> 
  group_by(fLake) |> 
  mutate(sum_species = sum(num_species))

species_endemism_short$endemism <- as.factor(species_endemism_short$endemism)

str(species_endemism_short)

# adding number of endemic and native etc species.
df_species_endemism <- species_endemism_short |> 
  select(-species, - num_species) |> 
  add_count(fLake, endemism) |> 
  rename(count = n) |> 
  distinct(fLake, count, sum_species, endemism)

str(df_species_endemism)

df_species_endemism$count <- as.numeric(df_species_endemism$count)

# making it wide and adding percentages
# Regional endemism is often characterised by the proportion of endemics: 
#   the number of endemic taxa (E) divided by the total number 
# of taxa (S) [as a percentage: (E × 100 %)/S]. 
# Endemism in Vascular Plants pp 11–48

df_species_endemism_long <- df_species_endemism |> 
  pivot_wider(names_from = endemism, values_from = count) |> 
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) |> 
  rename(Lake = fLake)
  # mutate(perc_endemism = sum(endemic * 100 / sum_species)) |> 
  # mutate(perc_non_native = sum(non_native * 100 / sum_species)) |> 
  # mutate(perc_non_endemic_native = sum(non_endemic_native * 100 / sum_species)) |>

str(df_species_endemism_long)

# merge endemism info and lake info 

endemism_lake <- merge(df_species_endemism_long, lake_selection)

# resp diversity per lake df
resp_div_all <- readRDS("total_models/resp_div_all.rds") |> 
  select(temp:fLake)

# only the columns we need
rdiv <- resp_div_all |>  
  select(temp, fLake, rdiv, sign) |>
  drop_na()

# calculate mean values and rename Lake
df_rdiv <- rdiv |> 
  group_by(fLake) |> 
  mutate(mean_rdiv = mean(rdiv)) |> 
  mutate(max_rdiv = max(rdiv)) |> 
  mutate(mean_sign = mean(sign)) |> 
  mutate(max_sign = max(sign)) |> 
  distinct(fLake, mean_rdiv, max_rdiv, mean_sign, max_sign) |> 
  rename(Lake = fLake)

# combine endemism and rdiv

df_lm <- merge(df_rdiv, endemism_lake)

str(df_lm)

df_lm$Phos_max <- as.numeric(df_lm$Phos_max)

###############################################################################

# lm(mean_rdiv  ~ species richness + eutrophication)
# predict manually with new data eutrophication value fixed at mean and species richness min() to max()
# also add one with max_rdiv
# do the same the other way around: mean_rdiv  ~  eutrophication + species richness, with species richness fixed at mean and eutrophication min() to max()species richness +
#   prepare these two plots

df_lm_excl <- df_lm |> 
  filter(Lake != "Poschiavo")

# overview plot


df_lm_excl |> 
  ggplot(aes(Phos_max, sum_species)) +
  geom_point() +
  geom_text_repel(aes(label = Lake),
                  size = 3.5,
                  max.overlaps = 13)

library(ggrepel)
 geom_text_repel(aes(label = Lake),
                                   size = 3.5,
                                   max.overlaps = 13) +
  labs(x = "mean dissimilarity", y = "mean divergence") +
  theme_bw(base_size = 16)

plot_means2



#   prepare these two plots

df_lm_excl <- df_lm |> 
  filter(Lake != "Poschiavo")

lm1 <- lm(mean_rdiv ~ sum_species + Phos_max, data = df_lm_excl)

# checking model assumptions
summary(lm1)
shapiro.test(resid(lm1))
lmtest::bptest(lm1)
plot(lm1)

# looking good

new_data <- tibble(sum_species = seq(from = min(df_lm_excl$sum_species), to = max(df_lm_excl$sum_species),
                                    length = 50),
                  Phos_max = mean(df_lm_excl$Phos_max))

prediction_lm1 <- predict.lm(lm1, newdata = new_data, se.fit = TRUE, type = "response")

bind_model1 <- cbind(new_data, prediction_lm1)



lm1_plot <- bind_model1 |> 
  ggplot(aes(sum_species, fit)) +
  geom_line(color = "#E08214") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw(base_size = 20) +
  ylab("mean dissimilarity") +
  xlab("species richness")

lm1_plot

# change it
lm2 <- lm(mean_rdiv ~ Phos_max + sum_species, data = df_lm_excl)

# checking model assumptions: super!
summary(lm2)
shapiro.test(resid(lm2))
lmtest::bptest(lm2)
plot(lm2)

new_data2 <- tibble(Phos_max = seq(from = min(df_lm_excl$Phos_max), to = max(df_lm_excl$Phos_max),
                                     length = 50),
                   sum_species = mean(df_lm_excl$sum_species))

prediction_lm2 <- predict.lm(lm2, newdata = new_data2, se.fit = TRUE, type = "response")

bind_model2 <- cbind(new_data2, prediction_lm2)



lm2_plot <- bind_model2 |> 
  ggplot(aes(Phos_max, fit)) +
  geom_line(color = "#35978F") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw(base_size = 20) +
  ylab("mean dissimilarity") +
  xlab("maxmimum historical eutrophication")
  ylim(4,7)

lm2_plot

library(ggpubr)
plot2 <- ggarrange(lm1_plot, lm2_plot)

tiff(paste("total_models/plots/lm_species_richness_eutroph.tiff", sep = ""), units="in", width=12, height=5, res=300)


plot(plot2)

# Closing the graphical device
dev.off()


# maximum dissimilarity


lm3 <- lm(max_rdiv ~ sum_species + Phos_max, data = df_lm_excl)

# checking model assumptions
summary(lm3)
shapiro.test(resid(lm3))
lmtest::bptest(lm3)
plot(lm3)

# looking good

new_data3 <- tibble(sum_species = seq(from = min(df_lm_excl$sum_species), to = max(df_lm_excl$sum_species),
                                     length = 50),
                   Phos_max = mean(df_lm_excl$Phos_max))

prediction_lm3 <- predict.lm(lm3, newdata = new_data3, se.fit = TRUE, type = "response")

bind_model3 <- cbind(new_data3, prediction_lm3)



lm3_plot <- bind_model3 |> 
  ggplot(aes(sum_species, fit)) +
  geom_line(color = "#E08214") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw() +
  ylab("maximum dissimilarity") +
  xlab("species richness")

lm3_plot

# change it
lm4 <- lm(max_rdiv ~ Phos_max + sum_species, data = df_lm_excl)

# checking model assumptions: super!
summary(lm4)
shapiro.test(resid(lm4))
lmtest::bptest(lm4)
plot(lm4)

new_data4 <- tibble(Phos_max = seq(from = min(df_lm_excl$Phos_max), to = max(df_lm_excl$Phos_max),
                                   length = 50),
                    sum_species = mean(df_lm_excl$sum_species))

prediction_lm4 <- predict.lm(lm4, newdata = new_data4, se.fit = TRUE, type = "response")

bind_model4 <- cbind(new_data4, prediction_lm4)



lm4_plot <- bind_model4 |> 
  ggplot(aes(Phos_max, fit)) +
  geom_line(color = "#35978F") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw() +
  ylab("maximum dissimilarity") +
  xlab("maxmimum historical eutrophication")
  ylim(4,7)

lm4_plot

ggarrange(lm1_plot, lm2_plot, lm3_plot, lm4_plot)

lm1_plot + lm3_plot
lm2_plot + lm4_plot

# divergence


lm_sign1 <- lm(mean_sign ~ sum_species + Phos_max, data = df_lm_excl)

# checking model assumptions
summary(lm_sign1)
shapiro.test(resid(lm_sign1))
lmtest::bptest(lm_sign1)
plot(lm_sign1)

# looking good

new_data_sign1 <- tibble(sum_species = seq(from = min(df_lm_excl$sum_species), to = max(df_lm_excl$sum_species),
                                     length = 50),
                   Phos_max = mean(df_lm_excl$Phos_max))

prediction_sign1 <- predict.lm(lm_sign1, newdata = new_data_sign1, se.fit = TRUE, type = "response")

bind_model_s1 <- cbind(new_data_sign1, prediction_sign1)



sign1_plot <- bind_model_s1 |> 
  ggplot(aes(sum_species, fit)) +
  geom_line(color = "#E08214") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw() +
  ylab("mean divergence") +
  xlab("species richness")

sign1_plot



# change it
sign2 <- lm(mean_sign ~ Phos_max + sum_species, data = df_lm_excl)


new_data_sign2 <- tibble(Phos_max = seq(from = min(df_lm_excl$Phos_max), to = max(df_lm_excl$Phos_max),
                                   length = 50),
                    sum_species = mean(df_lm_excl$sum_species))

prediction_sign2 <- predict.lm(sign2, newdata = new_data_sign2, se.fit = TRUE, type = "response")

bind_model_sign2 <- cbind(new_data_sign2, prediction_sign2)


sign2_plot <- bind_model_sign2 |> 
  ggplot(aes(Phos_max, fit)) +
  geom_line(color = "#35978F") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw() +
  ylab("mean divergence") +
  xlab("maxmimum historical eutrophication")
ylim(4,7)

sign2_plot

# 
# lm_s_endemic <- lm(mean_sign ~ endemic, data = df_lm)
# 
# # checking model assumptions
# # summary(lm_endemic)
# # shapiro.test(resid(lm_endemic))
# # lmtest::bptest(lm_endemic)
# # plot(lm_endemic)
# 
# # looking good
# 
# new_data_1 <- tibble(endemic = seq(from = min(df_lm$endemic), to = max(df_lm$endemic),
#                                    length = 30))
# 
# prediction_endemic1 <- predict.lm(lm_s_endemic, newdata = new_data_1, se.fit = TRUE, type = "response")
# 
# df_lm_s_endemic <- cbind(new_data_1, prediction_endemic1)
# 
# 
# 
# plot1 <- df_lm_s_endemic |> 
#   ggplot(aes(endemic, fit)) +
#   geom_line(color = "#E08214") +
#   geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
#   theme_bw() +
#   ylab("mean divergence") +
#   xlab("Number of endemic species")
# # ylim(4,7.5)
# 
# 
# plot1
###########################################################################
# lms categories: endemic, non_native (as in not swiss) and non_endemic_native


lm_endemic <- lm(mean_rdiv ~ endemic, data = df_lm)

# checking model assumptions
summary(lm_endemic)
shapiro.test(resid(lm_endemic))
lmtest::bptest(lm_endemic)
plot(lm_endemic)

# looking good

new_data_a <- tibble(endemic = seq(from = min(df_lm$endemic), to = max(df_lm$endemic),
                                     length = 15))

prediction_endemic <- predict.lm(lm_endemic, newdata = new_data_a, se.fit = TRUE, type = "response")

df_lm_endemic <- cbind(new_data_a, prediction_endemic)



plot_a <- df_lm_endemic |> 
  ggplot(aes(endemic, fit)) +
  # geom_line(color = "#0097A7") +
  geom_line(color = "#512DA8") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  ylab("mean dissimilarity") +
  xlab("number of endemic species") +
  theme_bw(base_size = 20) +
  ylim(0, 3.2)

plot_a



tiff(paste("total_models/plots/category_endemic.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(plot_a)

# Closing the graphical device
dev.off()

# non_native

lm_nn <- lm(mean_rdiv ~ non_native, data = df_lm)

# checking model assumptions: okay
summary(lm_nn)
shapiro.test(resid(lm_nn))
lmtest::bptest(lm_nn)
plot(lm_nn)

# looking good

new_data_b <- tibble(non_native = seq(from = min(df_lm$non_native), to = max(df_lm$non_native),
                                   length = 15))

prediction_nn <- predict.lm(lm_nn, newdata = new_data_b, se.fit = TRUE, type = "response")

df_lm_nn <- cbind(new_data_b, prediction_nn)



b <- df_lm_nn |> 
  ggplot(aes(non_native, fit)) +
  # geom_line(color = "#E64A19") +
  geom_line(color = "#512DA8") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw() +
  ylab("mean dissimilarity") +
  xlab("number of non-native species") +
  ylim(1,3.2) +
  theme_bw(base_size = 20)

b


tiff(paste("total_models/plots/category_non_native.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(b)

# Closing the graphical device
dev.off()
b
# non endemic native

lm_nne <- lm(mean_rdiv ~ native, data = df_lm)

# checking model assumptions: okay
summary(lm_nne)
shapiro.test(resid(lm_nne))
lmtest::bptest(lm_nne)
plot(lm_nne)

# looking good

new_data_c <- tibble(native = seq(from = min(df_lm$native), to = max(df_lm$native),
                                      length = 15))

prediction_nne <- predict.lm(lm_nne, newdata = new_data_c, se.fit = TRUE, type = "response")

df_lm_nne <- cbind(new_data_c, prediction_nne)



plot_c <- df_lm_nne|> 
  ggplot(aes(native, fit)) +
  geom_line(color = "#512DA8") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw() +
  ylab("mean dissimilarity") +
  xlab("number of non-endemic native species") +
  ylim(1,3.2) +
  theme_bw(base_size = 20)

plot_c



tiff(paste("total_models/plots/category_non_endemic_native.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(plot_c)

# Closing the graphical device
dev.off()

ggarrange(plot_a, plot_b, plot_c)

plot_a + plot_b + plot_c


# translocated species, non-native in synthesis report

lm_trans <- lm(mean_rdiv ~ non_native_region, data = df_lm)

# checking model assumptions: okay
summary(lm_trans)
shapiro.test(resid(lm_trans))
lmtest::bptest(lm_trans)
plot(lm_trans)

# looking good

new_data_d <- tibble(non_native_region = seq(from = min(df_lm$non_native_region), to = max(df_lm$non_native_region),
                                              length = 15))

prediction_trans <- predict.lm(lm_trans, newdata = new_data_d, se.fit = TRUE, type = "response")

df_lm_trans <- cbind(new_data_d, prediction_trans)



plot_d <- df_lm_trans|> 
  ggplot(aes(non_native_region, fit)) +
  geom_line(color = "#9E9D24") +
  geom_line(color = "#512DA8") +
  geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)),  alpha = 0.1) +
  theme_bw() +
  ylab("mean dissimilarity") +
  xlab("number of species regionally translocated to the lake") +
  ylim(1,3.2) +
  theme_bw(base_size = 20)

plot_d

library(ggpubr)
ggarrange(plot_a, b, plot_c, plot_d)

tiff(paste("total_models/plots/category_translocated.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(plot_d)

# Closing the graphical device
dev.off()

library(ggpubr)

plot_all <- ggarrange(plot_a, b, plot_c, plot_d)

tiff(paste("total_models/plots/all_categories.tiff", sep = ""), units="in", width=15, height=10, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(plot_all)

# Closing the graphical device
dev.off()
#################################################################################



# partial regression

source(here("functions_regression.R"))

# library(sjPlot)
# library(sjmisc)
# model <- lm(mean_rdiv ~ sum_species + Max_depth, data = df_lm)
# plot_model(model, type = "pred", terms = c("sum_species", "Max_depth"))
# 

# species richness
# model assumptions only need to be checked for either a or b

# model assumptions okay
a1 <- partial_regression_x1(df_lm$mean_rdiv, df_lm$sum_species, df_lm$Max_depth, df =  df_lm) +
  labs(title = "Species richness") +
  xlab(" Species richness \n (x1 given others)") +
  ylab(" mean_rdiv \n (y given others)")

b1 <- partial_regression_x2(df_lm$mean_rdiv, df_lm$sum_species, df_lm$Max_depth, df =  df_lm) +
  labs(title = "") +
  xlab(" Depth_max \n (x2 given others)") +
  ylab(" mean_rdiv \n (y given others)")


tiff(paste("total_models/plots/partial_species_richness.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(a1 + b1)

# Closing the graphical device
dev.off()

# native

# x1 against x2 not normally distributed, resid vs fitted x2 against x1 looks bad
a2 <- partial_regression_x1(df_lm$mean_rdiv, df_lm$native, df_lm$Max_depth, df =  df_lm) +
  labs(title = "Native") +
  xlab(" Native \n (x1 given others)") +
  ylab(" mean_rdiv \n (y given others)")


b2 <- partial_regression_x2(df_lm$mean_rdiv, df_lm$native, df_lm$Max_depth, df =  df_lm) +
  labs(title = "") +
  xlab(" Depth_max \n (x2 given others)") +
  ylab(" mean_rdiv \n (y given others)")

# a2 + b2

tiff(paste("total_models/plots/partial_native.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(a2 + b2)

# Closing the graphical device
dev.off()

# endemic

# x2 against x1 no homodeicasity, rest okay
a3 <- partial_regression_x1(df_lm$mean_rdiv, df_lm$endemic, df_lm$Max_depth, df =  df_lm) +
  labs(title = "Endemic") +
  xlab(" Endemic \n (x1 given others)") +
  ylab(" mean_rdiv \n (y given others)")


b3 <- partial_regression_x2(df_lm$mean_rdiv, df_lm$endemic, df_lm$Max_depth, df =  df_lm) +
  labs(title = "") +
  xlab(" Depth_max \n (x2 given others)") +
  ylab(" mean_rdiv \n (y given others)")



tiff(paste("total_models/plots/partial_endemic.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(a3 + b3)

# Closing the graphical device
dev.off()

# non_native
# model assumptions okay
a4 <- partial_regression_x1(df_lm$mean_rdiv, df_lm$non_native, df_lm$Max_depth, df =  df_lm) +
  labs(title = "Non-native") +
  xlab(" Non_native \n (x1 given others)") +
  ylab(" mean_rdiv \n (y given others)")

b4 <- partial_regression_x2(df_lm$mean_rdiv, df_lm$non_native, df_lm$Max_depth, df =  df_lm) +
  labs(title = "") +
  xlab(" Depth_max \n (x2 given others)") +
  ylab(" mean_rdiv \n (y given others)")
# 
# a4 + b4

tiff(paste("total_models/plots/partial_non_native.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(a4 + b4)

# Closing the graphical device
dev.off()

# exotic
# model assumptions are okay 
a5 <- partial_regression_x1(df_lm$mean_rdiv, df_lm$exotic, df_lm$Max_depth, df =  df_lm) +
  labs(title = "Exotic") +
  xlab(" Exotic \n (x1 given others)") +
  ylab(" mean_rdiv \n (y given others)")

b5 <- partial_regression_x2(df_lm$mean_rdiv, df_lm$exotic, df_lm$Max_depth, df =  df_lm) +
  labs(title = "") +
  xlab(" Depth_max \n (x2 given others)") +
  ylab(" mean_rdiv \n (y given others)")

# a5 + b5


tiff(paste("total_models/plots/partial_exotic.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(a5 + b5)

# Closing the graphical device
dev.off()

# eutrophication
# poschiavo has a missing phos max value

df_lm_excl <- df_lm |> 
  filter(Lake != "Poschiavo")

# model assumptions are okay

a6 <- partial_regression_x1(df_lm_excl$mean_rdiv, df_lm_excl$Phos_max, df_lm_excl$Max_depth, df =  df_lm_excl) +
  labs(title = "Eutrophication history") +
  xlab(" Phos_max \n (x1 given others)") +
  ylab(" mean_rdiv \n (y given others)")

b6 <- partial_regression_x2(df_lm_excl$mean_rdiv, df_lm_excl$Phos_max, df_lm_excl$Max_depth, df =  df_lm_excl) +
  labs(title = "") +
  xlab(" Depth_max \n (x2 given others)") +
  ylab(" mean_rdiv \n (y given others)")



tiff(paste("total_models/plots/partial_eutrophication.tiff", sep = ""), units="in", width=12, height=7, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(a6 + b6)

# Closing the graphical device
dev.off()


c <- partial_regression_x1(df_lm_excl$mean_rdiv, df_lm_excl$Phos_max, df_lm_excl$sum_species, df =  df_lm_excl) +
  labs(title = "Eutrophication history") +
  xlab(" Phos_max \n (x1 given others)") +
  ylab(" mean_rdiv \n (y given others)")
c

d <- partial_regression_x2(df_lm_excl$mean_rdiv, df_lm_excl$Phos_max, df_lm_excl$sum_species, df =  df_lm_excl) +
  labs(title = "") +
  xlab(" Depth_max \n (x2 given others)") +
  ylab(" mean_rdiv \n (y given others)")


# #######################################################################



lm_analysis <- function(y, x) {
  
  mod <- lm(y ~ x, data = df_lm)
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


# plot residuals 

models <- lm(mean_rdiv ~ Phos_max, data = df_lm)
model1 <- lm(mean_rdiv ~ Phos_max )

plot_model(models)

res <- resid(models)

plot(res)

lm <- lm(mean_rdiv ~ Phos_max + sum_species, data = df_lm_excl)
summary(lm)
library(sjPlot)

model1 <- lm(mean_rdiv ~ Phos_max * sum_species, data = df_lm_excl)
summary(model1)
plot_model(model1, type = "pred", terms = c("Phos_max", "endemic"))
# maximum_depth:

depth1 <- lm_analysis(df_lm$mean_rdiv, df_lm$Max_depth)  +
  labs(x = "maximum_depth", y = "mean_dissimilarity")
# 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
# depth2 <- lm_analysis(df_lm$max_rdiv, df_lm$Max_depth) +
#   labs(x = "maximum_depth", y = "max_dissimilarity")
# 1: naja, 2: ok, 3: naja mit pattern, test ok, 4: ok -> ?
depth3 <-lm_analysis(df_lm$mean_sign, df_lm$Max_depth) +
  labs(x = "maximum_depth", y = "mean_divergence")  +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")
# 1: naja, 2: no, 3: naja mit pattern, test ok, 4: ok -> 0
# depth4 <- lm_analysis(df_lm$max_sign, df_lm$Max_depth) +
#   geom_smooth(method = "lm", col = "#EE1289",  fill = "#CDC9C9") +
#   labs(x = "maximum_depth", y = "mean_divergence")
# 1: naja, 2: no, 3: naja mit pattern, test ok, 4: ok -> 0

ggarrange(depth1, depth2, depth3, depth4, ncol = 2, nrow = 2)

#tiff(paste("total_models/plots/lm_depth_max.tiff", sep = ""), units="in", width=8, height=4, res=300)
tiff(paste("total_models/plots/07_02_depth_max.tiff", sep = ""), units="in", width=4, height=8, res=300)
# plot(ggarrange(depth1, depth2, ncol = 2))
# plot science discussion
plot(ggarrange(depth1, depth3, nrow = 2))

# Closing the graphical device
dev.off()

# eutrophication

phos1 <- lm_analysis(df_lm$mean_rdiv, df_lm$Phos_max)  +
  labs(x = "maximum_phosphorus", y = "mean_dissimilarity")
# 1: ok, 2: ok, 3: ok, 4: ok -> 1
# phos2 <- lm_analysis(df_lm$max_rdiv, df_lm$Phos_max) +
#   labs(x = "maximum_phosphorus", y = "max_dissimilarity")
# 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
phos3 <- lm_analysis(df_lm$mean_sign, df_lm$Phos_max)  +
  labs(x = "maximum_phosphorus", y = "mean_divergence")  +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")
# # 1: ok, 2: no, 3: naja, test ok, 4: ok -> 0
# phos4 <- lm_analysis(df_lm$max_sign, df_lm$Phos_max)
# # 1: no, 2: no, 3: naja, test ok, 4: ok -> 0

ggarrange(phos1, phos2, ncol = 2)

# tiff(paste("total_models/plots/lm_max_phos.tiff", sep = ""), units="in", width=8, height=4, res=300)
tiff(paste("total_models/plots/07_02_phos_max.tiff", sep = ""), units="in", width=4, height=8, res=300)

# plot(ggarrange(phos1, phos2, ncol = 2))
# plot sicence discussion
plot(ggarrange(phos1, phos3, nrow = 2))
# Closing the graphical device
dev.off()

###############################################################################
#endemism 

e1 <- lm_analysis(df_lm$mean_rdiv, df_lm$sum_species) +
  labs(x = "species_richness", y = "mean_dissimilarity")
  
e11 <- lm_analysis(df_lm$mean_sign, df_lm$sum_species) +
  labs(x = "species_richness", y = "mean_divergence")  +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")

tiff(paste("total_models/plots/07_02_species_richness.tiff", sep = ""), units="in", width=4, height=8, res=300)


# plot sicence discussion
plot(ggarrange(e1, e11, nrow = 2))

# Closing the graphical device
dev.off()

a1 <- lm_analysis(df_lm$mean_rdiv, df_lm$non_native) +
  labs(x = "non_native", y = "mean_dissimilarity")
# 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
a11 <- lm_analysis(df_lm$mean_sign, df_lm$non_native) +
  labs(x = "non_native", y = "mean_divergence") +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")
  
# b1 <- lm_analysis(df_lm$max_rdiv, df_lm$non_native) +
#   labs(x = "non_native", y = "max_dissimilarity")
# # 1: ok, 2: naja, shapiro ok, 3: naja, test ok, 4: ok -> 1
# c1 <- lm_analysis(df_lm$mean_rdiv, df_lm$perc_non_native) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9") +
#   labs(x = "percentage_non_native", y = "mean_dissimilarity")
# # 1: ok, 2:  ok, 3: naja, test ok, 4: ok -> 1
# d1 <- lm_analysis(df_lm$max_rdiv, df_lm$perc_non_native) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9") +
#   labs(x = "percentage_non_native", y = "max_dissimilarity")
# # 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1

# ggarrange(a1,b1,c1,d1,ncol = 2, nrow = 2)

# tiff(paste("total_models/plots/lm_rdiv_non_native.tiff", sep = ""), units="in", width=10, height=9, res=300)
tiff(paste("total_models/plots/07_02_non_native.tiff", sep = ""), units="in", width=4, height=8, res=300)

# plot(ggarrange(a1,b1,c1,d1,ncol = 2, nrow = 2))

# plot sicence discussion
plot(ggarrange(a1, a11, nrow = 2))

# Closing the graphical device
dev.off()

a2 <- lm_analysis(df_lm$mean_rdiv, df_lm$native) +
  labs(x = "native", y = "mean_dissimilarity")
# 1: naja, 2: ok, 3: naja, 4: ok -> 1

a22 <- lm_analysis(df_lm$mean_sign, df_lm$native) +
  labs(x = "native", y = "mean_divergence") +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")

# b2 <- lm_analysis(df_lm$max_rdiv, df_lm$native) +
#   labs(x = "native", y = "max_dissimilarity")
# # 1: naja, 2: ok, 3: naja, test ok, 4: ok -> 1
# c2 <- lm_analysis(df_lm$mean_rdiv, df_lm$perc_native) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")+
#   labs(x = "percentage_native", y = "mean_dissimilarity")
# 1: naja, 2: ok, 3: naja, 4: ok -> 1
# d2 <- lm_analysis(df_lm$max_rdiv, df_lm$perc_native) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")
# # 1: no, 2: ok, 3: no, test ok, 4: ok -> 0 

# ggarrange(a2,b2,c2,d2,ncol = 2, nrow = 2)
ggarrange(a2,b2,c2,ncol = 2, nrow = 2)

# tiff(paste("total_models/plots/lm_rdiv_native.tiff", sep = ""), units="in", width=9, height=10, res=300)
tiff(paste("total_models/plots/07_02_native.tiff", sep = ""), units="in", width=4, height=8, res=300)

# 
# plot(ggarrange(a2,b2,c2,ncol = 2, nrow = 2))

# plot sicence discussion
plot(ggarrange(a2, a22, nrow = 2))

# Closing the graphical device
dev.off()


a3 <- lm_analysis(df_lm$mean_rdiv, df_lm$exotic) +
  labs(x = "exotic", y = "mean_dissimilarity")

a33 <- lm_analysis(df_lm$mean_sign, df_lm$exotic) +
  labs(x = "exotic", y = "mean_divergence") +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")

# # 1: ok, 2: ok, 3: kanpp unter 0.05, test ok, 4: ok -> 1
# b3 <- lm_analysis(df_lm$max_rdiv, df_lm$exotic) +
#   labs(x = "exotic", y = "max_dissimilarity")
# # 1: naja, 2: ok, 3: naja, test ok, 4: ok -> 1
# c3 <- lm_analysis(df_lm$mean_rdiv, df_lm$perc_exotic) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9") +
#   labs(x = "percentage_exotic", y = "mean_dissimilarity")
# # 1: ok, 2: ok, 3: ok, test ok, 4: ok -> 1
# d3 <- lm_analysis(df_lm$max_rdiv, df_lm$perc_exotic) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9") +
#   labs(x = "percentage_exotic", y = "max_dissimilarity")
# # 1: ok, 2: ok, 3: ok, 4: ok -> 1


ggarrange(a3,b3,c3,d3,ncol = 2, nrow = 2)

# tiff(paste("total_models/plots/lm_rdiv_exotic.tiff", sep = ""), units="in", width=10, height=9, res=300)
tiff(paste("total_models/plots/07_02_exotic.tiff", sep = ""), units="in", width=4, height=8, res=300)

# plot(ggarrange(a3,b3,c3,d3,ncol = 2, nrow = 2))
# plot sicence discussion
plot(ggarrange(a3, a33, nrow = 2))

# Closing the graphical device
dev.off()

a4 <- lm_analysis(df_lm$mean_rdiv, df_lm$endemic) +
  labs(x = "endemic", y = "mean_dissimilarity")

a44 <- lm_analysis(df_lm$mean_sign, df_lm$endemic) +
  labs(x = "endemic", y = "mean_divergence") +
  geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")
# 1: naja, 2: ok, 3: naja mit pattern, test ok, 4: ok -> 1
# b4 <- lm_analysis(df_lm$max_rdiv, df_lm$endemic)
# # 1: no, 2: ok, 3: no, test ok, 4: ok -> 0
# c4 <- lm_analysis(df_lm$mean_rdiv, df_lm$perc_endemism) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9") +
#   labs(x = "percentage_endemic", y = "mean_dissimilarity")
# 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
# d4 <- lm_analysis(df_lm$max_rdiv, df_lm$perc_endemism, df_lm) +
#   geom_smooth(method = "lm", col = "blue",  fill = "#CDC9C9")
# # 1: naja, 2: ok, 3: naja, test ok, 4: ok -> ?

# ggarrange(a4,b4,c4,d4,ncol = 2, nrow = 2)
ggarrange(a4,c4,ncol = 2)

# tiff(paste("total_models/plots/lm_rdiv_endemic.tiff", sep = ""), units="in", width=8, height=4, res=300)
tiff(paste("total_models/plots/07_02_endemism.tiff", sep = ""), units="in", width=4, height=8, res=300)

# plot(ggarrange(a4,c4,ncol = 2))

# plot sicence discussion
plot(ggarrange(a4, a44, nrow = 2))


# Closing the graphical device
dev.off()



# tiff(paste("total_models/plots/lm_rdiv_native.tiff", sep = ""), units="in", width=6, height=9, res=300)
# 
# plot(rdiv_native)
# 
# # Closing the graphical device
# dev.off()


# i <- lm_analysis(df_lm$mean_sign, df_lm$non_native)
# # 1: no, 2: no, 3: no, test ok, 4: ok -> 0 
# j <- lm_analysis(df_lm$max_sign, df_lm$non_native)
# # 1: no, 2: no, 3: no test ok, 4: ok -> 0
# k <- lm_analysis(df_lm$mean_sign, df_lm$native)
# # 1: no, 2: ok, 3: no, test ok, 4: ok -> 
# l <- lm_analysis(df_lm$max_sign, df_lm$native)
# # 1: no, 2: no, 3: no, test ok, 4: ok -> 0
# m <- lm_analysis(df_lm$mean_sign, df_lm$exotic)
# # 1: no, 2: no, 3: naja, test ok, 4: ok -> 0
# n <- lm_analysis(df_lm$max_sign, df_lm$exotic)
# # 1: no, 2: no, 3: no, test ok, 4: no
# o <- lm_analysis(df_lm$mean_sign, df_lm$endemic)
# # 1: no, 2: no, 3: no, test ok, 4: no -> 0
# p <- lm_analysis(df_lm$max_sign, df_lm$endemic)
# # 1: no, 2: no, 3: no, test ok, 4: no -> 0 

# none of them fulfills models assumptions
# redo with percentages

i <- lm_analysis(df_lm$mean_sign, df_lm$perc_non_native) + 
  labs(x = "percentage_non_native", y = "mean_divergence")
# 1: ok, 2: ok, 3: no, test ok, 4: ok -> ?
# j <- lm_analysis(df_lm$max_sign, df_lm$perc_non_native)
# 1: no, 2: no, 3: ok, 4: ok -> 0
k <- lm_analysis(df_lm$mean_sign, df_lm$perc_native) + 
  labs(x = "percentage_native", y = "mean_divergence")
# 1: ok, 2: ok, 3: no, test ok, 4: ok -> ?
# l <- lm_analysis(df_lm$max_sign, df_lm$perc_native)
# 1: ok, 2: no, 3: ok, test ok, 4: ok -> 0
# m <- lm_analysis(df_lm$mean_sign, df_lm$perc_exotic)
# # 1: ok, 2: no, 3: naja, test ok, 4: ok -> 0
# n <- lm_analysis(df_lm$max_sign, df_lm$perc_exotic)
# 1: no, 2: no, 3: ok, 4: no -> 0
o <- lm_analysis(df_lm$mean_sign, df_lm$perc_endemism) + 
  labs(x = "percentage_endemic", y = "mean_divergence")
# 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
# p <- lm_analysis(df_lm$max_sign, df_lm$perc_endemism)
# 1: ok, 2: no, 3: ok, 4: no -> 0





# percentages and divergence 
ggarrange(i, o, k, ncol = 3)

tiff(paste("total_models/plots/lm_sign_type.tiff", sep = ""), units="in", width=12, height=4, res=300)

plot(ggarrange(i, o, k, ncol = 3))

# Closing the graphical device
dev.off()

#################### regression: eutrophication and mean_rdiv

# drdiv1 <- lm(mean_rdiv ~ Max_depth, data = eutroph_dissimilarity)
# summary(drdiv1)
# plot(drdiv1)
# 
# plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$mean_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
# #add fitted regression line
# abline(drdiv1)
# 
# summary(drdiv1)
# anova(drdiv1)
# 
# # dissimilarity 
# ggplot(mapping = aes(x = Max_depth, y = mean_rdiv), data = eutroph_dissimilarity) +
#   geom_point()
# #might be positive
# # check assumptions
# 
# # overview
# 
# par(mfrow=c(2,2))
# plot(drdiv1, pch = 16, col = "blue")
# 
# autoplot(drdiv1, which = 1:6, ncol = 2, label.size = 3)
# 
# # 1
# # ideally a horizontal line across zero
# plot(drdiv1,1)
# 
# # equal variance: no patterns should be there
# 
# phos_res <- resid(drdiv1)
# phos_fit <- fitted(drdiv1)
# 
# ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
#   geom_point() +
#   geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
# 
# # okay, not great
# 
# # 2
# # histogram of residuals
# hist(drdiv1$residuals)
# 
# # qqplot
# plot(drdiv1, 2)
# 
# # qq
# 
# ggplot(mapping = aes(sample = phos_res)) +
#   stat_qq() + 
#   stat_qq_line()
# 
# 
# shapiro.test(resid(drdiv1))
# # normality is okay
# 
# # 3: Testing the Homoscedasticity Assumption
# plot(drdiv1, 3)
# # some deviation from horizontal line
# 
# # breusch pagan test for Homoscedasticity 
# lmtest::bptest(drdiv1) 
# # p value > 0.05 Thus, we assume that homoscedasticity is present.
# 
# # 4: leverage
# 
# # Get leverage for each observation in the data set
# leverage <- as.data.frame(hatvalues(drdiv1))
# 
# # Print leverage for each observation
# leverage
# 
# # look at highest values, 8, maggiore
# 
# par(mfrow=c(2,1))
# barplot(hatvalues(drdiv1), 
#         col = "aquamarine3")
# 
# 
# leveragePlots(drdiv1)
# 
# # look for points with high leverage
# sum(hatvalues(drdiv1) > 2 * mean(hatvalues(drdiv1)))
# # 1 point of high leverage
# 
# sum(abs(rstandard(drdiv1)) > 2)
# # also 0 point with big residual
# # There is also one point with a large residual. Do these result in any
# # points that are considered influential?
# 
# drdv1_add = cooks.distance(drdiv1)
# sum(drdv1_add > 4 / length(drdv1_add))
# 
# large_drdv1_add = drdv1_add > 4 / length(drdv1_add)
# drdv1_add[large_drdv1_add]
# 
# # joux is very different
# 
# coef(drdiv1)
# 
# # what happens if we remove this point?
# drdiv1_fix = lm(mean_rdiv ~ Max_depth,
#                 data = eutroph_dissimilarity,
#                 subset = drdv1_add <= 4 / length(drdv1_add))
# coef(drdiv1_fix)
# # no real difference, let joux inside the model 
# 
# # summary:
# # 1: semi okay
# # 2: okay
# # 3: okay
# # 4: okay
# 
# # plot lm
# ggplotRegression(drdiv1)
# 
# ###########################################################################
# drdiv2 <- lm(max_rdiv ~ Max_depth, data = eutroph_dissimilarity)
# summary(drdiv2)
# plot(drdiv2)
# 
# plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$max_rdiv, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
# #add fitted regression line
# abline(drdiv2)
# 
# summary(drdiv2)
# anova(drdiv2)
# 
# # dissimilarity 
# ggplot(mapping = aes(x = Max_depth, y = max_rdiv), data = eutroph_dissimilarity) +
#   geom_point()
# #might be positive
# # check assumptions
# 
# # overview
# 
# par(mfrow=c(2,2))
# plot(drdiv2, pch = 16, col = "blue")
# 
# autoplot(drdiv2, which = 1:6, ncol = 2, label.size = 3)
# 
# # 1
# # ideally a horizontal line across zero
# par(mfrow=c(1,1))
# plot(drdiv2,1)
# 
# # equal variance: no patterns should be there
# 
# phos_res <- resid(drdiv2)
# phos_fit <- fitted(drdiv2)
# 
# ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
#   geom_point() +
#   geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
# 
# # no patterns, but not very horizontal 
# 
# # 2
# # histogram of residuals
# hist(drdiv2$residuals)
# 
# # qqplot
# plot(drdiv2, 2)
# 
# # qq
# 
# ggplot(mapping = aes(sample = phos_res)) +
#   stat_qq() + 
#   stat_qq_line()
# 
# 
# shapiro.test(resid(drdiv2))
# # normality is okay
# 
# # 3: Testing the Homoscedasticity Assumption
# plot(drdiv2, 3)
# # some deviation from horizontal line
# 
# # breusch pagan test for Homoscedasticity 
# lmtest::bptest(drdiv2) 
# # p value > 0.05 Thus, we assume that homoscedasticity is present.
# 
# # 4: leverage
# 
# # Get leverage for each observation in the data set
# leverage <- as.data.frame(hatvalues(drdiv2))
# 
# # Print leverage for each observation
# leverage
# 
# # look at highest values, 8, maggiore
# 
# par(mfrow=c(2,1))
# barplot(hatvalues(drdiv2), 
#         col = "aquamarine3")
# 
# 
# leveragePlots(drdiv2)
# 
# # look for points with high leverage
# sum(hatvalues(drdiv1) > 2 * mean(hatvalues(drdiv1)))
# # 1 point of high leverage
# 
# sum(abs(rstandard(drdiv2)) > 2)
# # also 0 point with big residual
# # There is also one point with a large residual. Do these result in any
# # points that are considered influential?
# 
# drdv2_add = cooks.distance(drdiv2)
# sum(drdv2_add > 4 / length(drdv2_add))
# 
# large_drdv2_add = drdv2_add > 4 / length(drdv2_add)
# drdv2_add[large_drdv2_add]
# 
# # lugano is very different
# 
# coef(drdiv2)
# 
# # what happens if we remove this point?
# drdiv2_fix = lm(mean_rdiv ~ Max_depth,
#                 data = eutroph_dissimilarity,
#                 subset = drdv2_add <= 4 / length(drdv2_add))
# coef(drdiv2_fix)
# 
# # summary:
# # 1: semi okay
# # 2: okay
# # 3: okay
# # 4: okay
# 
# # plot lm
# ggplotRegression(drdiv2)
# 
# #################################################################################
# dsign1 <- lm(mean_sign ~ Max_depth, data = eutroph_dissimilarity)
# summary(dsign1)
# plot(dsign1)
# 
# plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$mean_sign, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
# #add fitted regression line
# abline(dsign1)
# 
# summary(dsign1)
# anova(dsign1)
# 
# # dissimilarity 
# ggplot(mapping = aes(x = Max_depth, y = mean_sign), data = eutroph_dissimilarity) +
#   geom_point()
# #might be positive
# # check assumptions
# 
# # overview
# 
# par(mfrow=c(2,2))
# plot(dsign1, pch = 16, col = "blue")
# 
# autoplot(dsign1, which = 1:6, ncol = 2, label.size = 3)
# 
# # 1
# # ideally a horizontal line across zero
# par(mfrow=c(1,1))
# plot(dsign1,1)
# 
# # not very horizontal
# 
# # equal variance: no patterns should be there
# 
# phos_res <- resid(dsign1)
# phos_fit <- fitted(dsign1)
# 
# ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
#   geom_point() +
#   geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
# 
# # no patterns, but not very horizontal 
# 
# # 2
# # histogram of residuals
# hist(dsign1$residuals)
# 
# # qqplot
# plot(dsign1, 2)
# 
# # qq
# 
# ggplot(mapping = aes(sample = phos_res)) +
#   stat_qq() + 
#   stat_qq_line()
# 
# 
# shapiro.test(resid(dsign1))
# # normality is okay, but looks baaad
# 
# # 3: Testing the Homoscedasticity Assumption
# plot(dsign1, 3)
# # some deviation from horizontal line
# 
# # breusch pagan test for Homoscedasticity 
# lmtest::bptest(dsign1) 
# # p value > 0.05 Thus, we assume that homoscedasticity is present.
# 
# # 4: leverage
# 
# # Get leverage for each observation in the data set
# leverage <- as.data.frame(hatvalues(dsign1))
# 
# # Print leverage for each observation
# leverage
# 
# # look at highest values, 8, maggiore
# 
# par(mfrow=c(2,1))
# barplot(hatvalues(dsign1), 
#         col = "aquamarine3")
# 
# 
# leveragePlots(dsign1)
# 
# # look for points with high leverage
# sum(hatvalues(dsign1) > 2 * mean(hatvalues(dsign1)))
# # 1 point of high leverage
# 
# sum(abs(rstandard(dsign1)) > 2)
# # also 1 point with big residual
# # There is also one point with a large residual. Do these result in any
# # points that are considered influential?
# 
# dsign1_add = cooks.distance(dsign1)
# sum(dsign1_add > 4 / length(dsign1_add))
# 
# large_dsign1_add = dsign1_add > 4 / length(dsign1_add)
# dsign1_add[large_dsign1_add]
# 
# # joux and  lugano is very different
# 
# coef(dsign1)
# 
# # what happens if we remove this point?
# dsign1_fix = lm(mean_sign ~ Max_depth,
#                 data = eutroph_dissimilarity,
#                 subset = dsign1_add <= 4 / length(dsign1_add))
# coef(dsign1_fix)
# 
# # not very different
# 
# # summary:
# # 1: semi okay
# # 2: okay
# # 3: okay
# # 4: okay
# 
# # plot lm
# ggplotRegression(dsign1)
# 
# ##################################################################################
# 
# dsign2 <- lm(max_sign ~ Max_depth, data = eutroph_dissimilarity)
# summary(dsign2)
# par(mfrow=c(2,2))
# plot(dsign2)
# 
# plot(eutroph_dissimilarity$Max_depth, eutroph_dissimilarity$max_sign, col='red', main='Summary of Regression Model', xlab='x', ylab='y')
# #add fitted regression line
# abline(dsign2)
# 
# summary(dsign2)
# anova(dsign2)
# 
# # dissimilarity 
# ggplot(mapping = aes(x = Max_depth, y = max_sign), data = eutroph_dissimilarity) +
#   geom_point()
# #might be positive
# # check assumptions
# 
# # overview
# 
# par(mfrow=c(2,2))
# plot(dsign2, pch = 16, col = "blue")
# 
# autoplot(dsign2, which = 1:6, ncol = 2, label.size = 3)
# 
# # 1
# # ideally a horizontal line across zero
# par(mfrow=c(1,1))
# plot(dsign2,1)
# 
# # not very horizontal
# 
# # equal variance: no patterns should be there
# 
# phos_res <- resid(dsign2)
# phos_fit <- fitted(dsign2)
# 
# ggplot(mapping = aes(x = phos_fit, y = phos_res)) +
#   geom_point() +
#   geom_hline(yintercept = 0, colour = "red", linetype = "dashed")
# 
# # no patterns, but not very horizontal 
# 
# # 2
# # histogram of residuals
# hist(dsign2$residuals)
# 
# # qqplot
# plot(dsign2, 2)
# 
# # qq
# 
# ggplot(mapping = aes(sample = phos_res)) +
#   stat_qq() + 
#   stat_qq_line()
# 
# 
# shapiro.test(resid(dsign2))
# # normality not okay
# 
# # 3: Testing the Homoscedasticity Assumption
# plot(dsign2, 3)
# # some deviation from horizontal line
# 
# # breusch pagan test for Homoscedasticity 
# lmtest::bptest(dsign2) 
# # p value > 0.05 Thus, we assume that homoscedasticity is present.
# 
# # 4: leverage
# 
# # Get leverage for each observation in the data set
# leverage <- as.data.frame(hatvalues(dsign2))
# 
# # Print leverage for each observation
# leverage
# 
# # look at highest values, 8, maggiore
# 
# par(mfrow=c(2,1))
# barplot(hatvalues(dsign2), 
#         col = "aquamarine3")
# 
# 
# leveragePlots(dsign2)
# 
# # look for points with high leverage
# sum(hatvalues(dsign2) > 2 * mean(hatvalues(dsign2)))
# # 1 point of high leverage
# 
# sum(abs(rstandard(dsign2)) > 2)
# # also 0 point with big residual
# # There is also one point with a large residual. Do these result in any
# # points that are considered influential?
# 
# dsign2_add = cooks.distance(dsign2)
# sum(dsign2_add > 4 / length(dsign2_add))
# 0
# 
# large_dsign2_add = dsign2_add > 4 / length(dsign2_add)
# dsign2_add[large_dsign2_add]
# 
# # none
# 
# # summary:
# # 1: not okay
# # 2: not normally distributed
# # 3: okay
# # 4: okay
# 
# ################################################################################lm
# lm_analysis <- function(y, x, df) {
#   
#   mod <- lm(y ~ x, data = df)
#   print(summary(mod))
#   print(shapiro.test(resid(mod)))
#   print(lmtest::bptest(mod))
#   par(mfrow=c(2,2))
#   print(plot(mod))
#   
#   ggplot(mod$model, aes_string(x = names(mod$model)[2], y = names(mod$model)[1])) + 
#     geom_point() +
#     geom_smooth(method = "lm", col = "red",  fill = "#CDC9C9") +
#     # stat_smooth(method = "lm", col = "#FF3030", fill = "#CDC9C9") +
#     labs(title = paste("R^2 = ",signif(summary(mod)$r.squared, 2),
#                        # "adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                        # "Intercept =",signif(fit$coef[[1]],5 ),
#                        # " Slope =",signif(fit$coef[[2]], 5),
#                        " p-value =", signif(summary(mod)$coef[2,4], 2))) +
#     theme_bw()
# }
# 
# # maximum_depth:
# 
# depth1 <- lm_analysis(eutroph_dissimilarity$mean_rdiv, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity)  +
#   geom_smooth(method = "lm", col = "#66CD00",  fill = "#CDC9C9") +
#   labs(x = "maximum_depth", y = "mean_dissimilarity")
# # 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
# depth2 <- lm_analysis(eutroph_dissimilarity$max_rdiv, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity) +
#   geom_smooth(method = "lm", col = "#66CD00",  fill = "#CDC9C9") +
#   labs(x = "maximum_depth", y = "max_dissimilarity")
# # 1: naja, 2: ok, 3: naja mit pattern, test ok, 4: ok -> ?
# # depth3 <-lm_analysis(eutroph_dissimilarity$mean_sign, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity) +
# #   geom_smooth(method = "lm", col = "#EE1289",  fill = "#CDC9C9") +
# #   labs(x = "maximum_depth", y = "mean_divergence")
# # 1: naja, 2: no, 3: naja mit pattern, test ok, 4: ok -> 0
# # depth4 <- lm_analysis(eutroph_dissimilarity$max_sign, eutroph_dissimilarity$Max_depth, eutroph_dissimilarity) +
# #   geom_smooth(method = "lm", col = "#EE1289",  fill = "#CDC9C9") +
# #   labs(x = "maximum_depth", y = "mean_divergence")
# # 1: naja, 2: no, 3: naja mit pattern, test ok, 4: ok -> 0
# 
# ggarrange(depth1, depth2, depth3, depth4, ncol = 2, nrow = 2)
# 
# tiff(paste("total_models/plots/lm_depth_max.tiff", sep = ""), units="in", width=8, height=4, res=300)
# 
# plot(ggarrange(depth1, depth2, ncol = 2))
# 
# # Closing the graphical device
# dev.off()
# 
# # eutrophication
# 
# phos1 <- lm_analysis(eutroph_dissimilarity$mean_rdiv, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)  +
#   labs(x = "maximum_phosphorus", y = "mean_dissimilarity")
# # 1: ok, 2: ok, 3: ok, 4: ok -> 1
# phos2 <- lm_analysis(eutroph_dissimilarity$max_rdiv, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity) +
#   labs(x = "maximum_phosphorus", y = "max_dissimilarity")
# # 1: ok, 2: ok, 3: naja, test ok, 4: ok -> 1
# # phos3 <- lm_analysis(eutroph_dissimilarity$mean_sign, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)
# # # 1: ok, 2: no, 3: naja, test ok, 4: ok -> 0
# # phos4 <- lm_analysis(eutroph_dissimilarity$max_sign, eutroph_dissimilarity$Phos_max, eutroph_dissimilarity)
# # # 1: no, 2: no, 3: naja, test ok, 4: ok -> 0
# 
# ggarrange(phos1, phos2, ncol = 2)
# 
# tiff(paste("total_models/plots/lm_max_phos.tiff", sep = ""), units="in", width=8, height=4, res=300)
# 
# plot(ggarrange(phos1, phos2, ncol = 2))
# 
# # Closing the graphical device
# dev.off()