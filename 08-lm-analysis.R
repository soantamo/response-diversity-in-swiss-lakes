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
select(-6) |> 
  rename(endemism = details)

species_endemism$endemism <- as.factor(species_endemism$endemism)

str(species_endemism)

# adding number of endemic and native etc species.
df_species_endemism <- species_endemism |> 
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
  rename(Lake = fLake) |> 
  group_by(Lake) |> 
  mutate(perc_endemism = sum(endemic * 100 / sum_species)) |> 
  mutate(perc_exotic = sum(exotic * 100 / sum_species)) |> 
  mutate(perc_non_native = sum(non_native * 100 / sum_species)) |> 
  mutate(perc_native = sum(native * 100 / sum_species))

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

res <- resid(models)

plot(res)

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