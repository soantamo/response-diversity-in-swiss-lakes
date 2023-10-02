#making a heatmap of divergence values
library(tidyverse)
library(patchwork)
library(ggtext)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)

#subset!!!

df_biel <- readRDS("Subset/df_derivatives/df_deriv_Biel.rds")
test <- df_biel |> 
  pivot_wider(names_from = Species, values_from = derivative) |> 
  select(!smooth:upper)

corr_mat <- round(cor(test, method = "pearson", use = "pairwise.complete.obs"),2) 
head(corr_mat)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)


#mean derivative per species

lake_mean <- df_biel |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_biel <- aggregate(derivative ~ Species, data = df_biel, mean)
df_heat_biel <- df_heat_biel |> 
  mutate(Lake = factor("Biel"))

df_heat_biel <- bind_cols(df_heat_biel, lake_mean)



df_brienz <- readRDS("Subset/df_derivatives/df_deriv_Brienz.rds")

lake_mean <- df_brienz |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_brienz <- aggregate(derivative ~ Species, data = df_brienz, mean)
df_heat_brienz <- df_heat_brienz |> 
  mutate(Lake = factor("Brienz"))

df_heat_brienz <- bind_cols(df_heat_brienz, lake_mean)

df_joux <- readRDS("Subset/df_derivatives/df_deriv_Joux.rds")

df_heat_joux <- aggregate(derivative ~ Species, data = df_joux, mean)
df_heat_joux <- df_heat_joux |> 
  mutate(Lake = factor("Joux"))

lake_mean <- df_joux |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_joux<- bind_cols(df_heat_joux, lake_mean)

df_morat <- readRDS("Subset/df_derivatives/df_deriv_Morat.rds")

df_heat_morat <- aggregate(derivative ~ Species, data = df_morat, mean)
df_heat_morat <- df_heat_morat |> 
  mutate(Lake = factor("Morat"))

lake_mean <- df_morat |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_morat <- bind_cols(df_heat_morat, lake_mean)

df_maggiore <- readRDS("Subset/df_derivatives/df_deriv_Maggiore.rds")

df_heat_maggiore <- aggregate(derivative ~ Species, data = df_maggiore, mean)
df_heat_maggiore <- df_heat_maggiore |> 
  mutate(Lake = factor("Maggiore"))

lake_mean <- df_maggiore |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_maggiore <- bind_cols(df_heat_maggiore, lake_mean)

df_lucerne <- readRDS("Subset/df_derivatives/df_deriv_Lucerne.rds")

df_heat_lucerne <- aggregate(derivative ~ Species, data = df_lucerne, mean)
df_heat_lucerne <- df_heat_lucerne |> 
  mutate(Lake = factor("Lucerne"))

lake_mean <- df_lucerne |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_lucerne <- bind_cols(df_heat_lucerne, lake_mean)

df_lugano <- readRDS("Subset/df_derivatives/df_deriv_Lugano.rds")

df_heat_lugano <- aggregate(derivative ~ Species, data = df_lugano, mean)
df_heat_lugano <- df_heat_lugano |> 
  mutate(Lake = factor("Lugano"))

lake_mean <- df_lugano |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_lugano <- bind_cols(df_heat_lugano, lake_mean)

df_neuchatel <- readRDS("Subset/df_derivatives/df_deriv_Neuchatel.rds")

df_heat_neuchatel <- aggregate(derivative ~ Species, data = df_neuchatel, mean)
df_heat_neuchatel <- df_heat_neuchatel |> 
  mutate(Lake = factor("Neuchatel"))

lake_mean <- df_neuchatel |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_neuchatel <- bind_cols(df_heat_neuchatel, lake_mean)

df_poschiavo <- readRDS("Subset/df_derivatives/df_deriv_Poschiavo.rds")

df_heat_poschiavo <- aggregate(derivative ~ Species, data = df_poschiavo, mean)
df_heat_poschiavo <- df_heat_poschiavo |> 
  mutate(Lake = factor("Poschiavo"))

lake_mean <- df_poschiavo |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_poschiavo <- bind_cols(df_heat_poschiavo, lake_mean)

df_sarnen <- readRDS("Subset/df_derivatives/df_deriv_Sarnen.rds")

df_heat_sarnen <- aggregate(derivative ~ Species, data = df_sarnen, mean)
df_heat_sarnen <- df_heat_sarnen |> 
  mutate(Lake = factor("Sarnen"))

lake_mean <- df_sarnen |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_sarnen <- bind_cols(df_heat_sarnen, lake_mean)

df_walen <- readRDS("Subset/df_derivatives/df_deriv_Walen.rds")

df_heat_walen <- aggregate(derivative ~ Species, data = df_walen, mean)
df_heat_walen <- df_heat_walen |> 
  mutate(Lake = factor("Walen"))

lake_mean <- df_walen |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_walen <- bind_cols(df_heat_walen, lake_mean)

df_zug <- readRDS("Subset/df_derivatives/df_deriv_Zug.rds")

df_heat_zug <- aggregate(derivative ~ Species, data = df_zug, mean)
df_heat_zug <- df_heat_zug |> 
  mutate(Lake = factor("Zug"))

lake_mean <- df_zug |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_zug <- bind_cols(df_heat_zug, lake_mean)

df_zurich <- readRDS("Subset/df_derivatives/df_deriv_Zurich.rds")

df_heat_zurich <- aggregate(derivative ~ Species, data = df_zurich, mean)
df_heat_zurich <- df_heat_zurich |> 
  mutate(Lake = factor("Zurich"))

lake_mean <- df_zurich |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_zurich <- bind_cols(df_heat_zurich, lake_mean)

df_geneva <- readRDS("Subset/df_derivatives/df_deriv_Geneva.rds")

df_heat_geneva <- aggregate(derivative ~ Species, data = df_geneva, mean)
df_heat_geneva <- df_heat_geneva |> 
  mutate(Lake = factor("Geneva"))

lake_mean <- df_geneva |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_geneva <- bind_cols(df_heat_geneva, lake_mean)

df_thun <- readRDS("Subset/df_derivatives/df_deriv_Thun.rds")


df_heat_thun <- aggregate(derivative ~ Species, data = df_thun, mean)
df_heat_thun <- df_heat_thun |> 
  mutate(Lake = factor("Thun"))

lake_mean <- df_thun |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_thun <- bind_cols(df_heat_thun, lake_mean)

df_constance <- readRDS("Subset/df_derivatives/df_deriv_Constance.rds")

df_heat_constance <- aggregate(derivative ~ Species, data = df_constance, mean)
df_heat_constance <- df_heat_constance |> 
  mutate(Lake = factor("Constance"))

lake_mean <- df_constance |> 
  mutate(lake_derivative = mean(derivative)) |> 
  distinct(lake_derivative)

df_heat_constance <- bind_cols(df_heat_constance, lake_mean)

df <- bind_rows(df_heat_biel, df_heat_geneva, df_heat_thun, df_heat_constance, df_heat_brienz,
                df_heat_joux, df_heat_lucerne, df_heat_lugano, df_heat_maggiore, df_heat_morat,
                df_heat_neuchatel, df_heat_poschiavo, df_heat_sarnen, df_heat_walen, 
                df_heat_zug, df_heat_zurich)

saveRDS(df, file = "df_heatmap_subset")



ggplot(df, aes(Lake, Species)) +
  geom_tile(aes(fill = derivative), colour = "white") +
  scale_fill_gradient(low = "white", high = "steelblue")

ggplot(df, aes(x = Lake, y = Species, fill = derivative)) +
  geom_tile() +
  # scale_fill_gradient(low = "white", high = "red") +
  coord_fixed() +
  scale_fill_continuous(na.value = 'grey')

df  |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="plasma") 

df  |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="viridis") 

df  |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="turbo")

##repeat for all data
df_brienz <- readRDS("df_derivatives/df_deriv_Brienz.rds")

df_heat_brienz <- aggregate(derivative ~ Species, data = df_brienz, mean)
df_heat_brienz <- df_heat_brienz |> 
  mutate(Lake = factor("Brienz"))

df_brienz <- readRDS("df_derivatives/df_deriv_Biel.rds")

df_heat_biel <- aggregate(derivative ~ Species, data = df_biel, mean)
df_heat_biel <- df_heat_biel |> 
  mutate(Lake = factor("Biel"))

df_joux <- readRDS("df_derivatives/df_deriv_Joux.rds")

df_heat_joux <- aggregate(derivative ~ Species, data = df_joux, mean)
df_heat_joux <- df_heat_joux |> 
  mutate(Lake = factor("Joux"))

df_morat <- readRDS("df_derivatives/df_deriv_Morat.rds")

df_heat_morat <- aggregate(derivative ~ Species, data = df_morat, mean)
df_heat_morat <- df_heat_morat |> 
  mutate(Lake = factor("Morat"))

df_maggiore <- readRDS("df_derivatives/df_deriv_Maggiore.rds")

df_heat_maggiore <- aggregate(derivative ~ Species, data = df_maggiore, mean)
df_heat_maggiore <- df_heat_maggiore |> 
  mutate(Lake = factor("Maggiore"))

df_lucerne <- readRDS("Subset/df_derivatives/df_deriv_Lucerne.rds")

df_heat_lucerne <- aggregate(derivative ~ Species, data = df_lucerne, mean)
df_heat_lucerne <- df_heat_lucerne |> 
  mutate(Lake = factor("Lucerne"))

df_lugano <- readRDS("df_derivatives/df_deriv_Lugano.rds")

df_heat_lugano <- aggregate(derivative ~ Species, data = df_lugano, mean)
df_heat_lugano <- df_heat_lugano |> 
  mutate(Lake = factor("Lugano"))

df_neuchatel <- readRDS("df_derivatives/df_deriv_Neuchatel.rds")

df_heat_neuchatel <- aggregate(derivative ~ Species, data = df_neuchatel, mean)
df_heat_neuchatel <- df_heat_neuchatel |> 
  mutate(Lake = factor("Neuchatel"))

df_poschiavo <- readRDS("df_derivatives/df_deriv_Poschiavo.rds")

df_heat_poschiavo <- aggregate(derivative ~ Species, data = df_poschiavo, mean)
df_heat_poschiavo <- df_heat_poschiavo |> 
  mutate(Lake = factor("Poschiavo"))

df_sarnen <- readRDS("df_derivatives/df_deriv_Sarnen.rds")

df_heat_sarnen <- aggregate(derivative ~ Species, data = df_sarnen, mean)
df_heat_sarnen <- df_heat_sarnen |> 
  mutate(Lake = factor("Sarnen"))

df_walen <- readRDS("df_derivatives/df_deriv_Walen.rds")

df_heat_walen <- aggregate(derivative ~ Species, data = df_walen, mean)
df_heat_walen <- df_heat_walen |> 
  mutate(Lake = factor("Walen"))

df_zug <- readRDS("df_derivatives/df_deriv_Zug.rds")

df_heat_zug <- aggregate(derivative ~ Species, data = df_zug, mean)
df_heat_zug <- df_heat_zug |> 
  mutate(Lake = factor("Zug"))

df_zurich <- readRDS("df_derivatives/df_deriv_Zurich.rds")

df_heat_zurich <- aggregate(derivative ~ Species, data = df_zurich, mean)
df_heat_zurich <- df_heat_zurich |> 
  mutate(Lake = factor("Zurich"))

df_zurich <- readRDS("df_derivatives/df_deriv_Geneva.rds")

df_heat_geneva <- aggregate(derivative ~ Species, data = df_geneva, mean)
df_heat_geneva <- df_heat_geneva |> 
  mutate(Lake = factor("Geneva"))

df_zurich <- readRDS("df_derivatives/df_deriv_Thun.rds")
df_heat_thun <- aggregate(derivative ~ Species, data = df_thun, mean)
df_heat_thun <- df_heat_thun |> 
  mutate(Lake = factor("Thun"))

df_zurich <- readRDS("df_derivatives/df_deriv_Constance.rds")

df_heat_constance <- aggregate(derivative ~ Species, data = df_constance, mean)
df_heat_constance <- df_heat_constance |> 
  mutate(Lake = factor("Constance"))

df_all <- bind_rows(df_heat_biel, df_heat_geneva, df_heat_thun, df_heat_constance, df_heat_brienz,
                df_heat_joux, df_heat_lucerne, df_heat_lugano, df_heat_maggiore, df_heat_morat,
                df_heat_neuchatel, df_heat_poschiavo, df_heat_sarnen, df_heat_walen, 
                df_heat_zug, df_heat_zurich)

saveRDS(df_all, file = "df_heatmap")

df_all  |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="turbo") 
