
library(tidyverse)
library(readr)
library(viridis)
library(corrmorant)
library(reshape2)
library(corrplot)
library(lattice)

#What do I want to show?
#Lets try getting the correlation values in a column 

df_heatmap_subset <- readRDS("df_heatmap_subset")
df_heatmap <- readRDS("df_heatmap")

deriv_species <- df_heatmap_subset |>
  pivot_wider(names_from = "Species", values_from = "derivative") |> 
  select(-Lake, -lake_derivative)

corr_column <- deriv_species |> 
  as.matrix() |> 
  cor(method = "pearson", use = "pairwise.complete.obs") |> 
  # `[<-`(lower.tri(., TRUE), NA) |> 
  as_tibble(rownames = "species1") |> 
  pivot_longer(cols=-1, names_to="species2", values_to="cor", values_drop_na = TRUE)


corr_column$composition <- paste(corr_column$species1, corr_column$species2, sep="-")

#following visualization of leary and petchey. still way too much information

alb <- corr_column |> 
  # filter(species1 == "Abramis_brama") |>
  ggplot(aes(x = species2, y = cor, fill = species2)) +
  geom_col() +
  scale_fill_viridis_d(option = "turbo") +
  facet_wrap(~species1, scales = "fixed") +
  guides(x = "none") +
  labs(x = "Species 2", y = "Pearson correlation", title = "Pearson correlation between all species")
  

alb


#trying to make correlation between species and lake derivative

deriv_lake <- df_heatmap_subset |>
  pivot_wider(names_from = "Species", values_from = "derivative") |> 
  select(-Lake, -lake_derivative)

df_heatmap_subset$species_lake <- paste(df_heatmap_subset$Species, df_heatmap_subset$Lake, sep="-")


correlation <- cor(df_heatmap_subset$derivative, df_heatmap_subset$lake_derivative)
print(correlation)

head(df_heatmap_subset)
df <- df_heatmap_subset |> 
  select(-Lake, -Species) 


#not working at all

#trying a levelplot
#works either with two continous variables + correlation coefficient or with 
#correlation matrix

corr_matrix <- deriv_species |> 
  as.matrix() |> 
  cor(method = "pearson", use = "pairwise.complete.obs")


levelplot(corr_matrix)

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)

corrplot(corr = cor(deriv_species, method = "pearson", use = "pairwise.complete.obs"),
         method = "color",  na.label = ".", tl.cex = 0.5, tl.col = "black",
         col = COL2("RdYlBu"))

#or 
#how did Radinger et al. do it?
#how does my data need to look like?


#new idea, correlation per lake
# df_biel <- readRDS("Subset/df_derivatives/df_deriv_Biel.rds")
# 
# test <- df_biel |> 
#   select(Species, temp, derivative) |> 
#   pivot_wider(names_from = "Species", values_from = "derivative") |> 
#   select(-temp)
# 
# cor_test <- cor(test)


#no idea what I am doing
# df_heatmap_subset |> 
#   select(-lake_derivative) |> 
#   ggplot(aes(x = Lake, y = Species, fill = derivative)) + 
#   geom_tile() +
#   scale_fill_viridis_c(option = "turbo")
# 
# 
# df_biel_temp <- readRDS("df_derivatives/df_deriv_Biel.rds")
# df_biel_depth <- readRDS("Depth/df_derivatives_abundance/df_deriv_Biel.rds")
# 
# df_biel_temp <- df_biel_temp |> 
#   select(Species, temp, derivative) |> 
#   rename(deriv = derivative)
# 
# df_biel_depth <- df_biel_depth |> 
#   select(Species, depth, derivative) |> 
#   select(-Species)
# 
# df <- bind_cols(df_biel_temp, df_biel_depth)
# 
# df |> 
#   ggplot(aes(x = temp, y = deriv)) +
#   geom_point()
# 
# levelplot(deriv ~ temp*depth, data = df)

