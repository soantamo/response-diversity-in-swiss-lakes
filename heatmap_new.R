
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
  pivot_longer(cols=-1, names_to="species2", values_to="cor", values_drop_na=TRUE)


corr_column$composition <- paste(corr_column$species1, corr_column$species2, sep="-")

#following visualization of leary and petchey. still way too much information
corr_column |> 
  filter(species1 == "Alburnus_alburnus") |> 
  ggplot(aes(x = composition, y = cor, fill = species2)) +
  geom_col()

#trying a levelplot
#works either with two continous variables + correlation coefficient or with 
#correlation matrix

corr_matrix <- deriv_species |> 
  as.matrix() |> 
  cor(method = "pearson", use = "pairwise.complete.obs")

levelplot(corr_matrix)

#or 
#how did Radinger et al. do it?

