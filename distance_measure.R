
library(tidyverse)
library(readr)
library(viridis)
library(corrmorant)
library(reshape2)
library(corrplot)
library(lattice)
library(ggpubr)
library(vegan)

#distance measure bray-curtis

df_subset <- readRDS("df_heatmap_subset")

shared <- df_subset |> 
  select(-lake_derivative) |>
  pivot_wider(names_from = Species, values_from = derivative) |> 
  as.data.frame()


rownames(shared) <- shared$Lake
shared <- shared[, -1] #remove Species column
shared <- as.matrix(shared)

dist <- vegdist(shared, method = "bray", na.rm = TRUE)

test <- dist |> 
  as_tibble()

metaMDS(dist)


data(varespec)
