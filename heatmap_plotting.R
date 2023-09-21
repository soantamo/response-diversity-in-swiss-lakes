#heatmap plotting

library(tidyverse)
library(readr)
library(viridis)

df_heatmap_subset <- readRDS("df_heatmap_subset")
df_heatmap <- readRDS("df_heatmap")


df_heatmap_subset |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="plasma") 

df_heatmap_subset |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="viridis") 

df_heatmap_subset |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="turbo")

##all species

df_heatmap |> 
  ggplot(aes(Lake, Species, fill = derivative)) +
  geom_tile() +
  scale_fill_viridis(option="turbo")
