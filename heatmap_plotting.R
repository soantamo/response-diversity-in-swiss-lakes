#heatmap plotting

library(tidyverse)
library(readr)
library(viridis)
library(corrmorant)
library(reshape2)
library(corrplot)

#df with mean values of derivatives per species and lake

df_heatmap_subset <- readRDS("df_heatmap_subset")
df_heatmap <- readRDS("df_heatmap")




#make dataframe wide for correlation, 4 biggest lakes

deriv_species <- df_heatmap_subset |>
  filter(Lake %in% c("Geneva", "Neuchatel", "Constance", "Lucerne")) |>
  # filter(Species %in% c("Alburnus_alburnus", "Coregonus_sp", "Perca_fluviatilis", "Rutilus_rutilus")) |> 
  pivot_wider(names_from = "Species", values_from = "derivative") |> 
  select(-Lake, -lake_derivative)
#   mutate(Geneva = 2.034901) |> 
#   mutate(Neuchatel = 1.234619) |> 
#   mutate(Lucerne = 0.05487739) |> 
#   mutate(Constance = 0.8363541)
# # 
# deriv_lakes <-  df_heatmap_subset |>
#   filter(Lake %in% c("Geneva", "Neuchatel", "Constance", "Lucerne")) |>
#   pivot_wider(names_from = "Lake", values_from = "lake_derivative") |>
#   select(-derivative, -Species)


head(deriv_species)

#if two data points per species are only there -> can only be -1 or 1
#with pairwise.complete.obs the problem of missing values is solved
#the problem is that I dont need species vs species, but species vs lakes

cor(deriv_species, method = "pearson", use = "pairwise.complete.obs")

COL2(diverging = c("RdBu", "BrBG", "PiYG", "PRGn", "PuOr", "RdYlBu"), n = 200)

corrplot(corr = cor(deriv_species, method = "pearson", use = "pairwise.complete.obs"),
         method = "color",  na.label = ".", tl.cex = 0.5, tl.col = "black",
         col = COL2("RdYlBu"))

# type = "lower",

#based on internet

corr_mat <- round(cor(deriv_species, method = "pearson", use = "pairwise.complete.obs"),2) 
head(corr_mat)

# reduce the size of correlation matrix
melted_corr_mat <- melt(corr_mat)

# plotting the correlation heatmap

ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2, fill=value)) +
  geom_tile() +
  # geom_text(aes(Var2, Var1, label = value),
  #           color = "white", size = 4) +
  scale_fill_viridis(option="viridis")
  # scale_fill_gradient2(low = "white", high = "red")


#only working without NAs
corrmorant(deriv_species)

suitability_corplot <- corrmorant(deriv_species, corr_method = "pearson", style = "binned")+
  theme(legend.position = "bottom") +
  scale_color_viridis(name = "Correlation", option = "E", direction = -1, limits = c(-1, 1)) +
  theme(axis.text = element_text(size=5),
        axis.text.x = element_text(angle = 45) )

suitability_corplot

# df_heatmap_subset |> 
#   ggplot(aes(Lake, Species, fill = derivative)) +
#   geom_tile() +
#   scale_fill_viridis(option="plasma") 
# 
# df_heatmap_subset |> 
#   ggplot(aes(Lake, Species, fill = derivative)) +
#   geom_tile() +
#   scale_fill_viridis(option="viridis") 
# 
# df_heatmap_subset |> 
#   ggplot(aes(Lake, Species, fill = derivative)) +
#   geom_tile() +
#   scale_fill_viridis(option="turbo")
# 
# ##all species
# 
# df_heatmap |> 
#   ggplot(aes(Lake, Species, fill = derivative)) +
#   geom_tile() +
#   scale_fill_viridis(option="turbo")
