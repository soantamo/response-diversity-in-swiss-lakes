#heatmap plotting

library(tidyverse)
library(readr)
library(viridis)


df_heatmap_subset <- readRDS("df_heatmap_subset")
df_heatmap <- readRDS("df_heatmap")

df_subset_wide <-  df_heatmap_subset |>
  filter(Lake %in% c("Biel", "Brienz")) |> 
  pivot_wider(names_from = "Species", values_from = "derivative") |> 
  select(-Lake)
#make these correlations, dont know how :((

head(df_subset_wide)

#problem is NAs, but we do have a lot of them because species dont occur at the same lakes
#no correlation possible
correlation_test <- cor(df_subset_wide, use = "complete.obs", method = "pearson")


##this works!!, but is it correct?
cormat <- round(cor(x = as.matrix(df_subset_wide), method = "pearson", use = "pairwise.complete.obs"), 2)
head(cormat)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

suitability_corplot <- corrmorant::corrmorant(as.matrix(d_index_chart), corr_method = "pearson",
                                              use = "pairwise.complete.obs", style = "binned")+
  theme(legend.position = "bottom") +
  scale_color_viridis(name = "Correlation", option = "E", direction = -1, limits = c(-1, 1)) +
  theme(axis.text = element_text(size=5),
        axis.text.x = element_text(angle = 45) )

library(corrmorant)

data_numeric <- as.numeric(df_subset_wide)

cor(df_subset_wide, method = "pearson")

suitability_corplot <- corrmorant::corrmorant(df_subset_wide, corr_method = "pearson", style = "binned")+
  theme(legend.position = "bottom") +
  scale_color_viridis(name = "Correlation", option = "E", direction = -1, limits = c(-1, 1)) +
  theme(axis.text = element_text(size=5),
        axis.text.x = element_text(angle = 45) )




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
