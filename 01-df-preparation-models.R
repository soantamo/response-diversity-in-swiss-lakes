library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)


df_final <- readRDS("df_final.rds")

str(df_final)
head(df_final)

###OVERVIEW
#three problems: 
#1. species that dont have any catch > 1 -> 0 and 1 only -> binomial data
#prepare subsets of df with those species in it
#2. species that occur only in one lake
#3. species that occur only once in a lake

#0. species that occur only once in one lake are excluded

#are some of those only occuring in one Lake?

#only occuring in one lake
one_occurence <- df_final |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(Lakes == 1) |> 
  pull(Species)

#which species are occuring once in one lake and generally only in one lake?
#overall one individual of those caught
exclude <- df_final |> 
  group_by(Lake, Species) |> 
  summarize(TotalAbundance = sum(Abundance)) |> 
  filter(TotalAbundance == 1) |> 
  filter(Species %in% one_occurence) |> 
  distinct(Species) |> 
  pull(Species)


#double-check

double_check <- df_final |> 
  filter(Species %in% "Salaria_fluviatilis_Italian")

table(double_check$Abundance)

#true

#these 10 species can be excluded from the whole analysis -> 82 total species
df_models <- df_final |> 
  filter(!Species %in% exclude)

###OVERVIEW
#three problems: 
#0. species that occurin only once in one lake are excluded
#1. species that dont have any catch > 1 -> 0 and 1 only -> binomial data
#prepare subsets of df with those species in it
#2. species that occur only in one lake
#3. species that occur only once in a lake


#1. binomial vs. abundance data

non_binomial_species <- df_models |> 
  filter(Abundance > 1) |> 
  group_by(Species) |>
  count() |> 
  pull(Species)

binomial_species <- df_models |> 
  filter(!Species %in% non_binomial_species) |> 
  distinct(Species) |> 
  pull(Species)

#2. binomial vs. abundance species occuring only in one lake

#gam without random effects, binomial
bi_one_occurence <- df_models |> 
  filter(Species %in% binomial_species) |>
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(Lakes == 1) |> 
  pull(Species)

#prepare df

df_binomial_gam <- df_models |> 
  filter(Species %in% bi_one_occurence)

saveRDS(df_binomial_gam, "data_frame_models/df_binomial_gam")

#gam without re -> zip probably
abu_one_occurence <- df_models |> 
  filter(Abundance > 1) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(Lakes == 1) |> 
  pull(Species)

df_abundance_gam <- df_models |> 
  filter(Species %in% abu_one_occurence)

saveRDS(df_abundance_gam, "data_frame_models/df_abundance_gam")
  

#binomial vs. abundance species occuring in several lakes

#gam with re, binomial
bi_multi_occurence <- df_models |> 
  filter(Species %in% binomial_species) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(!Lakes == 1) |> 
  pull(Species)

df_binomial_re <- df_models |> 
  filter(Species %in% bi_multi_occurence)

saveRDS(df_binomial_re, "data_frame_models/df_binomial_re")

#gam with re, zip
abu_multi_occurence <- df_models |> 
  filter(Abundance > 1) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(!Lakes == 1) |> 
  pull(Species)

df_abundance_re <- df_models |> 
  filter(Species %in% abu_multi_occurence)

saveRDS(df_abundance_re, "data_frame_models/df_abundance_re")

#What about the ones with only one occurence in one of the lakes?
#3. 
one_fish_in_lake <- df_models |> 
  group_by(Lake, Species) |> 
  summarize(TotalAbundance = sum(Abundance), .groups = 'drop') |> 
  filter(TotalAbundance == 1) |> 
  distinct(Species) |> 
  pull(Species)


#18 species
#look at the lakes and species

lakes_one_fish <- df_models |> 
  group_by(Lake, Species) |> 
  summarize(TotalAbundance = sum(Abundance), .groups = 'drop') |> 
  filter(TotalAbundance == 1) |> 
  pivot_wider(names_from = Species, values_from = TotalAbundance)

#what should I do with those? remove those lakes? i dont know
#testing what happens for one of those fish: Tinca_tinca
#no problem with this


df_tinca <- df_models |> 
  filter(Species %in% "Tinca_tinca")

df_tinca$fLake <- as.factor(df_tinca$Lake)

M3 <- gam(Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're'),
          family = ziP(), data = df_tinca)
plot(M3)

