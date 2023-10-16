library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)

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
  filter(!Species %in% non_binary_species) |> 
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

#gam without re -> zip probably
abu_one_occurence <- df_models |> 
  filter(Abundance > 1) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(Lakes == 1) |> 
  pull(Species)

#binomial vs. abundance species occuring in several lakes

#gam with re, binomial
bi_multi_occurence <- df_models |> 
  filter(Species %in% binomial_species) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(!Lakes == 1) |> 
  pull(Species)

#gam with re, zip
abu_multi_occurence <- df_models |> 
  filter(Abundance > 1) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(!Lakes == 1) |> 
  pull(Species)

#What about the ones with only one occurence in one of the lakes?
#3. 
one_fish_in_lake <- df_models |> 
  group_by(Lake, Species) |> 
  summarize(TotalAbundance = sum(Abundance), .groups = 'drop') |> 
  filter(TotalAbundance == 1) |> 
  distinct(Species) |> 
  pull(Species)

#18 species
#what should I do with those? remove those lakes? i dont know
