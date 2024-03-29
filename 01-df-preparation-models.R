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

#
# only include brienzii from lake brienz

df_final <- df_final |> 
  filter(!(Species == "Coregonus_brienzii" & Lake == "Thun"))

df_final$Species <- as.factor(df_final$Species)
levels(df_final$Species)

# cottus_gobio_profundal lucerne, thun and walen put together 

df_final <- df_final |> 
  mutate(Species = ifelse(Species %in% c(
   "Cottus_gobio_Profundal_Thun",
   "Cottus_gobio_Profundal_Lucerne", "Cottus_gobio_Profundal_Walen"
 ),
 "Cottus_gobio_Profundal", as.character(Species)
 ))

# df_final$Species <- as.factor(df_final$Species)
levels(df_final$Species)

# test <- df_final |> 
#   filter(Species == "Cottus_gobio_Profundal") |> 
#   select(Lake, Species, Abundance)
  

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

df_models |> 
  distinct(Species) |> 
  pull(Species)
###OVERVIEW
#three problems: 
#0. species that occurin only once in one lake are excluded
#1. species that dont have any catch > 1 -> 0 and 1 only -> binomial data
#prepare subsets of df with those species in it
#2. species that occur only in one lake
#3. species that occur only once in a lake

#new problem: should I put a minimum total_abundance per lake to be included in the analysis?
#write code and then decide threshold
#not finished yet

# tot_abundance_lake <- df_models |> 
#   group_by(Lake, Species) |> 
#   summarize(TotalAbundance = sum(Abundance)) |> 
#   filter(TotalAbundance >= 5) 

#1. binomial vs. abundance data

non_binomial_species <- df_models |> 
  filter(Abundance > 1) |> 
  group_by(Species) |>
  distinct(Species) |> 
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
  summarize(n_lake = n_distinct(Lake)) |>
  filter(n_lake == 1) |>
  pull(Species)

df_bi <- df_models |> 
  filter(Species %in% bi_one_occurence)

#add  #Coregonus_duplex and Salmo_marmormatus

overview <- df_models |>  
  filter(Species %in% c("Coregonus_duplex", "Salmo_marmoratus")) |> 
  group_by(Species, Lake, Abundance) |> 
  summarize(abu = sum(Abundance))

# only lake with > 1 observation
coregonus_duplex <- df_models |>
  filter(Species == "Coregonus_duplex") |> 
  filter(Lake == "Zurich")

salmo_marmo <- df_models |> 
  filter(Species == "Salmo_marmoratus") |> 
  filter(Lake == "Poschiavo")
  

df_binomial_gam <- bind_rows(df_bi, coregonus_duplex, salmo_marmo)

df_binomial_gam |> 
  distinct(Species) |> 
  pull(Species)

# df_binomial_gam |> 
#   filter(Species == "Coregonus_duplex") |> 
#   distinct(Lake)

# saveRDS(df_binomial_gam, "data_frame_models/df_binomial_gam")

#gam without re -> zip probably
abu_one_occurence <- df_models |> 
  filter(Species %in% non_binomial_species) |> 
  group_by(Species) |> 
  summarize(n_lake = n_distinct(Lake)) |>
  filter(n_lake == 1) |>
  pull(Species)


df_abu <- df_models |> 
  filter(Species %in% abu_one_occurence)

#Alosa_agone and  "Cottus_sp_Po" need to go to model 2

overview2 <- df_models |> 
  filter(Species %in% c("Alosa_agone", "Cottus_sp_Po")) |> 
  group_by(Species, Lake, Abundance) |> 
  summarize(abu = sum(Abundance))


# only lake with > 1 observation
alosa_agone <- df_models |>
  filter(Species == "Alosa_agone") |> 
  filter(Lake == "Maggiore")

cottus_sp_po <- df_models |> 
  filter(Species == "Cottus_sp_Po") |> 
  filter(Lake == "Poschiavo")


df_abundance_gam <- bind_rows(df_abu, alosa_agone, cottus_sp_po)

df_abundance_gam |> 
  distinct(Species) |> 
  pull(Species)

df_abundance_gam |> 
  filter(Species == "Cottus_sp_Po") |> 
  distinct(Lake)

# saveRDS(df_abundance_gam, "data_frame_models/df_abundance_gam")

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


# saveRDS(df_binomial_re, "data_frame_models/df_binomial_re")

#gam with re, zip
abu_multi_occurence <- df_models |> 
  filter(Species %in% non_binomial_species) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(!Lakes == 1) |> 
  pull(Species)

df_abundance_re <- df_models |> 
  filter(Species %in% abu_multi_occurence)
# df_abundance_re |> 
#   distinct(Species) |> 
#   pull(Species)
 
# saveRDS(df_abundance_re, "data_frame_models/df_abundance_re")

#What about the ones with only one occurence in one of the lakes?
#10.11. here we could set a higher threshold
# should those be included again? does code run with this data? 
#3. 
one_fish_in_lake <- df_models |> 
  group_by(Lake, Species) |> 
  summarize(TotalAbundance = sum(Abundance), .groups = 'drop') |> 
  filter(TotalAbundance == 1) |> 
  # distinct(Species) |>
  # pull(Species)
  distinct(Species, Lake)


#18 species
#what should I do with those? remove!!
#for model 3 

lakes_one_fish <- df_binomial_re|> 
  group_by(Lake, Species) |> 
  summarize(TotalAbundance = sum(Abundance)) |> 
  filter(TotalAbundance == 1) |>
  select(-TotalAbundance)

model_3 <- df_binomial_re |> 
  anti_join(lakes_one_fish, by = c("Lake", "Species"))

#check if all species occur in multiple lakes, 2 occur only in one lake, remove

species_mod_2 <- model_3 |> 
  group_by(Species) |>
  summarize(n_lake = n_distinct(Lake)) |>
  filter(n_lake == 1) |> 
  distinct(Species) |>
  pull(Species)

#Coregonus_duplex and Salmo_marmormatus need to be added to model 1 and removed from 3

df_binomial_re_excluded <- model_3 |> 
  filter(!Species %in% c("Coregonus_duplex", "Salmo_marmoratus"))

df_binomial_re_excluded |> 
  distinct(Species) |> 
  pull(Species)

saveRDS(df_binomial_re_excluded, "data_frame_models/df_binomial_re")

#model 4, repeat

lakes_one_fish_4 <- df_abundance_re |> 
  group_by(Lake, Species) |> 
  summarize(TotalAbundance = sum(Abundance)) |> 
  filter(TotalAbundance == 1) |>
  select(-TotalAbundance)

model_4 <- df_abundance_re |> 
  anti_join(lakes_one_fish_4, by = c("Lake", "Species"))

species_mod_2 <- model_4 |> 
  group_by(Species) |>
  summarize(n_lake = n_distinct(Lake)) |>
  filter(n_lake == 1) |> 
  distinct(Species) |>
  pull(Species)


df_abundance_re_excluded <- model_4 |> 
  filter(!Species %in% c("Cottus_sp_Po", "Alosa_agone"))

df_abundance_re_excluded |> 
  distinct(Species) |> 
  pull(Species)


# saveRDS(df_abundance_re_excluded, "data_frame_models/df_abundance_re")

#Alosa_agone and  "Cottus_sp_Po" need to go to model 2


###solve this problem next, include the 2 species each and newly safe those rds for model 1 and 2
