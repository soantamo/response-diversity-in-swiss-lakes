library(tidyverse)
library(here)
library(readr)
library(viridis)
library(lattice)
library(gt)


df_final <- readRDS("df_final.rds")

species <- df_final |> 
  group_by(Species) |> 
  summarize(tot_obs = sum(Abundance))


# df_coregonus <- df_final |> 
#   filter(str_detect(Species, "Coregonus")) |> 
#   group_by(Species, Lake) |> 
#   distinct(Lake)


str(df_final)
head(df_final)


################refining mistakes in taxa

# c. brienzii should not be in lake thun

df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Coregonus_brienzii") & Lake %in% c("Thun"),
                          "Coregonus_sp", as.character(Species)))

test <- df_final |> 
  filter(Species == "Coregonus_brienzii" & Lake == "Thun")

# salvelinus umbla in lake thun is salvelinus sp. 

df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Salvelinus_umbla") & Lake %in% c("Thun"),
                          "Salvelinus_sp", as.character(Species)))


test <- df_final |> 
  filter(Species == "Salvelinus_umbla" & Lake == "Thun")

# rename squalius squalus in biel and neuchatel to squalius cephalus

df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Squalius_squalus") & Lake %in% c("Biel", 
                                                                           "Neuchatel"),
                          "Squalius_cephalus", as.character(Species)))


# cottus_gobio_profundal lucerne, thun and walen put together 

df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c(
    "Cottus_gobio_Profundal_Thun",
    "Cottus_gobio_Profundal_Lucerne", "Cottus_gobio_Profundal_Walen"
  ),
  "Cottus_sp_Profundal", as.character(Species)
  ))


# pprepare salvelinus profundal and salvelinus limnetic

df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c( "Salvelinus_sp_Profundal_dwarf_Thun", 
                                          "Salvelinus_sp_Profundal_dwarf_VWS", 
                                          "Salvelinus_sp_Profundal_extreme_Thun",
    "Salvelinus_sp_Profundal_Walen_I", "Salvelinus_sp_Profundal_Walen_II"),
  "Salvelinus_sp_Profundal", as.character(Species)
  ))


# limnetic 
df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Salvelinus_sp_Limnetic_Thun", "Salvelinus_sp_Limnetic_VWS"),
                          "Salvelinus_sp_Limnetic", as.character(Species)))


# coregonus new groups based on ecomorphs
# following de-kayne et al. 2022
# supplementary data 1
# 1. Balchen: large-body, shallo spawning, feeding on benthic macroinvertebrates
# 2. Albeli: small species, deeper spawning (intermediate depth to deep), feed on 
# zooplankton in pelagic zone of lakes
# 3. Felchen: grow to larger sizes than the ‘Albeli’ ecomorph but not as large as the ‘Balchen’,
# feed on zooplankton,and feed and spawn fromn an intermediate depth to very deep
# now three less common ecomorphs
# 4. benthic profundal: C.profundus from Lake Thun, few gill-rakers but spawn at intermediat
# to great depth
# 5. pelagic-profundal: C. nobilis in Lake Lucerne, spawn deep but high number of 
# gill rakers
# 6. large_pelagic: u.a. c.wartmanii c.acrinasu and c. suspensus. Large-bodies, high
# gill-raker count and feed on zooplankton

# and new grouping based on Ole and Oliver Selz
# coregonus groups

# albeli
df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Coregonus_albellus", "Coregonus_candidus", "Coregonus_confusus",
                                         "Coregonus_heglingus", "Coregonus_zugensis"),
                          "Coregonus_sp_albeli", as.character(Species)))

# balchen
df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Coregonus_alpinus", "Coregonus_arenicolus", "Coregonus_duplex",
                        "Coregonus_helveticus", "Coregonus_palaea"),
                          "Coregonus_sp_balchen", as.character(Species)))

# felchen

df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Coregonus_brienzii", "Coregonus_fatioi", "Coregonus_intermundia",
                                         "Coregonus_litoralis", "Coregonus_macrophthalmus", "Coregonus_zuerichensis"),
                          "Coregonus_sp_felchen", as.character(Species)))
# large pelagic

df_final <- df_final |>
  mutate(Species = ifelse(Species %in% c("Coregonus_acrinasus", "Coregonus_wartmanni"),
                          "Coregonus_sp_large_pelagic", as.character(Species)))

# benthic profundal
df_final <- df_final |> 
  mutate(Species = ifelse(Species == "Coregonus_profundus",
                          "Coregonus_sp_benthic_profundal", as.character(Species)))
# pelagic profundal
df_final <- df_final |> 
  mutate(Species = ifelse(Species == "Coregonus_nobilis",
                          "Coregonus_sp_pelagic_profundal", as.character(Species)))

# "Coregonus_sarnensis": albeli or felchen

# check species
species <- df_final |> 
  distinct(Species)

# check which have less than 10 overall observations
exclusion <- df_final |> 
  group_by(Species) |> 
  summarize(tot_abu = sum(Presence)) |> 
  filter(tot_abu <= 10)

# species with less than 10 total observations are excluded
df_models <- df_final |> 
  group_by(Species) |> 
  mutate(tot_abu = sum(Presence)) |> 
  ungroup() |> 
  filter(tot_abu >= 10) 

df_models$Species <- as.factor(df_models$Species)
levels(df_models$Species)

species <- df_models |> 
  group_by(Species) |> 
  summarize(tot_obs = sum(Abundance))


###OVERVIEW
#1. species that dont have any catch > 1, 0 and 1 only -> binomial data
#prepare subsets of df with those species in it, separat for binomial and abundance data
#2. species that occur only in one lake: Lake not added as random effect

#1. binomial vs. abundance data

df_models$Species <- as.character(df_models$Species)

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



saveRDS(df_bi, "data_frame_models/df_binomial_gam")

#gam abundance without re -> zip
abu_one_occurence <- df_models |> 
  filter(Species %in% non_binomial_species) |> 
  group_by(Species) |> 
  summarize(n_lake = n_distinct(Lake)) |> 
  filter(n_lake == 1) |>
  pull(Species)


df_abu <- df_models |> 
  filter(Species %in% abu_one_occurence)

saveRDS(df_abu, "data_frame_models/df_abundance_gam")

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
  filter(Species %in% non_binomial_species) |> 
  group_by(Species) |> 
  summarize(Lakes = n_distinct(Lake)) |> 
  filter(!Lakes == 1) |> 
  pull(Species)

df_abundance_re <- df_models |> 
  filter(Species %in% abu_multi_occurence)

saveRDS(df_abundance_re, "data_frame_models/df_abundance_re")

# prepare excel to add all the species based on Projet Lac matrix

# mod1 <- readRDS("data_frame_models/df_binomial_re")
# mod2 <- readRDS("data_frame_models/df_binomial_gam")
# mod3 <- readRDS("data_frame_models/df_abundance_re")
# mod4 <- readRDS("data_frame_models/df_abundance_gam")
# 
# model_all <- bind_rows(mod1, mod2, mod3, mod4)


# all species with lakes they were caught in
# library(writexl)
# 
# df_species_lake <- model_all |> 
#   group_by(Species, Lake) |> 
#   distinct(Lake)
# 
# 
# 
# write_xlsx(df_species_lake, 'species_lake.xlsx')
