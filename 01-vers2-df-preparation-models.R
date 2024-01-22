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

species <- df_final |> 
  filter(str_detect(Species, "Coregonus")) |> 
  group_by(Species) |> 
  summarize(tot_obs = sum(Presence))
  distinct(Species)
  
possible_exclusion <- df_final |> 
  filter(Species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii",
                        "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II",
                        "Cottus_sp_Profundal")) |> 
  group_by(Species) |> 
  summarize(tot_obs = sum(Presence))
  distinct(Species)

species |> 
  arrange(tot_obs) |> 
  gt()

str(df_final)
head(df_final)


# only include Coregonus brienzii from lake brienz

# df_final <- df_final |>
#   filter(!(Species == "Coregonus_brienzii" & Lake == "Thun"))
# 
# df_final$Species <- as.factor(df_final$Species)
# levels(df_final$Species)

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

# problems:
# coregonus helveticus: balchen based on publication from 1982 Svarvar and Müller

# coregonus zugensis: ????? extinct.
# not found c.sp. zugeralbeli and zugeralbock see. sehaausen page 44.
# only caught in lake lucerne, 99 individuals
# discuss in science discussion
# https://boris.unibe.ch/177154/
# The whitefish populations previously referred to as C. suidteri and C. zugensis 
# from Lake Lucerne are described as C. litoralis sp. nov. and C. muelleri sp. nov., respectively
# c.zugensis as c-muelleri?????
# https://zookeys.pensoft.net/article/67747/
# coregonus sarnensis: Translocated from northern to southern lakes, newly described
# ask Ole
# medium sized, 
# https://publication.plazi.org/GgServer/html/BA3CB832F9F2546AAF696C8564F07BDD
# spawning at 20m to lake bottom 50m
# evtl albeli


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

# can be deleted because less than 10 observations are excluded anyway

#0. species that occur only once in one lake are excluded
#only caught once in a lake
# one_occurence <- df_final |>
#   group_by(Species) |>
#   summarize(Lakes = n_distinct(Lake)) |> 
#   filter(Lakes == 1) |>
#   pull(Species)
# 
# #all species that were only caught once across whole projet lac are excluded
# df_final$Species <- as.character(df_final$Species)
# 
# exclude <- df_final |>
#   group_by(Lake, Species) |>
#   summarize(TotalAbundance = sum(Abundance)) |>
#   filter(TotalAbundance == 1) |>
#   filter(Species %in% one_occurence) |>
#   distinct(Species)
#   pull(Species)


#double-check
# 
# double_check <- df_final |> 
#   filter(Species %in% "Salaria_fluviatilis_Italian")
# 
# table(double_check$Abundance)



#these 10 species can be excluded from the whole analysis -> 80 total species
# df_models <- df_final |> 
#   filter(!Species %in% exclude)
# 
# df_models |>
#   distinct(Species) |> 
#   pull(Species)


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


