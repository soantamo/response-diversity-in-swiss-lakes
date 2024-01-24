###df for data analysis preparation

library(tidyverse)

projet_lac_data <- read.csv("~/Downloads/PLDB_final_short_depth_climate_cleantaxa_20052022(2).csv")

abundance_data <- projet_lac_data |> 
  # take out no fish
  filter(!Taxa_latin_FINAL %in% "NO_FISH") |> 
  filter(!Protocol %in% "electro") |> 
  #add LakeBasin, Fishec_Action (observation, defines presence absence of species), temp. day or 7days, 
  group_by(Lake, Fishec_action, mean_temp_per_day, mean_last_7days, Weightg_raw, Weightg_soak, Depth_sample, Taxa_latin_FINAL, Protocol) |> #added Protocol to be able to only look at netting
  tally() |> 
  # values_fill = 0 added to make all NAs into 0
  pivot_wider(names_from = Taxa_latin_FINAL, values_from = n, values_fill = 0) 

abundance_data_long <- abundance_data |> 
  pivot_longer(cols = !(Lake : Protocol), names_to = "Species", values_to = "Abundance")

abundance_data_long$Presence <- ifelse(abundance_data_long$Abundance >= 1, 1, 0)

# View(abundance_data_long)

max(abundance_data_long$Presence)
max(abundance_data_long$Abundance)

#we only need present species per lake, not all of them
abundance_data_long_lakepresence <- abundance_data_long |> 
  group_by(Lake, Species) |>
  # . stands for data being piped in do function
  do(LakePresence = max(.$Presence)) |> 
  unnest(cols = LakePresence)

# species per lake
abundance_data_long_lakepresence |>
  filter(LakePresence %in% "1") |> 
  group_by(Lake, LakePresence) |>
  count()

df_final <- abundance_data_long_lakepresence |> 
  filter(LakePresence %in% "1") |> 
  left_join(abundance_data_long )

#double-check if all lakepresence = 1
max(df_final$LakePresence)
min(df_final$LakePresence)

saveRDS(df_final, "/home/sophie/Dokumente/Master Thesis R/response-diversity-in-swiss-lakes/df_final.rds")