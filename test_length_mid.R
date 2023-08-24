# testing to do predictions with length_mid
#does this make sense though??

library(tidyverse)
library(patchwork)
library(ggtext)
library(mgcv)
library(gratia)
#library(hypervolume)
library(here)
library(readr)

# function includes lakeColors
source(here("color-and-functions.R"))

# load final dataset
df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

# change that presence and abundance are in one column with one column for value
nets_df_final <- df_final_no_electro |> 
  pivot_longer(
    cols = c("Abundance", "Presence"), 
    names_to = "Parameter", 
    values_to = "value"
  )
 
# make list of all lakes
lakes_list <- nets_df_final |> 
  distinct(Lake) |> 
  pull(Lake)
# working with an abundance subset of this dataset
# if else statement in loop would be a possibility
abu_nets_df_final <- nets_df_final |> 
  filter(Parameter == "Abundance")

