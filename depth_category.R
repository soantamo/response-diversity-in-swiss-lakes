library(tidyverse)
library(patchwork)
library(ggtext)
library(gratia)
library(here)
library(readr)
library(Hmisc)
library(glue)
# load final dataset
df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

max(df_final_no_electro$Depth_sample, na.rm = TRUE)

test <- df_final_no_electro |> 
  mutate(Depth_category = cut(Depth_sample, breaks = 305, labels = paste0(1:305)))

df_sites <- test |> 
  mutate(Site_id = glue("{Lake}_{Depth_category}"))


df_sites$Site_id <- as.factor(df_sites$Site_id)

biel <- df_sites |> 
  filter(Lake %in% "Joux")

levels(biel$Site_id)
