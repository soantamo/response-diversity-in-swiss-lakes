library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)


#loading subsets of dfs 
#rename, names too long

df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")

df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")

df_binomial_re <- readRDS("data_frame_models/df_binomial_re")

df_abundance_re <- readRDS("data_frame_models/df_abundance_re")


