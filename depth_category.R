library(tidyverse)
library(patchwork)
library(ggtext)
library(gratia)
library(here)
library(readr)
library(glue)
library(TITAN2)

# load final dataset
df_final_no_electro <- read_rds("/home/sophie/Dokumente/Master Thesis R/Master Thesis Analysis/df_final_ne.rds")

max(df_final_no_electro$Depth_sample, na.rm = TRUE)

df_sites <- df_final_no_electro |> 
  mutate(Depth_category = cut(Depth_sample, breaks = 305, labels = paste0(1:305))) |> 
  mutate(Site_id = glue("{Lake}_{Depth_category}")) |> 
  group_by(Site_id, Species) |> 
  summarise(Abundance = sum(Abundance), temp = mean(mean_last_7days))

df_sites_wide <- df_sites |> 
  select(-temp) |> 
  pivot_wider(names_from = Species, values_from = Abundance, values_fill = 0)

site_temp <- df_sites |> 
  select(Site_id, temp) |> 
  distinct(Site_id, .keep_all = TRUE) |> 
  drop_na()


taxa_titan_temp <- df_sites_wide %>% 
  right_join(site_temp)

taxa_titan_temp |> skimr::skim()

taxa_remove<- taxa_titan_temp %>%
  dplyr::select(-temp) %>% 
  pivot_longer(2:93,names_to = "Species", values_to = "Abundance") %>%
  filter(Abundance > 0) %>%
  count(Site_id,Species) |> 
  group_by(Species) %>%
  dplyr::summarize(frequency= sum(n,na.rm = TRUE)) %>%
  filter(frequency > 4) %>%
  pull(Species)

taxa_titan_temp <- taxa_titan_temp %>%
  dplyr::select(Site_id, temp,!!taxa_remove)

#prep data for TITAN analysis

#environmental gradient

titan_temp <- taxa_titan_temp %>% 
  dplyr::select(Site_id, temp)

#taxa
titan_fish_temp <- taxa_titan_temp %>% 
  dplyr::select(-temp)

#prepare data matrix for TITAN
row.names(titan_temp) <- titan_temp$Site_id
row.names(titan_fish_temp) <- titan_fish_temp$Site_id


titan_temp<-titan_temp[,-1]
titan_fish_temp<-titan_fish_temp[,-1]

install.packages("TITAN2")

maes.taxa.titan.sub <- titan(titan_temp, titan_fish_temp, minSplt = 5, numPerm = 250, boot = TRUE, nBoot = 500, 
                             imax = FALSE, ivTot = FALSE, pur.cut = 0.95, rel.cut = 0.95, ncpus = 1, memory = FALSE)

maes.taxa.titan.sub$sumz.cp

tiff("Optimum_TITAN_Substrate_Genera_BMI_Final_Revised.tiff", units="in", width=20, height=20, res=600, bg="white", pointsize=20)

plot_taxa(maes.taxa.titan.sub, xlabel = "Median Substrate diameter [cm]",leg.x = 2,leg.y=2,cex.taxa = 1,legend = FALSE)

dev.off()

plot_taxa_ridges(maes.taxa.titan.sub,xlabel = "Median Substrate diameter [cm]",axis.text.x = 15,axis.text.y = 15,axis.title.x = 15,axis.title.y = 15)

ggsave("TITAN_Ridgeplot_Sub_Genus_Final_Revised.tiff", dpi = 600,height = 25,width = 25,units = "cm")

plot_sumz_density(maes.taxa.titan.sub,xlabel = "Median Substrate Diameter (cm)",axis.text.x = 15,axis.text.y = 15,axis.title.x = 15,axis.title.y = 15)

ggsave("TITAN_Sumz_Density_Community_Substrate_Final_Revised.tiff", dpi = 600,height = 25,width = 25,units = "cm")

