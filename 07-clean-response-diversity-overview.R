library(tidyverse)
library(mgcv)
library(gratia)
library(here)
library(readr)
library(viridis)
library(gamm4)
library(lattice)
library(broom)
library(mgcViz)
library(readxl)
library(forcats)
library(gridExtra)
library(grid)
library(gghighlight)


source(here("functions.R"))

df_final <- readRDS("df_final.rds")
  

# interpretation:

mod_1_pred <- readRDS("total_models/pred_model_1_total")
mod_2_pred <- readRDS("total_models/pred_model_2_total")
mod_3_pred <- readRDS("total_models/pred_model_3_total")
mod_4_pred <- readRDS("total_models/pred_model_4_total")

model_predictions <- bind_rows(mod_1_pred, mod_2_pred, mod_3_pred, mod_4_pred) |> 
  select(-fProtocol)

mod_1_deriv <- readRDS("total_models/deriv_model_1_total")
mod_2_deriv <- readRDS("total_models/deriv_model_2_total")
mod_3_deriv <- readRDS("total_models/deriv_model_3_total")
mod_4_deriv <- readRDS("total_models/deriv_model_4_total")

all_models_derivatives <- bind_rows(mod_1_deriv, mod_2_deriv, mod_3_deriv, mod_4_deriv)
all_lakes_tib <- as_tibble(all_models_derivatives)

resp_div_all <- readRDS("total_models/lakes_all_models/resp_div_all.rds")
# species_overview <- readRDS("total_models/lakes_all_models/species_overview.rds")

################ overview table for models and success

success_models <- read_excel("model_1/model_success_final.xlsx") |> 
  select(-2)

library(gt)

success_models |> 
  arrange(species) |> 
  gt()

success_models |> 
  arrange(tot_obs) |> 
  gt()


successful <- success_models |> 
  filter(success == 1)


#################species overview

# lollipop with number of species in each lake 
species_overview |> 
  ggplot(aes(x = fct_reorder(fLake, sum_species), y = sum_species)) +
  geom_segment(aes(x=fct_reorder(fLake, sum_species), xend=fct_reorder(fLake, sum_species), y=0, yend=sum_species), color ="darkgrey") +
  geom_point(size=2) +
  coord_flip() +
  xlab("") +
  ylab("Number of species") +
  theme_classic()



###################Response diversity plots


resp_div_all |>
  ggplot(aes(temp, rdiv)) +
  geom_line() +
  facet_wrap(~fLake)

resp_div_all |>
  ggplot(aes(temp, sign)) +
  geom_line() +
  facet_wrap(~fLake)

df_mean_all <- resp_div_all |>
  group_by(fLake) |> 
  summarise(mean_rdiv = mean(rdiv))

# dissimilarity_all <- resp_div_all |>
#   ggplot(aes(x = temp, y = rdiv)) +
#   geom_line(color = "#54008B") +
#   geom_hline(data = df_mean_all, aes(yintercept = mean_rdiv), linewidth = 0.5,
#              lty = "dashed")+
#   facet_wrap(~fLake) +
#   theme_bw() +
#   ggtitle("80 Species") +
#   ylim(0,7.5)


df_mean_divergence_all <- resp_div_all |>
  group_by(fLake) |> 
  summarise(mean_sign = mean(sign))

# divergence_all <- resp_div_all|>
#   ggplot(aes(x = temp, y = sign, color = fLake)) +
#   geom_line(color = "#54008B") +
#   geom_hline(data = df_mean_divergence_all, aes(yintercept = mean_sign), linewidth = 0.5,
#              lty = "dashed")+
#   facet_wrap(~fLake)  +
#   ggtitle("80 Species") +
#   theme_bw()


df_means <- merge(df_mean_all, df_mean_divergence_all) |> 
  rename(Lake = fLake, mean_dissimilarity = mean_rdiv, mean_divergence = mean_sign)


#plotting means

plot_means <- df_means |> 
  ggplot(aes(mean_dissimilarity, mean_divergence)) +
  # geom_point(aes(color = Lake))
  # geom_label(aes(label = Lake))
  # geom_point() +
  geom_text(aes(label = Lake)) +
  # geom_text(aes(label = Lake), nudge_x = 0.01, nudge_y = 0.01,  check_overlap = T) +
  #   check_overlap = T) +
  theme_bw()

plot_means

library(ggrepel)
library(hrbrthemes)

plot_means + 
  # horizontal
  geom_hline(yintercept=0.75, color="orange", linewidth=1) + 
  # vertical
  geom_vline(xintercept=1.9, color="orange", linewidth=1)

p <- df_means |> 
  ggplot(aes(mean_dissimilarity, mean_divergence)) +
  geom_point(color = "#007ED3") +
  theme_ipsum()

plot_means <- p + geom_text_repel(aes(label = Lake),
                    size = 3.5, 
                    max.overlaps = 13)


tiff(paste("total_models/plots/poster_fig_3.tiff", sep = ""), units="in", width=5, height=5, res=300)

plot(plot_means)

# Closing the graphical device
dev.off()



# tiff("total_models/.tiff")
# print(plot_means)
# dev.off()

# save as excel 
# write_xlsx(df_means, "total_models/response_diversity_overview.xlsx")


# looop plotting predictions each lake and divergence and dissimilarity

lake_list <- resp_div_all |> 
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)



for (i in lake_list){
  
  species_list <- all_lakes_tib |> 
    filter(fLake == i) |> 
    distinct(species) |> 
    pull(species)
  
  data_deriv <- all_lakes_tib |> 
    filter(fLake == i)
  
  minimum <- min(data_deriv$temp)
  maximum <- max(data_deriv$temp)
  
  data_pred <- model_predictions |> 
    filter(species %in% species_list) |> 
    filter(temp > minimum & temp < maximum)
  
  data_resp_div <-resp_div_all |> 
    filter(fLake == i)
  
  lake_prediction <- data_pred |>
    mutate(upper_se = fit + se.fit, lower_se = fit - se.fit)  |>
    ggplot(aes(temp, fit, color = factor(species))) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower_se, ymax = upper_se), alpha = 0.3) +
    theme_bw() +
    theme(strip.background = element_rect(fill="lightgrey")) +
    scale_color_viridis(discrete=TRUE, guide = NULL) +
    ylab("Abundance") +
    ggtitle(i)
  
  deriv_plot <- data_deriv |> 
    ggplot(aes(temp, derivative, color = factor(species))) +
    geom_line() +
    theme_bw() +
    theme(strip.background = element_rect(fill="lightgrey")) +
    scale_color_viridis(discrete=TRUE, guide = NULL) +
    ylab("Derivative")
  
  
  df_mean_all <- data_resp_div |>
    group_by(fLake) |>
    summarise(mean_rdiv = mean(rdiv))
  
  dissimilarity_all <- data_resp_div |>
    ggplot(aes(x = temp, y = rdiv)) +
    geom_line(color = "#54008B") +
    geom_hline(data = df_mean_all, aes(yintercept = mean_rdiv), linewidth = 0.5,
               lty = "dashed")+
    # facet_wrap(~fLake) +
    theme_bw() +
    ylab("Dissimilarity") +
    ylim(1,7.5)
  
  df_mean_divergence_all <- data_resp_div |>
    group_by(fLake) |>
    summarise(mean_sign = mean(sign))
  
  divergence_all <- data_resp_div |>
    ggplot(aes(x = temp, y = sign, color = fLake)) +
    geom_line(color = "#54008B") +
    # geom_hline(data = df_mean_divergence_all, aes(yintercept = mean_sign), linewidth = 0.5,
    #            lty = "dashed") +
    # facet_wrap(~fLake)  +
    theme_bw() +
    ylab("Divergence") +
    ylim(0, 1)
  
  # Opening the graphical device
  
  # pdf(paste("total_models/plots/plot_lake_", i, ".pdf", sep = ""), width = 4, height = 8)
  # tiff(paste("total_models/plots/plot_lake_", i, ".tiff", sep = ""), compression = "lzw",  units = "cm",
  #      width = 6, height = 13, pointsize = 18, res = 300)
  
  # tiff(paste("total_models/plots/plot_predictions_no_guide_", i, ".tiff", sep = ""), compression = "lzw",  units = "cm",
  #      width = 12, height = 8, pointsize = 18, res = 300)
  # 
  # 
  # grid_all <- grid.arrange(lake_prediction, deriv_plot,
  #                          dissimilarity_all, divergence_all, nrow = 4)
  # 
  # grid_all <- grid.arrange(lake_prediction, nrow = 1)
  # 
  # # Closing the graphical device
  # dev.off()
  # # 
    tiff(paste("total_models/plots/response_diversity_", i, ".tiff"), compression = "lzw",  units = "cm",
         width = 8, height = 13, pointsize = 18, res = 300)

    grid_all <- grid.arrange(dissimilarity_all, divergence_all, nrow = 2,
                             top = textGrob( i ,gp=gpar(fontsize=20,font=3)))

    # Closing the graphical device
    dev.off()
  
  
  
}

# boxplots of species and derivatives for each lake

# differences in derivatives per species
# boxplot is a possibility

lake_list <- all_lakes_tib |> 
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 25
mycolors <- colorRampPalette(brewer.pal(9, "Purples"))(nb.cols)


for (i in lake_list){
  data <- all_lakes_tib |> 
    filter(fLake == i)
  
  boxplot_deriv <- data |> 
    ggplot(aes(x = fct_reorder(species, derivative), y = derivative, fill = species)) +
    geom_boxplot() +
    coord_flip() +
    facet_wrap(~fLake) +
    xlab("") +
    scale_fill_manual(values = mycolors, guide = NULL)
    # scale_fill_viridis(discrete = TRUE, option = "B", guide = NULL)
  
  
  tiff(paste("total_models/plots/plot_derivatives_species_", i, ".tiff", sep = ""), compression = "lzw",  units = "cm",
       width = 12, height = 8, pointsize = 18, res = 300)
  
  plot(boxplot_deriv)
  
  # Closing the graphical device
  dev.off()
}



# highlight specific lines in one lake 

# not working https://www.data-to-viz.com/caveat/spaghetti.html
# https://www.datanovia.com/en/blog/gghighlight-easy-way-to-highlight-a-ggplot-in-r/
# https://yutannihilation.github.io/gghighlight/articles/gghighlight.html$



library(RColorBrewer)
# Define the number of colors you want
# nb.cols <- 80
# mycolors_lakes <- colorRampPalette(brewer.pal(9, "Purples"))(nb.cols)

nb.cols <- 80
testcol <- colorRampPalette("#54008B")(nb.cols)

# nb.cols <- 80
# test_colors <- colorRampPalette(c("#150E39", "#FA9107"))(nb.cols)
all_lakes_excl <- all_lakes_tib |> 
  filter(!species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii",
                         "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II"))

lake_list <- all_lakes_tib |>
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)


for (i in lake_list){
  data <- all_lakes_excl |> 
    arrange(species) |> 
    filter(fLake == i)
  
  highlight_plot <- data |> 
    ggplot() +
    geom_line(aes(x=temp, y=derivative, color = species)) +
    gghighlight(use_direct_label = FALSE) +
    # scale_color_viridis(discrete = TRUE, guide = NULL, option = "H") +
    scale_color_manual(values = testcol, guide =  NULL) +
    facet_wrap(~species)
  
  
  tiff(paste("total_models/plots/plot_highlight_line_", i, ".tiff", sep = ""), units="in", width=5, height=5, res=300)
  
  plot(highlight_plot)
  
  # Closing the graphical device
  dev.off()
  
}

# same with predictions 

lake_list <- all_lakes_tib |> 
  # filter(fLake == "Joux") |>
  distinct(fLake) |> 
  pull(fLake)

for (i in lake_list){
  
  species_list <- all_lakes_tib |> 
    filter(fLake == i) |> 
    distinct(species) |> 
    pull(species)
  
  data_deriv <- all_lakes_tib |> 
    filter(fLake == i)
  
  minimum <- min(data_deriv$temp)
  maximum <- max(data_deriv$temp)
  
  data_pred <- model_predictions |> 
    filter(species %in% species_list) |> 
    filter(!species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii",
                           "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II"))
    filter(temp > minimum & temp < maximum)
  
  highlight_plot <- data_pred |> 
    ggplot() +
    geom_line(aes(x=temp, y=fit, color = species)) +
    gghighlight(use_direct_label = FALSE) +
    scale_color_viridis(discrete = TRUE, guide = NULL) +
    # scale_color_manual(values = mycolors) +
    facet_wrap(~species)
  
  
  tiff(paste("total_models/plots/prediction_highlight_line_", i, ".tiff", sep = ""), units="in", width=5, height=5, res=300)
  
  plot(highlight_plot)
  
  # Closing the graphical device
  dev.off()
  

  
}


# how can I visualize divergence and temperature? to see changes there?


# heatmap testing for Lake vs Species and derivative to look for outliers
# https://www.data-to-viz.com/graph/heatmap.html
# heatmap of lake, species and mean_derivative 
library(plotly)

data <- all_lakes_tib |> 
  mutate(species = factor(species)) |> 
  group_by(fLake, species) |> 
  mutate(mean_derivative = mean(derivative)) |> 
  mutate(mean_temp = mean(temp)) |> 
  distinct(species, fLake, mean_temp, mean_derivative) |> 
  ungroup() |> 
  arrange(mean_derivative)

max(data$mean_derivative)
min(data$mean_derivative)

data_new <- data                                      # Duplicate data
data_new$groups <- cut(data_new$mean_derivative,               # Add group column
                       breaks = c(-7.634168, -3, 0, 1, 2, 3, 4, 10, 1045.769))
head(data_new)   

data_new |> 
  filter(!species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii",
                        "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II")) |>
  ggplot(aes(fLake, y = fct_reorder(species, mean_derivative), fill= groups)) + 
  geom_tile() +
  # scale_fill_distiller(palette = "PRGn")
  # scale_fill_gradient(low = "#006FAB",
  #                     high = "#971B20",
  #                     guide = "colorbar") +
  scale_fill_manual(breaks = levels(data_new$groups),
                    values = c("#053061", "#2166AC","#92C5DE", "#FDDBC7",  "#F4A582", "#D6604D", "#B2182B", "#67001F"))
  # scale_fill_manual(breaks = levels(data_new$groups),
  #                   values = c("#290AD8", "#3FA0FF","#72D9FF", "#FFE099",  "#FFAD72", "#F76D5E", "#D82632", "#A50021"))


# details in lucerne and thun
all_lakes_tib |> 
  filter(fLake %in% c("Thun", "Lucerne")) |> 
  filter(temp > 8 & temp < 12.5) |> 
  ggplot(aes(temp, derivative, color = species)) +
  geom_line() +
  facet_wrap(~fLake)
 

all_lakes_tib |> 
  filter(species == "Phoxinus_sp") |> 
  distinct(fLake)

# ################################################################################
# plots poster

library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 25
mycolors <- colorRampPalette(brewer.pal(9, "Purples"))(nb.cols)

all_lakes_excl <- all_lakes_tib |> 
  filter(!species %in% c("Barbatula_sp_Lineage_I", "Phoxinus_csikii",
                         "Cottus_sp_Po_profundal", "Barbatula_sp_Lineage_II"))

library(RColorBrewer)
# Define the number of colors you want
# nb.cols <- 80
# mycolors_lakes <- colorRampPalette(brewer.pal(9, "Purples"))(nb.cols)

nb.cols <- 80
testcol <- colorRampPalette("#2C738E")(nb.cols)
"#3C508B"

data <- all_lakes_excl |> 
  arrange(species) |> 
  filter(fLake == "Lucerne")
  
library(hrbrthemes)
library(svglite)

  highlight_plot_1 <- data |> 
    arrange(species) |> 
    ggplot() +
    geom_line(aes(x=temp, y=derivative, color = species)) +
    scale_color_manual(values = testcol, guide =  NULL) +
    geom_hline(yintercept=0, color = "red") +
    ggtitle("High response diversity: Lake Lucerne") +
    xlab("temperature") +
    ylim(-2, 3.5)
  
  
  library(ggthemes)
  library(envalysis)
  
  # highlight_plot_1 + theme_publish(base_size = 20, base_family = "", base_linewidth = 1)
  # 
  # q <- highlight_plot_1 + theme_publish(base_size = 20, base_family = "", base_linewidth = 1)
    
  q <- highlight_plot_1 + theme_bw(base_size = 20) + theme(panel.border = element_rect(size = 2.5))
  q 
  
  tiff(paste("total_models/plots/pposter_plot_lucerne.tiff", sep = ""), units="in", width=8, height=5, res=300)
  # svglite("total_models/plots/poster_plot_lucerne.svg", width = 4, height = 4)
  
  plot(q)
  
  # Closing the graphical device
  dev.off()
  
  # plot morat
  # 
  # data <- all_lakes_excl |> 
  #   arrange(species) |> 
  #   filter(fLake == "Morat")
  # 
  # highlight_plot <- data |> 
  #   arrange(species) |> 
  #   ggplot() +
  #   geom_line(aes(x=temp, y=derivative, color = species)) +
  #   scale_color_manual(values = testcol, guide =  NULL) +
  #   geom_hline(yintercept=0, color = "#8B0E00") +
  #   ggtitle("High response diversity: Lake Morat") +
  #   xlab("temperature") +
  #   ylim(-2, 3.5)
  # 
  # 
  # p <- highlight_plot + theme_bw(base_size = 20)
  # 
  # tiff(paste("total_models/plots/pposter_plot_morat.tiff", sep = ""), units="in", width=8, height=3, res=300)
  # 
  # plot(p)
  # 
  # # Closing the graphical device
  # dev.off()
  # 
  
  # plot biel
  
  data <- all_lakes_excl |> 
    arrange(species) |> 
    filter(fLake == "Zurich")
  
  highlight_plot_2 <- data |> 
    arrange(species) |> 
    ggplot() +
    geom_line(aes(x=temp, y=derivative, color = species)) +
    scale_color_manual(values = testcol, guide =  NULL) +
    geom_hline(yintercept=0, color = "red") +
    ggtitle("Medium response diversity: Lake Zurich") +
    xlab("temperature") +
    ylim(-2, 3.5)
  
  
  
 # qrs <-  highlight_plot_2 + theme_publish(base_size = 20, base_family = "", base_linewidth = 1)
 qrs <- highlight_plot_2 + theme_bw(base_size = 20) + theme(panel.border = element_rect(size = 2.5))
 qrs

  tiff(paste("total_models/plots/pposter_plot_biel.tiff", sep = ""), units="in", width=8, height=5, res=300)
  
  plot(qrs)
  
  # Closing the graphical device
  dev.off()
  
  
  
  # plot joux
  
  data <- all_lakes_excl |> 
    arrange(species) |> 
    filter(fLake == "Joux")
  
  highlight_plot_3 <- data |> 
    arrange(species) |> 
    ggplot() +
    geom_line(aes(x=temp, y=derivative, color = species)) +
    scale_color_manual(values = testcol, guide =  NULL) +
    geom_hline(yintercept=0, color = "red") +
    ggtitle("Low response diversity: Lake Joux") +
    xlab("temperature") +
    ylim(-2, 3.5)
  
  
  tus <-highlight_plot_3 + theme_bw(base_size = 20) + theme(panel.border = element_rect(size = 2.5))
  
  tiff(paste("total_models/plots/pposter_plot_Joux.tiff", sep = ""), units="in", width=8, height=5, res=300)
  
  plot(tus)
  
  # Closing the graphical device
  dev.off()
  
  
  library(ggpubr)
  
  
  tiff(paste("total_models/plots/poster_plot_three_lakes.tiff", sep = ""), units="in", width=20, height=6, res=300)
  
  plot(ggarrange(q, qrs, tus, ncol = 3))
  
  # Closing the graphical device
  dev.off()
  
  # dissimilarity vs divergence
  
  
  df_mean_all <- resp_div_all |>
    group_by(fLake) |> 
    summarise(mean_rdiv = mean(rdiv))

  
  df_mean_divergence_all <- resp_div_all |>
    group_by(fLake) |> 
    summarise(mean_sign = mean(sign))
  
  
  df_means <- merge(df_mean_all, df_mean_divergence_all) |> 
    rename(Lake = fLake, mean_dissimilarity = mean_rdiv, mean_divergence = mean_sign)
  
  
  #plotting means
  
  plot_means <- df_means |> 
    ggplot(aes(mean_dissimilarity, mean_divergence)) +
    geom_point(color = "#26AD81") +
    # theme_publish(base_size = 18, base_family = "", base_linewidth = 1)
    theme_bw(base_size = 18) + 
    theme(panel.border = element_rect(size = 1.5))
 plot_means
 
 library(ggrepel)
 library(hrbrthemes)
 plot_means_2 <- plot_means + geom_text_repel(aes(label = Lake),
                                   size = 3.5, 
                                   max.overlaps = 15)
 plot_means_2

  tiff(paste("total_models/plots/poster_fig_3.tiff", sep = ""), units="in", width=5, height=5, res=300)
  
  plot(plot_means_2)
  
  # Closing the graphical device
  dev.off()
  
  
  
  # response diversity explanation 
  
  fig_a <- model_predictions |> 
    filter(species %in% c("Perca_fluviatilis", "Rutilus_rutilus",
                          "Coregonus_sp")) |>
    ggplot(aes(temp, fit, color = factor(species))) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    # theme(strip.background = element_rect(fill="lightgrey")) +
    # scale_color_viridis(discrete=TRUE, guide = NULL) +
    scale_color_manual(values = c("#481B6D", "#FDE725", "#26AD81"), guide = NULL) +
    ylab("Abundance (Performance)") +
    xlab("Temperature") +
    # ggtitle("Abundance-Temperature relationship") +
    theme_bw(base_size = 20) +
    theme(panel.border = element_rect(size = 2))
  fig_a
  
  
  fig_b <- all_lakes_tib |> 
    filter(species %in% c("Perca_fluviatilis", "Rutilus_rutilus",
                          "Coregonus_sp")) |>
    ggplot(aes(temp, derivative, color = species)) +
    geom_line() +
    # geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.3) +
    # theme(strip.background = element_rect(fill="lightgrey")) +
    # scale_color_viridis(discrete=TRUE, guide = NULL) +
    scale_color_manual(values = c("#481B6D", "#FDE725", "#26AD81"), guide = NULL) +
    ylab("Derivative") +
    xlab("Temperature") +
    # ggtitle("Abundance-Temperature relationship") +
    theme_bw(base_size = 20) +
    theme(panel.border = element_rect(size = 2))
  fig_b
  
# resp div
  
  data <- all_lakes_tib |>
    filter(species %in% c("Perca_fluviatilis", "Rutilus_rutilus",
                          "Coregonus_sp")) |>
    select(temp, derivative, species)
  
  
  df_resp_div <- data |>
    pivot_wider(
      names_from = species,
      values_from = derivative) |> 
    drop_na()
  
  
  df_resp_div$rdiv <- apply(df_resp_div[,-1, drop = FALSE], 1, resp_div, sign_sens = F)
  df_resp_div$sign <- apply(df_resp_div[,-1,drop = FALSE], 1, resp_div, sign_sens = T)
  df_resp_div$Med <- median(df_resp_div$rdiv)
  
  
  fig_c <- df_resp_div |> 
    ggplot() +
    geom_line(aes(temp, rdiv)) +
    ylab("Dissimilarity") +
    xlab("Temperature") +
    # ggtitle("Abundance-Temperature relationship") +
    theme_bw(base_size = 20) +
    theme(panel.border = element_rect(size = 2))
  
  fig_d <- df_resp_div |> 
    ggplot() +
    geom_line(aes(temp, sign)) +
    ylab("Divergence") +
    xlab("Temperature") +
    # ggtitle("Abundance-Temperature relationship") +
    theme_bw(base_size = 20) +
    theme(panel.border = element_rect(size = 2))

  
  ggarrange(fig_a, fig_b, ncol = 2)    
  ggarrange(fig_c, fig_d, nrow = 2)
  
  
  tiff(paste("total_models/plots/poster_concet_a.tiff", sep = ""), units="in", width=5, height=4, res=300)
  
  plot(fig_a)
  
  # Closing the graphical device
  dev.off()
  
  
  tiff(paste("total_models/plots/poster_concept_a1.tiff", sep = ""), units="in", width=5, height=4, res=300)
  
  plot(fig_b)
  
  # Closing the graphical device
  dev.off()
  
  
  tiff(paste("total_models/plots/poster_concet_b.tiff", sep = ""), units="in", width=5, height=8, res=300)
  
  plot(ggarrange(fig_c, fig_d, nrow = 2))
  
  # Closing the graphical device
  dev.off()
