

df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")

predictions_zip(df_abundance_gam)


# function for zip predictions without re

predictions_zip <- function(df_zip){
  require(tidyverse)
  require(DHARMa)
  require(mgcv)
  require(gratia)
  require(mgcViz)
  
  
  species_list <- df_zip |>
    distinct(Species) |> 
    pull(Species)
  
  species_list <- sort(species_list)
  
  gam_output <- list()
  model_prediction <- list()
  derivatives <- list()
  pred_df <- list()
  grid <- list()
  unique_method <- list()
  simulationOutput <- list()
  tiff_file_2  <- list()
  temp_data <- list()
  
  
  df_zip$fProtocol <- as.factor(df_zip$Protocol)
  str(df_zip)
  
  # loop
  for (i in species_list) {
    data <- df_zip |> 
      filter(Species == i)
    unique_method <- distinct(data, fProtocol)
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data$mean_last_7days, na.rm = TRUE),
      to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
      fProtocol = unique_method$fProtocol)
    
    if (i == "Coregonus_sp_benthic_profundal") { 
      gam_output[[i]] <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) +
                               s(fProtocol, bs = 're'), family = binomial)
simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
tiff_filename <- paste("model_2/gam_check/gam_check_", i, ".tiff", sep = "")
tiff(tiff_filename, width = 800, height = 600)
print(plot(simulationOutput))
dev.off()
# # get rid of NAs in temp datat
temp_data <- data |>
  drop_na(mean_last_7days)
# # Plotting standardized residuals against predictors
tiff_file_2 <- paste("model_2/gam_check/predictor_", i, ".tiff", sep = "")
tiff(tiff_file_2, width = 800, height = 600)
print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
dev.off()
model_prediction[[i]] <- predict.gam(gam_output[[i]], grid, type = "response", se.fit = TRUE) #adding se, $fit
model_bind <- cbind(model_prediction[[i]], grid)
pred_df <- model_bind |>
  group_by(mean_last_7days) |>
  mutate(fit = mean(fit)) |>
  mutate(lower = fit - 1*se.fit, upper = fit + 1*se.fit) |>
  summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
            across(se.fit), across(fProtocol)) |>
  rename(temp = mean_last_7days) |>
  mutate(species = factor(i))
saveRDS(pred_df, paste0("model_2/predictions/predictions_",i,".rds"))

    } else {   
      gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                               s(fProtocol, bs = 're'), family = ziP())
      
      simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
      tiff_filename <- paste("model_2/gam_check/gam_check_", i, ".tiff", sep = "")
      tiff(tiff_filename, width = 800, height = 600)
      print(plot(simulationOutput))
      dev.off()
      # # get rid of NAs in temp datat
      temp_data <- data |>
        drop_na(mean_last_7days)
      # # Plotting standardized residuals against predictors
      tiff_file_2 <- paste("model_2/gam_check/predictor_", i, ".tiff", sep = "")
      tiff(tiff_file_2, width = 800, height = 600)
      print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
      dev.off()
      model_prediction[[i]] <- predict.gam(gam_output[[i]], grid, type = "response", se.fit = TRUE) #adding se, $fit
      model_bind <- cbind(model_prediction[[i]], grid)
      pred_df <- model_bind |>
        group_by(mean_last_7days) |>
        mutate(fit = mean(fit)) |>
        mutate(lower = fit - 1*se.fit, upper = fit + 1*se.fit) |>
        summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper),
                  across(se.fit), across(fProtocol)) |>
        rename(temp = mean_last_7days) |>
        mutate(species = factor(i))
      saveRDS(pred_df, paste0("model_2/predictions/predictions_",i,".rds"))
    }
  
  
  }
}


# function for zip with re

predictions_zip_re <- function(df_zip){
  require(tidyverse)
  require(DHARMa)
  require(mgcv)
  require(gratia)
  require(mgcViz)
  
  species_list <- df_zip |> 
    distinct(Species) |> 
    pull(Species)
  
  species_list <- sort(species_list)
  
  gam_output <- list()
  model_prediction <- list()
  derivatives <- list()
  grid <- list()
  pred_df <- list()
  unique_lakes <- list()
  unique_protocol <- list()
  tiff_filename <- list()
  tiff_file_2 <- list()
  temp_data <- list()
  
  
  
  df_zip$fLake <- as.factor(df_zip$Lake)
  
  df_zip$fProtocol <- as.factor(df_zip$Protocol)
  
  str(df_zip)
  #make new loop 
  
  for (i in species_list) {
    data <- df_zip |> 
      filter(Species == i)
    unique_lakes <- distinct(data, fLake)
    unique_protocol <- distinct(data, fProtocol)
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data$mean_last_7days, na.rm = TRUE),
      to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
    ), fLake = unique_lakes$fLake, fProtocol = unique_protocol$fProtocol)
    gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                           +  s(fProtocol, bs = 're'), family = ziP())
    # prepare residuals
    simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
    tiff_filename <- paste("model_4/gam_check/gam_check_", i, ".tiff", sep = "")
    tiff(tiff_filename, width = 800, height = 600)
    print(plot(simulationOutput))
    dev.off()
    # get rid of NAs in temp datat
    temp_data <- data |>
      drop_na(mean_last_7days)
    # Plotting standardized residuals against predictors
    tiff_file_2 <- paste("model_4/gam_check/predictor_", i, ".tiff", sep = "")
    tiff(tiff_file_2, width = 800, height = 600)
    print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
    dev.off()
    
    print(glance(gam_output[[i]]))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid, type = "response", se.fit = TRUE)
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    pred_df <- model_bind |>
      group_by(mean_last_7days) |>
      mutate(fit = mean(fit)) |>
      mutate(lower = fit - 1 * se.fit, upper = fit +  1 * se.fit) |>
      summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit), across(fLake)) |>
      rename(temp = mean_last_7days) |> 
      mutate(species = factor(i))
    saveRDS(pred_df, paste0("model_4/predictions/predictions_",i,".rds"))
  
  }
}


# predictions binomial re

predictions_bin_re <- function(df_bin){
  require(tidyverse)
  require(DHARMa)
  require(mgcv)
  require(gratia)
  require(mgcViz)
  
  species_list <- df_bin |> 
    distinct(Species) |> 
    pull(Species)
  
  species_list <- sort(species_list)
  
  gam_output <- list()
  model_prediction <- list()
  derivatives <- list()
  grid <- list()
  pred_df <- list()
  unique_lakes <- list()
  unique_protocol <- list()
  tiff_filename <- list()
  tiff_file_2 <- list()
  temp_data <- list()
  
  
  
  df_bin$fLake <- as.factor(df_bin$Lake)
  
  df_bin$fProtocol <- as.factor(df_bin$Protocol)
  
  str(df_zip)
  #make new loop 
  
  for (i in species_list) {
    data <- df_bin |> 
      filter(Species == i)
    unique_lakes <- distinct(data, fLake)
    unique_protocol <- distinct(data, fProtocol)
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data$mean_last_7days, na.rm = TRUE),
      to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
    ), fLake = unique_lakes$fLake, fProtocol = unique_protocol$fProtocol)
    gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                           +  s(fProtocol, bs = 're'), family = binomial)
    # prepare residuals
    simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
    tiff_filename <- paste("model_4/gam_check/gam_check_", i, ".tiff", sep = "")
    tiff(tiff_filename, width = 800, height = 600)
    print(plot(simulationOutput))
    dev.off()
    # get rid of NAs in temp datat
    temp_data <- data |>
      drop_na(mean_last_7days)
    # Plotting standardized residuals against predictors
    tiff_file_2 <- paste("model_4/gam_check/predictor_", i, ".tiff", sep = "")
    tiff(tiff_file_2, width = 800, height = 600)
    print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
    dev.off()
    
    print(glance(gam_output[[i]]))
    
    model_prediction[[i]] <- predict.gam(gam_output[[i]], newdata = grid, type = "response", se.fit = TRUE)
    model_bind <- cbind(grid, as.data.frame(model_prediction[[i]]))
    pred_df <- model_bind |>
      group_by(mean_last_7days) |>
      mutate(fit = mean(fit)) |>
      mutate(lower = fit - 1 * se.fit, upper = fit +  1 * se.fit) |>
      summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit), across(fLake)) |>
      rename(temp = mean_last_7days) |> 
      mutate(species = factor(i))
    saveRDS(pred_df, paste0("model_4/predictions/predictions_",i,".rds"))
    
  }
}



df_abundance_gam <- readRDS("data_frame_models/df_abundance_gam")
df_binomial_gam <- readRDS("data_frame_models/df_binomial_gam")

predictions(df_abundance_gam)
predictions(df_binomial_gam)


# loop for lakes without RE
  
predictions <- function(df){
  require(tidyverse)
  require(DHARMa)
  require(mgcv)
  require(gratia)
  require(mgcViz)
  
  species_list <- df |> 
    distinct(Species) |> 
    pull(Species)
  
  species_list <- sort(species_list)
  
  gam_output <- list()
  model_prediction <- list()
  derivatives <- list()
  grid <- list()
  pred_df <- list()
  unique_lakes <- list()
  unique_protocol <- list()
  tiff_filename <- list()
  tiff_file_2 <- list()
  temp_data <- list()

  df$fLake <- as.factor(df$Lake)
  
  df$fProtocol <- as.factor(df$Protocol)
  
  for (i in species_list) {
    data <- df |> 
      filter(Species == i)
    unique_method <- distinct(data, fProtocol)
    grid <- expand.grid(mean_last_7days = seq(
      from = min(data$mean_last_7days, na.rm = TRUE),
      to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
      fProtocol = unique_method$fProtocol)

    if (max(data$Abundance) > 1) { 
      cat("abundance")
      # gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
      #                         s(fProtocol, bs = 're'), family = ziP())
    
      
      } else if (i == "Coregonus_sp_benthic_profundal")  {
        cat("coregonus")
        
        # gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
        #                         s(fProtocol, bs = 're'), family = ziP())
      
      }
    else {   
      cat("binomial")
      # gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
      #                          s(fProtocol, bs = 're'), family = binomial)

  
    }
  }
}

