
################
#function to calculate predictions of all GAMs 
# temp data 
  
predictions <- function(df){
  require(broom)
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
      filter(Species == i) |> 
      mutate(n_lake = n_distinct(Lake))
    
    if(max(data$n_lake) == 1) {
      
      unique_method <- distinct(data, fProtocol)
      grid <- expand.grid(mean_last_7days = seq(
        from = min(data$mean_last_7days, na.rm = TRUE),
        to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
        fProtocol = unique_method$fProtocol)
      
      
      if (i == "Coregonus_sp_benthic_profundal")  { 
        
        gam_output[[i]] <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) +
                                 s(fProtocol, bs = 're'), family = binomial)
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in temp datat
        temp_data <- data |>
          drop_na(mean_last_7days)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
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
          summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        
      } else if (max(data$Abundance) > 1)  {

        gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                                 s(fProtocol, bs = 're'), family = ziP())
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in temp datat
        temp_data <- data |>
          drop_na(mean_last_7days)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
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
          summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
      }
      else {   
  
        gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                                 s(fProtocol, bs = 're'), family = binomial)
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in temp datat
        temp_data <- data |>
          drop_na(mean_last_7days)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
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
          summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
      
      }
    }
    
    else {
      
      unique_lakes <- distinct(data, fLake)
      unique_protocol <- distinct(data, fProtocol)
      grid <- expand.grid(mean_last_7days = seq(
        from = min(data$mean_last_7days, na.rm = TRUE),
        to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
      ), fLake = unique_lakes$fLake, fProtocol = unique_protocol$fProtocol)
      
      if (max(data$Abundance) > 1)  { 
        
        gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                               +  s(fProtocol, bs = 're'), family = ziP())
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in temp datat
        temp_data <- data |>
          drop_na(mean_last_7days)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
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
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
      
      
      } else {
        gam_output[[i]] <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                               +  s(fProtocol, bs = 're'), family = binomial)
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output[[i]], plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in temp datat
        temp_data <- data |>
          drop_na(mean_last_7days)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
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
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
      }
  
    }
  }
}

