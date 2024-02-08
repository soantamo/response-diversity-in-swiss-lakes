
################
#function to calculate predictions of all GAMs 
# temp data 

# In the most updated version of mgcv (1.8-37). Wood elaborated on the r.sq d
# efinition by stating "The proportion null deviance explained is probably more 
# appropriate for non-normal errors." Therefore, deviance explained should be a 
# more generalized measurement of goodness of fit especially for non-gaussian models.
# 


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
  summary <- list()
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
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output[[i]])
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          # facet_wrap(~fLake, scale = "free") +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        
    
        
      } else if (max(data$Abundance) > 1)  {
        
        unique_method <- distinct(data, fProtocol)
        grid <- expand.grid(mean_last_7days = seq(
          from = min(data$mean_last_7days, na.rm = TRUE),
          to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
          fProtocol = unique_method$fProtocol)

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
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output[[i]])
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          # facet_wrap(~fLake, scale = "free") +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
       
        
      }
      else {   
        
    
  
        gam_output<- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                                 s(fProtocol, bs = 're'), family = binomial)
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
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

        print(glance(gam_output))
        

        model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          group_by(mean_last_7days) |>
          mutate(fit = mean(fit)) |>
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
       
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          # facet_wrap(~fLake, scale = "free") +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))

        dev.off()
        
      
      }
    }
    
    else {
      
      unique_lakes <- distinct(data, fLake)
      unique_method <- distinct(data, fProtocol)
      grid <- expand.grid(mean_last_7days = seq(
        from = min(data$mean_last_7days, na.rm = TRUE),
        to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
      ), fLake = unique_lakes$fLake, fProtocol = unique_method$fProtocol)
      
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
          rename(temp = mean_last_7days) |> 
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output[[i]])
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          # facet_wrap(~fLake, scale = "free") +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
       
        
      
      
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
          rename(temp = mean_last_7days) |> 
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output[[i]])
        
        summary <- summary(gam_output[[i]])
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          # facet_wrap(~fLake, scale = "free") +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
    
        
      }
  
    }
  }
}


##############################################################################
#function predictions depth

depth_predictions <- function(df){
  
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
  depth_data <- list()
  summary <- list()

  df$fLake <- as.factor(df$Lake)

  df$fProtocol <- as.factor(df$Protocol)
  
  for (i in species_list) {
    data <- df |> 
      filter(Species == i) |> 
      mutate(n_lake = n_distinct(Lake))
    
    # only one lake, no re for lakes
    if(max(data$n_lake) == 1) {
      
      unique_method <- distinct(data, fProtocol)
      grid <- expand.grid(Depth_sample = seq(
        from = min(data$Depth_sample, na.rm = TRUE),
        to = max(data$Depth_sample, na.rm = TRUE), by = 0.02),
        fProtocol = unique_method$fProtocol)
      
      # special case
      if (i == "Coregonus_sp_benthic_profundal")  { 
        
        gam_output <- gam(data = data, Presence ~ s(Depth_sample, k = 3) +
                            s(fProtocol, bs = 're'), family = binomial)
        
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/Depth/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in depth_data
        depth_data <- data |>
          drop_na(Depth_sample)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/Depth/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, depth_data$Depth_sample, xlab = "temp", main=NULL))
        dev.off()
        
        
        model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          group_by(Depth_sample) |>
          mutate(fit = mean(fit)) |>
          rename(depth = Depth_sample) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/Depth/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(depth, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/depth_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        
     
        # zip
      } else if (max(data$Abundance) > 1)  {
        
        gam_output <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                            s(fProtocol, bs = 're'), family = ziP())
        
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/Depth/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in depth_data
        depth_data <- data |>
          drop_na(Depth_sample)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/Depth/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, depth_data$Depth_sample, xlab = "temp", main=NULL))
        dev.off()
        
        
        model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          group_by(Depth_sample) |>
          mutate(fit = mean(fit)) |>
          rename(depth = Depth_sample) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/Depth/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(depth, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/depth_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        

        
      } 
      #binomial 
      else {   
        gam_output <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                            s(fProtocol, bs = 're'), family = binomial)
        
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/Depth/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in depth_data
        depth_data <- data |>
          drop_na(Depth_sample)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/Depth/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, depth_data$Depth_sample, xlab = "temp", main=NULL))
        dev.off()
        
        
        model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          group_by(Depth_sample) |>
          mutate(fit = mean(fit)) |>
          rename(depth = Depth_sample) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/Depth/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(depth, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/depth_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        
 
        
      }
    }
    # multiple lakes
    else {
      
      unique_method <- distinct(data, fProtocol)
      unique_lake <- distinct(data, fLake)
      grid <- expand.grid(Depth_sample = seq(
        from = min(data$Depth_sample, na.rm = TRUE),
        to = max(data$Depth_sample, na.rm = TRUE), by = 0.02),
        fProtocol = unique_method$fProtocol, fLake = unique_lake$fLake)
  
      
      
      # zip
      # special case
      if (i == "Coregonus_sp_large_pelagic"){
        
        gam_output <- gam(data = data, Presence ~ s(Depth_sample, k = 3) +
                            s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/Depth/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in depth_data
        depth_data <- data |>
          drop_na(Depth_sample)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/Depth/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, depth_data$Depth_sample, xlab = "temp", main=NULL))
        dev.off()
        
        
        model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          group_by(Depth_sample) |>
          mutate(fit = mean(fit)) |>
          rename(depth = Depth_sample) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/Depth/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(depth, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/depth_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        
        
        
      }
      else if (max(data$Abundance) > 1)  { 
        
        
        gam_output <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                            s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = ziP())
        
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/Depth/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in depth_data
        depth_data <- data |>
          drop_na(Depth_sample)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/Depth/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, depth_data$Depth_sample, xlab = "temp", main=NULL))
        dev.off()
        
        
        model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          group_by(Depth_sample) |>
          mutate(fit = mean(fit)) |>
          rename(depth = Depth_sample) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/Depth/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(depth, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/depth_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        
      
        #binomial
      } else {
        
        
        gam_output <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                            s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/Depth/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
        # get rid of NAs in depth_data
        depth_data <- data |>
          drop_na(Depth_sample)
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/Depth/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, depth_data$Depth_sample, xlab = "temp", main=NULL))
        dev.off()
        
        
        model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          group_by(Depth_sample) |>
          mutate(fit = mean(fit)) |>
          rename(depth = Depth_sample) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/Depth/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(depth, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/depth_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        
       
        
      }
      
    }
  }
}
 
# data <- readRDS("data_frame_models/df_binomial_gam") |> 
#   filter(Species == "Alosa_fallax") 
# 
# i <- "Alosa_fallax"
# 
# data$fLake <- as.factor(data$Lake)
# 
# data$fProtocol <- as.factor(data$Protocol)
# unique_protocol <- distinct(data, fProtocol)
# 
# grid <- expand.grid(Depth_sample = seq(
#   from = min(data$Depth_sample, na.rm = TRUE),
#   to = max(data$Depth_sample, na.rm = TRUE), by = 0.02),
#   fProtocol = unique_protocol$fProtocol)
  
# depth_gradient <- data.frame(Depth_sample = seq
#              (from = min(data$Depth_sample,na.rm = TRUE),
#                to = max(data$Depth_sample, na.rm = TRUE), by = 0.02),
#              fProtocol = unique_method$fProtocol)
  


# gam_output <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
#                          s(fProtocol, bs = 're'), family = binomial)
# # prepare residuals
# simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
# tiff_filename <- paste("total_models/Depth/gam_check/gam_check_", i, ".tiff", sep = "")
# tiff(tiff_filename, width = 800, height = 600)
# print(plot(simulationOutput))
# dev.off()
# # get rid of NAs in depth_data
# depth_data <- data |>
#   drop_na(Depth_sample)
# # Plotting standardized residuals against predictors
# tiff_file_2 <- paste("total_models/Depth/gam_check/predictor_", i, ".tiff", sep = "")
# tiff(tiff_file_2, width = 800, height = 600)
# print(plotResiduals(simulationOutput, depth_data$Depth_sample, xlab = "temp", main=NULL))
# dev.off()
# 
# 
# model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
# model_bind <- cbind(grid, as.data.frame(model_prediction))
# pred_df <- model_bind |>
#   group_by(Depth_sample) |>
#   mutate(fit = mean(fit)) |>
#   rename(depth = Depth_sample) |>
#   mutate(species = factor(i))
# saveRDS(pred_df, paste0("total_models/Depth/predictions/predictions_",i,".rds"))
# 
# summary <- summary(gam_output)
# 
# plot_pred <- pred_df |>
#   ggplot(aes(depth, fit)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill="lightgrey")) +
#   labs(title = paste("Species = ", i,
#                      "deviance explained = ", signif(summary[["dev.expl"]])))
# 
# tiff(paste("total_models/plot_predictions/depth_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
# 
# print(plot(plot_pred))
# 
# dev.off()
