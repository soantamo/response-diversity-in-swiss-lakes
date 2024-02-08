
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
          mutate(lower = fit - 1 * se.fit, upper = fit +  1 * se.fit) |>
          summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
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
          mutate(lower = fit - 1 * se.fit, upper = fit +  1 * se.fit) |>
          summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit)) |>
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
        
        summary <- summary(gam_output[[i]])
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
          theme_bw() +
          facet_wrap(~fLake, scale = "free") +
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
          mutate(lower = fit - 1 * se.fit, upper = fit +  1 * se.fit) |>
          summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit), across(fLake)) |>
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
          facet_wrap(~fLake, scale = "free") +
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


# data <- readRDS("data_frame_models/df_abundance_re") |> 
#   filter(Species == "Perca_fluviatilis")
# 
# data$fLake <- as.factor(data$Lake)
# 
# data$fProtocol <- as.factor(data$Protocol)
# 
# 
# 
# gam_output <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
#                        +  s(fProtocol, bs = 're'), family = ziP())
# 
# unique_lakes <- distinct(data, fLake)
# unique_protocol <- distinct(data, fProtocol)
# grid <- expand.grid(mean_last_7days = seq(
#   from = min(data$mean_last_7days, na.rm = TRUE),
#   to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02
# ), fLake = unique_lakes$fLake, fProtocol = unique_protocol$fProtocol)
# 
# 
# temp_data <- data |>
#   drop_na(mean_last_7days)
# 
# 
# model_prediction <- predict.gam(gam_output, newdata = grid, type = "response", se.fit = TRUE)
# model_bind <- cbind(grid, as.data.frame(model_prediction))
# pred_df <- model_bind |>
#   group_by(mean_last_7days) |>
#   mutate(fit = mean(fit)) |>
#   mutate(lower = fit - 1 * se.fit, upper = fit +  1 * se.fit) |>
#   summarize(fit = mean(fit), lower = mean(lower), upper = mean(upper), across(se.fit), across(fLake)) |>
#   rename(temp = mean_last_7days)
# 
# 
# 
# 
# ggplot(gam_output$model, aes_string(x = names(gam_output$model)[2], y = names(gam_output$model)[1])) + 
#   geom_point() +
#   # stat_smooth(method = "lm", col = "#FF3030", fill = "#CDC9C9") +
#   labs(title = paste("R^2 = ",signif(summary(mod)$r.squared, 2),
#                      # "adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
#                      # "Intercept =",signif(fit$coef[[1]],5 ),
#                      # " Slope =",signif(fit$coef[[2]], 5),
#                      " p-value =", signif(summary(mod)$coef[2,4], 2))) +
#   theme_bw()
# 
# 
# tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=4, height=8, res=300)
# 
# 
# pred_df |> 
#   ggplot(aes(temp, fit)) +
#   geom_line() +
#   geom_ribbon(aes(ymin = fit - se.fit, ymax = fit + se.fit), alpha = 0.3) +
#   theme_bw() +
#   facet_wrap(~fLake, scale = "free") +
#   theme(strip.background = element_rect(fill="lightgrey")) +
#   labs(title = paste("Species = ", i, 
#                      "deviance explained = ", signif(summary.gam(gam_output)$dev.expl),
#                      "R² = ", summary.gam(gam_output)$r.sq))
# 
# 
# dev.off()



# In the most updated version of mgcv (1.8-37). Wood elaborated on the r.sq d
# efinition by stating "The proportion null deviance explained is probably more 
# appropriate for non-normal errors." Therefore, deviance explained should be a 
# more generalized measurement of goodness of fit especially for non-gaussian models.
# 