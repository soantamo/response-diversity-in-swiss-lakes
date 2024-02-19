
################
#function to calculate predictions of all GAMs 
# temp data 

# In the most updated version of mgcv (1.8-37). Wood elaborated on the r.sq d
# efinition by stating "The proportion null deviance explained is probably more 
# appropriate for non-normal errors." Therefore, deviance explained should be a 
# more generalized measurement of goodness of fit especially for non-gaussian models.
# 
# random effects exlusion: 
# https://fromthebottomoftheheap.net/2021/02/02/random-effects-in-gams/
# following this post does not change that we still get values for each protocol
# https://stats.stackexchange.com/questions/159380/gam-models-with-random-effect-r
# solving re prediction probelm
# comment section in the bottom of the heap blog
# You can predict from the fixed effects only (i.e., exclude the random effects) in two ways:


# gavin:
# Yeah; I would just choose a level of the subject that exists in the data and 
# then use the second idea of using the exclude argument to remove the effect of 
# the random effect smooth(s) as needed. You then don't need to set 
# newdata.guaranteed = TRUE; the checks are helpful to insure you have created
# the rest of the prediction data correctly.


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
    
    unique_lakes <- unique(data$fLake)
    
    random_lake <- sample(levels(unique_lakes), 1)
  
    
    if(max(data$n_lake) == 1) {
      
      grid <- expand.grid(mean_last_7days = seq(
        from = min(data$mean_last_7days, na.rm = TRUE),
        to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
        fProtocol = factor("VERT"))
      
      
      if (i == "Coregonus_sp_benthic_profundal")  { 
        
        gam_output<- gam(data = data, Presence ~ s(mean_last_7days, k = 3) +
                                 s(fProtocol, bs = 're'), family = binomial)
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
  
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
        dev.off()

        

        model_prediction <- predict.gam(gam_output, newdata = grid,
                                             exclude = "s(fProtocol)", type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        print(signif(summary[["dev.expl"]]))
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), alpha = 0.3) +
          theme_bw() +
          # facet_wrap(~fLake, scale = "free") +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
        
    
        
      } else if (max(data$Abundance) > 1)  {

  

        gam_output <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                                 s(fProtocol, bs = 're'), family = ziP())
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
   
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
        dev.off()

        # print(glance(gam_output[[i]]))

        model_prediction <- predict.gam(gam_output, newdata = grid,
                                             exclude= "s(fProtocol)",
                                             type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), alpha = 0.3) +
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
        
    
        gam_output <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) +
                                 s(fProtocol, bs = 're'), family = binomial)
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
     
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
        dev.off()

        print(glance(gam_output))
        

        model_prediction <- predict.gam(gam_output, newdata = grid,
                                        exclude = "s(fProtocol)",  type = "response",
                                        se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          rename(temp = mean_last_7days) |>
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
       
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), alpha = 0.3) +
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
      
      unique_lakes <- distinct(data, Lake) |>
        pull()
      
      random_lake <- sample(unique_lakes, 1)
      
      grid <- expand.grid(mean_last_7days = seq(
        from = min(data$mean_last_7days, na.rm = TRUE),
        to = max(data$mean_last_7days, na.rm = TRUE), by = 0.02),
        fProtocol = factor("VERT"), fLake = factor(random_lake))

      if (max(data$Abundance) > 1)  { 
      
        
        gam_output<- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                               +  s(fProtocol, bs = 're'), family = ziP())
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
      
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
        dev.off()
        
        print(glance(gam_output))
        
        model_prediction <- predict.gam(gam_output, newdata = grid,
                                             exclude = c("s(fProtocol)", "s(fLake)"), 
                                             type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          rename(temp = mean_last_7days) |> 
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), alpha = 0.3) +
          theme_bw() +
          # facet_wrap(~fLake, scale = "free") +
          theme(strip.background = element_rect(fill="lightgrey")) +
          labs(title = paste("Species = ", i,
                             "deviance explained = ", signif(summary[["dev.expl"]])))
        
        tiff(paste("total_models/plot_predictions/temp_predictions_", i ,".tiff", sep = ""), units="in", width=8, height=6, res=300)
        
        print(plot(plot_pred))
        
        dev.off()
       
        
      
      
      } else {
        
        
        gam_output <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
                               +  s(fProtocol, bs = 're'), family = binomial)
        # prepare residuals
        simulationOutput <- simulateResiduals(fittedModel = gam_output, plot = F)
        tiff_filename <- paste("total_models/gam_check/gam_check_", i, ".tiff", sep = "")
        tiff(tiff_filename, width = 800, height = 600)
        print(plot(simulationOutput))
        dev.off()
     
        # Plotting standardized residuals against predictors
        tiff_file_2 <- paste("total_models/gam_check/predictor_", i, ".tiff", sep = "")
        tiff(tiff_file_2, width = 800, height = 600)
        print(plotResiduals(simulationOutput, temp_data$mean_last_7days, xlab = "temp", main=NULL))
        dev.off()
        
        print(glance(gam_output))
        
        model_prediction <- predict.gam(gam_output, newdata = grid,
                                        exclude = c("s(fProtocol)", "s(fLake)"),
                                        type = "response", se.fit = TRUE)
        model_bind <- cbind(grid, as.data.frame(model_prediction))
        pred_df <- model_bind |>
          rename(temp = mean_last_7days) |> 
          mutate(species = factor(i))
        saveRDS(pred_df, paste0("total_models/predictions/predictions_",i,".rds"))
        
        summary <- summary(gam_output)
        
        plot_pred <- pred_df |>
          ggplot(aes(temp, fit)) +
          geom_line() +
          geom_ribbon(aes(ymin = (fit - se.fit), ymax = (fit + se.fit)), alpha = 0.3) +
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
 



############################## temp + depth model

depth_temp_deviance <- function(df){
  
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
        
        
        model1 <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) + 
                        s(fProtocol, bs = 're'), family = binomial)
        
        model2 <- gam(data = data, Presence ~ s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're'), family = binomial)
        model3 <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) + s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're'), family = binomial)
        
        summary1 <- summary(model1)
        summary2 <- summary(model2)
        summary3 <- summary(model3)
        
        print(paste(i, "temp: ", summary1[["dev.expl"]]))
        print(paste(i, "depth: ", summary2[["dev.expl"]]))
        print(paste(i, "temp + depth: ", summary3[["dev.expl"]]))
        
    
        
        # zip
      } else if (max(data$Abundance) > 1)  {
        
        model1 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + 
                        s(fProtocol, bs = 're'), family = ziP())
        
        model2 <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're'), family = ziP())
        model3 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're'), family = ziP())
        
        
        summary1 <- summary(model1)
        summary2 <- summary(model2)
        summary3 <- summary(model3)
        
        print(paste(i, "temp: ", summary1[["dev.expl"]]))
        print(paste(i, "depth: ", summary2[["dev.expl"]]))
        print(paste(i, "temp + depth: ", summary3[["dev.expl"]]))
  
        
      } 
      #binomial 
      else {   
        model1 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + 
                        s(fProtocol, bs = 're'), family = binomial)
        
        model2 <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're'), family = binomial)
        model3 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're'), family = binomial)
        
        summary1 <- summary(model1)
        summary2 <- summary(model2)
        summary3 <- summary(model3)
        
        print(paste(i, "temp: ", summary1[["dev.expl"]]))
        print(paste(i, "depth: ", summary2[["dev.expl"]]))
        print(paste(i, "temp + depth: ", summary3[["dev.expl"]]))
      }
    }
    # multiple lakes
    else {

      # zip
      # special case
      if (i == "Coregonus_sp_large_pelagic"){
        
        model1 <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) + 
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        
        model2 <- gam(data = data, Presence ~ s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        model3 <- gam(data = data, Presence ~ s(mean_last_7days, k = 3) + s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        
        summary1 <- summary(model1)
        summary2 <- summary(model2)
        summary3 <- summary(model3)
        
        print(paste(i, "temp: ", summary1[["dev.expl"]]))
        print(paste(i, "depth: ", summary2[["dev.expl"]]))
        print(paste(i, "temp + depth: ", summary3[["dev.expl"]]))
        
        
      }
      else if (max(data$Abundance) > 1)  { 
        
        
        model1 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + 
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = ziP())
        
        model2 <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = ziP())
        model3 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = ziP())
        
        summary1 <- summary(model1)
        summary2 <- summary(model2)
        summary3 <- summary(model3)
        
        print(paste(i, "temp: ", summary1[["dev.expl"]]))
        print(paste(i, "depth: ", summary2[["dev.expl"]]))
        print(paste(i, "temp + depth: ", summary3[["dev.expl"]]))
        
        #binomial
      } else {
        
        model1 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + 
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        
        model2 <- gam(data = data, Abundance ~ s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        model3 <- gam(data = data, Abundance ~ s(mean_last_7days, k = 3) + s(Depth_sample, k = 3) +
                        s(fProtocol, bs = 're') + s(fLake, bs = "re"), family = binomial)
        
        summary1 <- summary(model1)
        summary2 <- summary(model2)
        summary3 <- summary(model3)
        
        print(paste(i, "temp: ", summary1[["dev.expl"]]))
        print(paste(i, "depth: ", summary2[["dev.expl"]]))
        print(paste(i, "temp + depth: ", summary3[["dev.expl"]]))
        
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

##############################################################################
#derivatives temp
# 
# derivatives <- function(df){
#   require(broom)
#   require(tidyverse)
#   require(mgcv)
#   require(gratia)
#   
#   # species_list <- df |>
#   #   distinct(Species) |>
#   #   pull(Species)
#   # 
#   # 
#   # species_list <- sort(species_list)
#   
#   species_list <- c("Alosa_fallax")
#   # 
#   
#   df$fLake <- as.factor(df$Lake)
#   df$fProtocol <- as.factor(df$Protocol)
#   
#   # we need to get the derivatives for every lake
#   
#   derivatives <- list()
#   gam_output <- list()
#   newdf_analysis <- tibble()
#   df_analysis <- tibble()
#   # df to get list of all lakes where species should be
#   
#   species_lake <- read_xlsx("species_lake.xlsx") 
#   
#   species_lake$fLake <- as.factor(species_lake$Lake)
#   species_lake$fProtocol <- as.factor(species_lake$Protocol)
#   
#   
#   
#   
#   for (i in species_list) {
#     df_analysis <- df |> 
#       filter(Species == "Alosa_fallax") |> 
#       mutate(n_lake = n_distinct(Lake))
#     
#     lake_df_analysis <- species_lake |>
#       filter(Species == i)
#     
#     lake_list <- distinct(lake_df_analysis, Lake) |>
#       pull()
#     
#     
#     # no random effect for lake
#     if(max(df_analysis$n_lake) == 1) {
#       
#       # zip
#       if (i == "Coregonus_sp_benthic_profundal")  {
#         cat("special ")
#         
#         gam_output <- gam(data = df_analysis, Presence ~ s(mean_last_7days, k = 3)
#                           +  s(fProtocol, bs = 're'), family = binomial)
#         
#         lake_df_analysis <- species_lake |>
#           filter(Species == i)
#         
#         lake_list <- distinct(lake_df_analysis, Lake) |>
#           pull()
#         
#         
#       } else if (max(df_analysis$Abundance) > 1)  {
#         cat("zip ")
#         
#         gam_output <- gam(data = df_analysis, Abundance ~ s(mean_last_7days, k = 3)
#                           +  s(fProtocol, bs = 're'), family = ziP())
#         
#         lake_df_analysis <- species_lake |>
#           filter(Species == i)
#         
#         lake_list <- distinct(lake_df_analysis, Lake) |>
#           pull()
#         
#         
#         
#       }
#       # binomial
#       else {
#         
#         gam_output[[i]] <- gam(data = df_analysis, Abundance ~ s(mean_last_7days, k = 3)
#                                +  s(fProtocol, bs = 're'), family = binomial)
#         
#         # unique_lakes <- distinct(df_analysis, fLake)
#         # unique_protocol <- distinct(df_analysis, fProtocol)
#         # 
#         # newdf_analysis <- tibble(mean_last_7days = seq(
#         #   from = min(df_analysis$mean_last_7days, na.rm = TRUE),
#         #   to = max(df_analysis$mean_last_7days, na.rm = TRUE), length = 200),
#         #   fProtocol = sample(levels(unique_protocol$fProtocol), size = 200, replace = TRUE))
#         
#         derivatives <- derivatives(gam_output[[i]])
#         
#         cat("binomial ")
#         
#         # for (j in lake_list){
#         # 
#         # 
#         #   derivatives <- derivatives(gam_output)
#         #   
#         #   cat("test ")
#         #   #   mutate(fLake = factor(j)) |>
#         #   #   mutate(species = factor(i)) |>
#         #   #   rename(temp = df_analysis)
#         #   # saveRDS(derivatives, paste0("total_models/derivatives/derivatives_", i, "_",  j, ".rds"))
#         # 
#         # }
#         
#         
#       }
#     }
#     
#     # multiple lakes, with fLake as random effect
#     else {
#       
#       
#       # zip
#       if (max(df_analysis$Abundance) > 1)  { 
#         cat("zip re ")
#         
#         gam_output <- gam(data = df_analysis, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
#                           +  s(fProtocol, bs = 're'), family = ziP())
#         
#         lake_df_analysis <- species_lake |>
#           filter(Species == i)
#         
#         lake_list <- distinct(lake_df_analysis, Lake) |>
#           pull()
#         
#         
#       } 
#       # binomial 
#       else {
#         
#         cat("binomial re ")
#         
#         gam_output <- gam(data = df_analysis, Abundance ~ s(mean_last_7days, k = 3) + s(fLake, bs = 're')
#                           +  s(fProtocol, bs = 're'), family = binomial)
#         
#         lake_df_analysis <- species_lake |>
#           filter(Species == i)
#         
#         lake_list <- distinct(lake_df_analysis, Lake) |>
#           pull()
#         
#       }
#       
#     }
#   }
# }
# 
# 
# # not working because of derivatives
# 
# deriv <- function(model_species, df){
#   
#   species_lake <- read_xlsx("species_lake.xlsx") 
#   
#   lake_list <- distinct(species_lake, Lake) |> 
#     pull()
#   
#   derivatives(model_species)
#   
#   for (j in lake_list){
#     
#     data_lake <- df |> 
#       filter(Lake == j)
#     
#     unique_lakes <- distinct(data_lake, fLake)
#     unique_protocol <- distinct(data_lake, fProtocol)
#     
#     newdata <- tibble(mean_last_7days = seq(
#       from = min(data_lake$mean_last_7days, na.rm = TRUE),
#       to = max(data_lake$mean_last_7days, na.rm = TRUE), length = 200),
#       fProtocol = sample(levels(unique_protocol$fProtocol), size = 200, replace = TRUE))
#     
#     derivatives <- derivatives(gam_output, data = newdata) |> 
#       mutate(fLake = factor(j)) |> 
#       rename(temp = data)
#     saveRDS(derivatives, paste0("total_model/derivatives/derivatives_", j, ".rds"))
#     
#     
#   }
# 
#   
# }
# 

