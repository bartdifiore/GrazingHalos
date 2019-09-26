plot.bd <- function(data, focal, constant, model1, model2, ylab = NULL, xlab = NULL){
  #Create new data.frame to predict on
  newdat1 <- expand.grid(Distance = seq(0,20,length.out = 100), focal = seq(min(data[, focal]), max(data[, focal]), length.out = 100))
  newdat1$constant <- mean(data[, constant]) # hold pisc.den at its global mean
  names(newdat1) <- c("Distance", focal, constant)
  
  mm1 <- model.matrix(delete.response(terms(model1)),newdat1) #create a new model matrix
  newdat1$predict.model1 <- plogis(drop(mm1 %*% fixef(model1)[[1]])) # calculate predictions on the fixed effects using matrix multiplication and convert to the scale of the response using plogis (see Brooks et al. 2017, appendix B)
  
  mm2 <- model.matrix(delete.response(terms(model2)),newdat1)
  newdat1$predict.model2 <- plogis(drop(mm2 %*% fixef(model2)[[1]])) # calculate predictions on the fixed effects using matrix multiplication and convert to the scale of the response using plogis (see Brooks et al. 2017, appendix B)
  
  newdat1$pred.unconditional <- newdat1$predict.model1 * newdat1$predict.model2
  
  ##### Plot comp1 fixed effects heat map 
  
  # Some stuff for plotting
  
  redtoblue <- colorRampPalette(rev(c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7","#d1e5f0", "#92c5de","#4393c3","#2166ac")))
  
  redtoblue2 <- colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4')))
  
  # Jitter on Distance for Plotting Purposes
  data$jitter.dist <- jitter(as.numeric(ifelse(data$Distance == 0, 0.5,
                                               ifelse(data$Distance == 20, 19.5, 
                                                      ifelse(data$Distance < 20 & data$Distance > 0, data$Distance, "")))))
  
  
  ggplot(newdat1, aes_string(x = "Distance", y = focal))+
    geom_raster(aes_string(fill = "pred.unconditional"))+
    scale_fill_gradientn(colors = redtoblue2(20), limits = c(0,0.8), guide = guide_colorbar(barheight = 15))+
    scale_x_continuous(breaks = seq(0,20, by = 4)) +
    geom_contour(aes_string(z = "pred.unconditional"), color = "white", binwidth = 0.1)+
    geom_point(data = data[data$ratio.stancor > 0, ], aes_string(x = "jitter.dist", y = focal, size = "ratio.stancor"), pch = 21, col = "gray80", bg = adjustcolor("black",alpha.f=0.6))+
    scale_size(range = c(2,6), breaks = seq(0, 1, by = 0.2), guide = guide_legend(title = expression("Observed \nproportion \ngrazed"), order = 1))+
    geom_point(data = data[data$ratio.stancor == 0, ], aes_string(x = "jitter.dist", y = focal), pch = 21, col = "gray80")+
    #scale_y_continuous(trans = scales::log_trans(), breaks = seq(0, 0.03, by = 0.005), labels = seq(0, 0.03, by = 0.005))+
    labs(x = xlab, y = ylab, fill = expression(paste("Predicted \ngrazing"))) +
    theme(legend.spacing = unit(1, "cm"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
}

############################################
## Log transformed scale... 
############################################


plot.bd.log <- function(data, focal, constant, model1, model2){
  #Create new data.frame to predict on
  newdat1 <- expand.grid(Distance = seq(0,20,length.out = 100), focal = seq(min(data[, focal]), max(data[, focal]), length.out = 100))
  newdat1$constant <- mean(data[, constant]) # hold pisc.den at its global mean
  names(newdat1) <- c("Distance", focal, constant)
  
  mm1 <- model.matrix(delete.response(terms(model1)),newdat1) #create a new model matrix
  newdat1$predict.model1 <- plogis(drop(mm1 %*% fixef(model1)[[1]])) # calculate predictions on the fixed effects using matrix multiplication and convert to the scale of the response using plogis (see Brooks et al. 2017, appendix B)
  
  mm2 <- model.matrix(delete.response(terms(model2)),newdat1)
  newdat1$predict.model2 <- plogis(drop(mm2 %*% fixef(model2)[[1]])) # calculate predictions on the fixed effects using matrix multiplication and convert to the scale of the response using plogis (see Brooks et al. 2017, appendix B)
  
  newdat1$pred.unconditional <- newdat1$predict.model1 * newdat1$predict.model2
  
  ##### Plot comp1 fixed effects heat map 
  
  # Some stuff for plotting
  
  redtoblue <- colorRampPalette(rev(c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7","#d1e5f0", "#92c5de","#4393c3","#2166ac")))
  
  redtoblue2 <- colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4')))
  
  # Jitter on Distance for Plotting Purposes
  data$jitter.dist <- jitter(as.numeric(ifelse(data$Distance == 0, 0.5,
                                               ifelse(data$Distance == 20, 19.5, 
                                                      ifelse(data$Distance < 20 & data$Distance > 0, data$Distance, "")))))
  
  
  newdat1$forgg <- exp(newdat1[, focal])
  data$forgg <- exp(data[,focal])
  
  ggplot(newdat1, aes_string(x = "Distance", y = "forgg"))+
    geom_raster(aes_string(fill = "pred.unconditional"))+
    scale_fill_gradientn(colors = redtoblue2(20), limits = c(0,1), guide = guide_colorbar(barheight = 15))+
    scale_x_continuous(breaks = seq(0,20, by = 4)) +
    geom_contour(aes_string(z = "pred.unconditional"), color = "white")+
    geom_point(data = data[data$ratio.stancor > 0, ], aes_string(x = "jitter.dist", y = "forgg", size = "ratio.stancor"), pch = 21, col = "gray80", bg = adjustcolor("black",alpha.f=0.6))+
    scale_size(range = c(2,6), breaks = seq(0, 1, by = 0.2), guide = guide_legend(title = expression("Observed \nProportion \nGrazed"), order = 1))+
    geom_point(data = data[data$ratio.stancor == 0, ], aes_string(x = "jitter.dist", y = "forgg"), pch = 21, col = "gray80")+
    scale_y_continuous(trans = scales::log_trans(), breaks = scales::pretty_breaks(n=6))+
    labs(x = "Distance from Reef (m)", y = expression(paste("forgg", "density (ind. "~m^{-2}~" habitat)")), fill = expression(paste("Predicted \nGrazing"))) +
    theme(legend.spacing = unit(1, "cm"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
}

##########################################################
## Incidence or Intensity
##########################################################

plot.one <- function(data, focal, constant, model1, ylab = NULL, xlab = NULL){
  #Create new data.frame to predict on
  newdat1 <- expand.grid(Distance = seq(0,20,length.out = 100), focal = seq(min(data[, focal]), max(data[, focal]), length.out = 100))
  newdat1$constant <- mean(data[, constant]) # hold pisc.den at its global mean
  names(newdat1) <- c("Distance", focal, constant)
  
  mm1 <- model.matrix(delete.response(terms(model1)),newdat1) #create a new model matrix
  newdat1$predict.model1 <- plogis(drop(mm1 %*% fixef(model1)[[1]])) # calculate predictions on the fixed effects using matrix multiplication and convert to the scale of the response using plogis (see Brooks et al. 2017, appendix B)
  
  
  # Some stuff for plotting
  
  redtoblue <- colorRampPalette(rev(c("#b2182b", "#d6604d", "#f4a582", "#fddbc7", "#f7f7f7","#d1e5f0", "#92c5de","#4393c3","#2166ac")))
  
  redtoblue2 <- colorRampPalette(rev(c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4')))
  
  # Jitter on Distance for Plotting Purposes
  data$jitter.dist <- jitter(as.numeric(ifelse(data$Distance == 0, 0.5,
                                               ifelse(data$Distance == 20, 19.5, 
                                                      ifelse(data$Distance < 20 & data$Distance > 0, data$Distance, "")))))
  
  
  ggplot(newdat1, aes_string(x = "Distance", y = focal))+
    geom_raster(aes_string(fill = "predict.model1"))+
    scale_fill_gradientn(colors = redtoblue2(20), limits = c(0,0.8), guide = guide_colorbar(barheight = 15))+
    scale_x_continuous(breaks = seq(0,20, by = 4)) +
    geom_contour(aes_string(z = "predict.model1"), color = "white")+
    geom_point(data = data[data$ratio.stancor > 0, ], aes_string(x = "jitter.dist", y = focal, size = "ratio.stancor"), pch = 21, col = "gray80", bg = adjustcolor("black",alpha.f=0.6))+
    scale_size(range = c(2,6), breaks = seq(0, 1, by = 0.2), guide = guide_legend(title = expression("Observed \nProportion \nGrazed"), order = 1))+
    geom_point(data = data[data$ratio.stancor == 0, ], aes_string(x = "jitter.dist", y = focal), pch = 21, col = "gray80")+
    #scale_y_continuous(trans = scales::log_trans(), breaks = seq(0, 0.03, by = 0.005), labels = seq(0, 0.03, by = 0.005))+
    labs(x = xlab, y = ylab, fill = expression(paste("Predicted \nGrazing"))) +
    theme(legend.spacing = unit(1, "cm"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
}
