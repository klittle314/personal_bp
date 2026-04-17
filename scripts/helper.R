run_chart_mean <- function(data = df, 
                           systolic = TRUE,
                           pulse = FALSE,
                           var_y,
                           point_size = 2,
                           title,
                           ylab,
                           y_ref) {
 
  mean_use <- round(colMeans(df[,var_y], na.rm = TRUE),1)
   
  if(systolic && pulse) {
    ylab = "Pulse Pressure mm HG"
    subtitle = paste0("Pulse Pressure avg = ", mean_use)
  }
  
  if(systolic && !pulse){
    ylab <- "Systolic mm HG"
    subtitle <- paste0("Systolic avg = ", mean_use)
  }
  
  if(!systolic && !pulse) {
    ylab <- "Diastolic mm HG"
    subtitle <- paste0("Diastolic avg = ", mean_use)
  }
  
  if(!systolic && pulse) {
    ylab = "Pulse BPM"
    subtitle = paste0("Pulse BPM = ", mean_use)
  }
  
  
  
  p_out <- ggplot(data = data, aes(y = !!sym(var_y), x= Date))+
                  theme_bw() +
    geom_point(size = rel(point_size)) +
    labs(title = paste0(var_y),
         subtitle = subtitle,
         y = ylab) +
    geom_hline(yintercept = y_ref, linetype = "dashed")
  
  return(p_out)
}