run_chart_mean <- function(data = df, 
                           systolic = TRUE,
                           pulse = FALSE,
                           var_y,
                           point_size = 2,
                           title,
                           ylab,
                           y_ref) {
  
  if(systolic){
    ylab <- "Systolic mm HG"
  } else {
    ylab <- "Diastolic mm HG"
  }
  
  if(pulse) {
    ylab = "Pulse BPM"
  }
  
  if(!systolic && !pulse) {
    ylab = "Pulse Pressure mm HG"
  }
  
  p_out <- ggplot(data = data, aes(y = !!sym(var_y), x= Date))+
                  theme_bw() +
    geom_point(size = rel(point_size)) +
    labs(title = paste0(var_y),
         y = ylab) +
    geom_hline(yintercept = y_ref, linetype = "dashed")
  
  return(p_out)
}