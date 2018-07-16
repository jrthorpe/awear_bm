# TOTAL DAILY STEPS BY DEVICE ----
daily_steps <- function(steps.log, device){
  # Extract data for specified device and fix to cumulate by day
  
  # Filter only specified device data, and get increments in steps and time for that source only
  steps <-
    steps.log %>% filter(dsource == device) %>% select(timestamp, step_count, dates, times) %>%
    mutate(dstep = c(0, diff(step_count)),
           dtime = c(0, difftime(
             tail(timestamp, -1), head(timestamp, -1), units = "mins"
           )))
  
  # Remove drops in steps (caused by reboot) #note: some small drops appear to happen without reboot, see issues
  steps$dstep[steps$dstep<0] <- 0
  
  # cummulate over day instead of between reboots
  steps %<>% group_by(dates) %>% mutate(stepcounter = cumsum(dstep))
  
  return(steps)
}

# For visualising step data:
steps_vis <- function(steps.log){
  
  plot_ly(steps.log,
          x = ~timestamp,
          y = ~step_count,
          color = ~dsource,
          colors = c("orange","deepskyblue2"),
          type = "scatter",
          mode = "lines+markers")
  
  # P <- plot_ly(steps.log,
  #       x = ~timestamp,
  #       y = ~step_count,
  #       color = ~dsource,
  #       colors = c("orange","deepskyblue2"),
  #       type = "scatter",
  #       mode = "lines+markers")
  # return(P)
}

