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
  
  # # Remove drops in steps (caused by reboot) #note: some small drops appear to happen without reboot, see issues
  # steps$dstep[steps$dstep<0] <- 0
  
  # Instead of removing drops in steps, remove drastic changes according to threshold (set the differences in steps to 0):
  T.steps <- 10000 #threshold for increments in steps
  steps$dstep[abs(steps$dstep)>T.steps] <- 0
  
  # cummulate over day instead of between reboots
  steps %<>% group_by(dates) %>% mutate(stepcounter = cumsum(dstep))
  
  return(steps)
}

pattern_steps <- function(steps, win.size){
  
  # get steps in windows
  minute <- as.numeric(format(steps$timestamp, "%H"))*60 + as.numeric(format(steps$timestamp, "%M")) # minute since midnight
  steps %<>% mutate(window = ceiling(minute/win.size))
  win.totals <- steps %>% group_by(dates,window) %>% summarise(steps.win = sum(dstep))
  
  return(win.totals)

}


# # TESTING & DEBUGGING
# steps.log %<>% arrange(dsource) %>% mutate(dSdT = abs(c(0,diff(step_count))))
# 
# plot_ly(data = steps.log %>% group_by(dsource),
#         x = ~timestamp,
#         y = ~step_count,
#         color = ~dsource,
#         colors = colourset[1:2],
#         type = "scatter",
#         mode = "markers+lines") %>%
#   add_trace(data = steps.log %>% group_by(dsource),
#             x = ~timestamp,
#             y = ~dSdT,
#             inherit = F,
#             type = "scatter",
#             mode = "markers+lines",
#             color = ~dsource,
#             colors = colourset[3:4])
