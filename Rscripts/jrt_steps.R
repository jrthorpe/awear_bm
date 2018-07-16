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

pattern_steps <- function(steps, win.size){
  
  # get steps in windows
  minute <- as.numeric(format(steps$timestamp, "%H"))*60 + as.numeric(format(steps$timestamp, "%M")) # minute since midnight
  steps %<>% mutate(window = ceiling(minute/win.size))
  win.totals <- steps %>% group_by(dates,window) %>% summarise(steps.win = sum(dstep))
  
  return(list(steps, win.totals))

  }
