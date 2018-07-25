# ------------------------------------------------
# MODULE: STEPS
#
# Description:
#
#
#
# ------------------------------------------------

# step count log file
steps.log <- datasets.all$step_count %>% 
  filter(timestamp>=d.start, timestamp<=(d.start %m+% days(d.study))) %>% # get timeframe of just the day of interest
  select(step_count, dsource, timestamp, intervals.alt, dates, times)

# step count over day
steps.watch <- daily_steps(steps.log,"watch")
steps.phone <- daily_steps(steps.log,"phone")

# summary of daily total steps by device
steps.totals <- merge(x=steps.watch %>% summarise(total = max(stepcounter)), 
                      y=steps.phone %>% summarise(total = max(stepcounter)),
                      by="dates", suffixes=c(".watch",".phone"), 
                      all.x = TRUE, all.y = TRUE,
                      incomparables = NA)