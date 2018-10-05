# ------------------------------------------------
# MODULE: STEPS
#
# This module splits step data by source (watch or phone) then restructures it
# such that the cumulative count is over a daily cycle (24hrs) instead of
# between device reset/restart (uses the custom function "daily_steps"). A
# summary of daily totals is created merging the watch and phone data.
#
# ------------------------------------------------


# step count over day (restructure cumulative count) for each device:
steps.watch <- daily_steps(steps.log,"watch") 
steps.phone <- daily_steps(steps.log,"phone")

# summary of daily total steps by device
steps.totals <- merge(x=steps.watch %>% summarise(total = max(stepcounter)), 
                      y=steps.phone %>% summarise(total = max(stepcounter)),
                      by="dates", suffixes=c(".watch",".phone"), 
                      all.x = TRUE, all.y = TRUE,
                      incomparables = NA)