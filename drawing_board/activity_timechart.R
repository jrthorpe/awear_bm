# Script to plot a timeline chart showing all activity bouts along a time axis for each day.
bout_melt <- melt(activity.bouts, id.vars = c("dates","bout","activity"), 
             measure.vars = c("b.start","b.end")) %>% 
  group_by(dates, activity, bout)

plot_ly(data = bout_melt %>% filter(activity == "Still"),
        x = ~format(value, "%H:%M"),
        y = ~dates,
        type = "scatter",
        mode = "lines",
        line = list(color = "grey", width = 2),
        name = "Still") %>%
  add_trace(data = bout_melt %>% filter(activity == "Foot"),
            x = ~format(value, "%H:%M"),
            y = ~dates,
            type = "scatter",
            mode = "lines",
            opacity = 0.5,
            line = list(color = "red", width = 20),
            name = "On Foot") %>%
  add_trace(data = bout_melt %>% filter(activity == "Bicycle"),
            x = ~format(value, "%H:%M"),
            y = ~dates,
            type = "scatter",
            mode = "lines",
            opacity = 0.5,
            line = list(color = "blue", width = 20),
            name = "Bicycle") %>%
  add_trace(data = bout_melt %>% filter(activity == "Vehicle"),
            x = ~format(value, "%H:%M"),
            y = ~dates,
            type = "scatter",
            mode = "lines",
            opacity = 0.5,
            line = list(color = "green", width = 20),
            name = "Vehicle")