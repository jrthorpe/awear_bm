# LOCATION DATA (MOBILITY) ----

# **Visualise restructured data  ----
show_plots(datasets.all, to_plot)

# **Visual inspection of stay detection results ----
# basic plot
plot_ly(gps.traj.day,
        x=~lon,
        y=~lat,
        type = "scatter",
        mode = "markers",
        color = ~as.factor(loc.id)) #or traj.event

# **locations/events overlaid over GPS trace for all datapoints:  ----
plot_ly(gps.traj.day,
        x=~lon, y=~lat,
        type = "scatter",
        mode = "lines+markers",
        color = I('grey40')) %>%
  add_trace(x=~lon, y=~lat,
            type = "scatter",
            mode = "lines+markers",
            color = ~as.factor(traj.event), #or loc.id
            colors = "Set1")

# **map in background:  ----
mappoints <- gps.traj.day
coordinates(mappoints) <- ~ lon + lat
proj4string(mappoints) <- "+init=epsg:4326"
mapview(mappoints, zcol = "traj.event", burst = TRUE, map.types = "OpenStreetMap") #or loc.id


# STEPCOUNTS ----

# For visualising step data:
steps_vis <- function(steps.log){
  
  plot_ly(steps.log,
          x = ~timestamp,
          y = ~step_count,
          color = ~dsource,
          colors = c("orange","deepskyblue2"),
          type = "scatter",
          mode = "lines+markers")
}

# Watch and phone steps summed over day
plot_ly(data = steps.watch,
        x = ~timestamp,
        y = ~stepcounter,
        type = "scatter",
        mode = "lines+markers",
        name = "watch") %>%
  add_trace(data = steps.phone,
            x = ~timestamp,
            y = ~stepcounter,
            name = "phone")

# Lines to see if there is a consistent pattern
plot_ly(
  data = steps.watch,
  x = ~ times,
  y = ~ stepcounter,
  type = "scatter",
  mode = "lines"
)
plot_ly(
  data = steps.phone,
  x = ~ times,
  y = ~ stepcounter,
  type = "scatter",
  mode = "lines"
)

plot_ly(
  patterns[[2]],
  x = ~ dates,
  y = ~ window,
  z = ~ steps.win,
  #zmin = 0,
  zmax = ~floor(quantile(steps.win, .99)),
  type = "heatmap",
  colorscale = "Greys"
)


# ACTIVITY ----


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

# Bar chart with durations in different activities:

# in hours per day
plot_ly(daily.summary %>% group_by(activity),
        x = ~dates,
        y = ~total.time/60,
        color = ~activity,
        type = "bar") %>%
  layout(barmode = "stack")

# as percentage of recording time per day
plot_ly(daily.summary %>% group_by(activity),
        x = ~dates,
        y = ~act.percent,
        color = ~activity,
        colors = c("purple","orange","grey","blue"),
        type = "bar") %>%
  layout(barmode = "stack")


