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
