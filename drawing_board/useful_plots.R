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
