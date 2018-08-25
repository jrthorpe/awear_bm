
# Plots for A1
screen.plots <- list()
battery.plots.A <- list()
battery.plots.B <- list()

activity.plots <- list()
steps.plots <- list()
mobility.plots <- list()
mcp.plots <- list()


i <- 1 #counter

for(p in p.codes){
  
  # screen on lines:
  p.screen <- screen %>% filter(participant == p) %>% 
    arrange(timestamp) %>%
    mutate(on.event = cumsum(screen_on),
           switch = c(screen_on[1], diff(screen_on)))
  
  p.screen %<>% filter(switch != 0)
  
  screen.plots[[i]] <- plot_ly(p.screen %>% group_by(dates, on.event),
                               x = ~times,
                               y = ~dates,
                               type = "scatter",
                               mode = "lines",
                               line = list(width = 6)) %>%
    layout(title=p, titlefont = f1)
  
  
  # battery charging patterns:
  p.battery <- battery %>% filter(participant == p)
  
  battery.plots.A[[i]] <- plot_ly(data=p.battery,
          x = ~times,
          y = ~dates,
          type = "scatter",
          mode = "markers",
          color = ~level) %>%
    layout(xaxis =list(range = c(min(p.battery$times),max(p.battery$times))),
           yaxis = list(range = c(min(p.battery$dates),max(p.battery$dates))),
           title=p, titlefont = f1)
  
  
  battery.plots.B[[i]] <- plot_ly(data=p.battery %>% filter(plugged==1),
          x = ~times,
          y = ~dates,
          type = "scatter",
          mode = "markers",
          name = "charging")  %>%
    add_trace(data=p.battery %>% filter(plugged==0),
              x = ~times,
              y = ~dates,
              type = "scatter",
              mode = "markers",
              name = "unplugged") %>%
    layout(title=p, titlefont = f1)
  
  # traj plots
  p.traj <- traj %>% filter(participant == p)
  
  traj_melt <- melt(p.traj, id.vars = c("dates","traj.event","is.home","action.range"), 
                    measure.vars = c("T.start","T.end")) %>% 
    mutate(times = as.POSIXct(strftime(value, format="%H:%M:%S"), format="%H:%M:%S"),
           dist.group = cut(action.range,
                            breaks = c(0, 100,  500,  1000,  2000, 5000, 10000, Inf),
                            labels = c("<0.1km", "0.1-0.5km", "0.5-1km", "1-2km", "2-5km", "5-10km", ">10km"))) %>%
    group_by(dates, traj.event)
  
  mobility.plots[[i]] <- plot_ly(data = traj_melt,
          x = ~times,
          y = ~dates,
          type = "scatter",
          mode = "lines",
          color = ~dist.group,
          colors = "YlGnBu",
          line = list(width=4)) %>%
    layout(xaxis = timeaxis,
           title=p, titlefont = f1,
           #margin = list(l = 50, r = 50, b = 80, t = 50, pad = 4),
           legend = l)
  
  p.mcp <- mcp %>% filter(participant == p) %>% ungroup()
  p.mcp %<>% mutate(dweek =  weekdays(as.Date(dates)))
  mcp.plots[[i]] <-   plot_ly() %>%
    add_polygons(data=p.mcp %>% group_by(dates), 
                 x=~lon, 
                 y=~lat, 
                 #type="scatter", 
                 #mode = "lines", 
                 color = ~dweek,
                 colors = "Spectral")
    
    # plot_ly(p.mcp %>% group_by(dates), 
    #       x=~lon, 
    #       y=~lat, 
    #       type="scatter", 
    #       mode = "lines", 
    #       color = ~dweek,
    #       colors = "Spectral")
  
  # plot_ly() %>%
  #   add_polygons(data=mcp %>% group_by(participant,dates), 
  #         x=~lon, 
  #         y=~lat, 
  #         #type="scatter", 
  #         #mode = "lines", 
  #         color = ~participant,
  #         colors = "Spectral")
  
  
  
  # activity plots
  p.activity <- activities %>% filter(participant == p)
  
  bout_melt <- melt(p.activity, id.vars = c("dates","bout","activity"), 
                    measure.vars = c("b.start","b.end")) %>% 
    mutate(times = as.POSIXct(strftime(value, format="%H:%M:%S"), format="%H:%M:%S")) %>%
    group_by(dates, activity, bout)

  activity.plots[[i]] <- plot_ly(data = bout_melt %>% filter(activity == "Still"),
          x = ~times,
          y = ~dates,
          type = "scatter",
          mode = "lines",
          line = list(color = "grey", width = 2),
          name = "Still") %>%
    add_trace(data = bout_melt %>% filter(activity == "Foot"),
              x = ~times,
              y = ~dates,
              type = "scatter",
              mode = "lines",
              opacity = 0.5,
              line = list(color = "red", width = 20),
              name = "On Foot") %>%
    add_trace(data = bout_melt %>% filter(activity == "Bicycle"),
              x = ~times,
              y = ~dates,
              type = "scatter",
              mode = "lines",
              opacity = 0.5,
              line = list(color = "blue", width = 20),
              name = "Bicycle") %>%
    add_trace(data = bout_melt %>% filter(activity == "Vehicle"),
              x = ~times,
              y = ~dates,
              type = "scatter",
              mode = "lines",
              opacity = 0.5,
              line = list(color = "green", width = 20),
              name = "Vehicle") %>%
    layout(xaxis = timeaxis,
           title=p, titlefont = f1,
           #margin = list(l = 50, r = 50, b = 80, t = 50, pad = 4),
           legend = l)
  
  
  
  # steps plots
  p.steps <- stepsdaily %>% filter(participant == p)
  
  steps.plots[[i]] <- plot_ly(p.steps %>% group_by(device),
                              x = ~dates,
                              y = ~steps,
                              color = ~device,
                              colors = colourset[1:2],
                              type = "scatter",
                              mode = "markers+lines") %>%
    layout(title=p)
  
  # Replaced step count signals with daily steps to be more visually comprehendible and less data (was slowing everything down)
  # p.steps <- stepcounters %>% filter(participant == p)
  # 
  # steps.plots[[i]] <- plot_ly(p.steps %>% group_by(source),
  #                             x = ~timestamp,
  #                             y = ~stepcounter,
  #                             color = ~source,
  #                             colors = colourset[1:2],
  #                             type = "scatter",
  #                             mode = "markers+lines")
  
  
  
  
  i <- i+1
  
}
# 
# plot_ly(metrics,
#         x=~dates,
#         y=~AR.max,
#         type = "scatter",
#         mode = "markers",
#         color= ~participant)
# 
# plot_ly(metrics,
#         x=~dates,
#         y=~dist.total,
#         type = "scatter",
#         mode = "markers",
#         color= ~participant)
# 
# plot_ly(metrics,
#         x=~dates,
#         y=~mcp.area,
#         type = "scatter",
#         mode = "markers",
#         color= ~participant)








# 
# p.homedist <- actionrange %>% filter(participant == p) %>%
#   mutate(dist.group = cut(
#     homedist,
#     breaks = c(0, 100, 10000, Inf),
#     labels = c("home", "out", "OOT")
#   ))
# 
# 
# #mobility.plots.A[[i]] <- 
# 
# plot_ly(p.homedist %>% filter(dist.group=="out"),
#         x = ~times,
#         y = ~dates,
#         type = "scatter",
#         mode = "markers",
#         color = ~homedist) %>%
#   layout(xaxis =list(range = c(min(p.homedist$times),max(p.homedist$times))),
#          yaxis = list(range = c(min(p.homedist$dates),max(p.homedist$dates))),
#          title=p, titlefont = f1)

#colors = colorRamp(c("yellow", "green","blue","purple"))
#colorbar(limits = c(0,10000), title = "TITLE") %>%

# # create time axis range covering 30 minutes before first / after last move of the day
# min.time <-  as.POSIXct("06:00:00", format="%H:%M:%S")
# max.time <-  as.POSIXct("23:00:00", format="%H:%M:%S")
# timeaxis$range <- as.numeric(c(min.time,max.time)-24*60*60)*1000 # time axis defined in stylesheet script

# # create time axis range covering 30 minutes before first / after last move of the day
# min.time <-  min(p.battery$times)
# max.time <-  max(p.battery$times)
# timeaxis$range <- as.numeric(c(min.time,max.time))*1000 # time axis defined in stylesheet script

