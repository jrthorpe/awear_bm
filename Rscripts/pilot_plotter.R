# Creates all the plots in a participant-date loop

plotlist_mob_act <- list()
plotlist_steps <- list()
plotlist_places <- list()
counter <- 1
counter_p <- 1

# Create plots per participant per day (trajectories, activities, steps)
for(p in p.codes){
  
  # get algorithm and logsheet dataset for current participant
  traj.logsheets.p <- traj.logsheets %>% filter(participant==p)    # stay/move trajectories, logsheets
  traj.algorithm.p <- traj.algorithm %>% filter(participant==p)    # stay/move trajectories, algorithm results
  act.algorithm.p <- act.algorithm %>% filter(participant==p)      # activity bouts, algorithm results
  stepcounters.p <- stepcounters %>% filter(participant==p)        # cumulative steps from phone and watch
  metrics.compare.p <- metrics.compare %>% filter(participant==p)  # metrics results combined from logsheets and algorithm
  
  # get set of dates covered by both algorithm/logsheets results
  overlap <- unique(traj.algorithm.p$dates) %in% unique(traj.logsheets.p$dates) 
  period <- unique(traj.algorithm.p$dates)[overlap]
  
  for(i in 1:1){ #length(period)
    
    # unpack data for specified date/participant into environment
    d <- period[i]
    data.d <- pilot_data_bydate(traj.logsheets.p, traj.algorithm.p, act.algorithm.p, d) 
    list2env(data.d, .GlobalEnv); remove(data.d)
    
    # trajectories plot:
    
    # create time axis range covering 15 minutes before first / after last move of the day
    min.time <- min(log.data$Time[2],alg.data$Time[2])-15*60 # earliest between logsheets/algorithm
    max.time <- max(log.data$Time[length(log.data)-1],alg.data$Time[length(alg.data)-1])+15*60 # latest between logsheets/algorithm
    timeaxis$range <- as.numeric(c(min.time,max.time))*1000 # time axis defined in stylesheet script
    
    # create plot by layering logsheets trajectory, algorithm trajectory and text for moves as annotations
    tmp.traj <- plot_ly(data = log.data %>% arrange(event,Time),
                        x=~Time, y=~StayGo,
                        type = "scatter", mode = "lines", line = list(color = "black"),
                        legendgroup = "Trajectories", name = "Logsheet results") %>%
      add_trace(data = alg.data %>% arrange(event,Time),
                x=~Time, y=~StayGo,
                line = list(dash = "dot",color = colourset[7]),
                legendgroup = "Trajectories", name = "Algorithm results")  %>%
      add_text(data = log.data, x=~Time, y=~StayGo, 
               text = ~Mode, name = "Logged transport mode", inherit=FALSE,
               textposition = "bottom right", textfont = list(color = "black", size = 12)) %>%
      layout(yaxis=list(title=" ", range = c(-1,3)),
             xaxis=timeaxis,
             margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
             title=paste(p,d))
    
    # activities: plots "still" as a grey line then layers each other type of bout in a different colour
    tmp.act <- plot_ly(data = act.data %>% filter(activity == "Still"),
                       x = ~value, y = ~dates, name = "Still",
                       type = "scatter", mode = "lines", line = list(color = "grey", width = 2)) %>% #legendgroup = "Activity")
      add_trace(data = act.data %>% filter(activity == "Foot"),
                x = ~value, y = ~dates, name = "On Foot",
                type ="scatter", mode="lines",
                opacity=0.5, line=list(color=colourset[1], width=20)) %>% 
      #legendgroup = "Activity")
      add_trace(data = act.data %>% filter(activity=="Bicycle"),
                x = ~value, y = ~dates, name = "Bicycle",
                type = "scatter", mode = "lines",
                opacity = 0.5, line = list(color=colourset[2], width = 20)) %>% #legendgroup = "Activity") 
      add_trace(data = act.data %>% filter(activity == "Vehicle"),
                x = ~value, y = ~dates, name = "Vehicle",
                type = "scatter", mode = "lines",
                opacity = 0.5, line = list(color=colourset[3], width = 20)) %>% #legendgroup = "Activity") 
      layout(yaxis=list(title="Activity", showticklabels = FALSE),
             xaxis=timeaxis,
             margin = list(l = 50, r = 50, b = 80, t = 50, pad = 4))
    
    # steps plot
    tmp.steps <- plot_ly(stepcounters.p %>% filter(dates==d) %>% group_by(source),
                         x = ~timestamp, y = ~stepcounter,
                         type = "scatter", mode = "lines+markers",
                         color = ~source, colors = colourset[4:5],
                         legendgroup = "Steps", name = "source")  %>%
      layout(yaxis=list(title="Step count"), 
             xaxis=timeaxis,
             margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
    
    # # to show individual plots
    # print(subplot(tmp.traj, tmp.act, nrows=2, shareX = TRUE))
    # print(subplot(tmp.traj, tmp.steps, nrows=2, shareX = TRUE))
    
    # add individual plots to respectives lists list
    plotlist_mob_act[[counter]] <- subplot(tmp.traj, tmp.act, nrows=2, shareX = TRUE) 
    plotlist_steps[[counter]] <- subplot(tmp.traj, tmp.steps, nrows=2, shareX = TRUE)
    counter <- counter + 1
  }
  
  # get comparison of places per day for current participant
  anno_subtitle$text <- p  # anno_subtitle defined in stylesheet script
  sp <- plot_ly(metrics.compare.p,
                x=~Day,
                y=~N.places,
                type = "bar",
                color = ~results,
                colors = colours.compare[1:2],
                showlegend = ifelse(counter_p==1,TRUE,FALSE))%>%
    layout(annotations = anno_subtitle,
           yaxis = list(title="N unique places"),
           xaxis = list(title="Day"))
  plotlist_places[[counter_p]] <- sp; counter_p <- counter_p+1
  
}