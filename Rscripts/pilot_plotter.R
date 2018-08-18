# Creates all the plots in a participant-date loop

# Initialise lists and counters
plotlist_mob_act <- list()
#l_mob_act <- htmltools::tagList() #trying to get plots to show in word/pdf
plotlist_steps <- list()
plotlist_places <- list()
counter <- 1
counter_p <- 1

# Define colours for activities
colours.act <- list(Foot=colourset[1], Bicycle=colourset[2], Vehicle=colourset[3])

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
  
  for(i in 1:length(period)){ #
    
    # unpack data for specified date/participant into environment
    d <- period[i]
    data.d <- pilot_data_bydate(traj.logsheets.p, traj.algorithm.p, act.algorithm.p, d) 
    list2env(data.d, .GlobalEnv); remove(data.d)
    
    # trajectories plot:
    
    # create time axis range covering 30 minutes before first / after last move of the day
    min.time <- min(log.data$Time[2],alg.data$Time[2])-15*60 # earliest between logsheets/algorithm
    max.time <- max(log.data$Time[nrow(log.data)-1],alg.data$Time[nrow(alg.data)-1])+15*60 # latest between logsheets/algorithm
    timeaxis$range <- as.numeric(c(min.time,max.time))*1000 # time axis defined in stylesheet script
    
    # create plot by layering logsheets trajectory, algorithm trajectory and text for moves as annotations
    if(nrow(alg.data) > 0 & nrow(log.data) > 0 & nrow(act.data) > 0){
      tmp.traj <- plot_ly(data = log.data %>% arrange(event,Time),
                        x=~Time, y=~StayGo,
                        type = "scatter", mode = "lines", line = list(color = "black"),
                        height = 350,
                        width = 1200,
                        name = "Logsheets") %>% #legendgroup = "Trajectories", 
      add_trace(data = alg.data %>% arrange(event,Time),
                x=~Time, y=~StayGo,
                line = list(dash = "dot", color = colourset[7], width = 4),
                name = "Algorithm")  %>% #legendgroup = "Trajectories", 
      add_text(data = log.data, x=~Time, y="Mode", 
               text = ~Mode, name = "Reported transport", inherit=FALSE,
               textposition = "top right", textfont = f3) %>%
      layout(yaxis=list(title=" ", tickfont = f3, range = c(-1,3), 
                        categoryarray = c("Mode","Go","Stay"), categoryorder="array"),
             xaxis=timeaxis,
             margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
             title=paste0(p," day",i), titlefont = f1, #use participation day instead date
             legend = l)
    
    # activities: plots "still" as a grey line then layers each other type of bout in a different colour
    
      # make the plot
      stdaxis$title <- "Activity"; stdaxis$showticklabels <- FALSE # customise standard axis
      tmp.act <- plot_ly(
          data = act.data %>% filter(activity == "Still"),
          x = ~ value,
          y = ~ dates,
          height = 350,
          width = 1200,
          name = "Still",
          type = "scatter",
          mode = "lines",
          line = list(color = "grey", width = 2)) %>%
        layout(yaxis = list(title = "Activity", showticklabels = FALSE, titlefont = f2, tickfont = f3),
               xaxis = timeaxis,
               margin = list(l = 50, r = 50, b = 80, t = 50, pad = 4),
               legend = l)
      
      # layer it up
      act.data %<>% filter(activity != "Still")
      for (act in unique(act.data$activity)) {
        #subset data
        dataFilt <- act.data %>% filter(activity == act)
        
        #add it
        tmp.act <- add_trace(
          tmp.act,
          data = dataFilt,
          x =  ~ value,
          y =  ~ dates,
          name = act,
          type = "scatter",
          mode = "lines",
          opacity = 0.5,
          line = list(color = colours.act[[act]], width = 20))
      }
      
      # add individual plots to respectives lists list
      plotlist_mob_act[[counter]] <- subplot(tmp.traj, tmp.act, nrows=2, shareX = TRUE)   
      
    } else{
      # add individual plots to respectives lists list
      plotlist_mob_act[[counter]] <- NULL 
    }
    
    # steps plot
    steps.dat <- stepcounters.p %>% filter(dates==d)
    if("watch" %in% steps.dat$source & "phone" %in% steps.dat$source){
      unique(stepcounters.p$source)
      tmp.steps <- plot_ly(steps.dat %>% group_by(source),
                         x = ~timestamp, y = ~stepcounter,
                         height = 400,
                         width = 500,
                         type = "scatter", mode = "lines+markers",
                         color = ~source, colors = colourset[4:5])  %>% #legendgroup = "Steps", 
      layout(yaxis = list(title="Step count", titlefont=f2, tickfont=f3), 
             xaxis = timeaxis,
             title=paste0(p," day",i), titlefont = f1, #use participation day instead date
             margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
             legend = l)
    
     #plotlist_steps[[counter]] <- subplot(tmp.traj, tmp.steps, nrows=2, shareX = TRUE)
     plotlist_steps[[counter]] <- tmp.steps # to have only steps without trajectories
    } else{
      plotlist_steps[[counter]] <- NULL
    }
    
    # # to show individual plots
    # print(subplot(tmp.traj, tmp.act, nrows=2, shareX = TRUE))
    # print(subplot(tmp.traj, tmp.steps, nrows=2, shareX = TRUE))
    
    # add individual plots to respectives lists list
    #plotlist_mob_act[[counter]] <- subplot(tmp.traj, tmp.act, nrows=2, shareX = TRUE) 
    #plotlist_steps[[counter]] <- subplot(tmp.traj, tmp.steps, nrows=2, shareX = TRUE)
    # l_mob_act[[counter]] <- as_widget(subplot(tmp.traj, tmp.act, nrows=2, shareX = TRUE)) #trying to get plots to show in word
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
                showlegend = ifelse(counter_p==1,TRUE,FALSE),
                height = 300,
                width = 900)%>%
    layout(annotations = anno_subtitle,
           yaxis = list(title="N unique places"),
           xaxis = list(title="Day", dtick = 1),
           legend = l)
  plotlist_places[[counter_p]] <- sp; counter_p <- counter_p+1
  
}
remove(tmp.act, tmp.steps, tmp.traj,
       traj.algorithm, traj.algorithm.p,
       traj.logsheets, traj.logsheets.p,
       stepcounters, stepcounters.p, steps.dat,
       act, counter, counter_p, d, p, period, i)



