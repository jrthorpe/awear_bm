# RELATION TO QUESTIONNAIRES ----


# Calculate required variables ----

# ** Mobility Zones ----
# mobility boundary crossings based on the mobility baseline questionnaire:
# counts the number of times the person leaves a certain radius around their home
# levels: daily, 4-6 per week, 1-3 per week, less than 1
# TODO: fix this so it is not like every small trip at work is another additional trip out of town. -- done, only need in form TRUE/FALSE per day

mobility.zones <- traj.summary %>%
  mutate(zone = cut(action.range, breaks = c(0,25, 50, 1000, 10000,Inf), 
                    labels = c("mz1", "mz2", "mz3", "mz4", "mz5"))) %>%
  group_by(dates, zone) %>%
  summarize(entry = n()>0)%>% 
  ungroup() %>% 
  dcast(dates~zone,value.var="entry") %>%
  mutate(dweek = (as.numeric(dates-dates[1])) %/% 7)

#saveRDS(mobility.zones,paste0("M:/PhD_Folder/awear_bm/output_data/mobilityzones_",p,".Rds"))


# ** Activity Patterns/Levels ----

# Transport:
# vehicle, bicycle, foot>moves: days/week

# Active time (not for transport):
# foot > stays+moves > hrs/day
# step count?

# sedentary bouts:
# still > stays

# indicate whether activity bouts overlap with a move
activity.moves <- bout_moves(activity.bouts, traj.summary)

# daily summary for activity bouts with stay/move information 
activity.moves.pday <- activity.moves %>% 
  group_by(dates, activity, moving) %>% 
  summarise(total.time = sum(duration))

#saveRDS(activity.moves.pday,paste0("M:/PhD_Folder/awear_bm/output_data/activitymoves_",p,".Rds"))


# Create the comparison plots ----

# define variables:
p.codes <- c("P03JJ","P06SS","P07MG","P08UH","P10JL","P13NB")


# ** Load questionnaire and sensor results ----

# Note: for questionnaires -- first time imported from excel, edited and saved, thereafter loaded from "output" folder.

# / Activity
# act.assessments <- activity.assessments %>% filter(assessment == "post") %>% select(-assessment)
# remove(activity.assessments)
# saveRDS(act.assessments,"./output_data/activity_assessments_post.Rds")
act.assessments <- readRDS("./output_data/activity_assessments_post.Rds")

act.list <- list(P03JJ = readRDS("./output_data/activitymoves_P03JJ.Rds"),
                P06SS = readRDS("./output_data/activitymoves_P06SS.Rds"),
                P07MG = readRDS("./output_data/activitymoves_P07MG.Rds"),
                P08UH = readRDS("./output_data/activitymoves_P08UH.Rds"),
                P10JL = readRDS("./output_data/activitymoves_P10JL.Rds"),
                P13NB = readRDS("./output_data/activitymoves_P13NB.Rds"))

steps.list <- list(P03JJ = readRDS("./output_data/steps_P03JJ.Rds"),
                 P06SS = readRDS("./output_data/steps_P06SS.Rds"),
                 P07MG = readRDS("./output_data/steps_P07MG.Rds"),
                 P08UH = readRDS("./output_data/steps_P08UH.Rds"),
                 P10JL = readRDS("./output_data/steps_P10JL.Rds"),
                 P13NB = readRDS("./output_data/steps_P13NB.Rds"))

# / Mobility
# mb.assessments <- mobility_assessment %>% filter(assessment == "post") %>% select(-assessment)
# remove(mobility_assessment)
# saveRDS(mb.assessments,"./output_data/mobility_assessments_post.Rds")
mz.assessments <- readRDS("./output_data/mobility_assessments_post.Rds")

mz.list <- list(P03JJ = readRDS("./output_data/mobilityzones_P03JJ.Rds"),
                P06SS = readRDS("./output_data/mobilityzones_P06SS.Rds"),
                P07MG = readRDS("./output_data/mobilityzones_P07MG.Rds"),
                P08UH = readRDS("./output_data/mobilityzones_P08UH.Rds"),
                P10JL = readRDS("./output_data/mobilityzones_P10JL.Rds"),
                P13NB = readRDS("./output_data/mobilityzones_P13NB.Rds"))



# ** Mobility results ----
mz.plot_list = vector("list",length(mz.list))
for(i in 1:length(mz.list)){

  #i<-1 #debug
  p.code <- p.codes[i] # get participant code
  mz.dat <- mz.list[[p.code]] # use participant code to ensure matching data
  
  mz.weekly <- mz.dat %>% group_by(dweek) %>% summarise_at(vars(mz2:mz5), sum, na.rm = TRUE) %>% 
    mutate(adherence = count(mz.dat,dweek)$n) %>% 
    filter(adherence==7) %>% 
    select(-adherence)
  
  q.post <- mz.assessments %>% filter(participant == p.code)
  
  mz.compare <- data.frame(sensor = colMeans(tail(mz.weekly,4)[,2:5]),
                           survey = as.numeric(q.post[,3:6])) 
  mz.compare %<>% mutate(zone=rownames(mz.compare))
  mz.toplot <- melt(mz.compare,id.vars = "zone")
  
  # p <- plot_ly(mz.toplot,
  #              x=~variable,
  #              y=~zone,
  #              type="scatter",
  #              mode = "markers",
  #              color=~variable,
  #              colors=c("lightblue","orange"),
  #              size = ~value,
  #              marker = list(opacity = 0.8, sizeref=1.5, sizemode = "diameter")) %>%
  #   layout(xaxis=list(title=FALSE),
  #          showlegend=FALSE)
  
  p <- plot_ly(mz.toplot, 
                     x = ~zone, 
                     y = ~value,
                     color = ~variable,
                     colors = c("lightblue","orange"),
                     type = "bar", 
                     showlegend=ifelse(p.code=="P13NB",TRUE,FALSE))
  
  mz.plot_list[[i]] <- p
}

subplot(mz.plot_list, nrows = 2, margin=0.08) # for 2 columns use ceiling(i/2)
#Reference for bubble charts: https://plot.ly/r/bubble-charts/

# # Heatmap alternative:
# plot_ly(mb.results.toplot,
#         x=~dweek,
#         y=~variable,
#         z=~value,
#         type="heatmap",
#         colorscale = "Greys")

remove(mz.compare,mz.dat,mz.toplot,mz.weekly,q.post)

# ** Activity results ----


# TODO: adherence step from mz doesn't work here because they don't go out as
# much. Need to get adherence information "globally" (e.g. from screen touch)
# and apply to all tests.

# survey results:
activity.results <- act.assessments %>% mutate(vehicle.days = car.days+dot.days,
                            source = "survey") %>%
  select(participant,
         active.work.days,
         active.work.mpd,
         active.sport.days,
         active.sport.mpd,
         still.mpd,
         vehicle.days,
         bike.days,
         foot.days,
         bike.mpd,
         foot.mpd,
         source)


# sensor results:
for(p in p.codes){
  
  #p.code <- "P03JJ" #p.codes[i] # get participant code
  tmp <- act.list[[p]]
  
  sensor.active <- tmp %>% ungroup() %>% filter(activity=="Foot") %>%
    select(dates, activity, total.time, moving) %>%
    mutate(dweek = (as.numeric(dates-dates[1])) %/% 7) %>%
    filter(dweek %in% c((max(dweek)-5):(max(dweek)-1))) 
  
  sensor.active.days <- sensor.active %>%
    filter(moving==0) %>%
    group_by(dweek) %>%
    summarise(active.days = n())
  
  sensor.active.mpd <- sensor.active %>%
    group_by(moving) %>%
    summarise(mpd=mean(total.time))
  
  # sensor.still ...
  
  sensor.transport <- tmp  %>% ungroup() %>% filter((activity=="Foot" & moving==1) |
                                                      activity=="Vehicle" | 
                                                      activity=="Bicycle") %>%
    dcast(dates~activity, value.var="total.time",
          fun.aggregate=sum) %>% # sum where there is total.time for both move & stay (applies to vehicle and bicycle)
    mutate_if(is.numeric,as.logical) %>%  # convert total.time to logical indicator for if mode used that day
    mutate(dweek = (as.numeric(dates-dates[1])) %/% 7) %>% group_by(dweek) %>%
    summarise_at(vars(Foot,Bicycle,Vehicle), sum, na.rm = TRUE)
  
  sensor.transport.days <- sensor.transport %>% filter(dweek %in% c((max(dweek)-5):(max(dweek)-1))) %>%
    summarise_at(vars(Foot,Bicycle,Vehicle), mean, na.rm = TRUE)
  
  # Create a dataframe of one row matching the variables in the assessment, and append.
  current <- data.frame(
    participant = p,
    active.work.days = mean(sensor.active.days$active.days),
    active.work.mpd = sensor.active.mpd %>% filter(moving==0) %>% select(mpd) %>% as.numeric(),
    active.sport.days = NA,
    active.sport.mpd = NA,
    still.mpd = "TBC",
    vehicle.days = sensor.transport.days$Vehicle,
    bike.days = sensor.transport.days$Bicycle,
    foot.days = sensor.transport.days$Foot,
    bike.mpd = NA,
    foot.mpd = sensor.active.mpd %>% filter(moving==1) %>% select(mpd) %>% as.numeric(),
    source = "sensor"
  )
  
  activity.results <- rbind(activity.results, current)
  
}
remove(current, 
       sensor.active, sensor.active.days, sensor.active.mpd, 
       sensor.transport, sensor.transport.days)


# Plot A1: transport modes days per week

td.plot_list = list()
for(p in p.codes){
  
  transport.days <- activity.results %>% filter(participant==p) %>% 
    select(vehicle.days, bike.days, foot.days, source) %>%
    melt(id.vars="source", variable.name = "mode")
  
  
  td.plot <- plot_ly(transport.days %>% group_by(source), 
                     x = ~mode, 
                     y = ~value,
                     color = ~source,
                     colors = c("lightblue","orange"),
                     type = "bar",
                     legendgroup = ~source, 
                     showlegend=ifelse(p=="P03JJ",TRUE,FALSE))
  
  td.plot_list[[p]] <- td.plot
  
}
subplot(td.plot_list, nrows = 2, margin=0.08) # for 2 columns use ceiling(i/2)



# Plot A2: active hrs/day by stay/move

active.plot_list = list()
for(p in p.codes){
  
  active.mpd <- activity.results %>% filter(participant==p) %>% 
    select(active.work.mpd, foot.mpd, source) %>%
    melt(id.vars="source", variable.name = "type", value.name = "minutes")
  
  
  active.plot <- plot_ly(active.mpd, 
          x = ~source, 
          y = ~minutes,
          color = ~type,
          colors = c("lightblue","orange"),
          type = "bar",
          showlegend=ifelse(p=="P03JJ",TRUE,FALSE)) %>%
    layout(barmode = "stack")
  
  active.plot_list[[p]] <- active.plot
  
}
subplot(active.plot_list, nrows = 2, margin=0.08) # for 2 columns use ceiling(i/2)


# Plot A3: still time hrs/day by (stay only)


# STEPS:



