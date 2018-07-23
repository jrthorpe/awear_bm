# Activity results compared to Questionnaire:

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
                P07MG = readRDS("./output_data/activitymoves_P03JJ.Rds"),
                P08UH = readRDS("./output_data/activitymoves_P06SS.Rds"),
                P10JL = readRDS("./output_data/activitymoves_P03JJ.Rds"),
                P13NB = readRDS("./output_data/activitymoves_P06SS.Rds"))
# / Mobility
# mb.assessments <- mobility_assessment %>% filter(assessment == "post") %>% select(-assessment)
# remove(mobility_assessment)
# saveRDS(mb.assessments,"./output_data/mobility_assessments_post.Rds")
mz.assessments <- readRDS("./output_data/mobility_assessments_post.Rds")

mz.list <- list(P03JJ = readRDS("./output_data/mobilityzones_P03JJ.Rds"),
                P06SS = readRDS("./output_data/mobilityzones_P06SS.Rds"),
                P07MG = readRDS("./output_data/mobilityzones_P03JJ.Rds"),
                P08UH = readRDS("./output_data/mobilityzones_P06SS.Rds"),
                P10JL = readRDS("./output_data/mobilityzones_P03JJ.Rds"),
                P13NB = readRDS("./output_data/mobilityzones_P06SS.Rds"))



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
  
  p <- plot_ly(mz.toplot,
               x=~variable,
               y=~zone,
               type="scatter",
               mode = "markers",
               color=~variable,
               colors=c("lightblue","orange"),
               size = ~value,
               marker = list(opacity = 0.8, sizeref=1.5, sizemode = "diameter")) %>%
    layout(xaxis=list(title=FALSE),
           showlegend=FALSE)
  
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

# survey results:
activity.results <- act.assessments %>% mutate(vehicle.days = sum(car.days,dot.days),
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
    filter(dweek %in% c(10:14)) 
  
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
  
  sensor.transport.days <- sensor.transport %>% filter(dweek %in% c(10:14)) %>%
    summarise_at(vars(Foot,Bicycle,Vehicle), mean, na.rm = TRUE)
  
  # Create a dataframe of one row matching the variables in the assessment, and append.
  current <- data.frame(
    participant = p.code,
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






























# Plot A1: transport modes days per week

sensor.transport <- tmp  %>% ungroup() %>% filter((activity=="Foot" & moving==1) |
                                  activity=="Vehicle" | 
                                  activity=="Bicycle") %>%
  dcast(dates~activity, value.var="total.time",
        fun.aggregate=sum) %>% # sum where there is total.time for both move & stay (applies to vehicle and bicycle)
  mutate_if(is.numeric,as.logical) %>%  # convert total.time to logical indicator for if mode used that day
  mutate(dweek = (as.numeric(dates-dates[1])) %/% 7) %>% group_by(dweek) %>%
  summarise_at(vars(Foot,Bicycle,Vehicle), sum, na.rm = TRUE)

sensor.transport.days <- sensor.transport %>% filter(dweek %in% c(10:14)) %>%
  summarise_at(vars(Foot,Bicycle,Vehicle), mean, na.rm = TRUE)


q.post.act <- act.assessments %>% filter(participant == "P03JJ") %>% 
  mutate(vehicle.days = sum(car.days, dot.days)) %>%
  select(foot.days, bike.days, vehicle.days)
colnames(q.post.act) <- c("Vehicle", "Bicycle", "Foot")

# # option A: stacked bar style
# transport.compare <- rbind(sensor = t(data.frame(colMeans(sensor.transport[10:14,2:4]))),
#                            survey = q.post.act)
# transport.compare  %<>% mutate(source = rownames(transport.compare))
# 
# plot_ly(transport.compare, x = ~source, y = ~Foot, type = "bar") %>%
#   add_trace(y = ~Bicycle) %>%
#   add_trace(y = ~Vehicle) %>%
#   layout(barmode = "stack")
# remove(transport.compare); 


# option B: grouped bar style
transport.compare <- rbind(sensor = t(data.frame(colMeans(sensor.transport[10:14,2:4]))),
                           survey = q.post.act) %>% t() %>% data.frame()
transport.compare  %<>% mutate(mode = rownames(transport.compare))

plot_ly(transport.compare, x = ~mode, y = ~sensor, type = "bar", name="sensor") %>%
  add_trace(y = ~survey, name="survey") %>%
  layout(barmode = "group")



# don't need per week, its an average time per day
# %>% group_by(dweek, moving) %>%
#   summarise(mpd=mean(total.time))












# Currently not in use, gets the data in another format:
# transport.compare <- merge(data.frame(colMeans(sensor.transport[10:14,2:4])),
#                            t(q.post.act),
#                            by="row.names",all.x=TRUE)
# colnames(transport.compare) <- c("Mode", "Sensor", "Survey")
# 
# transport.compare.toplot <- melt(transport.compare,
#                                  id.vars = "Mode", 
#                                  variable.name = "source", value.name = "days")


# TODO: adherence step from mz doesn't work here because they don't go out as
# much. Need to get adherence information "globally" (e.g. from screen touch)
# and apply to all tests.




# Plot A2: active hrs/day by stay/move





# Plot A3: still time hrs/day by (stay only)



