# ** Load questionnaire and sensor results ----

# Note: for questionnaires -- first time imported from excel, edited and saved, thereafter loaded from "output" folder.

# / Mobility
# mb.assessments <- mobility_assessment %>% filter(assessment == "post") %>% select(-assessment)
# remove(mobility_assessment)
# saveRDS(mb.assessments,"./output_data/mobility_assessments_post.Rds")

mz.assessments <- readRDS("./output_data/mobility_assessments_post.Rds")  # from clinical assessments
mz.list <- list(P03JJ = readRDS("./output_data/mobilityzones_P03JJ.Rds"), # from algorithm results
                P06SS = readRDS("./output_data/mobilityzones_P06SS.Rds"),
                P07MG = readRDS("./output_data/mobilityzones_P07MG.Rds"),
                P08UH = readRDS("./output_data/mobilityzones_P08UH.Rds"),
                P10JL = readRDS("./output_data/mobilityzones_P10JL.Rds"),
                P13NB = readRDS("./output_data/mobilityzones_P13NB.Rds"))


# / Activity
# act.assessments <- activity.assessments %>% filter(assessment == "post") %>% select(-assessment)
# remove(activity.assessments)
# saveRDS(act.assessments,"./output_data/activity_assessments_post.Rds")
act.assessments <- readRDS("./output_data/activity_assessments_post.Rds")  # from clinical assessments
act.list <- list(P03JJ = readRDS("./output_data/activity_moves_pday_P03JJ.Rds"), # from algorithm results
                 P06SS = readRDS("./output_data/activity_moves_pday_P06SS.Rds"),
                 P07MG = readRDS("./output_data/activity_moves_pday_P07MG.Rds"),
                 P08UH = readRDS("./output_data/activity_moves_pday_P08UH.Rds"),
                 P10JL = readRDS("./output_data/activity_moves_pday_P10JL.Rds"),
                 P13NB = readRDS("./output_data/activity_moves_pday_P13NB.Rds"))

steps.list <- list(P03JJ = readRDS("./output_data/steps_P03JJ.Rds"), # from algorithm results
                   P06SS = readRDS("./output_data/steps_P06SS.Rds"),
                   P07MG = readRDS("./output_data/steps_P07MG.Rds"),
                   P08UH = readRDS("./output_data/steps_P08UH.Rds"),
                   P10JL = readRDS("./output_data/steps_P10JL.Rds"),
                   P13NB = readRDS("./output_data/steps_P13NB.Rds"))


# Get activity data into a usable format:

# TODO: adherence step from mz doesn't work here because they don't go out as
# much. Need to get adherence information "globally" (e.g. from screen touch)
# and apply to all tests.

# TODO: might need to just add the leisure activities?! And then compare
# directly all active time froms sensors with all active time from assessments
# without distinguishing

# survey results:
activity.results <- act.assessments %>% mutate(vehicle = car.days+dot.days,
                                               source = "survey") %>%
  select(participant,
         active.work.days,
         active.work.mpd,
         active.sport.days,
         active.sport.mpd,
         still.mpd,
         vehicle,
         bike.days,
         foot.days,
         bike.mpd,
         foot.mpd,
         source)
colnames(activity.results)[8:9] <- c("bike","foot") # for better plot ticklabels
remove(act.assessments)

# sensor results:
# create and append to survey results in same format
for(p in p.codes){
  
  #p.code <- "P03JJ" #p.codes[i] # get participant code
  tmp <- act.list[[p]]
  
  # get all active time (Doesn't include activity for transport, therefore cycling left out)
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
  
  # sensor.still ...incomplete
  sensor.still <- tmp  %>% ungroup() %>% filter((activity=="Still" & moving==0)) %>%
    mutate(dweek = (as.numeric(dates-dates[1])) %/% 7) %>%
    filter(dweek %in% c((max(dweek)-5):(max(dweek)-1))) %>%
    summarise(mpd=mean(total.time))
  
  # get all bouts corresponding to transport
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
    still.mpd = sensor.still$mpd,
    vehicle = sensor.transport.days$Vehicle,
    bike = sensor.transport.days$Bicycle,
    foot = sensor.transport.days$Foot,
    bike.mpd = NA,
    foot.mpd = sensor.active.mpd %>% filter(moving==1) %>% select(mpd) %>% as.numeric(),
    source = "sensor")
  
  activity.results <- rbind(activity.results, current)
  
}
remove(current, 
       sensor.active, sensor.active.days, sensor.active.mpd,
       sensor.still,
       sensor.transport, sensor.transport.days)



