# AWEAR: BEHAVIOURAL ANALYSIS FRAMEWORK
#
#********************************************************************************************
# Author: Julia Thorpe
# Written for AWEAR Case Studies, January-June 2018, as part of an ongoing PhD
# project on Engineering Systems Design in Healthcare at DTU in collaboration
# with Rigshospitalet-Glostrup

# This script ...

# SETUP -------------------------------------------------------------------------------------------

# Load packages and functions.
library(data.table)
library(dtplyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(maptools)
library(plotly)
library(maps)
library(mapproj)
library(sp)
library(caTools)
library(geosphere) # added in v3 of data checker
library(mapview) # added in v1 of analysis framework
library(tidyr) # added for function "complete" in assessments section

# For DBSCAN clustering
library(fpc)
library(dbscan)
library(factoextra)

# Load custom functions:
source("./Rscripts/jrt_utils.R")
source("./Rscripts/jrt_mobility.R")
source("./Rscripts/jrt_steps.R")
source("./Rscripts/jrt_activity.R")
source("./Rscripts/users.R")

# SETTINGS ==========================================

# Select participant and load data
p <- "P03JJ" # single participant
#p.codes <- c("P03JJ","P06SS","P07MG","P08UH","P10JL","P13NB") # case studies
#p.codes <- c("daisy","violet","agapantha","anthurium","nasturtium") # pilot
#for(p in p.codes){
  P <- participants[[p]]
  list2env(P, .GlobalEnv); remove(P)
  d.study <- as.numeric(round(difftime(d.stop,d.start,units="days")))
  datasets.all <- readRDS(paste0("M:/PhD_Folder/awear_bm/output_data/datasets_",p,".Rds"))
#}   
# MOBILITY  ==========================================

# ~  Define variables ----
loc.accuracy <- 25 # threshold for accuracy of location points in meters
dT <- 5  # delta T, time window in minutes
dD <- 100 # delta D, diagonal distance boundary in meters
time.threshold.stay <- 10 # minimum duration of a stay, in minutes
time.threshold.go <- 5 # cut-off for filtering out "go" events to/from same location, in minutes
dist.threshold <- 50 # distance in meters within which two centriods belong to same stay location
Qd <- .99 # quantile of distances to include in the MC polygon
#doi <- as.POSIXct("2018-01-30") # day of interest (use when not d.start)

# ~  Extract trajectories and metrics ----
# Run trajectory extraction module on location dataset
# inputs: datasets.all$location
# outputs: home, gps.traj, traj.summary
source("./Rscripts/mod_traj.R")
mob.metrics <- get_metrics(traj.summary, gps.traj)

# ~  Save results ----
# p.traj <- traj.summary %>% select(dates, traj.event, T.start, T.end, is.stay, loc.id, durations) %>%
#   mutate(participant = p)
# saveRDS(p.traj,paste0("M:/PhD_Folder/awear_bm/output_data/traj_",p,".Rds"))

# p.metrics <- mob.metrics %>% select(dates, Tt.out, N.places) %>% 
#   filter(dates < as.POSIXct("2018-06-24") | dates > as.POSIXct("2018-07-06")) %>% # for Nina only
#   mutate(day=c(1:nrow(mob.metrics)), participant = p)
# saveRDS(p.metrics,paste0("M:/PhD_Folder/awear_bm/output_data/metrics_",p,".Rds"))

# ~  Clear environment ----
remove(loc.accuracy, dT, dD, time.threshold.stay, time.threshold.go, dist.threshold, Qd)

# STEP COUNT ===========================

# ~  Define variables ----
win.size <- 15 # window size in minutes for...

# ~  Calculate step counts and bouts ----
source("./Rscripts/mod_steps.R")
steps.win <- pattern_steps(steps.watch %>% ungroup(), win.size = win.size) # counts steps by time windows over the day

# ~  Save steps results ----

# # save a set with phone and watch counts over day
# stepcounters <- rbind(steps.watch %>% ungroup() %>% select(dates, timestamp, stepcounter) %>% mutate(source="watch"),
#                       steps.phone %>% ungroup() %>% select(dates, timestamp,stepcounter) %>% mutate(source="phone")) %>%
#   mutate(participant = p)
# saveRDS(stepcounters,paste0("M:/PhD_Folder/awear_bm/output_data/steps_",p,".Rds"))

# saveRDS(steps.totals,paste0("M:/PhD_Folder/awear_bm/output_data/steps_",p,".Rds"))

# ~  Clear environment ----
remove(win.size, steps.log, steps.phone, steps.watch)


# ACTIVITY ===========================

#** Extraction of activity bouts -------
acts <- c("Still", "Foot", "Vehicle", "Bicycle")
activity.log <- datasets.all$activity %>% filter(label %in% acts)

# To look at only "active" time of day
# activity.log.day <- datasets.all$activity %>% filter(label %in% acts) %>%
#   filter(times > as.POSIXct(x="06:00:00",format="%H:%M:%S",tz="CET"),
#          times < as.POSIXct(x="22:00:00",format="%H:%M:%S",tz="CET"))

activity.bouts <- activity_bouts(activity.log, acts); remove(activity.log)
#saveRDS(activity.bouts %>% mutate(participant=p),paste0("M:/PhD_Folder/awear_bm/output_data/activity_",p,".Rds"))


activity.bouts.pday <- activity.bouts %>% group_by(dates, activity) %>% 
  summarise(total.time =  sum(duration), N = n()) %>%
  mutate(mean.duration = total.time/N)

# used to have as percentage of recording time here (see commits 18 July 2018), but wasn't that meaningful due to overlapping bouts etc.

remove(acts)
# END

# RELATION TO QUESTIONNAIRES ----


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


# 
# # Sections below currently not in use: keeping until the comparison with questionnaires is complete in separate script.
# 
# # for bicycle and vehicle stay/move information not necessary:
# transport.modes <- daily.summary %>% ungroup() %>% 
#   mutate(dweek = (as.numeric(dates-dates[1])) %/% 7) %>% 
#   group_by(dweek, activity) %>% 
#   summarise(days.per.week = sum(N>0),
#             time.per.day = mean(total.time))
# 
# # for "Foot" and "Still" bouts, need to filter any during moves:
# 
# # get a summary of "Foot" events that overlap with moves
# transport.foot <- activity.bouts %>% 
#   filter(activity=="Foot", moving==1)%>% 
#   group_by(dates) %>% 
#   summarise(total.time=sum(duration))
# 
# transport.foot.weekly <- transport.foot %>% ungroup() %>% 
#   mutate(dweek = (as.numeric(dates-dates[1])) %/% 7) %>% 
#   group_by(dweek) %>% 
#   summarise(days.per.week = n(),
#             time.per.day = mean(total.time))
# 
# # for interest, may be useful in future:
# activity.vs.moves <- activity.bouts %>% 
#   group_by(dates,activity,moving) %>% 
#   summarise(tt=sum(duration))

# ** Still bouts ----
# get from transport modes
# TODO: need to exclude sleep time somehow. Options could be to take from period
# between first and last step or screen touch.



# DEBUGGING AREA ----


# Looking at other variables where stepcount drops
steps.watch$step_count[steps.watch$dstep<0]
steps.phone$step_count[steps.phone$dstep<0]



Metrics_from_Logsheets %<>% mutate(dates=as.Date(dates))
test<-merge(metrics.results,Metrics_from_Logsheets,by="dates")
plot_ly(test,
        x=~dates,
        y=~N.places,
        type = "bar") %>%
  add_trace(y=~Places)

plot_ly(test[2:8,],
        x=~dates,
        y=~Tt.out,
        type = "scatter",
        mode = "lines+markers") %>%
  add_trace(y=~TimeOH)

# For a single day:
# append trajectory information to the gps log file, including if the points is
# a stay/go and location ID (note: all "go" points are assigned location ID of 0)
doi <- as.POSIXct("2018-07-08") # day of interest YYYY-MM-DD
singleday <- gps.log %>% filter(timestamp>=doi, timestamp<=(doi %m+% days(1)))
gps.traj.day <- get_trajectories(df=singleday,
                            dT = dT,
                            dD = dD,
                            T.stay = time.threshold.stay,
                            T.go = time.threshold.go,
                            dist.threshold = dist.threshold)

# summarise all trajectory events
traj.summary.day <- gps.traj.day %>% group_by(traj.event) %>%
  summarize(T.start = min(timestamp), is.stay = median(is.stay), loc.id = mean(loc.id)) %>%
  mutate(T.end = c(T.start[-1],max(gps.traj.day$timestamp))) %>%
  mutate(durations = difftime(T.end,T.start, units = "mins"))


# End of DEBUGGING AREA ---