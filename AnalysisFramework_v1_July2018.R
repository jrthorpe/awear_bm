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

# Select participant
P <- participants$anthurium
list2env(P, .GlobalEnv); remove(P)

# Define constants:
folder <- "M:/PhD_Folder/CaseStudies/Data_dumps/dump_current_analysis/" # path to folder that holds multiple .csv files, downloaded from nightingale webportal
not.in.use <- c("battery", 
                "screen",
                "experience_sampling",
                #"activity",
                #"step_count",
                #"location",
                "bluetooth","hardware_info","wifi","wearable","calllog","sms") #last row items never used
# to_plot <- c("activity", # from data checker, for debugging purposes only (visualise all datasets)
#              "battery",
#              "exps",
#              "location",
#              "screen",
#              "steps")
d.study <- as.numeric(round(difftime(d.stop,d.start,units="days")))

# IMPORT AND RESTRUCTURE DATA ------------------------

datasets.all <- get.data(folder, not.in.use) %>% 
  lapply(restructure, userid, d.start, d.stop)
datasets.all <- Filter(function(x) !is.null(x)[1],datasets.all) # remove any null dataframes

remove(folder, not.in.use, userid)

# MOBILITY  ==========================================

#** Moblity Setup: create datasets and variables required for mobility calculations -------

# define variables:
loc.accuracy <- 25 # threshold for accuracy of location points in meters
dT <- 5  # delta T, time window in minutes
dD <- 100 # delta D, diagonal distance boundary in meters
time.threshold.stay <- 10 # minimum duration of a stay, in minutes
time.threshold.go <- 5 # cut-off for filtering out "go" events to/from same location, in minutes
dist.threshold <- 30 # distance in meters within which two centriods belong to same stay location
Qd <- .99 # quantile of distances to include in the MC polygon
#doi <- as.POSIXct("2018-01-30") # day of interest (use when not d.start)

# GPS log file
gps.log <- datasets.all$location %>% 
  filter(accuracy<=loc.accuracy) %>% # get rid of data points with low accuracy
  select(lat, lon, timestamp, intervals.alt, dates, times) %>% 
  filter(timestamp>=d.start, timestamp<=(d.start %m+% days(d.study))) # get timeframe of just the day of interest

# calculate home coordinates based on location data
home <- find_home(gps.log,"lat","lon")

#** Extract trajectories: get series of stay/go events ("mobility traces") ====

# For mulitday sets:
gps.traj <- gps.log %>% group_by(dates) %>% do(get_trajectories(.,
                                                             dT = dT,
                                                             dD = dD,
                                                             T.stay = time.threshold.stay,
                                                             T.go = time.threshold.go,                                                             dist.threshold = dist.threshold))

# ** Prepare inputs -----

# update home location based on all stays that are home (stay points close to current home estimation)
home.updated <- update_home(df=gps.traj,home=home,dist.threshold = dist.threshold)
 
# add "distance to home" column for all points (in trajectories dataframe)
gps.traj %<>% ungroup(gps.traj) %>%
  mutate(homedist = distGeo(home.updated,
                            ungroup(gps.traj) %>% select(lon,lat),
                            a=6378137, f=1/298.257223563))

# add columns for action range and displacement, and summarise data by trajectory segment/event
# action range: straight line distance between home and most distal point of a journey (furthest point for moves, average for stays)
# displacements: straight line distances between consecutive stays
traj.summary <- summarise_trajectories(gps.traj=gps.traj,
                                       dist.threshold=dist.threshold)

# ** Calculate all metrics by day -----

metrics.results <- get_metrics(traj.summary)

# minimum convex polygon(reference: http://mgritts.github.io/2016/04/02/homerange-mcp/)
mcp.areas <- gps.traj  %>% group_by(dates) %>% 
  summarise(mcp.area=get_mcp_area(lon=lon,lat=lat,Qd=Qd))                                  
metrics.results %<>% mutate(mcp.area = mcp.areas$mcp.area) # TODO: create the possibility to get out the coords and plot for a visual check and for the paper (or other demo).

# ** Save Results ------

# # Saving output for plots etc:
# nina.metrics <- metrics.results %>% select(dates, Tt.out, N.places) %>%
#   mutate(day=c(1:nrow(metrics.results)), participant = "nina")
# saveRDS(nina.metrics,"M:/PhD_Folder/CaseStudies/Data_analysis/output/nina_metrics.Rds")

# saveRDS(metrics.results,"M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_p03jj.Rds")

# END ---

# STEP COUNT ===========================

#define variables
win.size <- 15 # window size in minutes for...

# step count log file
steps.log <- datasets.all$step_count %>% 
  filter(timestamp>=d.start, timestamp<=(d.start %m+% days(d.study))) %>% # get timeframe of just the day of interest
  select(step_count, dsource, timestamp, intervals.alt, dates, times)

# for debugging/investigation
# steps_vis(steps.log)
# steps.log %>% count(dsource) 

#** Daily total step counts -------
steps.watch <- daily_steps(steps.log,"watch")
steps.phone <- daily_steps(steps.log,"phone")

# daily totals #TODO:fix the merge
steps.totals <- merge(x=steps.watch %>% summarise(total = max(stepcounter)), 
                      y=steps.phone %>% summarise(total = max(stepcounter)),
                      by="dates", suffixes=c(".watch",".phone"),
                      incomparables = NA)

#** Extraction walking bouts from step counts -------

patterns <- pattern_steps(steps.watch %>% ungroup(), win.size = win.size)

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

# END


# ACTIVITY ===========================

#** Extraction of activity bouts -------
acts <- c("Still", "Foot", "Vehicle", "Bicycle")
activity.log <- datasets.all$activity %>% filter(label %in% acts)

# To look at only "active" time of day
# activity.log.day <- datasets.all$activity %>% filter(label %in% acts) %>%
#   filter(times > as.POSIXct(x="06:00:00",format="%H:%M:%S",tz="CET"),
#          times < as.POSIXct(x="22:00:00",format="%H:%M:%S",tz="CET"))

activity.bouts <- activity_bouts(activity.log, acts)

daily.summary <- activity.bouts %>% group_by(dates, activity) %>% 
  summarise(total.time =  sum(duration), N = n()) %>%
  mutate(mean.duration = total.time/N)

# used to have as percentage of recording time here (see commits 18 July 2018), but wasn't that meaningful due to overlapping bouts etc.

#** Metrics to relate to questionnaire -------

# Create one dataset out of stay/move events and activity bouts
# plot stay/move as solid/dotted lines and overlay bike/foot/vehicle chart



# Activity in daily life: stepcount as proxy
# Activity in leisure: currently not possible, but with location detection or other more personalised approaches yes
# Still time: get, but note in discussion the obvious limitations
# Transport: get using above


# RELATION TO QUESTIONNAIRES ----

# mobility boundary crossings based on the mobility baseline questionnaire:
# counts the number of times the person leaves a certain radius around their home
# levels: daily, 4-6 per week, 1-3 per week, less than 1
# TODO: fix this so it is not like every small trip at work is another additional trip out of town. -- done, only need in form TRUE/FALSE per day
N.boundary2 <- traj.summary %>% tally(action.range>20 & action.range<=50) # out into garden or postbox etc, not really detectable
N.boundary3 <- traj.summary %>% tally(action.range>50 & action.range<=1000) # neighborhood
N.boundary4 <- traj.summary %>% tally(action.range>1000 & action.range<=10000) # within town
N.boundary5 <- traj.summary %>% tally(action.range>10000) # out of town

mb.results %<>% mutate(mb50 = N.boundary2$n>0,
                            mb1km = N.boundary3$n>0,
                            #mb5km = N.boundary4a$n>0,
                            mb10km = N.boundary4$n>0,
                            mb.oot = N.boundary5$n>0)










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
doi <- as.POSIXct("2018-06-12") # day of interest YYYY-MM-DD
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