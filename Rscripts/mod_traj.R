# ------------------------------------------------
# MODULE: MOBLITITY // TRAJECTORY EXTRACTION
#
# Description:
#
#
#
# ------------------------------------------------



# Get GPS log file from dataset
gps.log <- datasets.all$location %>% 
  filter(accuracy<=loc.accuracy) %>% # get rid of data points with low accuracy
  select(lat, lon, timestamp, intervals.alt, dates, times) #%>% 
#filter(timestamp>=d.start, timestamp<=(d.start %m+% days(d.study))) # get timeframe of just the day of interest

# Calculate home coordinates based on location data
home <- find_home(gps.log,"lat","lon")

# Extract trajectories: get series of stay/go events ("mobility traces") 
gps.traj <- gps.log %>% group_by(dates) %>% do(get_trajectories(.,
                                                                dT = dT,
                                                                dD = dD,
                                                                T.stay = time.threshold.stay,
                                                                T.go = time.threshold.go,
                                                                dist.threshold = dist.threshold))
remove(gps.log)

# Update home location based on all stays that are home (stay points close to current home estimation)
home <- update_home(df=gps.traj,home=home,dist.threshold = dist.threshold)

# Add "distance to home" column for all points (in trajectories dataframe)
gps.traj %<>% ungroup(gps.traj) %>%
  mutate(homedist = distGeo(home,
                            ungroup(gps.traj) %>% select(lon,lat),
                            a=6378137, f=1/298.257223563))

# add columns for action range and displacement, and summarise data by trajectory segment/event
# action range: straight line distance between home and most distal point of a journey (furthest point for moves, average for stays)
# displacements: straight line distances between consecutive stays
traj.summary <- summarise_trajectories(gps.traj=gps.traj,
                                       dist.threshold=dist.threshold)