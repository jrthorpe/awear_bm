# ------------------------------------------------
# MODULE: MOBLITITY // TRAJECTORY EXTRACTION
#
# This module detects "stay" and "move" events from raw location data. The
# script below calls custom functions from jrt_mobility for detecting a home
# location, detecting stay events, inferring location ID's and cleaning the
# results.
#
# ------------------------------------------------

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
traj.summary <- summarise_trajectories(gps.traj=gps.traj,
                                       dist.threshold=dist.threshold)
