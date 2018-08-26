# screen
screen <-
  rbind.data.frame(
    readRDS("./output_data/screen_P03JJ.Rds"),
    readRDS("./output_data/screen_P06SS.Rds"),
    readRDS("./output_data/screen_P07MG.Rds"),
    readRDS("./output_data/screen_P08UH.Rds"),
    readRDS("./output_data/screen_P10JL.Rds"),
    readRDS("./output_data/screen_P13NB.Rds")
  ) %>% ungroup()

# battery
battery <-
  rbind.data.frame(
    readRDS("./output_data/battery_P03JJ.Rds"),
    readRDS("./output_data/battery_P06SS.Rds"),
    readRDS("./output_data/battery_P07MG.Rds"),
    readRDS("./output_data/battery_P08UH.Rds"),
    readRDS("./output_data/battery_P10JL.Rds"),
    readRDS("./output_data/battery_P13NB.Rds")
  ) %>% ungroup()

# trajectories
traj <-
  rbind.data.frame(
    readRDS("./output_data/traj_P03JJ.Rds"),
    readRDS("./output_data/traj_P06SS.Rds"),
    readRDS("./output_data/traj_P07MG.Rds"),
    readRDS("./output_data/traj_P08UH.Rds"),
    readRDS("./output_data/traj_P10JL.Rds"),
    readRDS("./output_data/traj_P13NB.Rds")
  ) %>% ungroup()

# action range
actionrange <-
  rbind.data.frame(
    readRDS("./output_data/homedist_P03JJ.Rds"),
    readRDS("./output_data/homedist_P06SS.Rds"),
    readRDS("./output_data/homedist_P07MG.Rds"),
    readRDS("./output_data/homedist_P08UH.Rds"),
    readRDS("./output_data/homedist_P10JL.Rds"),
    readRDS("./output_data/homedist_P13NB.Rds")
  ) %>% ungroup()

# mcp
mcp <-
  rbind.data.frame(
    readRDS("./output_data/mcp_P03JJ.Rds"),
    readRDS("./output_data/mcp_P06SS.Rds"),
    readRDS("./output_data/mcp_P07MG.Rds"),
    readRDS("./output_data/mcp_P08UH.Rds"),
    readRDS("./output_data/mcp_P10JL.Rds"),
    readRDS("./output_data/mcp_P13NB.Rds")
  ) %>% ungroup()

# activities
activities <-
  rbind.data.frame(
    readRDS("./output_data/activity_P03JJ.Rds"),
    readRDS("./output_data/activity_P06SS.Rds"),
    readRDS("./output_data/activity_P07MG.Rds"),
    readRDS("./output_data/activity_P08UH.Rds"),
    readRDS("./output_data/activity_P10JL.Rds"),
    readRDS("./output_data/activity_P13NB.Rds")
  ) %>% ungroup()


# # stepcounters # don't need this anymore, keeping only daily stepcount summaries
# stepcounters <-
#   rbind.data.frame(
#     readRDS("./output_data/stepcounters_P03JJ.Rds"),
#     readRDS("./output_data/stepcounters_P06SS.Rds"),
#     readRDS("./output_data/stepcounters_P07MG.Rds"),
#     readRDS("./output_data/stepcounters_P08UH.Rds"),
#     readRDS("./output_data/stepcounters_P10JL.Rds"),
#     readRDS("./output_data/stepcounters_P13NB.Rds")
#   ) %>% ungroup()



# Analysis 2 ----

# Daily measures:

# mobility metrics
metrics.mob <-
  rbind.data.frame(
    readRDS("./output_data/metrics_P03JJ.Rds"),
    readRDS("./output_data/metrics_P06SS.Rds"),
    readRDS("./output_data/metrics_P07MG.Rds"),
    readRDS("./output_data/metrics_P08UH.Rds"),
    readRDS("./output_data/metrics_P10JL.Rds"),
    readRDS("./output_data/metrics_P13NB.Rds")
  ) %>% ungroup()

# activity bouts summarised by day
act.pday <-
  rbind.data.frame(
    readRDS("./output_data/activity_pday_P03JJ.Rds"),
    readRDS("./output_data/activity_pday_P06SS.Rds"),
    readRDS("./output_data/activity_pday_P07MG.Rds"),
    readRDS("./output_data/activity_pday_P08UH.Rds"),
    readRDS("./output_data/activity_pday_P10JL.Rds"),
    readRDS("./output_data/activity_pday_P13NB.Rds")
  ) %>% ungroup()

# get features relating to active/sedentary behvaiour from other activity datasets:

# summarise active time/bouts based on foot & bicycle activities
active.features <- 
  act.pday %>% 
  group_by(participant, dates) %>% 
  filter(activity == "Foot" | activity == "Bicycle") %>%
  summarise(
    active.time = sum(total.time),
    active.bouts = sum(N)
  )

# summarise still periods only during most active hours of day 

# bouts starting after 6am and before 10pm
# if ends after 10pm, cutoff at 10pm

morning <- as.POSIXct("06:00:00", format="%H:%M:%S")
night <- as.POSIXct("22:00:00", format="%H:%M:%S")

still.features <-
  activities %>% filter(activity == "Still") %>%
  mutate(b.start.times = as.POSIXct(strftime(b.start, format="%H:%M:%S"), format="%H:%M:%S"),
         b.end.times = as.POSIXct(strftime(b.end, format="%H:%M:%S"), format="%H:%M:%S")) %>%
  filter(b.end.times > morning & b.start.times < night) %>%
  mutate(b.start.times = pmax(b.start.times,morning),
         b.end.times = pmin(b.end.times,night)) %>%
  mutate(duration.cut = difftime(b.end.times, b.start.times, units = "hours")) %>%
  group_by(participant, dates) %>%
  summarise(still.time = sum(duration.cut),
            still.bouts = sum(size))

# merge the active and still features to show where there is in fact zero activity on a day (as opposed to just missing data)
act.features <- merge(active.features, still.features, by = c("participant","dates"),all=T) 
act.features[is.na(act.features)] <- 0 # show that missing active time equates to no active bouts.


# transit activity summarised by day
act.trans.pday <-
  rbind.data.frame(
    readRDS("./output_data/activity_moves_pday_P03JJ.Rds"),
    readRDS("./output_data/activity_moves_pday_P06SS.Rds"),
    readRDS("./output_data/activity_moves_pday_P07MG.Rds"),
    readRDS("./output_data/activity_moves_pday_P08UH.Rds"),
    readRDS("./output_data/activity_moves_pday_P10JL.Rds"),
    readRDS("./output_data/activity_moves_pday_P13NB.Rds")
  ) %>% ungroup()

# total daily steps
stepsdaily <-
  rbind.data.frame(
    readRDS("./output_data/steptotals_P03JJ.Rds"),
    readRDS("./output_data/steptotals_P06SS.Rds"),
    readRDS("./output_data/steptotals_P07MG.Rds"),
    readRDS("./output_data/steptotals_P08UH.Rds"),
    readRDS("./output_data/steptotals_P10JL.Rds"),
    readRDS("./output_data/steptotals_P13NB.Rds")
  ) %>% ungroup() %>%
  mutate(steps = pmax(total.phone, total.watch, na.rm = TRUE))
stepsdaily %<>% mutate(device = ifelse(total.phone==steps,"phone","watch"))

# merge the steps and activity features into one set:
metrics.act <- merge(act.features, stepsdaily, by = c("participant","dates"),all=T)
metrics.act$steps[is.na(metrics.act$steps)] <- 0 # replaces the one NA in the steps data with 0

# experience sampling
es <-
  rbind.data.frame(
    readRDS("./output_data/es_P03JJ.Rds"),
    readRDS("./output_data/es_P06SS.Rds"),
    readRDS("./output_data/es_P07MG.Rds"),
    readRDS("./output_data/es_P08UH.Rds"),
    readRDS("./output_data/es_P10JL.Rds"),
    readRDS("./output_data/es_P13NB.Rds")
  ) %>% ungroup()



# Remove all datasets not required for the analyses:
remove(
  act.features,
  act.pday,
  act.trans.pday,
  active.features
  )

