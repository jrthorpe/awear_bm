# RELATION TO QUESTIONNAIRES ----

# !!! NOT IN USE !!! 
# Was moved to "assessment_vs_sensor" and consequently worked into main Rmd analysis script

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