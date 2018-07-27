# trajectories
traj.algorithm <-
  rbind.data.frame(
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_daisy.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_violet.Rds"),
    #readRDS("M:/PhD_Folder/awear_bm/output_data/traj_nasturtium.Rds"), # still need Nina's logsheets updated
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_agapantha.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_anthurium.Rds")
  )
attributes(traj.algorithm$T.end) <- attributes(traj.algorithm$T.start) #otherwise different from T.start (no clue why), which causes problems when merged

# activities
act.algorithm <-
  rbind.data.frame(
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_daisy.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_violet.Rds"),
    #readRDS("M:/PhD_Folder/awear_bm/output_data/activity_nasturtium.Rds"), # still need Nina's logsheets updated
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_agapantha.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_anthurium.Rds")
  ) %>% ungroup()

# stepcount over day
stepcounters <-
  rbind.data.frame(
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_daisy.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_violet.Rds"),
    #readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_nasturtium.Rds"), # still need Nina's logsheets updated
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_agapantha.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_anthurium.Rds")
  )

# Logsheet results:
traj.logsheets <- 
  read_excel(paste("M:/PhD_Folder/Pilot2018/AWEAR_Logsheet_Testing/logsheets_combined.xlsx",sep=""),col_names = TRUE,
             col_types = c("text","date","date","date","text","text","text")) 
traj.logsheets %<>%
  mutate(Start = as.POSIXct(paste(traj.logsheets$Date, format(traj.logsheets$Start, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
         End = as.POSIXct(paste(traj.logsheets$Date, format(traj.logsheets$End, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
         dates = as.Date(Date))
colnames(traj.logsheets)[1] <- "StayGo"


# Getting data for a specific date from a single participipants data:

pilot_data_bydate <- function(traj.logsheets.p, traj.algorithm.p, act.algorithm.p, d){
  
  # Logsheet data: select date and manipulate dataset to get into useful format
  log.data <- traj.logsheets.p %>% filter(dates==d) %>%
    select(dates, StayGo, Start, End, Mode)
  log.data %<>%
    mutate(event=c(1:nrow(log.data))) %>% # changed from "rownames" to get numeric
    melt(id.vars=c("dates", "StayGo", "event", "Mode"), value.name = "Time")
  log.data[log.data$variable=="End", "Mode"] <- NA #only keep mode information at start to avoid duplicate text in plots
  
  # Algorithm data: select date and manipulate dataset to get into useful format
  alg.data <- traj.algorithm.p %>% filter(dates==d) %>%
    mutate(StayGo = ifelse(is.stay==1,"Stay","Go"))%>%
    select(dates, StayGo, T.start, T.end)
  alg.data %<>%
    mutate(event=c(1:nrow(alg.data))) %>%
    melt(id.vars=c("dates", "StayGo", "event"), value.name = "Time")
  
  # Activity ----
  act.data <- act.algorithm.p %>% filter(dates==d) %>%
    melt(id.vars = c("dates","bout","activity"), 
         measure.vars = c("b.start","b.end")) %>% 
    group_by(activity, bout) %>%
    mutate(StayGo = "Activity") #for same plot option below
  
  
  return(list(log.data=log.data, alg.data=alg.data, act.data=act.data))
}


