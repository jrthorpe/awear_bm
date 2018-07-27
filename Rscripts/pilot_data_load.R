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