# trajectories
traj.algorithm <-
  rbind.data.frame(
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_daisy.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_violet.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_nasturtium.Rds"), # still need Nina's logsheets updated
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_agapantha.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/traj_anthurium.Rds")
  )
attributes(traj.algorithm$T.end) <- attributes(traj.algorithm$T.start) #otherwise different from T.start (no clue why), which causes problems when merged

# activities
act.algorithm <-
  rbind.data.frame(
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_daisy.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_violet.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_nasturtium.Rds"), # still need Nina's logsheets updated
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_agapantha.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/activity_anthurium.Rds")
  ) %>% ungroup()

# stepcount over day
stepcounters <-
  rbind.data.frame(
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_daisy.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_violet.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_nasturtium.Rds"), # still need Nina's logsheets updated
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_agapantha.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/stepcounters_anthurium.Rds")
  )

# stepcount daily totals  
steptotals <-
  rbind.data.frame(
    readRDS("M:/PhD_Folder/awear_bm/output_data/steptotals_daisy.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/steptotals_violet.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/steptotals_nasturtium.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/steptotals_agapantha.Rds"),
    readRDS("M:/PhD_Folder/awear_bm/output_data/steptotals_anthurium.Rds")
  ) %>% 
  filter(!is.na(total.watch) & !is.na(total.phone) & total.watch>0 & total.phone>0) %>% 
  mutate(id = 1:n())

# metrics results: (old file location CaseStudies/Data_analysis/output)
metrics.algorithm <- rbind.data.frame(readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_daisy.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_violet.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_agapantha.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_anthurium.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/nina_metrics.Rds")
)
metrics.algorithm$day <- NULL # Day set in the logsheets instead



# Logsheet results:
traj.logsheets <- 
  read_excel(paste("M:/PhD_Folder/Pilot2018/AWEAR_Logsheet_Testing/logsheets_combined.xlsx",sep=""),col_names = TRUE,
             col_types = c("text","date","date","date","text","text","text")) 
traj.logsheets %<>%
  mutate(Start = as.POSIXct(paste(traj.logsheets$Date, format(traj.logsheets$Start, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
         End = as.POSIXct(paste(traj.logsheets$Date, format(traj.logsheets$End, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
         dates = as.Date(Date))
colnames(traj.logsheets)[1] <- "StayGo"


# Imported manually (from environment panel) once-off then saved for all future use
#saveRDS(metrics_logsheets_pseudonymised,"M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_logsheets.Rds")
# metrics.logsheets.NOI <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_logsheets.Rds")

metrics.logsheets <- 
  read_excel(paste("M:/PhD_Folder/Pilot2018/metrics_logsheets_pseudonymised.xlsx",sep=""),col_names = TRUE,
             col_types = c("text","numeric","date","numeric","numeric","text")) 

# Combine metrics results from logsheets and algorithm for comparison:
metrics.combined <- merge(metrics.algorithm, metrics.logsheets,
                          by=c("dates","participant"),
                          all = TRUE) %>% 
  filter(Day>0,Day<8)
metrics.compare.alg <- metrics.combined %>% select(dates,Day,participant,N.places, Tt.out) %>% mutate(results="algorithm")
metrics.compare.log <- metrics.combined %>% select(dates,Day,participant) %>%
  mutate(N.places=metrics.combined$N.places.logs, Tt.out = metrics.combined$Tt.out.logs, results="logsheets")
metrics.compare <- rbind(metrics.compare.alg,metrics.compare.log)






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


