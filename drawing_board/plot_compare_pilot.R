library(plotly)
library("readxl")
library(dplyr)
library(magrittr)
library(reshape2)

# CREATE VARIABLES ----
p.codes <- c("daisy","violet","agapantha","anthurium","nasturtium") # pilot

# GPS TRAJECTORY EXTRACTION ----
# Adapted from Luna's file "AWEAR_Logsheet_Testing.R"

# ** Load data files ----

# Sensor data results:
traj.algorithm <- rbind.data.frame(readRDS("M:/PhD_Folder/awear_bm/output_data/traj_daisy.Rds"),
                                   readRDS("M:/PhD_Folder/awear_bm/output_data/traj_violet.Rds"),
                                   #readRDS("M:/PhD_Folder/awear_bm/output_data/traj_nasturtium.Rds"), # still need Nina's logsheets updated
                                   readRDS("M:/PhD_Folder/awear_bm/output_data/traj_agapantha.Rds"),
                                   readRDS("M:/PhD_Folder/awear_bm/output_data/traj_anthurium.Rds")
                                   )
attributes(traj.algorithm$T.end) <- attributes(traj.algorithm$T.start) #otherwise different from T.start (no clue why), which causes problems when merged


# Logsheet results:
traj.logsheets <- read_excel(paste("M:/PhD_Folder/Pilot2018/AWEAR_Logsheet_Testing/logsheets_combined.xlsx",sep=""),col_names = TRUE,
                      col_types = c("text","date","date","date","text","text","text")) 
traj.logsheets %<>%
  mutate(Start = as.POSIXct(paste(traj.logsheets$Date, format(traj.logsheets$Start, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
         End = as.POSIXct(paste(traj.logsheets$Date, format(traj.logsheets$End, "%H:%M:%S")), format="%Y-%m-%d %H:%M:%S"),
         dates = as.Date(Date))
colnames(traj.logsheets)[1] <- "StayGo"


# moved from JRT testing area:

# FOR A SINGLE DATE AND PARTICIPANT:

# get data for specific participant/period
p.code <- "violet"

traj.logsheets.p <- traj.logsheets %>% filter(participant==p.code)
traj.algorithm.p <- traj.algorithm %>% filter(participant==p.code)

period <- unique(traj.algorithm.p$dates)[unique(traj.algorithm.p$dates) %in% unique(traj.logsheets.p$dates)]

plotlist <- list()

for(i in 1:length(period)){
  
  # Logsheet data: select date and manipulate dataset to get into useful format
  d <- period[i] 
  log.data <- traj.logsheets.p %>% filter(dates==d) %>%
    select(dates, StayGo, Start, End)
  
  log.data %<>%
    mutate(event=c(1:nrow(log.data))) %>% # changed from "rownames" to get numeric
    melt(id.vars=c("dates", "StayGo", "event"), value.name = "Time")
  
  # Algorithm data: select date and manipulate dataset to get into useful format
  alg.data <- traj.algorithm.p %>% filter(dates==d) %>%
    mutate(StayGo = ifelse(is.stay==1,"Stay","Go"))%>%
    select(dates, StayGo, T.start, T.end)
  
  alg.data %<>%
    mutate(event=c(1:nrow(alg.data))) %>%
    melt(id.vars=c("dates", "StayGo", "event"), value.name = "Time")
  
  # Create plot of results
  tmp <- plot_ly(data = log.data %>% arrange(event,Time),
        x=~Time,
        y=~StayGo,
        type = "scatter",
        mode = "lines",
        line = list(color = "red"),
        name = "Logsheet results") %>%
  add_trace(data = alg.data %>% arrange(event,Time),
            x=~Time,
            y=~StayGo,
            line = list(dash = "dot",color = "blue"),
            name = "Algorithm results")
  
  plotlist[[i]] <- tmp
}

subplot(plotlist, nrows = length(period))










# ACTIVITY BOUTS ----




# STEP COUNT ----



# MOBILITY METRICS ----
# Note: above plots all per participant over day (time axis), whereas metrics
# are for all participants over week (days axis)


# ** Load data files ----

# Sensor data results: (old file location CaseStudies/Data_analysis/output)
metrics.algorithm <- rbind.data.frame(readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_daisy.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_violet.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_agapantha.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_anthurium.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/nina_metrics.Rds")
                                      )
metrics.algorithm$day <- NULL # Day set in the logsheets instead

# Logsheet results:
# Imported manually (from environment panel) once-off then saved for all future use
#saveRDS(metrics_logsheets_pseudonymised,"M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_logsheets.Rds")
metrics.logsheets <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_logsheets.Rds")

# ** Create dataframe to compare results ----
# Combine into one data frame: 
metrics.combined <- merge(metrics.algorithm, metrics.logsheets,
                         by=c("dates","participant"),
                         all = TRUE) %>% 
  filter(Day>0,Day<8)
metrics.compare.alg <- metrics.combined %>% select(dates,Day,participant,N.places, Tt.out) %>% mutate(results="algorithm")
metrics.compare.log <- metrics.combined %>% select(dates,Day,participant) %>%
  mutate(N.places=metrics.combined$N.places.logs, Tt.out = metrics.combined$Tt.out.logs, results="logsheets")
metrics.compare <- rbind(metrics.compare.alg,metrics.compare.log)

# ** Plot: compare time out of home ----
plot_ly(metrics.compare, #can also use separate .log and .alg and add_trace to generate same plot
        x=~Day,
        y=~Tt.out,
        type = "scatter",
        mode = "lines+markers",
        color = ~participant,
        linetype = ~results) %>%  
  layout(yaxis = list(title="Time out of home in hours"),
         showlegend = FALSE)

# ** Plot: compare number of places visited ----
participants <- distinct(metrics.compare,participant)
N <- nrow(participants)
plot_list = vector("list",N)

for(i in 1:N){
  tmp <- metrics.compare %>% filter(participant==participants[i,])
  tmp.a <- list(
    text = paste0("P",i), #participants[i,],
    xref = "paper",
    yref = "paper",
    yanchor = "bottom",
    xanchor = "center",
    align = "center",
    x = 0.5,
    y = 0.8,
    showarrow = FALSE
  )
  sp <- plot_ly(tmp,
                x=~Day,
                y=~N.places,
                type = "bar",
                color = ~results,
                showlegend = ifelse(i==1,TRUE,FALSE))%>%
  layout(annotations = tmp.a)
  plot_list[[i]] <- sp
}

subplot(plot_list[c(1:4)], nrows = (N-1)/2, margin=0.08) # for 2 columns use ceiling(i/2)

# END metrics comparison















# << STORE FOR LATER >> ----

# Code for subplot titles (can also consider ggplot if too much of a pain for obviously static plots)
# a <- list(
#   text = "SUBPLOT TITLE A",
#   xref = "paper",
#   yref = "paper",
#   yanchor = "bottom",
#   xanchor = "center",
#   align = "center",
#   x = 0.5,
#   y = 1,
#   showarrow = FALSE
# )





