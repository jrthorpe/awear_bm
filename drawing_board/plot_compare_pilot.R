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
source("./Rscripts/pilot_data_load.R")


# moved from JRT testing area:

# FOR A SINGLE DATE AND PARTICIPANT:

# get data for specific participant/period
p.code <- "agapantha"

traj.logsheets.p <- traj.logsheets %>% filter(participant==p.code)
traj.algorithm.p <- traj.algorithm %>% filter(participant==p.code)
act.algorithm.p <- act.algorithm %>% filter(participant==p.code)
stepcounters.p <- stepcounters %>% filter(participant==p.code)

period <- unique(traj.algorithm.p$dates)[unique(traj.algorithm.p$dates) %in% unique(traj.logsheets.p$dates)]

plotlist <- list()

for(i in 1:length(period)){
  #i<-1
  
  d <- period[i]
  
  data.d <- pilot_data_bydate(traj.logsheets.p, traj.algorithm.p, act.algorithm.p, d)
  list2env(data.d, .GlobalEnv); remove(data.d)
  
  # Create plot of results
  tmp.traj <- plot_ly(data = log.data %>% arrange(event,Time),
        x=~Time, y=~StayGo,
        type = "scatter", mode = "lines", line = list(color = "red"),
        name = "Logsheet results") %>%
  add_trace(data = alg.data %>% arrange(event,Time),
            x=~Time, y=~StayGo,
            line = list(dash = "dot",color = "blue"),
            name = "Algorithm results") 
 
  tmp.act <- plot_ly(data = act.data %>% filter(activity == "Still"),
          x = ~value, y = ~dates,
          type = "scatter", mode = "lines", line = list(color = "grey", width = 2),
          name = "Still") %>%
    add_trace(data = act.data %>% filter(activity == "Foot"),
              x = ~value, y = ~dates,
              type ="scatter", mode="lines",
              opacity=0.5, line=list(color="red", width=20),
              name = "On Foot") %>%
    add_trace(data = act.data %>% filter(activity=="Bicycle"),
              x = ~value, y = ~dates,
              type = "scatter", mode = "lines",
              opacity = 0.5, line = list(color = "blue", width = 20),
              name = "Bicycle") %>%
    add_trace(data = act.data %>% filter(activity == "Vehicle"),
              x = ~value, y = ~dates,
              type = "scatter", mode = "lines",
              opacity = 0.5, line = list(color = "green", width = 20),
              name = "Vehicle")
  
  tmp.steps <- plot_ly(stepcounters.p %>% filter(dates==d) %>% group_by(source),
          x = ~timestamp, y = ~stepcounter,
          type = "scatter", mode = "lines+markers",
          color = ~source)
  
  
  plotlist[[i]] <- subplot(tmp.traj, tmp.act, tmp.steps, nrows=3, shareX = TRUE)
  
}

subplot(plotlist, nrows = length(period))

plotlist[[1]]
plotlist[[2]]
plotlist[[3]]
plotlist[[4]]
plotlist[[5]]
plotlist[[6]]
plotlist[[7]]
plotlist[[8]]



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

subplot(plot_list, nrows = (N-1)/2, margin=0.08) # for 2 columns use ceiling(i/2)

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





# # Plot both on same axes instead of subplot:
# # Create plot of results
# #tmp.combo <- 
#   plot_ly(data = log.data %>% arrange(event,Time),
#                     x=~Time,
#                     y=~StayGo,
#                     type = "scatter",
#                     mode = "lines",
#                     line = list(color = "red"),
#                     name = "Logsheet results") %>%
#   add_trace(data = alg.data %>% arrange(event,Time),
#             x=~Time,
#             y=~StayGo,
#             line = list(dash = "dot",color = "blue"),
#             name = "Algorithm results") %>%
#   add_trace(data = act.data %>% filter(activity == "Still"),
#             x = ~value, #NOTE: this does not display the time properly, irregular intervals are spread regularly
#             #y = ~dates,
#             type = "scatter",
#             mode = "lines",
#             line = list(color = "grey", width = 2),
#             name = "Still") %>%
#   add_trace(data = act.data %>% filter(activity == "Foot"),
#             x = ~value,
#             #y = ~dates,
#             type = "scatter",
#             mode = "lines",
#             opacity = 0.5,
#             line = list(color = "red", width = 20),
#             name = "On Foot") %>%
#   add_trace(data = act.data %>% filter(activity == "Bicycle"),
#             x = ~value,
#             #y = ~dates,
#             type = "scatter",
#             mode = "lines",
#             opacity = 0.5,
#             line = list(color = "blue", width = 20),
#             name = "Bicycle") %>%
#   add_trace(data = act.data %>% filter(activity == "Vehicle"),
#             x = ~value,
#             #y = ~dates,
#             type = "scatter",
#             mode = "lines",
#             opacity = 0.5,
#             line = list(color = "green", width = 20),
#             name = "Vehicle")
# 
# #plotlist[[i]] <- tmp.combo





