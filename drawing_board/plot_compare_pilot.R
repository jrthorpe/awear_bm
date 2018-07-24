library(plotly)

# GPS TRAJECTORY EXTRACTION ----



# MOBILITY METRICS ----

# ** Load data files: ----

# Sensor data results (old file location CaseStudies/Data_analysis/output)
metrics.algorithm <- rbind.data.frame(readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_daisy.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_violet.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_agapantha.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/metrics_anthurium.Rds"),
                                      readRDS("M:/PhD_Folder/awear_bm/output_data/nina_metrics.Rds"))
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


# ACTIVITY BOUTS ----




# STEP COUNT ----














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





