

library(plotly)

#load the rds files
metrics.algorithm <- rbind.data.frame(readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/agzam_metrics.Rds"),
                                readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/anja_metrics.Rds"),
                                readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/dean_metrics.Rds"),
                                readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/nina_metrics.Rds"),
                                readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/verena_metrics.Rds"))
metrics.algorithm$day <- NULL
#saveRDS(metrics_logsheets,"M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_logsheets.Rds")
metrics.logsheets <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_logsheets.Rds")

metrics.compare <- merge(metrics.algorithm,metrics.logsheets,by=c("dates","participant")) %>% filter(Day>0,Day<8)
write.csv(metrics.compare,"M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_compare.csv")

metrics.compare.alg <- metrics.compare %>% select(dates,Day,participant,N.places, Tt.out) %>% mutate(results="algorithm")
metrics.compare.log <- metrics.compare %>% select(dates,Day,participant) %>%
  mutate(N.places=metrics.compare$N.places.logs, Tt.out = metrics.compare$Tt.out.logs, results="logsheets")

metrics.compare.alt <- rbind(metrics.compare.alg,metrics.compare.log)

plot_ly(metrics.compare,
        x=~Day,
        y=~Tt.out,
        type = "scatter",
        mode = "lines+markers",
        color = ~participant,
        name = "algorithm")%>%
  add_trace(y=~Tt.out.logs,
            color = ~participant,
            line = list(dash = 'dash'),
            name = "logsheets") %>%
  layout(yaxis = list(title="Time out of home in hours"),
         showlegend = FALSE)

plot_ly(metrics.compare.alt,
        x=~Day,
        y=~Tt.out,
        type = "scatter",
        mode = "lines+markers",
        color = ~participant,
        linetype = ~results) %>%
  layout(yaxis = list(title="Time out of home in hours"),
         showlegend = FALSE)

participants <- distinct(metrics.compare.alt,participant)
N <- nrow(participants)
plot_list = vector("list",N)

for(i in 1:N){
  tmp <- metrics.compare.alt %>% filter(participant==participants[i,])
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





