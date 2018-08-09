## Evaluation -- Assessments
# Chunk taken from feature_extraction_main.Rmd after decision to keep case studies part out of it.

# Load case clinical assessment data (also equivalent from sensors)
p.codes <- c("P03JJ","P06SS","P07MG","P08UH","P10JL","P13NB") # case studies
source("./Rscripts/cassess_data_load.R")


# MOBILITY ----

# Create plot to compare results
mz.plot_list = vector("list",length(mz.list))
for(i in 1:length(mz.list)){
  
  #i<-1 #debug
  p.code <- p.codes[i] # get participant code
  mz.dat <- mz.list[[p.code]] # use participant code to ensure matching data
  mz.weekly <- mz.dat %>% group_by(dweek) %>% summarise_at(vars(mz2:mz5), sum, na.rm = TRUE) %>% 
    mutate(adherence = count(mz.dat,dweek)$n) %>% 
    filter(adherence==7) %>% 
    select(-adherence)
  
  q.post <- mz.assessments %>% filter(participant == p.code)
  mz.compare <- data.frame(sensor = colMeans(tail(mz.weekly,4)[,2:5]),
                           survey = as.numeric(q.post[,3:6])) 
  mz.compare %<>% mutate(zone=rownames(mz.compare))
  
  anno_subtitle$text <- paste0(p.code)
  p <- plot_ly(melt(mz.compare,id.vars = "zone"), 
               x = ~zone, 
               y = ~value,
               color = ~variable,
               colors = colours.compare[1:2],
               type = "bar", 
               showlegend=ifelse(p.code=="P13NB",TRUE,FALSE)) %>%
    layout(yaxis = list(title = "Days per week"), 
           xaxis = list(title = "Mobility zones"),
           annotations = anno_subtitle) #,margin = list(l = 0, r = 0, b = 100, t = 100, pad = 0)
  
  mz.plot_list[[i]] <- p
}

subplot(mz.plot_list, nrows = 2, margin=c(0.01,0.01,0.05,0.05), shareY = TRUE) # margin=0.08, widths = c(0.2,0.2,0.2),
remove(mz.dat,mz.weekly,mz.assessments,mz.compare,q.post)


# ACTIVITY ----

td.plot_list = list()
active.plot_list = list()
still.plot_list = list()

for(p in p.codes){
  
  anno_subtitle$text <- p
  
  # Plot A1: transport modes days per week
  transport.days <- activity.results %>% filter(participant==p) %>% 
    select(vehicle, bike, foot, source) %>%
    melt(id.vars="source", variable.name = "mode")
  
  td.plot <- plot_ly(transport.days %>% group_by(source), 
                     x = ~mode, 
                     y = ~value,
                     color = ~source,
                     colors = colours.compare[1:2],
                     type = "bar",
                     legendgroup = ~source, 
                     showlegend=ifelse(p=="P03JJ",TRUE,FALSE)) %>%
    layout(yaxis = list(title = "Days per week"), 
           xaxis = list(title = "Transport modes"),
           annotations = anno_subtitle) #,margin = list(l = 0, r = 0, b = 100, t = 100, pad = 0)
  td.plot_list[[p]] <- td.plot
  
  # Plot A2: active hrs/day by stay/move
  active.mpd <- activity.results %>% filter(participant==p) %>% 
    select(active.work.mpd, foot.mpd, source) %>%
    melt(id.vars="source", variable.name = "type", value.name = "minutes")
  
  active.plot <- plot_ly(active.mpd, 
                         x = ~source, 
                         y = ~minutes,
                         color = ~type,
                         colors = colourset[4:5],
                         type = "bar",
                         showlegend=ifelse(p=="P03JJ",TRUE,FALSE)) %>%
    layout(barmode = "stack",
           yaxis = list(title = "Minutes per day"),
           annotations = anno_subtitle)
  
  active.plot_list[[p]] <- active.plot
  
}

subplot(td.plot_list, nrows = 2, margin=c(0.01,0.01,0.05,0.05), shareY = TRUE)
subplot(active.plot_list, nrows = 2, margin=c(0.01,0.01,0.05,0.05), shareY = TRUE) # for 2 columns use ceiling(i/2)

# Plot A3: still time hrs/day by (stay only)
still.mpd <- activity.results %>% 
  select(still.mpd, source, participant)
anno_subtitle$text <- "Still periods"
still.plot <- plot_ly(still.mpd %>% group_by(source),
                      x = ~participant,
                      y = ~still.mpd,
                      color = ~source,
                      colors = colours.compare[1:2],
                      type = "bar",
                      showlegend = FALSE) %>%
  layout(yaxis = list(title = "Minutes/day"),
         xaxis = list(title = " "),
         annotations = anno_subtitle)

active.mpd2 <- activity.results %>% 
  select(active.work.mpd, foot.mpd, source, participant)

anno_subtitle$text <- "Transit on foot"
active.mpd.sub1 <- plot_ly(active.mpd2 %>% group_by(source),
                           x = ~participant,
                           y = ~foot.mpd,
                           color = ~source,
                           colors = colours.compare[1:2],
                           type = "bar",
                           showlegend = FALSE) %>%
  layout(yaxis = list(title = "Minutes/day"),
         annotations = anno_subtitle)
anno_subtitle$text <- "Active-Work (survey) vs Foot-Stay (sensor)"
active.mpd.sub2 <- plot_ly(active.mpd2 %>% group_by(source),
                           x = ~participant,
                           y = ~active.work.mpd,
                           color = ~source,
                           colors = colours.compare[1:2],
                           type = "bar") %>%
  layout(yaxis = list(title = "Minutes/day"),
         annotations = anno_subtitle)
subplot(active.mpd.sub2, still.plot, active.mpd.sub1, nrows = 3, margin=c(0.01,0.01,0.08,0.05), shareY = TRUE, shareX = TRUE)




```
