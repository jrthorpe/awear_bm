# Activity results compared to Questionnaire:

# ** Load questionnaire results ----
# First time imported from excel, edited and saved, thereafter loaded from "output" folder.

# / Activity
# act.assessments <- activity.assessments %>% filter(assessment == "post") %>% select(-assessment)
# remove(activity.assessments)
# saveRDS(act.assessments,"./output_data/activity_assessments_post.Rds")
act.assessments <- readRDS("./output_data/mobility_assessments_post.Rds")

# / Mobility
# mb.assessments <- mobility_assessment %>% filter(assessment == "post") %>% select(-assessment)
# remove(mobility_assessment)
# saveRDS(mb.assessments,"./output_data/mobility_assessments_post.Rds")
mb.assessments <- readRDS("./output_data/mobility_assessments_post.Rds")


# ** Load sensor data results ----



# Mobility Boundaries compared to Questionnaire:


# Load questionnaire results:
# First time imported from excel, edited and saved. Thereafter loaded from output folder.



# Load sensor data results:
metrics.results.p13 <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_p13nb.Rds")
metrics.results.p10 <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_p10jl.Rds")
metrics.results.p08 <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_p08uh.Rds")
metrics.results.p07 <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_p07mg.Rds")
metrics.results.p06 <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_p06ss.Rds")
metrics.results.p05 <- readRDS("M:/PhD_Folder/CaseStudies/Data_analysis/output/metrics_p03jj.Rds")

metrics.list <- list(metrics.results.p13,
                     metrics.results.p10,
                     metrics.results.p08,
                     metrics.results.p07,
                     metrics.results.p06,
                     metrics.results.p05)
participants <- c("P13NB","P10JL","P08UH","P07MG","P06SS","P03JJ")
plot_list = vector("list",length(metrics.list))

for(i in 1:length(metrics.list)){
  
  #i<-1
  # get week numbers from study start
  metrics.results <- mutate(metrics.list[[i]], dweek = (as.numeric(dates-dates[1]) %/% 7))
  
  # mobility boundaries
  mb.results <- metrics.results %>% group_by(dweek) %>% summarise(mb2 = sum(mb50),
                                                                  mb3 = sum(mb1km),
                                                                  #mb5km.week = sum(mb5km),
                                                                  mb4 = sum(mb10km),
                                                                  mb5 = sum(mb.oot))
  
  mb.results %<>% mutate(adherence = count(metrics.results,dweek)$n) %>% 
    filter(adherence==7) %>% 
    select(-adherence)
  
  q.post <- mb.assessments %>% filter(participant == participants[i])
  
  mb.compare <- data.frame(sensor = colMeans(tail(mb.results,4)[,2:5]),
                           survey = as.numeric(q.post[,3:6]))
  mb.compare %<>% mutate(boundary=rownames(mb.compare))
  
  mb.toplot <- melt(mb.compare,id.vars = "boundary")
  
  p <- plot_ly(mb.toplot,
          x=~variable,
          y=~boundary,
          type="scatter",
          mode = "markers",
          color=~variable,
          colors=c("lightblue","orange"),
          size = ~value,
          marker = list(opacity = 0.8, sizeref=1.5, sizemode = "diameter")) %>%
    layout(xaxis=list(title=FALSE),
           showlegend=FALSE)
  
  plot_list[[i]] <- p
}

subplot(plot_list, nrows = 2, margin=0.08) # for 2 columns use ceiling(i/2)







# get week numbers from study start
metrics.results %<>% mutate(dweek = (as.numeric(dates-dates[1]) %/% 7))

# mobility boundaries
mb.results <- metrics.results %>% group_by(dweek) %>% summarise(mb2 = sum(mb50),
                                                                mb3 = sum(mb1km),
                                                                #mb5km.week = sum(mb5km),
                                                                mb4 = sum(mb10km),
                                                                mb5 = sum(mb.oot))

mb.results %<>% mutate(adherence = count(metrics.results,dweek)$n) %>% 
  filter(adherence==7) %>% 
  select(-adherence)

q.post <- mb.assessments %>% filter(participant == "P13NB")

mb.compare <- data.frame(sensor = colMeans(tail(mb.results,4)[,2:5]),
                         survey = as.numeric(q.post.alt[,3:6]))
mb.compare %<>% mutate(boundary=rownames(mb.compare))

mb.toplot <- melt(mb.compare,id.vars = "boundary")

plot_ly(mb.toplot,
        x=~variable,
        y=~boundary,
        type="scatter",
        mode = "markers",
        color=~variable,
        colors=c("lightblue","orange"),
        size = ~value,
        marker = list(opacity = 0.8, sizemode = "diameter")) %>%
  layout(xaxis=list(title=FALSE),
         showlegend=FALSE)

#Reference for bubble charts: https://plot.ly/r/bubble-charts/


# Heatmap alternative:
plot_ly(mb.results.toplot,
        x=~dweek,
        y=~variable,
        z=~value,
        type="heatmap",
        colorscale = "Greys")