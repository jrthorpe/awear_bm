# Activity results compared to Questionnaire:

# define variables:
p.codes <- c("P03JJ","P06SS","P07MG","P08UH","P10JL","P13NB")


# ** Load questionnaire and sensor results ----

# Note: for questionnaires -- first time imported from excel, edited and saved, thereafter loaded from "output" folder.

# / Activity
# act.assessments <- activity.assessments %>% filter(assessment == "post") %>% select(-assessment)
# remove(activity.assessments)
# saveRDS(act.assessments,"./output_data/activity_assessments_post.Rds")
act.assessments <- readRDS("./output_data/activity_assessments_post.Rds")

act.list <- list(P03JJ = readRDS("./output_data/activitymoves_P03JJ.Rds"),
                P06SS = readRDS("./output_data/activitymoves_P06SS.Rds"),
                P07MG = readRDS("./output_data/activitymoves_P03JJ.Rds"),
                P08UH = readRDS("./output_data/activitymoves_P06SS.Rds"),
                P10JL = readRDS("./output_data/activitymoves_P03JJ.Rds"),
                P13NB = readRDS("./output_data/activitymoves_P06SS.Rds"))
# / Mobility
# mb.assessments <- mobility_assessment %>% filter(assessment == "post") %>% select(-assessment)
# remove(mobility_assessment)
# saveRDS(mb.assessments,"./output_data/mobility_assessments_post.Rds")
mz.assessments <- readRDS("./output_data/mobility_assessments_post.Rds")

mz.list <- list(P03JJ = readRDS("./output_data/mobilityzones_P03JJ.Rds"),
                P06SS = readRDS("./output_data/mobilityzones_P06SS.Rds"),
                P07MG = readRDS("./output_data/mobilityzones_P03JJ.Rds"),
                P08UH = readRDS("./output_data/mobilityzones_P06SS.Rds"),
                P10JL = readRDS("./output_data/mobilityzones_P03JJ.Rds"),
                P13NB = readRDS("./output_data/mobilityzones_P06SS.Rds"))



# ** Mobility results ----
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
  mz.toplot <- melt(mz.compare,id.vars = "zone")
  
  p <- plot_ly(mz.toplot,
               x=~variable,
               y=~zone,
               type="scatter",
               mode = "markers",
               color=~variable,
               colors=c("lightblue","orange"),
               size = ~value,
               marker = list(opacity = 0.8, sizeref=1.5, sizemode = "diameter")) %>%
    layout(xaxis=list(title=FALSE),
           showlegend=FALSE)
  
  mz.plot_list[[i]] <- p
}

subplot(mz.plot_list, nrows = 2, margin=0.08) # for 2 columns use ceiling(i/2)
#Reference for bubble charts: https://plot.ly/r/bubble-charts/

# # Heatmap alternative:
# plot_ly(mb.results.toplot,
#         x=~dweek,
#         y=~variable,
#         z=~value,
#         type="heatmap",
#         colorscale = "Greys")

remove(mz.compare,mz.dat,mz.toplot,mz.weekly,q.post)

# ** Activity results ----


