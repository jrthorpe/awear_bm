
# Plots for A2


hist.mob.ar <- list()
hist.mob.dist <- list()
hist.mob.mcp <- list()

hist.mob.Tout <- list()
hist.mob.Tmove <- list()

hist.mob.Nplace <- list()
hist.mob.Nmove <- list()

i <- 1 #counter

for(p in p.codes){
  
  # get dataset for current participant
  p.metrics <- metrics.mob %>% filter(participant == p)
  
  # create plots
  hist.mob.ar[[p]] <- plot_ly(x = p.metrics$AR.max/1000, type = "histogram")     # in km
  hist.mob.dist[[p]] <- plot_ly(x = p.metrics$dist.total/1000, type = "histogram") # in km
  hist.mob.mcp[[p]] <- plot_ly(x = p.metrics$mcp.area, type = "histogram")

  hist.mob.Tout[[p]] <- plot_ly(x = p.metrics$Tt.out, type = "histogram")
  hist.mob.Tmove[[p]] <- plot_ly(x = p.metrics$Tt.move, type = "histogram")

  hist.mob.Nplace[[p]] <- plot_ly(x = p.metrics$N.places, type = "histogram")
  hist.mob.Nmove[[p]] <- plot_ly(x = p.metrics$N.moves, type = "histogram")
 
  #i <- i+1
  
}

# plot_ly(alpha = 0.6) %>%
#   add_histogram(x = ~rnorm(500)) %>%
#   add_histogram(x = ~rnorm(500) + 1) %>%
#   layout(barmode = "overlay")
# 
# 
# 
# plot_ly(alpha = 0.6) %>%
#   add_histogram(data = metrics.mob %>% filter(participant == "P03JJ"), x = ~AR.max) %>%
#   add_histogram(data = metrics.mob %>% filter(participant == "P06SS"), x = ~AR.max) %>%
#   add_histogram(data = metrics.mob %>% filter(participant == "P07MG"), x = ~AR.max) %>%
#   add_histogram(data = metrics.mob %>% filter(participant == "P08UH"), x = ~AR.max) %>%
#   add_histogram(data = metrics.mob %>% filter(participant == "P10JL"), x = ~AR.max) %>%
#   add_histogram(data = metrics.mob %>% filter(participant == "P13NB"), x = ~AR.max) %>%
#   layout(barmode = "overlay")













