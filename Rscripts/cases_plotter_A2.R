
# Plots for A2

# Looking at the distrubtion of each of the metrics: ----

hist.mob.ar <- list()
hist.mob.dist <- list()
hist.mob.mcp <- list()

hist.mob.ar.f <- list()
hist.mob.dist.f <- list()
hist.mob.mcp.f <- list()

hist.mob.Tout <- list()
hist.mob.Tmove <- list()

hist.mob.Nplace <- list()
hist.mob.Nmove <- list()

hist.act.steps <- list()
hist.act.Tactive <- list()
hist.act.Nactive <- list()
hist.act.Tstill <- list()

# Comparing metrics ranges to ES answers: ----

# create required datasets:
es.mobility <- es %>% filter(question_id=="jrt_mobility")
mobility.compare <- merge(x=metrics.mob, y=es.mobility, by=c("participant", "dates"))

range.compare.ar <- list()
range.compare.dist <- list()
range.compare.mcp <- list()

range.compare.Tout <- list()
range.compare.Tmove <- list()

range.compare.Nplaces <- list()
range.compare.Nmoves <- list()

# Comparing metrics scores to ES answers
met.scores.data <- list()
met.scores <- list()


for(p in p.codes){
  
  # create basic plots with data for current participant
  p.hist.mob <- plot_ly(data = metrics.mob %>% filter(participant == p), 
                    name = p, width = 1200, height = 300)
  
  p.hist.mob.filt <- plot_ly(data = metrics.mob %>% filter(participant == p, AR.max < dist.cutoff), # action range limit of 50km
                         name = p, width = 1200, height = 300)  
  
  p.hist.steps <- plot_ly(data = stepsdaily %>% filter(participant == p),
                             name = p, width = 1200, height = 300)
  
  p.hist.act <- plot_ly(data = act.features %>% filter(participant == p),
                              name = p, width = 1200, height = 300)
  
  # create each participant-metrics histogram
  hist.mob.ar[[p]] <- p.hist.mob %>% add_histogram(x = ~AR.max/1000)
  hist.mob.dist[[p]] <- p.hist.mob %>% add_histogram(x = ~dist.total/1000) # in km
  hist.mob.mcp[[p]] <- p.hist.mob %>% add_histogram(x = ~mcp.area)
  
  hist.mob.Tout[[p]] <- p.hist.mob %>% add_histogram(x = ~Tt.out)
  hist.mob.Tmove[[p]] <- p.hist.mob %>% add_histogram(x = ~Tt.move)
  
  hist.mob.Nplace[[p]] <- p.hist.mob %>% add_histogram(x = ~N.places)
  hist.mob.Nmove[[p]] <- p.hist.mob %>% add_histogram(x = ~N.moves)
  
  hist.mob.ar.f[[p]] <- p.hist.mob.filt %>% add_histogram(x = ~AR.max/1000)     # in km
  hist.mob.dist.f[[p]] <- p.hist.mob.filt %>% add_histogram(x = ~dist.total/1000) # in km
  hist.mob.mcp.f[[p]] <- p.hist.mob.filt %>% add_histogram(x = ~mcp.area)
  
  hist.act.steps[[p]] <- p.hist.steps %>% add_histogram(x = ~steps)
  hist.act.Tactive[[p]] <- p.hist.act %>% add_histogram(x = ~active.time)
  hist.act.Nactive[[p]] <- p.hist.act %>% add_histogram(x = ~active.bouts)
  hist.act.Tstill[[p]] <- p.hist.act %>% add_histogram(x = ~still.time)

  
  # create basic plots for comparing ES answers to metrics
  p.compare <- plot_ly(data = mobility.compare %>% 
                         filter(participant == p, answer > 0) %>% 
                         group_by(answer),
                       name = p, width = 1200, height = 300, opacity = 0.8)
  
  p.compare.filt <- plot_ly(data = mobility.compare %>% 
                              filter(participant == p, answer > 0, AR.max < dist.cutoff) %>% 
                              group_by(answer),
                       name = p, width = 1200, height = 300, opacity = 0.8)
  
  # create plots for comparing ES answers to ranges of each metric
  range.compare.ar[[p]] <- p.compare.filt %>% add_markers(x = ~AR.max, 
                                                          y=~answer,
                                                          marker = list(size=10))
  range.compare.dist[[p]] <- p.compare.filt %>% add_markers(x = ~dist.total, 
                                                            y=~answer,
                                                            marker = list(size=10))
  range.compare.mcp[[p]] <- p.compare.filt %>% add_markers(x = ~mcp.area, 
                                                           y=~answer,
                                                           marker = list(size=10))
  range.compare.Tout[[p]] <- p.compare.filt %>% add_markers(x = ~Tt.out, 
                                                            y=~answer,
                                                            marker = list(size=10))
  range.compare.Tmove[[p]] <- p.compare %>% add_markers(x = ~Tt.move, 
                                                        y=~answer,
                                                        marker = list(size=10))
  range.compare.Nplaces[[p]] <- p.compare %>% add_markers(x = ~N.places, 
                                                          y=~answer,
                                                          marker = list(size=10))
  range.compare.Nmoves[[p]] <- p.compare %>% add_markers(x = ~N.moves,
                                                         y=~answer,
                                                         marker = list(size=10))
}  

# create scores for each metrics based on quantiles to compare with ES answers

for(p in p.codes){  
  metrics.mob.p  <- metrics.mob %>% filter(participant == p, AR.max < dist.cutoff)
  N <- 5
  scores.p <- data.frame(
    AR.score = score_converter(metrics.mob.p$AR.max, N=N),
    dist.score = score_converter(metrics.mob.p$dist.total, N=N),
    mcp.score = score_converter(metrics.mob.p$mcp.area %>% as.numeric(), N=N),
    Tmove.score = score_converter(metrics.mob.p$Tt.move %>% as.numeric(), N=N),
    Tout.score = score_converter(metrics.mob.p$Tt.out %>% as.numeric(), N=N),
    # Nplaces.score = score_converter(metrics.mob.p$N.places %>% as.numeric(), N=N),
    # Nmoves.score = score_converter(metrics.mob.p$N.moves %>% as.numeric(), N=N),
    Nplaces.score = (metrics.mob.p$N.places/max(metrics.mob.p$N.places))*5,
    Nmoves.score = (metrics.mob.p$N.moves/max(metrics.mob.p$N.moves))*5,
    dates = metrics.mob.p$dates,
    participant = p
  )
  
  scores.p %<>% mutate(spatial = (AR.score+dist.score+mcp.score)/3,
                       temporal = (Tmove.score+Tout.score)/2,
                       counts = (Nplaces.score+Nmoves.score)/2,
                       combo = (AR.score+dist.score+mcp.score+
                                  Tmove.score+Tout.score+
                                  Nplaces.score+Nmoves.score)/7)

  
  es.mobility.p <- es %>% filter(question_id=="jrt_mobility", participant == p)
  met.scores.d <- merge(x=scores.p, y=es.mobility.p, by=c("dates", "participant"))
  if(p=="P03JJ"){
    met.scores.d <- met.scores.d[-1,]
  }
  met.scores.data[[p]] <- met.scores.d

    
  anno_subtitle$text <- p  # anno_subtitle defined in stylesheet script
  p.scores <- plot_ly(data = met.scores.d,
                      x=~dates,
                      y=~answer,
                      type = "scatter",
                      mode = "lines",
                      line = list(color="red", width=4),
                      showlegend = ifelse(p=="P03JJ",TRUE,FALSE),
                      width = 1200,
                      height = 1200,
                      name = "self-report")
  p.scores %<>% 
    add_trace(y = ~spatial, name="spatial", line = list(color=greys[3], width=2, dash = "dot")) %>%
    add_trace(y = ~temporal, name="temporal", line = list(color=greys[4], width=2, dash = "dot")) %>%
    add_trace(y = ~counts, name="counts", line = list(color=greys[5], width=2, dash = "dot")) %>%
    add_trace(y = ~combo, name="combined", line = list(color="black", width=2)) %>%
    layout(annotations = anno_subtitle,
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 5))
    
  met.scores[[p]] <- p.scores
  
  # plot_ly(test,
  #         x=~dates,
  #         y=~answer,
  #         type = "scatter",
  #         mode = "lines",
  #         line = list(color="red", width=6),
  #         width = 1500) %>%
  #   add_trace(y=~AR.max.score, line = list(color="cyan", width=2)) %>%
  #   add_trace(y=~dist.total.score, line = list(color="lightblue", width=2)) %>%
  #   add_trace(y=~Tt.move.score, line = list(color="blue", width=2)) %>%
  #   add_trace(y=~Tt.out.score, line = list(color="darkblue", width=2)) %>%
  #   add_trace(y=~combo, line = list(color="green", width=4))
  
  
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


# plot_ly(alpha = 0.6) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 1), x = ~dist.total) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 2), x = ~dist.total) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 3), x = ~dist.total) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 4), x = ~dist.total) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 5), x = ~dist.total) %>%
#   layout(barmode = "overlay")
# plot_ly(alpha = 0.6) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 1), x = ~N.moves) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 2), x = ~N.moves) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 3), x = ~N.moves) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 4), x = ~N.moves) %>%
#   add_histogram(data = mobility.compare %>% filter(participant == "P03JJ", AR.max < dist.cutoff, answer == 5), x = ~N.moves) %>%
#   layout(barmode = "overlay")

# tester <- list()
# 
# for(i in c(1:6)){
#   p <- p.codes[i]
# test <- mobility.compare %>% filter(participant == p, AR.max < 50000)
# Nb <- 10
# h1 <- plot_ly(test %>% filter(answer==1), x=~AR.max, type = "histogram") #, nbinsx = Nb
# h2 <- plot_ly(test %>% filter(answer==2), x=~AR.max, type = "histogram")
# h3 <- plot_ly(test %>% filter(answer==3), x=~AR.max, type = "histogram")
# h4 <- plot_ly(test %>% filter(answer==4), x=~AR.max, type = "histogram")
# h5 <- plot_ly(test %>% filter(answer==5), x=~AR.max, type = "histogram")
# 
# tester[[i]] <- subplot(h1,h2,h3,h4,h5, nrows = 5, shareX = TRUE, shareY = TRUE)
# 
# }
# subplot(tester)



