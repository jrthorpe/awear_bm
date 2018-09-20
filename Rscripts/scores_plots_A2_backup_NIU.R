# create scores for each metrics based on quantiles to compare with ES answers

for(p in p.codes){  
  #p = "P03JJ"
  # metrics
  metrics.mob.p  <- metrics.mob %>% filter(participant == p, AR.max < dist.cutoff)
  metrics.act.p <- metrics.act %>% filter(participant == p)
  
  # experience sampling answers
  es.mobility.p <- es.mobility %>% filter(participant == p, answer > 0)
  es.activity.p <- es.activity %>% filter(participant == p, answer > 0)
  
  # convert metrics to scores
  
  N <- 5 # number of scores to categorise data into
  
  # mobility
  mob.scores.p <- data.frame(
    AR.score = score_converter(metrics.mob.p$AR.max, N=N),
    #dist.score = score_converter(metrics.mob.p$dist.total, N=N),
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
  
  # include combination scores
  mob.scores.p %<>% mutate(spatial = (AR.score+mcp.score)/2,
                           temporal = (Tmove.score+Tout.score)/2,
                           counts = (Nplaces.score+Nmoves.score)/2,
                           combo = (AR.score+mcp.score+
                                      Tmove.score+Tout.score+
                                      Nplaces.score+Nmoves.score)/6)
  
  mob.scores.d <- merge(x=mob.scores.p, y=es.mobility.p, by=c("dates", "participant"))
  
  # activity
  act.scores.p <- data.frame(
    activeT.score = score_converter(metrics.act.p$active.time, N=N),
    activeB.score = score_converter(metrics.act.p$active.bouts, N=N),
    still.score = score_converter(metrics.act.p$still.time %>% as.numeric(), N=N),
    steps.score = score_converter(metrics.act.p$steps, N=N),
    dates = metrics.act.p$dates,
    participant = p
  )
  
  # include combination scores
  act.scores.p %<>% mutate(combo = ((activeT.score+
                                       activeB.score+
                                       still.score+
                                       steps.score)/4))
  
  # merge with es answers and store in dataset
  act.scores.d <- merge(x=act.scores.p, y=es.activity.p, by=c("dates", "participant"))
  
  # exclude one random es answer right at start followed by long gap
  if(p=="P03JJ"){
    mob.scores.d <- mob.scores.d[-1,]
    act.scores.d <- act.scores.d[-1,]
  }
  
  
  
  # create individual plots:
  
  anno_subtitle$text <- p  # anno_subtitle defined in stylesheet script
  
  p.scores.mob <- plot_ly(data = mob.scores.d,
                          x=~dates,
                          y=~answer,
                          type = "scatter",
                          mode = "lines+markers",
                          line = list(color="red", width=4),
                          marker = list(color="red", size=10),
                          #showlegend = ifelse(p=="P03JJ",TRUE,FALSE),
                          width = 1000,
                          height = 250,
                          name = "self-report")
  p.scores.mob %<>%
    add_trace(y = ~spatial, name="spatial",marker = list(color=greys[3], size=2), line = list(color=greys[3], width=2, dash = "dot")) %>%
    add_trace(y = ~temporal, name="temporal",marker = list(color=greys[5], size=2), line = list(color=greys[5], width=2, dash = "dot")) %>%
    add_trace(y = ~counts, name="counts",marker = list(color=greys[7], size=2), line = list(color=greys[7], width=2, dash = "dot")) %>%
    add_trace(y = ~combo, name="combination",marker = list(color="black", size=2),line = list(color="black", width=2)) %>%
    layout(annotations = anno_subtitle,
           xaxis = list(title = "Date"),
           yaxis = list(title = "Quintile or score", range = c(0.5,5)),
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 5))
  
  p.scores.act <- plot_ly(data = act.scores.d,
                          x=~dates,
                          y=~answer,
                          type = "scatter",
                          mode = "lines+markers",
                          line = list(color="red", width=4),
                          marker = list(color="red", size=10),
                          #showlegend = ifelse(p=="P03JJ",TRUE,FALSE),
                          width = 1000,
                          height = 250,
                          name = "self-report")
  p.scores.act %<>% 
    add_trace(y = ~activeT.score, name="active time", marker = list(color=greys[3], size=2), line = list(color=greys[3], width=2, dash = "dot")) %>%
    add_trace(y = ~activeB.score, name="active bouts", marker = list(color=greys[4], size=2), line = list(color=greys[4], width=2, dash = "dot")) %>%
    add_trace(y = ~still.score, name="still time", marker = list(color=greys[5], size=2), line = list(color=greys[5], width=2, dash = "dot")) %>%
    add_trace(y = ~steps.score, name="steps", marker = list(color=greys[6], size=2), line = list(color=greys[6], width=2, dash = "dot")) %>%
    add_trace(y = ~combo, name="combined", marker = list(color="black", size=2), line = list(color="black", width=2)) %>%
    layout(annotations = anno_subtitle,
           xaxis = list(title = "Date"),
           yaxis = list(title = "Quintile or score"),
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 5))
  
  
  mob.scores.data[[p]] <- mob.scores.d
  act.scores.data[[p]] <- act.scores.d
  mob.scores[[p]] <- p.scores.mob
  act.scores[[p]] <- p.scores.act
  
  
  # remove(
  #   metrics.mob.p,
  #   metrics.act.p,
  #   mob.scores.p,
  #   act.scores.p,
  #   p.scores.mob,
  #   p.scores.act,
  #   mob.scores.d,
  #   act.scores.d
  # )
  
}