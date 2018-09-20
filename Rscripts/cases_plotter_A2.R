
# Plots for A2

p.nums <- data.frame(P03JJ="1",P06SS="2",P07MG="3",P08UH="4",P10JL="5",P13NB="6")

# Looking at the distrubtion of each of the metrics: ----

hist.mob.ar <- list()
hist.mob.dist <- list()
hist.mob.distfoot <- list()
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

range.compare.ar <- list()
range.compare.dist <- list()
range.compare.distfoot <- list()
range.compare.mcp <- list()

range.compare.Tout <- list()
range.compare.Tmove <- list()

range.compare.Nplaces <- list()
range.compare.Nmoves <- list()

range.compare.activeT <- list()
range.compare.activeB <- list()
range.compare.still <- list()
range.compare.steps <- list()

# Comparing metrics scores to ES answers
mob.probs <- list()
act.probs <- list()


for(p in p.codes){
  
  # create basic plots with data for current participant
  p.hist.mob <- plot_ly(data = metrics.mob %>% filter(participant == p), 
                    name = p, width = 1200, height = 150)
  
  p.hist.mob.filt <- plot_ly(data = metrics.mob %>% filter(participant == p, AR.max < dist.cutoff), # action range limit of 50km
                         name = p, width = 1200, height = 150)  
  
  p.hist.act <- plot_ly(data = metrics.act %>% filter(participant == p),
                              name = p, width = 1200, height = 150)
  
  # create each participant-metrics histogram
  hist.mob.ar[[p]] <- p.hist.mob %>% add_histogram(x = ~AR.max/1000)
  hist.mob.dist[[p]] <- p.hist.mob %>% add_histogram(x = ~dist.total/1000) # in km
  hist.mob.distfoot[[p]] <- p.hist.mob %>% add_histogram(x = ~dist.foot) # in km
  hist.mob.mcp[[p]] <- p.hist.mob %>% add_histogram(x = ~mcp.area)
  
  hist.mob.Tout[[p]] <- p.hist.mob %>% add_histogram(x = ~Tt.out)
  hist.mob.Tmove[[p]] <- p.hist.mob %>% add_histogram(x = ~Tt.move)
  
  hist.mob.Nplace[[p]] <- p.hist.mob %>% add_histogram(x = ~N.places)
  hist.mob.Nmove[[p]] <- p.hist.mob %>% add_histogram(x = ~N.moves)
  
  hist.mob.ar.f[[p]] <- p.hist.mob.filt %>% add_histogram(x = ~AR.max/1000)     # in km
  hist.mob.dist.f[[p]] <- p.hist.mob.filt %>% add_histogram(x = ~dist.total/1000) # in km
  hist.mob.mcp.f[[p]] <- p.hist.mob.filt %>% add_histogram(x = ~mcp.area)
  
  hist.act.steps[[p]] <- p.hist.act %>% add_histogram(x = ~steps)
  hist.act.Tactive[[p]] <- p.hist.act %>% add_histogram(x = ~active.time)
  hist.act.Nactive[[p]] <- p.hist.act %>% add_histogram(x = ~active.bouts)
  hist.act.Tstill[[p]] <- p.hist.act %>% add_histogram(x = ~still.time)

  
  # create basic plots for comparing ES answers to metrics
  p.compare.mob <- plot_ly(data = mobility.compare %>% 
                         filter(participant == p, answer > 0) %>% 
                         group_by(answer),
                       name = p, width = 1200, height = 150, opacity = 0.8)
  
  p.compare.mob.filt <- plot_ly(data = mobility.compare %>% 
                              filter(participant == p, answer > 0, AR.max < dist.cutoff) %>% 
                              group_by(answer),
                       name = p, width = 1200, height = 150, opacity = 0.8)
  
  p.compare.act <- plot_ly(data = activity.compare %>% 
                              filter(participant == p, answer > 0) %>% 
                              group_by(answer),
                            name = p, width = 1200, height = 150, opacity = 0.8)
  
  # create plots for comparing ES answers to ranges of each metric
  range.compare.ar[[p]] <- p.compare.mob.filt %>% add_markers(x = ~AR.max, 
                                                          y=~answer,
                                                          marker = list(size=10))
  range.compare.dist[[p]] <- p.compare.mob.filt %>% add_markers(x = ~dist.total, 
                                                            y=~answer,
                                                            marker = list(size=10))
  range.compare.distfoot[[p]] <- p.compare.mob.filt %>% add_markers(x = ~dist.foot, 
                                                                y=~answer,
                                                                marker = list(size=10))
  range.compare.mcp[[p]] <- p.compare.mob.filt %>% add_markers(x = ~mcp.area, 
                                                           y=~answer,
                                                           marker = list(size=10))
  range.compare.Tout[[p]] <- p.compare.mob.filt %>% add_markers(x = ~Tt.out, 
                                                            y=~answer,
                                                            marker = list(size=10))
  range.compare.Tmove[[p]] <- p.compare.mob %>% add_markers(x = ~Tt.move, 
                                                        y=~answer,
                                                        marker = list(size=10))
  range.compare.Nplaces[[p]] <- p.compare.mob %>% add_markers(x = ~N.places, 
                                                          y=~answer,
                                                          marker = list(size=10))
  range.compare.Nmoves[[p]] <- p.compare.mob %>% add_markers(x = ~N.moves,
                                                         y=~answer,
                                                         marker = list(size=10))
  range.compare.activeT[[p]] <- p.compare.act %>% add_markers(x = ~active.time,
                                                             y=~answer,
                                                             marker = list(size=10))
  range.compare.activeB[[p]] <- p.compare.act %>% add_markers(x = ~active.bouts,
                                                             y=~answer,
                                                             marker = list(size=10))
  range.compare.still[[p]] <- p.compare.act %>% add_markers(x = ~still.time,
                                                            y=~answer,
                                                            marker = list(size=10))
  range.compare.steps[[p]] <- p.compare.act %>% add_markers(x = ~steps,
                                                            y=~answer,
                                                            marker = list(size=10))
  # clean up environment by removing Current variables
  remove(
    p.compare.act,
    p.compare.mob,
    p.compare.mob.filt,
    p.hist.act,
    p.hist.mob,
    p.hist.mob.filt
    )
}  

# Comparison of self reports with CDF probability
for(p in p.codes){  
  
  #p <- "P10JL"
  # print(p.nums[[p]])
  # paste("hello",p.nums[[p]])
  
  # metrics
  metrics.mob.p  <- metrics.mob %>% filter(participant == p, AR.max < dist.cutoff)
  metrics.act.p <- metrics.act %>% filter(participant == p)
  
  # experience sampling answers
  es.mobility.p <- es.mobility %>% filter(participant == p, answer > 0)
  es.activity.p <- es.activity %>% filter(participant == p, answer > 0)
  
  # create probability functions
  ecdf.AR <- ecdf(metrics.mob.p$AR.max)
  ecdf.dist <- ecdf(metrics.mob.p$dist.total)
  ecdf.distfoot <- ecdf(metrics.mob.p$dist.foot)
  ecdf.mcp <-  ecdf(metrics.mob.p$mcp.area %>% as.numeric())
  ecdf.Tmove  = ecdf(metrics.mob.p$Tt.move %>% as.numeric())
  ecdf.Tout  = ecdf(metrics.mob.p$Tt.out %>% as.numeric())
  ecdf.Nplaces  = ecdf(metrics.mob.p$N.places)
  ecdf.Nmoves  = ecdf(metrics.mob.p$N.moves)

  # mobility
  mob.probs.p <- data.frame(
    AR.probs = ecdf.AR(metrics.mob.p$AR.max),
    dist.probs = ecdf.dist(metrics.mob.p$dist.total),
    distfoot.probs = ecdf.distfoot(metrics.mob.p$dist.foot),
    mcp.probs = ecdf.mcp(metrics.mob.p$mcp.area %>% as.numeric()),
    Tmove.probs = ecdf.Tmove(metrics.mob.p$Tt.move %>% as.numeric()),
    Tout.probs = ecdf.Tout(metrics.mob.p$Tt.out %>% as.numeric()),
    Nplaces.probs = ecdf.Nplaces(metrics.mob.p$N.places),
    Nmoves.probs = ecdf.Nmoves(metrics.mob.p$N.moves),
    dates = metrics.mob.p$dates,
    participant = p
  )
  
  # include combination scores
  mob.probs.p %<>% mutate(spatial = (AR.probs+mcp.probs+dist.probs)/3,
                           temporal = (Tmove.probs+Tout.probs)/2,
                           counts = (Nplaces.probs+Nmoves.probs)/2,
                           combo = (AR.probs+mcp.probs+dist.probs+
                                      Tmove.probs+Tout.probs+
                                      Nplaces.probs+Nmoves.probs)/7)

  mob.probs.dat <- merge(x=mob.probs.p, y=es.mobility.p, by=c("dates", "participant"))

  # Activity
  
  # create functions:
  ecdf.activeT = ecdf(metrics.act.p$active.time)
  ecdf.activeB = ecdf(metrics.act.p$active.bouts)
  ecdf.still = ecdf(metrics.act.p$still.time %>% as.numeric())
  ecdf.steps = ecdf(metrics.act.p$steps)
  
  # convert to probabilities
  act.probs.p <- data.frame(
    activeT.probs = ecdf.activeT(metrics.act.p$active.time),
    activeB.probs = ecdf.activeB(metrics.act.p$active.bouts),
    still.probs = 1-ecdf.still(metrics.act.p$still.time %>% as.numeric()),
    steps.probs = ecdf.steps(metrics.act.p$steps),
    dates = metrics.act.p$dates,
    participant = p
  )
  
  # include combination scores
  act.probs.p %<>% mutate(combo = ((activeT.probs+
                                       activeB.probs+
                                       still.probs+
                                       steps.probs)/4))
  
  act.probs.p <-  merge(x=act.probs.p, 
                         y=mob.probs.p %>% select(dates,distfoot.probs), 
                         by="dates",
                 all.x = TRUE)

  # merge with es answers and store in dataset
  act.probs.dat <- merge(x=act.probs.p, y=es.activity.p, by=c("dates", "participant"))
  
  # exclude one random es answer right at start followed by long gap
  if(p=="P03JJ"){
    mob.probs.dat <- mob.probs.dat[-1,]
    act.probs.dat <- act.probs.dat[-1,]
  }
  
  
  
  # create individual plots:
  anno_subtitle$text <- paste("Participant",p.nums[[p]])  # anno_subtitle defined in stylesheet script

  p.mob.probs <- plot_ly(data = mob.probs.dat,
                          x=~dates,
                          y=~combo,
                          type = "scatter",
                          mode = "lines+markers",
                          line = list(color="black", width=4),
                          marker = list(color="black", size=4),
                          #showlegend = ifelse(p=="P03JJ",TRUE,FALSE),
                          width = 1000,
                          height = 350,
                          name = "Combined")
  p.mob.probs %<>%
    add_trace(y = ~AR.probs, name="Action range",marker = list(color=greys[3], size=2), line = list(color=greys[3], width=2, dash = "dot")) %>%
    add_trace(y = ~dist.probs, name="Distance",marker = list(color=greys[4], size=2), line = list(color=greys[4], width=2, dash = "dot")) %>%
    add_trace(y = ~mcp.probs, name="MCP",marker = list(color=greys[5], size=2), line = list(color=greys[5], width=3, dash = "dot")) %>%
    add_trace(y = ~Tmove.probs, name="Move time",marker = list(color=greys[6], size=2), line = list(color=greys[6], width=2, dash = "dot")) %>%
    add_trace(y = ~Tout.probs, name="Time out",marker = list(color=greys[7], size=2), line = list(color=greys[7], width=2, dash = "dot")) %>%
    add_trace(y = ~Nmoves.probs, name="N moves",marker = list(color=greys[8], size=2), line = list(color=greys[8], width=2, dash = "dot")) %>%
    add_trace(y = ~Nplaces.probs, name="N places",marker = list(color=greys[9], size=2), line = list(color=greys[9], width=2, dash = "dot")) %>%
    # add_trace(y = ~spatial, name="spatial",marker = list(color=greys[3], size=2),  line = list(color=greys[3], width=2, dash = "dot")) %>%
    # add_trace(y = ~temporal, name="temporal",marker = list(color=greys[5], size=2),  line = list(color=greys[5], width=2, dash = "dot")) %>%
    # add_trace(y = ~counts, name="counts",marker = list(color=greys[7], size=2),  line = list(color=greys[7], width=2, dash = "dot")) %>%
    add_trace(y = ~(answer-1)/4, name="self-report",marker = list(color="red", size=10),  line = list(color="red", width=4)) %>%
    layout(annotations = anno_subtitle,
           xaxis = list(title = "Date"),
           yaxis = list(title = "Probability | Score"),
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 5))

  p.act.probs <- plot_ly(data = act.probs.dat,
                          x=~dates,
                          y=~(answer-1)/4,
                          type = "scatter",
                          mode = "lines+markers",
                          line = list(color="red", width=4),
                          marker = list(color="red", size=10),
                          #showlegend = ifelse(p=="P03JJ",TRUE,FALSE),
                          width = 1000,
                          height = 350,
                          name = "self-report")
  p.act.probs %<>%
    add_trace(y = ~distfoot.probs, name="dist foot",
              marker = list(color=greys[2], size=2), line = list(color=greys[2], width=2, dash = "dot")) %>%
    add_trace(y = ~activeT.probs, name="active time",
              marker = list(color=greys[3], size=2), line = list(color=greys[3], width=2, dash = "dot")) %>%
    add_trace(y = ~activeB.probs, name="active bouts",
              marker = list(color=greys[4], size=2), line = list(color=greys[4], width=2, dash = "dot")) %>%
    add_trace(y = ~still.probs, name="still time",
              marker = list(color=greys[5], size=2), line = list(color=greys[5], width=2, dash = "dot")) %>%
    add_trace(y = ~steps.probs, name="steps",
              marker = list(color=greys[6], size=2), line = list(color=greys[6], width=2, dash = "dot")) %>%
    add_trace(y = ~combo, name="combined",
              marker = list(color="black", size=2), line = list(color="black", width=4)) %>%
    layout(annotations = anno_subtitle,
           xaxis = list(title = "Date"),
           yaxis = list(title = "Probability | Score"),
           margin = list(l = 50, r = 50, b = 50, t = 50, pad = 5))
  
  mob.probs[[p]] <- p.mob.probs
  act.probs[[p]] <- p.act.probs
}

p7act <- metrics.act %>% filter(participant == "P07MG")
p7mob <- metrics.mob %>% filter(participant == "P07MG")

plot_ly(p7act,
        x = ~dates,
        y = ~steps,
        type = "scatter",
        mode = "lines+markers")

plot_ly(p7mob,
        x = ~dates,
        y = ~Tt.move,
        type = "scatter",
        mode = "lines+markers")

plot_ly(p7mob,
        x = ~dates,
        y = ~dist.foot,
        type = "scatter",
        mode = "lines+markers") %>%
  add_trace(y=~dist.total) %>%
  add_trace(data=p7act, y = ~steps)


# # Looking at correlations between ES answers and data.
# plot_ly(activity.compare,
#         x = ~steps,
#         y = ~answer,
#         type = "scatter",
#         mode = "markers",
#         color = ~participant
#         )


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



