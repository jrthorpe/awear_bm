# plot lifespace metrics

# locations/events overlaid over GPS trace for all datapoints:
plot_ly(df,
        x=~lon, y=~lat,
        type = "scatter",
        mode = "markers",
        color = ~is.stay,
        colors = c("lightgrey","darkgrey")) %>%
  add_trace(mcp, x=~lon, y=~lat, # outline of MCP
            type = "scatter",
            mode = "lines") %>%
  add_trace(x=home[1], y=home[2], # home centroid
            type = "scatter",
            mode = "markers") %>%
  add_trace(x=c(home[1],"lon of furthest stay centroid"), y=c(home[2],"lat of furthest stay centroid"), # action range
            type = "scatter",
            mode = "markers+lines")





# load metrics results
# load any assessment/other reported results


# get week numbers from study start
metrics.results %<>% mutate(dweek = (as.numeric(dates-dates[1]) %/% 7))

# get weekly summaries:
metrics.weekly <- metrics.results %>% group_by(dweek) %>% summarise(AR.max=max(AR.max),
                                                                    AR.mean=mean(AR.mean),
                                                                    dist.total=sum(dist.total),
                                                                    dist.max=max(dist.max),
                                                                    mcp.area=mean(mcp.area),
                                                                    Tt.out=as.numeric(sum(Tt.out)),
                                                                    Tt.move=as.numeric(sum(Tt.move)),
                                                                    N.moves=mean(N.moves),
                                                                    N.places=mean(N.places))


es <- datasets.all$experience_sampling
es.mobility <- filter(es, question_id=="jrt_mobility") %>% select(dates,answer)
es.mobility.weekly <- es.mobility %>% mutate(dweek = (as.numeric(dates-dates[1]) %/% 7)) %>% 
  group_by(dweek) %>% summarise(answer=mean(answer))

test <- merge(es.mobility.weekly,metrics.weekly)

metrics.weekly.scaled <- data.frame(dweek=test$dweek, scale(test %>% select(-dweek)))

plot_ly(
  metrics.weekly.scaled,
  x=~dweek,
  y=~answer,
  type="scatter",
  mode="markers+lines"
) %>%
  add_trace(y=~AR.mean) %>%
  add_trace(y=~AR.max) %>%
  add_trace(y=~AR.mean) %>%
  add_trace(y=~dist.total) %>%
  add_trace(y=~dist.max) %>%
  add_trace(y=~mcp.area) %>%
  add_trace(y=~Tt.out) %>%
  add_trace(y=~Tt.move) %>%
  add_trace(y=~N.moves) %>%
  add_trace(y=~N.places)



# mobility boundaries
mb.results <- metrics.results %>% group_by(dweek) %>% summarise(mb2 = sum(mb50),
                                                                mb3 = sum(mb1km),
                                                                #mb5km.week = sum(mb5km),
                                                                mb4 = sum(mb10km),
                                                                mb5 = sum(mb.oot))

mb.results %<>% mutate(adherence = count(metrics.results,dweek)$n,
                       dweek = paste0("week",dweek),
                       results = "sensor") %>% 
  filter(adherence==7) %>% 
  select(-adherence)

q.pre.answers <- data.frame(dweek="week0_pre",
                            mb2 = 7,
                            mb3 = 7,
                            mb4 = 0,
                            mb5 = 0,
                            results = "survey")
q.post.answers <- data.frame(dweek="week8_post",
                             mb2 = 7,
                             mb3 = 7,
                             mb4 = 2,
                             mb5 = 0,
                             results = "survey")
mb.results.toplot <- melt(rbind(mb.results,q.pre.answers, q.post.answers), 
                          id.vars = c("dweek","results"))

# Reference for bubble charts: https://plot.ly/r/bubble-charts/
plot_ly(mb.results.toplot,
        x=~dweek,
        y=~variable,
        type="scatter",
        mode = "markers",
        color=~results,
        colors=c("lightblue","orange"),
        size = ~value,
        marker = list(opacity = 0.8, sizemode = "diameter"))

plot_ly(mb.results.toplot,
        x=~dweek,
        y=~variable,
        z=~value,
        type="heatmap",
        colorscale = "Greys")

# **Plot results -----

# Spatial
plot_ly(metrics.results, 
        x=~dates, 
        y=~mcp.area, 
        type="scatter", 
        mode = "lines+markers",
        name = "MCP")%>%
  add_trace(y=~AR.max, name = 'AR max')%>%
  add_trace(y=~AR.mean, name = 'AR mean')%>%
  add_trace(y=~dist.total, name = 'Total distance covered')%>%
  add_trace(y=~dist.max, name = 'Max distance')

# Temporal
plot_ly(metrics.results, 
        x=~dates, 
        y=~Tt.move, 
        type="scatter", 
        mode = "lines+markers",
        name = "Time moving")%>%
  add_trace(y=~Tm.move, name = 'Avg. move time')%>%
  add_trace(y=~Tt.out, name = 'Time out of home')

# Frequency
plot_ly(metrics.results, 
        x=~dates, 
        y=~N.moves, 
        type="scatter", 
        mode = "lines+markers",
        name = "#moves")%>%
  add_trace(y=~N.stay.out, name = '#stays out of home')%>%
  add_trace(y=~N.places, name = '#unique places visited (incl home)')
# ___________________________________

es <- datasets.all$experience_sampling
es.mobility <- filter(es, question_id=="jrt_mobility") %>% select(dates,answer)
test <- merge(metrics.results,es.mobility)

# Compare with ES
plot_ly(test, 
        x=~dates, 
        y=~(answer-3)/4, 
        type="scatter", 
        mode = "lines+markers")%>%
  add_trace(y=~(mcp.area-mean(mcp.area))/(max(mcp.area)-min(mcp.area)), name = 'MCP')%>%
  add_trace(y=~(Tt.out-mean(Tt.out))/(max(as.numeric(Tt.out))-min(as.numeric(Tt.out))), name = 'Time out of home')%>%
  add_trace(y=~N.places/max(N.places), name = '#unique places visited (incl home)')%>%
  add_trace(y=~AR.max/max(AR.max), name = 'AR max')


test.scaled <- scale(test %>% select(AR.max,AR.mean,dist.total,dist.max,answer))
test.scaled <- data.frame(test.scaled,dates=test$dates)

plot_ly(test.scaled, 
        x=~dates, 
        y=~answer, 
        type="scatter", 
        mode = "lines+markers")%>%
  add_trace(y=~AR.max, name = 'AR.max')%>%
  add_trace(y=~AR.mean, name = 'AR.mean')%>%
  add_trace(y=~dist.total, name = 'dist.total')%>%
  add_trace(y=~dist.max, name = 'dist.max')

test.num <- mutate(test, Tt.move=as.numeric(Tt.move),Tt.out=as.numeric(Tt.out))
test.scaled <- scale(test.num %>% select(Tt.move,Tt.out,AR.max,dist.total,N.moves,N.places,answer))
test.scaled <- data.frame(test.scaled,dates=test$dates)

plot_ly(test.scaled, 
        x=~dates, 
        y=~answer, 
        type="scatter", 
        mode = "lines+markers")%>%
  add_trace(y=~Tt.move, name = 'Tt.move',line = list(dash = 'dash'))%>%
  add_trace(y=~Tt.out, name = 'Tt.out',line = list(dash = 'dash'))%>%
  add_trace(y=~AR.max, name = 'AR.max',line = list(dash = 'dash'))%>%
  add_trace(y=~dist.total, name = 'dist.total',line = list(dash = 'dash'))%>%
  add_trace(y=~N.moves, name = 'N.moves',line = list(dash = 'dash'))%>%
  add_trace(y=~N.places, name = 'N.places',line = list(dash = 'dash'))



shapiro.test(test$answer)

## Plot using a qqplot
qqnorm(test$answer)
qqline(test$answer, col = 2)