# plot lifespace metrics


# Plots for the system paper to demonstrate the metrics: ----
disp <- 1.23

# get out a single day:
doi <- as.POSIXct("2018-03-15") # day of interest YYYY-MM-DD
traj.sample <- gps.traj %>% filter(timestamp>=doi, timestamp<=(doi %m+% days(1)))
traj.sample %<>% mutate(events = ifelse(is.stay==1,"Stay","Move"),
                        lat = lat-disp, lon=lon+disp)
home.df <- data.frame(lon=home[1]+disp, lat=home[2]-disp)
mcp.list <- get_mcp(traj.sample %>% select(lat,lon) %>% data.frame(), Qd)
mcp.points <- mcp.list$mcp; remove(mcp.list)
ar <- rbind(traj.sample %>% filter(homedist==max(homedist)) %>% select(lat, lon),
                 home.df)


# locations/events overlaid over GPS trace for all datapoints:
plot_ly(traj.sample,
        x=~lon, y=~lat,     #check
        type = "scatter",
        mode = "markers",
        color = ~events,     #check
        colors = c("lightgrey","black"),
        marker = list(size = 8)) %>%
        #colors = c("lightgrey","black")) %>%
  add_trace(data = home.df, x=~lon, y=~lat, # home centroid      #check
            type = "scatter",
            mode = "markers",
            inherit = FALSE,
            name = "Home",
            marker = list(
              color = "red",
              size = 10,
              line = list(color = "darkred", width = 2))) %>%
  add_trace(data=mcp.points, x=~lon, y=~lat, # outline of MCP
            type = "scatter",
            mode = "lines",
            inherit = FALSE,
            name = "MCP",
            line = list(
              color = "green",
              width = 2)) %>%
  add_trace(data = ar,
            x=~lon, y=~lat,
            type = "scatter",
            mode = "lines",
            inherit = FALSE,
            name = "Action Range",
            line = list(
              color = "blue",
              width = 2))%>%
  layout(yaxis=list(title="latitude", titlefont = f1, tickfont = f2),
         xaxis=list(title="longitude", titlefont = f1, tickfont = f2),
         margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
         legend =  list(font = f1))

# plot trajectory events in chronological order
plot_ly(data = traj.sample %>% filter(is.stay==0) %>% group_by(traj.event),
        x=~lon, y=~lat,    
        type = "scatter",
        mode = "lines+markers",
        color = ~as.factor(traj.event),
        colors = "Spectral", #"Greens",
        line = list(width = 3)) %>%
  add_trace(data = traj.sample %>% filter(is.stay==1) %>% group_by(traj.event),
          x=~lon, y=~lat,    
          type = "scatter",
          mode = "markers",
          #color = "red",
          inherit = FALSE,
          name = "Stays",
          marker = list(size = 10, color = "black")) %>%
  layout(yaxis=list(title="latitude", titlefont = f1, tickfont = f2),
         xaxis=list(title="longitude", titlefont = f1, tickfont = f2),
         margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
         legend = list(font = f1))

# ---





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