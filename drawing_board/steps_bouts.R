# split steps into columns for watch and phone
steps.log %<>% mutate(watch = ifelse(dsource=="watch",step_count,NA),
                      phone = ifelse(dsource=="phone",step_count,NA))

# create "delta" columns for watch and phone
steps.log %<>% mutate(dsteps.watch = c(0,diff(watch)),
                      dsteps.phone = c(0,diff(phone)))

plot_ly(steps.log,
        x = ~timestamp,
        y = ~phone,
        type = "scatter",
        mode = "lines+markers") %>%
  add_trace(y = ~watch) %>%
  add_trace(y = ~dsteps.phone)%>%
  add_trace(y = ~dsteps.watch)

# summarise by minute to align watch and phone
steps.log %<>% mutate(minutes = floor_date(timestamp,unit="minutes"))

test1 <- steps.log %>% group_by(minutes) %>% summarise(phone.m = sum(dsteps.phone,na.rm=TRUE),
                                                       watch.m = sum(dsteps.watch,na.rm=TRUE))
test3 <- steps.log %>% filter(dsource=="phone") %>% select(timestamp, intervals.alt, phone, dsteps.phone)

watch <- steps.log %>% filter(dsource=="watch") %>% select(timestamp, step_count) %>%
  mutate(dstep = c(0,diff(step_count)),
         dtime = c(0,difftime(tail(timestamp,-1),head(timestamp,-1),units="mins")))

watch %<>% mutate(flag = ifelse(dtime >= 10,1,0)) %>%
  mutate(bout.id = cumsum(flag))

bout.info <- watch %>% group_by(bout.id) %>%
  summarise(b.start = min(timestamp),
            b.end = max(timestamp),
            duration = difftime(b.end, b.start, units = "mins"),
            steps = sum(dstep),
            spm = steps/as.numeric(duration))
bout.info %<>% mutate(N = count(watch,bout.id)$n)

bout.info %<>% mutate(keep = ifelse((steps<10 | duration<10 | spm<1),FALSE,TRUE))
drop.bouts <- bout.info %>% filter(steps<10 | spm<1) %>% select(bout.id)

watch.filtered <- watch %>% filter(!bout.id %in% drop.bouts$bout.id)

plot_ly(watch.filtered,
        x = ~timestamp,
        y = ~step_count,
        color = ~as.factor(bout.id),
        #colors = c("orange","deepskyblue2"),
        type = "scatter",
        mode = "markers")


phone <- steps.log %>% filter(dsource=="phone") %>% select(timestamp, step_count) %>%
  mutate(dstep = c(0,diff(step_count)),
         dtime = c(0,difftime(tail(timestamp,-1),head(timestamp,-1),units="mins")))

phone %<>% mutate(flag = ifelse(dtime >= 10,1,0)) %>%
  mutate(bout.id = cumsum(flag))

bout.info <- phone %>% group_by(bout.id) %>%
  summarise(b.start = min(timestamp),
            b.end = max(timestamp),
            duration = difftime(b.end, b.start, units = "mins"),
            steps = sum(dstep),
            spm = steps/as.numeric(duration))
bout.info %<>% mutate(N = count(phone,bout.id)$n)

plot_ly(phone,
        x = ~timestamp,
        y = ~step_count,
        color = ~as.factor(bout.id),
        #colors = c("orange","deepskyblue2"),
        type = "scatter",
        mode = "markers")

bout.info %<>% mutate(keep = ifelse((steps<10 | duration<10 | spm<1),FALSE,TRUE))
drop.bouts <- bout.info %>% filter(steps<10 | spm<1) %>% select(bout.id)

phone.filtered <- phone %>% filter(!bout.id %in% drop.bouts$bout.id)


plot_ly(data = watch.filtered,
        x = ~timestamp,
        y = ~step_count,
        #color = ~as.factor(bout.id),
        #colors = c("orange","deepskyblue2"),
        type = "scatter",
        mode = "markers") %>%
  add_trace(data = phone.filtered,
            x = ~timestamp,
            y = ~step_count,
            #color = ~as.factor(bout.id),
            #colors = c("orange","deepskyblue2"),
            type = "scatter",
            mode = "markers")

phone <- steps.log %>% filter(dsource=="phone") %>% select(timestamp, step_count) %>%
  mutate(dstep = c(0,diff(step_count)),
         dtime = c(0,difftime(tail(timestamp,-1),head(timestamp,-1),units="mins")))



plot_ly(data = watch,
        x = ~dtime,
        y = ~dstep,
        type = "scatter",
        mode = "markers")%>%
  add_trace(data = phone,
            x = ~dtime,
            y = ~dstep,
            type = "scatter",
            mode = "markers")



plot_ly(data=test2,
        x = ~timestamp,
        y = ~intervals.alt,
        type = "scatter",
        mode = "lines+markers")%>%
  add_trace(data = test3,
            x = ~timestamp,
            y = ~intervals.alt,
            type = "scatter",
            mode = "lines+markers")

