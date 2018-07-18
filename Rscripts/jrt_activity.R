# ACTIVITY BOUTS ----

activity_bouts <- function(activity.log, acts){
  # separate data into separate streams each with own difftime
  # split each stream into bouts using a time threshold
  
  activities_list = vector("list",length(acts))
  
  for(i in 1:length(acts)){
    bouts <- get_bouts(activity.log, acts[i])
    activities_list[[i]] <- bout_info(bouts)
    }
  
  activity.bouts <- do.call("rbind", activities_list) 
  
  
  return(activity.bouts)
}

get_bouts <- function(df, A){
  
  # Extract data for specified activity and detect bouts
  act.bouts <- df %>% filter(label == A) %>% select(timestamp, label, confidence, dates, times) %>%
    mutate(dtime = c(0, difftime(tail(timestamp,-1), head(timestamp,-1), units = "mins"))) %>%
    mutate(split = ifelse(dtime > 10, 1, 0)) %>%
    group_by(dates) %>% mutate(bout = cumsum(split))
  
  return(act.bouts)
}

bout_info <- function(bouts){
  # Extract data for specified activity and detect bouts
  bout.info <- bouts %>% group_by(dates, bout) %>%
    summarize( activity = label[1],
               b.start = min(timestamp), b.end = max(timestamp),
               mean.conf = mean(confidence),
               size = n()) %>%
    mutate(duration = ifelse(size > 1, difftime(b.end, b.start, units ="mins"), 5))
  
  return(bout.info)
}
