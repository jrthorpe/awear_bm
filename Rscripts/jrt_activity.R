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
               size = n())
  
  # assign duration and end time to bouts of a single reading
  bout.info %<>% mutate(duration = ifelse(size > 1, difftime(b.end, b.start, units ="mins"), 5),
                        b.end = b.start + 60*duration)
  
  return(bout.info)
}



bout_moves <- function(activity.bouts, traj.summary){
  # Append information about whether an activity bout overlaps a move.
  #browser()
  moves <- traj.summary %>% filter(!is.stay) %>% select(T.start,T.end,dates) # get set of moves
  overlap_list = list()
  for(i in 1:nrow(activity.bouts)){
    
    overlap.test <- ((activity.bouts$b.start[i] > moves$T.start) &
                       (activity.bouts$b.start[i] < moves$T.end)) |
      ((activity.bouts$b.end[i] > moves$T.start) &
         (activity.bouts$b.end[i] < moves$T.end))
    overlap_list[[i]] <- overlap.test
  }
  overlap.grid <- do.call("rbind", overlap_list)
  activity.moves <- activity.bouts %>% ungroup() %>% mutate(moving = rowSums(overlap.grid))
  
  return(activity.moves)
}