# ACTIVITY BOUTS ----
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
    mutate(duration = ifelse(size > 1, difftime(b.end, b.start, units ="mins"), 1))
  
  return(bout.info)
}