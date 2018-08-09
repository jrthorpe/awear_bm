# STYLE SHEET

colourset <- brewer.pal(9, "Set1")
colours.compare <- brewer.pal(3, "Set2")

# annotations for subplot titles
anno_subtitle <- list(
  #text = paste0("P",i), # define custom per plot
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE)

timeaxis <- list(
  title = " ",
  autotick = FALSE,
  ticks = "outside",
  #tick0 = 0,
  dtick = 86400000/48, #86400000 corresponds to 1 day
  ticklen = 5,
  tickwidth = 2#,
  #range = c(2,5)#,
  #tickcolor = toRGB("blue")
)

# # example for experimentation
# plot_ly(stepcounters.p %>% filter(dates==d) %>% group_by(source),
#         x = ~timestamp, y = ~stepcounter,
#         type = "scatter", mode = "lines+markers",
#         color = ~source, colors = colourset[4:5],
#         legendgroup = "Steps", name = "source")  %>%
#   layout(yaxis=list(title="Step count"),
#          xaxis = timeaxis,
#          margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4))
