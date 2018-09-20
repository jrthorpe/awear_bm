# STYLE SHEET

# To set size manually:
# p %>% layout(autosize = F, height = 500, margin = list(l=50, r=50, b=100, t=100, pad=4))

# colours
colourset <- brewer.pal(9, "Greys")
colourspect <- brewer.pal(9, "Spectral")
colours.compare <- brewer.pal(3, "Set2")
greys <- brewer.pal(9, "Greys")

# fonts
f1 <- list(
  #family = "Arial, sans-serif",
  size = 18
  #color = "lightgrey"
)
f2 <- list(
  #family = "Arial, sans-serif",
  size = 16
  #color = "black"
)
f3 <- list(
  #family = "Arial, sans-serif",
  size = 14
  #color = "black"
)
l <- list(
  font = f2
  #bgcolor = "#E2E2E2",
  #bordercolor = "#FFFFFF",
  #borderwidth = 2
  )

w <- 1000 # width for plots

# annotations for subplot titles
anno_subtitle <- list(
  #text = paste0("P",i), # define custom per plot
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  font = f2,
  x = 0.5,
  y = 1,
  showarrow = FALSE)

timeaxis <- list(
  title = " ",
  titlefont = f2,
  tickfont = f3,
  autotick = FALSE,
  ticks = "outside",
  #tick0 = 0,
  dtick = 86400000/12, #86400000 corresponds to 1 day
  ticklen = 5,
  tickwidth = 2#,
  #range = c(2,5)#,
  #tickcolor = toRGB("blue")
)

# hist.yaxis <- list(
#   title = "Count (days)",
#   titlefont = f2,
#   tickfont = f3
# )
# 
# hist.xaxis.spatial <- list(
#   title = "kilometers",
#   titlefont = f2,
#   tickfont = f3
# )

stdaxis <- list(
  titlefont = f1,
  tickfont = f2
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
