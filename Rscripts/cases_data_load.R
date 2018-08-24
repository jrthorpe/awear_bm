# screen
screen <-
  rbind.data.frame(
    readRDS("./output_data/screen_P03JJ.Rds"),
    readRDS("./output_data/screen_P06SS.Rds"),
    readRDS("./output_data/screen_P07MG.Rds"),
    readRDS("./output_data/screen_P08UH.Rds"),
    readRDS("./output_data/screen_P10JL.Rds"),
    readRDS("./output_data/screen_P13NB.Rds")
  ) %>% ungroup()

# battery
battery <-
  rbind.data.frame(
    readRDS("./output_data/battery_P03JJ.Rds"),
    readRDS("./output_data/battery_P06SS.Rds"),
    readRDS("./output_data/battery_P07MG.Rds"),
    readRDS("./output_data/battery_P08UH.Rds"),
    readRDS("./output_data/battery_P10JL.Rds"),
    readRDS("./output_data/battery_P13NB.Rds")
  ) %>% ungroup()

# trajectories
traj <-
  rbind.data.frame(
    readRDS("./output_data/traj_P03JJ.Rds"),
    readRDS("./output_data/traj_P06SS.Rds"),
    readRDS("./output_data/traj_P07MG.Rds"),
    readRDS("./output_data/traj_P08UH.Rds"),
    readRDS("./output_data/traj_P10JL.Rds"),
    readRDS("./output_data/traj_P13NB.Rds")
  ) %>% ungroup()

# action range
actionrange <-
  rbind.data.frame(
    readRDS("./output_data/homedist_P03JJ.Rds"),
    readRDS("./output_data/homedist_P06SS.Rds"),
    readRDS("./output_data/homedist_P07MG.Rds"),
    readRDS("./output_data/homedist_P08UH.Rds"),
    readRDS("./output_data/homedist_P10JL.Rds"),
    readRDS("./output_data/homedist_P13NB.Rds")
  ) %>% ungroup()

# mcp
mcp <-
  rbind.data.frame(
    readRDS("./output_data/mcp_P03JJ.Rds"),
    readRDS("./output_data/mcp_P06SS.Rds"),
    readRDS("./output_data/mcp_P07MG.Rds"),
    readRDS("./output_data/mcp_P08UH.Rds"),
    readRDS("./output_data/mcp_P10JL.Rds"),
    readRDS("./output_data/mcp_P13NB.Rds")
  ) %>% ungroup()

# activities
activities <-
  rbind.data.frame(
    readRDS("./output_data/activity_P03JJ.Rds"),
    readRDS("./output_data/activity_P06SS.Rds"),
    readRDS("./output_data/activity_P07MG.Rds"),
    readRDS("./output_data/activity_P08UH.Rds"),
    readRDS("./output_data/activity_P10JL.Rds"),
    readRDS("./output_data/activity_P13NB.Rds")
  ) %>% ungroup()


# Analysis 2 ----

# Daily measures:

# mobility metrics
metrics.mob <-
  rbind.data.frame(
    readRDS("./output_data/metrics_P03JJ.Rds"),
    readRDS("./output_data/metrics_P06SS.Rds"),
    readRDS("./output_data/metrics_P07MG.Rds"),
    readRDS("./output_data/metrics_P08UH.Rds"),
    readRDS("./output_data/metrics_P10JL.Rds"),
    readRDS("./output_data/metrics_P13NB.Rds")
  ) %>% ungroup()

# activity bouts summarised by day
act.pday <-
  rbind.data.frame(
    readRDS("./output_data/activity_pday_P03JJ.Rds"),
    readRDS("./output_data/activity_pday_P06SS.Rds"),
    readRDS("./output_data/activity_pday_P07MG.Rds"),
    readRDS("./output_data/activity_pday_P08UH.Rds"),
    readRDS("./output_data/activity_pday_P10JL.Rds"),
    readRDS("./output_data/activity_pday_P13NB.Rds")
  ) %>% ungroup()

# transit activity summarised by day
act.trans.pday <-
  rbind.data.frame(
    readRDS("./output_data/activity_moves_pday_P03JJ.Rds"),
    readRDS("./output_data/activity_moves_pday_P06SS.Rds"),
    readRDS("./output_data/activity_moves_pday_P07MG.Rds"),
    readRDS("./output_data/activity_moves_pday_P08UH.Rds"),
    readRDS("./output_data/activity_moves_pday_P10JL.Rds"),
    readRDS("./output_data/activity_moves_pday_P13NB.Rds")
  ) %>% ungroup()

# total daily steps
stepsdaily <-
  rbind.data.frame(
    readRDS("./output_data/steptotals_P03JJ.Rds"),
    readRDS("./output_data/steptotals_P06SS.Rds"),
    readRDS("./output_data/steptotals_P07MG.Rds"),
    readRDS("./output_data/steptotals_P08UH.Rds"),
    readRDS("./output_data/steptotals_P10JL.Rds"),
    readRDS("./output_data/steptotals_P13NB.Rds")
  ) %>% ungroup()


# experience sampling
es <-
  rbind.data.frame(
    readRDS("./output_data/es_P03JJ.Rds"),
    readRDS("./output_data/es_P06SS.Rds"),
    readRDS("./output_data/es_P07MG.Rds"),
    readRDS("./output_data/es_P08UH.Rds"),
    readRDS("./output_data/es_P10JL.Rds"),
    readRDS("./output_data/es_P13NB.Rds")
  ) %>% ungroup()


# Create a combined activity features by day plot:

