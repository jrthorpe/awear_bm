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


# metrics
metrics <-
  rbind.data.frame(
    readRDS("./output_data/metrics_P03JJs.Rds"),
    readRDS("./output_data/metrics_P06SSs.Rds"),
    readRDS("./output_data/metrics_P07MGs.Rds"),
    readRDS("./output_data/metrics_P08UHs.Rds"),
    readRDS("./output_data/metrics_P10JLs.Rds"),
    readRDS("./output_data/metrics_P13NBs.Rds")
  ) %>% ungroup()

