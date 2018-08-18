# # screen
# traj.algorithm <-
#   rbind.data.frame(
#     readRDS("./output_data/traj_daisy.Rds"),
#     readRDS("./output_data/traj_violet.Rds"),
#     readRDS("./output_data/traj_nasturtium.Rds"),
#     readRDS("./output_data/traj_agapantha.Rds"),
#     readRDS("./output_data/traj_anthurium.Rds")
#   )
# 
# # battery
# traj.algorithm <-
#   rbind.data.frame(
#     readRDS("./output_data/traj_daisy.Rds"),
#     readRDS("./output_data/traj_violet.Rds"),
#     readRDS("./output_data/traj_nasturtium.Rds"),
#     readRDS("./output_data/traj_agapantha.Rds"),
#     readRDS("./output_data/traj_anthurium.Rds")
#   )


# action range
homedist <-
  rbind.data.frame(
    readRDS("./output_data/homedist_P03JJ.Rds"),
    readRDS("./output_data/homedist_P06SS.Rds"),
    readRDS("./output_data/homedist_P07MG.Rds"),
    readRDS("./output_data/homedist_P08UH.Rds"),
    readRDS("./output_data/homedist_P10JL.Rds"),
    readRDS("./output_data/homedist_P13NB.Rds")
  )

# activities
activities <-
  rbind.data.frame(
    readRDS("./output_data/activity_P03JJ.Rds"),
    readRDS("./output_data/activity_06SS.Rds"),
    readRDS("./output_data/activity_P07MG.Rds"),
    readRDS("./output_data/activity_P08UH.Rds"),
    readRDS("./output_data/activity_P10JL.Rds"),
    readRDS("./output_data/activity_P13NB.Rds")
  )




