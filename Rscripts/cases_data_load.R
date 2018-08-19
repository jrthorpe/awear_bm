# screen
screen <-
  rbind.data.frame(
    readRDS("./output_data/screen_P03JJ.Rds"),
    readRDS("./output_data/screen_P06SS.Rds"),
    readRDS("./output_data/screen_P07MG.Rds"),
    readRDS("./output_data/screen_P08UH.Rds"),
    readRDS("./output_data/screen_P10JL.Rds"),
    readRDS("./output_data/screen_P13NB.Rds")
  )

# battery
battery <-
  rbind.data.frame(
    readRDS("./output_data/battery_P03JJ.Rds"),
    readRDS("./output_data/battery_P06SS.Rds"),
    readRDS("./output_data/battery_P07MG.Rds"),
    readRDS("./output_data/battery_P08UH.Rds"),
    readRDS("./output_data/battery_P10JL.Rds"),
    readRDS("./output_data/battery_P13NB.Rds")
  )


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
    readRDS("./output_data/activity_P06SS.Rds"),
    readRDS("./output_data/activity_P07MG.Rds"),
    readRDS("./output_data/activity_P08UH.Rds"),
    readRDS("./output_data/activity_P10JL.Rds"),
    readRDS("./output_data/activity_P13NB.Rds")
  )




