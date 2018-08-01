# AWEAR: DATA RESTRUCTURE
#
#********************************************************************************************
# Author: Julia Thorpe
# Written for AWEAR Case Studies, January-June 2018, as part of an ongoing PhD
# project on Engineering Systems Design in Healthcare at DTU in collaboration
# with Rigshospitalet-Glostrup

# This script ...

# SETUP -------------------------------------------------------------------------------------------

# Load packages and functions.
library(data.table)
library(dtplyr)
library(dplyr)
library(magrittr)
library(lubridate)
library(ggplot2)
library(reshape2)
library(maptools)
library(plotly)
library(maps)
library(mapproj)
library(sp)
library(caTools)
library(geosphere) # added in v3 of data checker
library(mapview) # added in v1 of analysis framework
library(tidyr) # added for function "complete" in assessments section

# Load custom functions:
source("./Rscripts/jrt_utils.R")
source("./Rscripts/users.R")

# SETTINGS ==========================================

# Define constants:
  folder <- "M:/PhD_Folder/CaseStudies/Data_dumps/dump_current_analysis/" # path to folder that holds multiple .csv files, downloaded from nightingale webportal
  not.in.use <- c("battery", 
                  "screen",
                  "experience_sampling",
                  #"activity",
                  #"step_count",
                  #"location",
                  "bluetooth","hardware_info","wifi","wearable","calllog","sms") #last row items never used
  # to_plot <- c("activity", # from data checker, for debugging purposes only (visualise all datasets)
  #              "battery",
  #              "exps",
  #              "location",
  #              "screen",
  #              "steps")


p.codes <- c("P03JJ","P06SS","P07MG","P08UH","P10JL","P13NB") # case studies
#p.codes <- c("daisy","violet","agapantha","anthurium","nasturtium") # pilot
for (p in p.codes) {
  
  P <- participants[[p]]
  list2env(P, .GlobalEnv); remove(P)

  d.study <- as.numeric(round(difftime(d.stop,d.start,units="days")))
  
  # IMPORT AND RESTRUCTURE DATA ------------------------
  
  datasets.all <- get.data(folder, not.in.use) %>% 
    lapply(restructure, userid, d.start, d.stop)
  datasets.all <- Filter(function(x) !is.null(x)[1],datasets.all) # remove any null dataframes
  
  saveRDS(datasets.all,paste0("M:/PhD_Folder/awear_bm/output_data/datasets_", p,".Rds"))
  
}