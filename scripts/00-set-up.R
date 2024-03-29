
# PROJECT-WIDE SET-UP

# *************************************************************************************************

library(tidyverse)
library(lubridate)
library(tictoc)
library(furrr)
library(stars)

sf_use_s2(F)

options(future.fork.enable = T)
options(future.globals.maxSize= 10000*1024^2)

# system("sudo sysctl -w vm.dirty_bytes=50331648")
# system("sudo sysctl -w vm.dirty_background_bytes=16777216")

# *************************************************************************************************

# MOUNT BUCKETS AND DRIVE

# risk team bucket
system("gcsfuse cmip5_data /home/cdobler/bucket_risk/")
dir_bucket_risk <- "/home/cdobler/bucket_risk/"

# my bucket
system("gcsfuse clim_data_reg_useast1 /home/cdobler/bucket_mine/")
dir_bucket_mine <- "/home/cdobler/bucket_mine/"

# persistent disk
system("sudo mount -o discard,defaults /dev/sdb /home/cdobler/pers_disk")
dir_disk <- "/home/cdobler/pers_disk"
  
# *************************************************************************************************

# OTHER DIRS
dir_data <- "/home/cdobler/bucket_mine/misc_data/"
dir_output <- "/home/cdobler/bucket_mine/results/global_tmp_ww/"

# VARIABLES DEF

func_var_def <- function(variable){
  
  case_when(variable == "mean_temp" ~ "average_temperature",
            variable == "mean_precip" ~ "precipitation",
            variable == "extr_precip" ~ "precipitation",
            variable == "extr_maxtemp" ~ "maximum_temperature",
            variable == "extr_wetbulbtemp" ~ "wetbulb_temperature",
            variable == "extr_fwi" ~ "fire_weather_index",
            variable == "extr_cdd" ~ "precipitation",
            variable == "extr_drought" ~ "palmer_drought_severity_index") -> variable_data
  
  case_when(variable == "mean_temp" ~ "avg_temperature",
            variable == "mean_precip" ~ "avg_precipitation",
            variable == "extr_precip" ~ "precipitation",
            variable == "extr_maxtemp" ~ "maximum_temperature",
            variable == "extr_wetbulbtemp" ~ "wetbulb_temperature",
            variable == "extr_fwi" ~ "fire_weather_index",
            variable == "extr_cdd" ~ "dry_days",
            variable == "extr_drought" ~ "palmer_drought_severity_index") -> variable_old
  
  c(name_data = variable_data, 
    name_old = variable_old)
}

