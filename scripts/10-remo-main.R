
# Master script to process REMO data and obtain extremes

# **************************************************************************************************

# CHOOSE VARIABLE !!!

variable <- c("mean_temp",
              "mean_precip",
              "extr_precip",
              "extr_maxtemp",
              "extr_wetbulbtemp",
              "extr_fwi",
              "extr_cdd",
              "extr_drought")[1]

# ******************************************************************************

# related variable names
variable_data <- func_var_def(variable)["name_data"]
variable_old <- func_var_def(variable)["name_old"]


# function to get time
func_get_time <- function()
{
  Sys.time() %>% 
    with_tz("America/Mexico_City") %>% 
    str_sub(12)
}

# *************************************************************************************************

for(domain in c(
  "CAM",
  "AUS",
  "EUR",
  "WAS",
  "SEA",
  "SAM",
  "EAS",
  "CAS",
  "NAM",
  "AFR"
)){
  
  print(str_glue(" "))
  print(str_glue(" "))
  print(str_glue("************ [{func_get_time()}] {domain} STARTED ************"))
  tic(str_glue("{domain} DONE"))
  
  source("scripts/11-remo-setup.R", echo = F, print.eval = T)
  source("scripts/12-remo-download.R", echo = F, print.eval = T)
  source("scripts/13-0-remo-process.R", echo = F, print.eval = T)
  
  toc()
  
}

# *************************************************************************************************

print(str_glue(" "))
tic(str_glue("{variable} DONE"))
source("scripts/14-remo-mosaic-ens-stats.R", echo = F, print.eval = T)
toc()

# *************************************************************************************************

# TURN OFF VM
system("gcloud compute instances stop cd-ubuntu-1 --zone us-east1-c")
