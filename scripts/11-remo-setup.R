
# PARAMETERS

if(variable == "extr_wetbulbtemp"){
  
  temp_res <- "daily"
  tail_side <- "right"
  pos_model <- 4
  pos_date <- 5
  
} else if(variable %in% c("extr_maxtemp",
                          "extr_precip",
                          "mean_precip",
                          "mean_temp",
                          "extr_cdd")){
  
  temp_res <- "daily"
  tail_side <- "right"
  pos_model <- 3
  pos_date <- 9
  
} else if(variable == "extr_drought"){
  
  temp_res <- "monthly"
  tail_side <- "left"
  pos_model <- 3
  
} else if(variable == "extr_fwi"){
  
  temp_res <- "daily"
  tail_side <- "right"
  pos_model <- 3
  pos_date <- 6
  
}

# *************************************************************************************************

# TABLE OF FILES AND DATES

if(variable_data == "precipitation" & domain == "AFR"){
  
  tibble(file = str_glue("{dir_bucket_risk}RCM_regridded_data/REMO2015/{domain}/daily/{variable_data}/") %>%
           list.files()) %>%
    mutate(
      model = file %>%
        str_split("_", simplify = T) %>%
        .[ , pos_model],
      
      t_i = file %>%
        str_split("_", simplify = T) %>%
        .[ , pos_date],
      
      t_res = file %>%
        str_split("_", simplify = T) %>%
        .[ , 8]
      
    ) %>% 
    
    filter(t_res == "day",
           str_detect(t_i, "-", negate = T)) %>%
    
    select(-t_res) %>% 
    
    mutate(t_i = parse_number(t_i) %>% 
             str_c(., "0101"),
           
           t_f = year(as_date(t_i)) %>% 
             str_c(., "1231")
           
    ) -> tb_files
  
  # **********
  
} else if(str_detect(variable_data, "palmer")){
  
  tibble(file = str_glue("{dir_bucket_risk}RCM_regridded_data/REMO2015/{domain}/monthly/{variable_data}/") %>%
           list.files()) %>%
    mutate(
      model = file %>%
        str_split("_", simplify = T) %>%
        .[ , pos_model],
      
      t_i = file %>%
        str_split("_", simplify = T) %>%
        .[ , 4],
      
      t_f = file %>%
        str_split("_", simplify = T) %>%
        .[ , 5] %>%
        str_sub(end = 4)
      
    ) -> tb_files
  
  # **********
  
} else {
  
  tibble(file = str_glue("{dir_bucket_risk}RCM_regridded_data/REMO2015/{domain}/daily/{variable_data}/") %>%
           list.files()) %>%
    mutate(
      model = file %>%
        str_split("_", simplify = T) %>%
        .[ , pos_model],
      
      t_i = file %>%
        str_split("_", simplify = T) %>%
        .[ , pos_date] %>%
        str_sub(end = 8),
      
      t_f = file %>%
        str_split("_", simplify = T) %>%
        .[ , pos_date] %>%
        str_sub(start = 10, end = 17)
      
    ) -> tb_files
  
  tb_files %>%
    filter(t_f != "") -> tb_files
  
}

# *************************************************************************************************

# vector of models
tb_files %>%
  pull(model) %>%
  unique() -> models

# table mid years
tibble(model = tb_files %>%
         pull(model) %>%
         unique() %>%
         rep(3),
       
       deg = rep(c("deg_1", "deg_1.5", "deg_2"), each = 3),
       
       mid_yr = c(2014, 2006, 2019,
                  2030, 2025, 2033,
                  2035, 2038, 2050)) -> tb_mid_yr
