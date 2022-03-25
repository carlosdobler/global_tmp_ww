

# INPUT SECTION ------------------------------------------------------------------------------------

variable <- c("mean_temp",
              "mean_precip",
              "extr_precip",
              "extr_maxtemp",
              "extr_wetbulbtemp",
              "extr_fwi",
              "extr_cdd",
              "extr_drought")[1]

# ******************************************************************************

ifelse(variable == "extr_drought",
       "left",
       "right") -> tail_side

# related variable names
variable_data <- func_var_def(variable)["name_data"]
variable_old <- func_var_def(variable)["name_old"]

case_when(variable == "extr_wetbulbtemp" ~ "wetbulb_max",
          variable == "extr_cdd" ~ "cdd") -> variable_short

# tribble(
#   ~variable, ~variable_short,
#   "precipitation", "pr_max",
#   "maximum_temperature", "tasmax_max",
#   "maximum_wetbulb_temperature", "wetbulb_max",
#   "fire_weather_index", "fwi",
#   "dry_days", "cdd"
# )



# MASTER TABLE -----------------------------------------------------------------

if(variable_data == "precipitation"){
  
  tibble(file = str_glue("{dir_bucket_mine}/era/daily/precipitation/") %>%
           list.files()) %>% 
    
    mutate(t_i = file %>%
             {str_c(
               str_sub(., -10, -7),
               str_sub(., -5, -4),
               "01")},
           
           t_f = file %>%
             {str_c(
               str_sub(., -10, -7),
               str_sub(., -5, -4),
               days_in_month(as_date(t_i) %>%
                               str_pad(2, "left", "0")))}
           
    ) -> tb_files
  
} else if(variable == "extr_fwi"){
  
  str_glue("{dir_bucket_risk}/Reanalysis_data/ERA5_raw_data/daily/{variable}/") %>% 
    list.files() %>% 
    .[str_detect(., "zip")] %>% 
    .[c(6:9,5)] -> zips
  
  zips %>% 
    {str_glue("{dir_bucket_risk}/Reanalysis_data/ERA5_raw_data/daily/{variable}/{.}")} %>% 
    walk(unzip, exdir = str_glue("{dir_disk}/fwi"))
  
  tibble(file = str_glue("{dir_disk}/fwi") %>%
           list.files()) %>% 
    
    mutate(t_i = file %>%
             str_sub(., 15, 22)) %>% 
    
    filter(year(as_date(t_i)) %in% seq(2000,2020)) -> tb_files
  
  
} else {
  
  tibble(file = str_glue("{dir_bucket_risk}/Reanalysis_data/ERA5_raw_data/daily/{variable_data}/") %>% 
           list.files() %>% 
           .[str_detect(., str_flatten(seq(2000, 2020), collapse = "|"))]) %>% 
    
    mutate(t_i = file %>%
             {str_c(
               str_sub(., -7, -4),
               "0101")},
           
           t_f = file %>%
             {str_c(
               str_sub(., -7, -4),
               "1231")}
           
    ) -> tb_files
  
}

# GLOBAL LAND MASK -------------------------------------------------------------

str_glue("{dir_data}/rast_nbr_era_res.tif") %>% 
  read_stars() %>% 
  setNames("CODE") -> rast_nbr_era


# DOWNLOAD + IMPORT + AGGREGATE -------------------------------------------------------------------

plan(multicore, workers = availableCores() - 1)

if(variable_data == "precipitation"){
  # * PRECIPITATION ----
  
  map(seq(2000,2020), function(yr){
    
    # ***********
    
    # DOWNLOAD FILES
    
    plan(multicore, workers = availableCores() - 1)
    
    dir.create(str_glue("{dir_disk}/{yr}/"))
    dir_down_temp <- str_glue("{dir_disk}/{yr}/")
    
    tb_files %>%
      filter(str_detect(file, "pr"), # variable
             year(as_date(t_i)) == yr) -> files_to_down
    
    print(str_glue(" "))
    print(str_glue("Downloading files:"))
    
    files_to_down %>%
      mutate(across(starts_with("t_"), ~as_date(.x))) %>%
      print(n = nrow(files_to_down))
    
    files_to_down %>%
      pull(file) -> files_to_down
    
    tic(str_glue("Files downloaded"))
    
    str_glue("{dir_bucket_mine}/era/daily/precipitation/{files_to_down}") %>%
      future_walk(file.copy, dir_down_temp)
    
    str_glue("")
    
    toc()
    
    # **********
    
    # IMPORT
    
    print(str_glue("Importing ..."))
    tic("Done")
    
    # import nc files
    str_glue("{dir_down_temp}/{files_to_down}") %>%
      map(~read_ncdf(.x) %>%
            suppressMessages()) %>%
      {do.call(c, c(., along = 3))} %>%
      
      st_warp(st_as_stars(st_bbox(), dx = 0.25, dy = 0.25)) -> s
    
    toc()
    
    # **********
    
    # AGGREGATE
    
    print(str_glue("Aggregating ..."))
    tic("Done")
    
    if(str_detect(variable, "mean")){
      # mean variables
      s %>%
        aggregate(by = "months", mean, na.rm = T) -> s
      
    } else if(str_detect(variable, "precip")){
      # extreme precip
      s %>%
        aggregate(by = "months", max, na.rm = T) -> s
      
    } else if(str_detect(variable, "cdd")){
      # dry days
      s %>% 
        aggregate(by = "months",
                  FUN = function(y) sum(y < 0.001)) -> s
    }
    
    
    if(names(st_dimensions(s))[1] == "time"){
      s %>% 
        aperm(c(2,3,1)) -> s
    }
    
    toc()
    
    # ***********
    
    # MASK
    print(str_glue("Masking ..."))
    tic("Done")
    s[is.na(rast_nbr_era)] <- NA
    toc()
    
    unlink(dir_down_temp, recursive = T)
    
    return(s)
    
  }) -> s_result
  
  
} else {
  # * OTHER VARS ----
  
  # DOWNLOAD FILES
  # not needed for fwi
  
  if(variable != "extr_fwi"){
    
    dir.create(str_glue("{dir_disk}/{variable}/"))
    dir_down_temp <- str_glue("{dir_disk}/{variable}/")
    
    print(str_glue("Downloading files:"))
    
    tb_files %>%
      mutate(across(starts_with("t_"), ~as_date(.x))) %>%
      print(n = nrow(tb_files))
    
    tic(str_glue("Files downloaded"))
    
    str_glue("{dir_bucket_risk}/Reanalysis_data/ERA5_raw_data/daily/{variable_data}/{tb_files$file}") %>% 
      future_walk(file.copy, dir_down_temp)
    
    toc()
    
  }
  
  
  map(seq(2000,2020), function(yr){
      
    print(str_glue("Processing {yr} ********** "))
    
    # ************
    
    # IMPORT 
    print(str_glue("Importing ..."))
    tic("Done")
    
    if(variable == "extr_fwi"){
      
      tb_files %>% 
        filter(year(as_date(t_i)) == yr) %>% 
        pull(file) %>%
        map(~read_ncdf(str_glue("{dir_disk}/fwi/{.x}")) %>% 
              suppressMessages()) %>% 
        {do.call(c, c(., along = 3))} %>% 
        st_set_dimensions("time", 
                          values = seq(as_date(str_glue("{yr}-01-01")), as_date(str_glue("{yr+1}-01-01")), by = "1 day") %>% .[-length(.)]) %>% 
        
        st_warp(st_as_stars(st_bbox(), dx = 0.25, dy = 0.25)) -> s
      
    } else {
      
      str_glue("{dir_disk}/{variable}/") %>%
        list.files(full.names = T) %>% 
        .[str_detect(., as.character(yr))] %>% 
        
        read_ncdf() %>% 
        suppressMessages() %>% 
        
        st_warp(st_as_stars(st_bbox(), dx = 0.25, dy = 0.25)) %>% 
        
        setNames("var") -> s
      
      tb_files %>% 
        filter(year(as_date(t_i)) == yr) %>% 
        {seq(as_date(.$t_i), as_date(.$t_f), by = "1 day")} -> date_vector
      
      s %>% 
        st_set_dimensions("time", values = date_vector) -> s
      
      if(variable == "extr_maxtemp" | variable == "mean_temp"){
        s %>% 
          mutate(var = var %>% units::set_units(degC)) -> s
      }
    }
    toc()
    
    # ***********
    
    # AGGREGATE 
    
    print(str_glue("Aggregating ..."))
    tic("Done")
    
    if(str_detect(variable, "mean")){
      # mean temp
      s %>%
        aggregate(by = "months", mean, na.rm = T) -> s
      
    } else if(str_detect(variable, "precip")){
      # extreme vars
      s %>%
        aggregate(by = "months", max, na.rm = T) %>% 
        mutate(var = ifelse(is.infinite(var), NA, var)) -> s # necessary?
    }
    
    if(names(st_dimensions(s))[1] == "time"){
      s %>% 
        aperm(c(2,3,1)) -> s
    }
    
    toc()
    
    # ***********
    
    # MASK
    print(str_glue("Masking ..."))
    tic("Done")
    s[is.na(rast_nbr_era)] <- NA
    toc()
    
    return(s)
    
    }) -> s_result
    
  if(variable == "extr_fwi"){
    unlink(str_glue("{dir_disk}/fwi"), recursive = T)
  } else {
    unlink(dir_down_temp, recursive = T)
  }
  
}

saveRDS(s_result, str_glue("{dir_output}/s_era_{variable}_pre.RDS")) # old files do not have "pre"
# s_result <- readRDS(str_glue("{dir_output}/s_era_{variable}_pre.RDS"))


# PROCESS MEAN CONDITIONS -------------------------------------------------------------------------

plan(multicore, workers = availableCores() - 1)

if(str_detect(variable, "mean")){
  
  map(seq_len(12), function(mth){
    
    print(str_glue("Processing {month.abb[mth]}"))
    
    s_result %>% 
      map(slice, time, mth) %>%
      do.call(c, .) %>% 
      merge(name = "time") -> s_month

    s_month %>% 
      st_apply(c(1,2),
               FUTURE = T,
               rename = F,
               FUN = mean, na.rm = T) %>%
      setNames(month.abb[mth])
    
  }) %>%
    do.call(c, .) %>%
    merge(name = "time") -> s_mean
  
  saveRDS(s_mean, str_glue("{dir_output}/s_era_{variable}_levels.RDS"))
  
}








# LEVELS -------------------------------------------------------------------------------------------

source("scripts/01-functions.R")

plan(multicore, workers = availableCores() - 1)


if(variable != "extr_cdd"){
  
  c("lmom", "maxl") %>% 
    map(function(gev_meth){
      
      seq_len(12) %>% 
        # .[c(1,5,9)] %>%                                                             # DELETE !!!
        map(function(mth){
          
          print(str_glue("Processing month {mth} / 12"))
          tic("Done")
          
          s_result %>% 
            map(slice, time, mth) %>%
            do.call(c, .) %>% 
            merge(name = "time") -> s_month
          
          s_month %>% 
            st_apply(c("x", "y"),
                     str_glue("func_level_{gev_meth}"),
                     FUTURE = T,
                     .fname = "func") %>% 
            setNames(month.abb[mth]) -> s_month
          
          toc()
          
          return(s_month)
          
        }) %>% 
        do.call(c, .) %>% 
        merge(name = "time") %>%
        setNames(gev_meth)
      
    }) %>% 
    do.call(c, .) %>% 
    merge(name = "gev_method") -> s_month_levels
  
  
# *************
  
# CDD 

} else {
  
  seq_len(12) %>% 
    map(function(mth){
      
      print(str_glue("Processing month {mth} / 12"))
      tic("Done")
      
      s_result %>% 
        map(slice, time, mth) %>%
        do.call(c, .) %>% 
        merge(name = "time") -> s_month
      
      str_glue("{seq(2000, 2020)}-{mth}-01") %>% 
        as_date() %>% 
        days_in_month() -> days_size
      
      s_month %>% 
        st_apply(c("x", "y"),
                 func_binom_level_maxl, md = 2, days_size = days_size,
                 FUTURE = T,
                 .fname = "func") %>% 
        setNames(month.abb[mth]) -> s_month
      
      toc()
      
      return(s_month)
      
    }) %>% 
    do.call(c, .) %>% 
    merge(name = "time") -> s_month_levels
  
}

saveRDS(s_month_levels, str_glue("{dir_output}/s_era_{variable}_levels.RDS"))
# s_month_levels <- readRDS(str_glue("{dir_output}/s_era_{variable}_levels.RDS"))



# EXCEEDANCE ------------------------------------------------------------------ 

plan(multicore, workers = availableCores() - 1)

if(variable != "extr_cdd"){
  
  # 
  
} else {
  
  map(seq_len(12), function(mth){
    
    print(str_glue("Processing month {mth} / 12"))
    tic("Done")
    
    s_month_levels %>%
      filter(func == "val") %>%
      filter(time == month.abb[mth]) %>%
      adrop() -> s_level_deg1_mm
    
    s_result %>% 
      map(slice, time, mth) %>%
      do.call(c, .) %>% 
      c(s_level_deg1_mm) %>% 
      merge() -> s_month_lev
    
    str_glue("{seq(2000, 2020)}-{mth}-01") %>% 
      as_date() %>% 
      days_in_month() -> days_size
    
    s_month_lev %>%   
      st_apply(c("x","y"),
               func_binom_exceed_maxl, md = 2, days_size = days_size,
               FUTURE = TRUE,
               future.seed = NULL) %>%
      
      setNames(month.abb[mth])
    
  }) %>%
    do.call(c, .) %>%
    merge(name = "time") -> s_month_exceed
  
  
}

# saveRDS(s_month_exceed, str_glue("{dir_output}/s_era_{variable}_exceed.RDS"))
# s_month_exceed <- readRDS(str_glue("{dir_output}/s_era_{variable}_exceed.RDS"))


# NBR STATISTICS ---------------------------------------------------------------

# Table of n of cells per nbr
rast_nbr_era %>% 
  as_tibble() %>%
  filter(!is.na(CODE)) %>% 
  group_by(CODE) %>% 
  count() -> tb_total_n_nbr

# Vector of ordered column names
list(
  as.list(str_c("q", seq(0,1,0.2))),
  as.list(c(str_c("n_", 1:5, "q"), "perc_cov"))
) %>% 
  transpose() %>% 
  
  map(function(i){
    
    month.abb %>%
      map(~str_c(.x,
                 "_",
                 c(i[[1]],
                   i[[2]])))
    
  }) %>% 
  transpose() %>% 
  flatten() %>% 
  unlist() -> vector_names



if(str_detect(variable, "mean")){
  # * MEAN CONDITIONS -----
  
  map_dfr(month.abb, function(mth){
    
    print(str_glue("Processing month {mth} ..."))
    
    s_mean %>% 
      filter(time == mth) %>% 
      adrop() %>% 
      c(rast_nbr_era, along = list(foo = c("v", "CODE"))) %>% 
      split("foo") %>% 
      as_tibble() %>% 
      filter(!is.na(v)) -> tb_1
    
    func_nbrstats(tb_1, tb_total_n_nbr) %>% 
      mutate(month = mth)
    
  }) -> tb
  
  # necessary?
  # tb %>% 
  #   filter(quintile == "perc_cov") %>% 
  #   group_by(CODE) %>% 
  #   summarize(value = sum(value)) %>% 
  #   filter(value == 0) %>% 
  #   pull(CODE) -> nbr_wo_dom
  # 
  # tb %>% 
  #   filter(!CODE %in% nbr_wo_dom) -> tb
  
  tb %>%
    left_join(tb_total_n_nbr, by = "CODE") %>% 
    pivot_wider(names_from = c(month, quintile), values_from = value) %>%
    arrange(CODE) %>% 
    select(CODE, n, all_of(vector_names)) -> tb_f
  

}



# non cdd !!!
s_month_levels %>%
  slice(func, 1) %>%
  slice(gev_method, 1) -> s_month_exceed

# cdd !!!
s_month_levels %>%
  slice(func, 1) -> s_month_exceed

s_month_exceed[!is.na(s_month_exceed)] <- 0.05

# ***********

rast_nbr_era %>% 
  as_tibble() %>%
  filter(!is.na(CODE)) %>% 
  group_by(CODE) %>% 
  count() -> tb_total_n_bin

s_month_levels %>% 
  slice(func, 1) %>%
  # slice(gev_method, 1) %>% 
  {c(., s_month_exceed)} %>% 
  setNames(c("lev", "exc")) %>% 
  merge(name = "var") -> guau

# saveRDS(guau, str_glue("output/starr_{variable}_era.RDS"))

# *********
# Result table

# plan(multicore, workers = availableCores() - 1)

map_df(c("lev", "exc"), function(lay){
  
  map_df(month.abb, function(mth){
    
    print(str_glue("Processing {lay} - {mth} ..."))
    
    # Table with nbr codes
    guau %>%
      filter(var == lay,
             time == mth) %>% 
      adrop() %>% 
      c(rast_nbr_era, along = list(foo = c("v", "CODE"))) %>% 
      split("foo") %>% 
      as_tibble() %>% 
      filter(!is.na(v)) -> tb_1
    
    # Table of n/bins
    tb_1 %>%
      # mutate(c = CODE) %>% # COMMENT
      group_by(CODE) %>% 
      nest() %>%
      mutate(
        quintile = map(data, function(df){
          
          # print(first(df$c))
          
          # not enough cells to obtain unique bins or
          # all cells of same value (exceedance in 1 deg)
          if(nrow(df) < 5 | near(mean(df$v), 0.05, 0.009)){
            
            tibble(quintile = str_c("n_", 1:5, "q"),
                   value = nrow(df)/5)
            
          } else if(any(df$v %>% quantile(seq(0,1,0.2), na.rm = T) %>% duplicated() == T)){
            
            df %>% 
              summarize(c = first(CODE)) %>% 
              pull(c) %>% 
              {print(str_glue("Duplicate: {.}"))}
            
            tibble(quintile = str_c("n_", 1:5, "q"),
                   value = df %>% nrow() %>% {./5})
            
          } else {
            
            df %>% 
              mutate(quintile = cut(v,
                                    breaks = quantile(v, probs = seq(0,1,0.2), na.rm = T),
                                    labels = str_c("n_", 1:5, "q"),
                                    include.lowest = T)) %>%
              group_by(quintile) %>% 
              count(quintile, name = "value") %>%
              ungroup()
            
          }
          
        })
      ) %>% 
      unnest(quintile) %>% 
      select(-data) %>% 
      ungroup() -> tb_2_1
    
    # Table of percent of nbr covered
    tb_2_1 %>% 
      group_by(CODE) %>% 
      summarize(value = sum(value)) %>% 
      right_join(tb_total_n_bin, by = "CODE") %>%
      mutate(value = ifelse(is.na(value), 0, value)) %>% 
      mutate(value = value/n*100,
             quintile = "perc_cov") %>%
      select(CODE, quintile, value) -> tb_2_2
    
    # Joined tables
    bind_rows(tb_2_1, tb_2_2) %>% 
      arrange(CODE, quintile) -> tb_2
    
    # Table of quintile levels
    tb_1 %>% 
      group_by(CODE) %>%
      summarize(value = quantile(v, probs = seq(0,1,0.2), na.rm = T),
                quintile = str_c("q", seq(0,1,0.2))) %>%
      suppressMessages() %>% 
      ungroup() -> tb_3
    
    bind_rows(tb_2, tb_3) %>%
      
      mutate(month = mth,
             var = lay)
    
  }) # end of month loop
  
}) -> tb

tb %>% 
  filter(quintile == "perc_cov",
         var == "lev") %>% 
  group_by(CODE) %>% 
  summarize(value = sum(value)) %>% 
  filter(value == 0) %>% 
  pull(CODE) -> nbr_wo_dom

tb %>% 
  filter(!CODE %in% nbr_wo_dom) -> tb

# tb %>% 
#   mutate(value = ifelse(var == "exc" & str_detect(quintile, "q(0|1)"), round(value, 2), value)) -> tb


# Vector of ordered column names
list(
  as.list(str_c("q", seq(0,1,0.2))),
  as.list(c(str_c("n_", 1:5, "q"), "perc_cov"))
) %>% 
  transpose() %>% 
  
  map(function(i){
    
    month.abb %>%
      map(~str_c(.x,
                 "_",
                 c(i[[1]],
                   i[[2]])))
    
  }) %>% 
  transpose() %>% 
  flatten() %>% 
  unlist() -> vector_names

tb %>%
  left_join(tb_total_n_bin, by = "CODE") %>% 
  pivot_wider(names_from = c(month, quintile), values_from = value) %>%
  arrange(CODE) %>% 
  select(var, CODE, n, all_of(vector_names)) -> tb_f



# ********

write_csv(tb_f, str_glue("{dir_output}/tbl_{variable}_era.csv"))




#

system("gcloud compute instances stop cd-ubuntu-1 --zone us-east1-c")
