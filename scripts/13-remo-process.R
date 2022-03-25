
print(str_glue(" "))
print(str_glue("[{func_get_time()}] Processing started"))

# Load functions
source("scripts/01-functions.R")



# MASK ------------------------------------------------------------------------

print(str_glue("Creating mask ..."))

read_stars(str_glue("{dir_data}/rast_nbr_dom_remo_res.tif")) -> rast_nbr_dom

c("SAM", "CAM", "NAM", "EUR", "AFR", "WAS", "EAS", "CAS", "AUS", "SEA") %>%
  sort() %>% 
  {which(. == domain)} -> dom_id

rast_nbr_dom %>% 
  slice(band, 2) %>% # domains 
  rename(dom = 1) %>%
  mutate(dom = ifelse(dom == dom_id, 1L, NA)) %>% 
  st_as_sf(as_points = F, merge = T) -> mask



# LIMITS ----------------------------------------------------------------------

print(str_glue("Setting up limits ..."))

list.files(dir_down_temp, full.names = T) %>% 
  .[1] %>% 
  read_ncdf(make_time = F,
            make_units = F,
            ncsub = cbind(start = c(1,1,1),
                          count = c(NA,NA,1))) %>%
  suppressMessages() %>% 
  adrop() -> s_proxy

mask %>%
  st_bbox() -> lim

# ggplot() +
#   geom_sf(data = mask, fill = "grey50") +
#   geom_sf(data = st_as_sfc(st_bbox(s_proxy)), fill = NA, linetype = 2) +
#   geom_sf(data = st_as_sfc(lim), fill = NA) -> p
# 
# ggsave(plot = p, filename = str_glue("output/lim_{domain}.png")) %>%
#   suppressMessages()

# *********

# lon
s_proxy %>% 
  st_get_dimension_values("lon") -> s_proxy_lon 

which.min(abs(s_proxy_lon - lim[1])) - 1 -> lon_start
if(lon_start == 0) lon_start <- 1
which.min(abs(s_proxy_lon - lim[3])) - lon_start + 1 -> lon_count

print(str_glue("Lon starts: {lon_start} / Lon count: {lon_count}"))

# **********

# lat
s_proxy %>% 
  st_get_dimension_values("lat") -> s_proxy_lat 

which.min(abs(s_proxy_lat - lim[2])) - 1 -> lat_start
which.min(abs(s_proxy_lat - lim[4])) - lat_start + 1 -> lat_count

print(str_glue("Lat starts: {lat_start} / Lat count: {lat_count}"))

rm(s_proxy, s_proxy_lon, s_proxy_lat, lim)



# LOOP ACROSS MODELS ----------------------------------------------------------

dir.create("temp/")

for(mod in seq_along(models)){
  
  print(str_glue(" "))
  print(str_glue("******** Processing {models[mod]} ********"))
  tic(str_glue("Model {models[mod]} processed")) # 1
  
  
  # IMPORT FILES + AGGREGATE --------------------------------------------------
  
  plan(multicore, workers = availableCores() - 1)
  
  tb_mid_yr %>% 
    filter(model == models[mod]) %>% 
    pull(mid_yr) %>% 
    map(~seq(.x - 10,
             .x + 10)) %>% 
    unlist() %>% 
    unique() -> all_years
  
  if(str_detect(variable_data, "palmer")) all_years <- c(all_years[1]-1, all_years)
  # (for rollmean)
  
  tb_files %>%
    filter(str_detect(file, models[mod])) %>% 
    filter(file %in% list.files(dir_down_temp)) -> tb_files_m
  
  print(str_glue(" "))
  print(str_glue("[{func_get_time()}] Importing files of {models[mod]} ..."))
  tic("Done importing") # 2
  
  tb_files_m %>% 
    mutate(r = row_number()) %>% 
    pmap(function(file, model, t_i, t_f, r){ # rowwise iteration
      
      # tb_files_m[[1,1]] -> file
      # tb_files_m[[1,2]] -> model
      # tb_files_m[[1,3]] -> t_i
      # tb_files_m[[1,4]] -> t_f
      
      print(str_glue("Importing file {r} / {nrow(tb_files_m)}"))
      
      # CREATE DATES VECTOR
      if(temp_res == "monthly"){ # palmer
        
        seq(as_date(str_c(t_i, "-01-01")),
            as_date(str_c(t_f, "-12-01")),
            by = "month") -> date_vector
        
      } else if(temp_res == "daily"){
        
        func_dates(model, t_i, t_f) %>% 
          suppressMessages() -> date_vector
        
      }
      
      
      # READ FILES
      read_ncdf(str_glue("{dir_down_temp}/{file}"),
                make_time = F,
                ncsub = cbind(start = c(lon_start,
                                        lat_start,
                                        1),
                              count = c(lon_count,
                                        lat_count,
                                        NA))) %>%
        suppressMessages() %>%
        setNames("var") %>%
        st_set_dimensions(3, names = "time", values = date_vector) %>%
        filter(year(time) >= first(all_years),
               year(time) <= last(all_years)) -> s
      
      
      # REGULARIZE GRID
      # lon
      s %>%
        st_get_dimension_values("lon") %>%
        {c(first(.), last(.))} %>%
        round(1) -> limaxis
      
      limaxis[1] %>% 
        {abs(. - trunc(.))*10} %>% 
        round() -> limaxis_i
      
      if(limaxis_i %% 2 == 0){ # shift if even
        limaxis - 0.1 -> limaxis
      }
      
      limaxis %>% 
        {seq(.[1], .[2], 0.2)} %>%
        {st_set_dimensions(s, "lon", values = .)} -> s
      
      # lat
      s %>%
        st_get_dimension_values("lat") %>%
        {c(first(.), last(.))} %>%
        round(1) -> limaxis
      
      limaxis[1] %>% 
        {abs(. - trunc(.))*10} %>% 
        round() -> limaxis_i
      
      if(limaxis_i %% 2 == 0){
        limaxis - 0.1 -> limaxis
      }
      
      limaxis %>%   
        {seq(.[1], .[2], 0.2)} %>%
        {st_set_dimensions(s, "lat", values = .)} -> s
      
      # crs
      s %>%
        st_set_crs(4326) -> s
      
      
      # CONVERT UNITS 
      # precip to mm/day
      if(variable_data == "precipitation"){
        s %>%
          mutate(var = var %>% units::set_units(kg/m^2/day)) -> s
        # (equal to x*60*60*24)
        
        # temp to C  
      } else if(variable_data == "maximum_temperature" |
                variable_data == "average_temperature") {
        s %>% 
          mutate(var = var %>% units::set_units(degC)) -> s
        # (equal to x-273.15)
      }
      
      
      # ROLLAPPLY
      # only wetbulb: 3-day heat wave
      if(str_detect(variable, "wetbulb")){
        print(str_glue("Rollapplying ..."))
        
        s %>% 
          st_get_dimension_values("time") -> date_vector_sub
        
        s %>%
          st_apply(c("lon", "lat"),
                   FUTURE = T,
                   rename = F,
                   .fname = "time",
                   function(x){
                     
                     zoo::rollmean(x,
                                   k = 3,
                                   na.rm = T,
                                   align = "right",
                                   fill = NA)
                     
                     
                   }) -> s
        
        s %>% 
          st_set_dimensions("time",
                            values = date_vector_sub) -> s
        
      }
      
      
      # AGGREGATE
      # monthly 1-day maximas:
      if(str_detect(variable, "mean", negate = T) &
         str_detect(variable_data, "palmer", negate = T)){
        print(str_glue("Aggregating ..."))
        
        # all vars expect pdsi and dry days:
        if(temp_res == "daily" & variable != "extr_cdd"){
          
          # failed attempt to parallelize (too slow):
          # s %>%
          #   st_get_dimension_values("lat") %>%
          #   seq_along() %>%
          #   split(cut(., 5, labels = F)) -> lat_index
          # 
          # lat_index %>%
          #   map(~slice(s, lat, first(.x):last(.x))) %>%
          #   future_map(~aggregate(.x, by = "months", max, na.rm = T) %>%
          #                mutate(var = ifelse(is.infinite(var), NA, var)) %>%
          #                aperm(c(2,3,1))) -> s_sliced_agg
          # 
          # s_sliced_agg %>%
          #   map(~.x %>% as("Raster")) %>%
          #   unname() %>%
          #   do.call(terra::merge, .) %>%
          #   st_as_stars() %>%
          #   st_set_dimensions("band",
          #                     names = "time",
          #                     values = st_get_dimension_values(s_sliced_agg[[1]], "time")) -> s
          
          s %>%
            aggregate(by = "months", max, na.rm = T) %>%
            mutate(var = ifelse(is.infinite(var), NA, var)) -> s
          
          # dry days (count):
        } else if(temp_res == "daily" & variable == "extr_cdd"){
          
          s %>% 
            aggregate(by = "months",
                      FUN = function(y) sum(y < 0.001)) -> s
          
        }
        
        # monthly daily means:  
      } else if(str_detect(variable_data, "palmer", negate = T)){
        print(str_glue("Aggregating ..."))
        
        s %>% 
          aggregate(by = "months", mean, na.rm = T) %>% 
          mutate(var = ifelse(is.infinite(var), NA, var)) -> s
        
      }
      
      return(s)
      
    }) -> s # end of pmap; list stars objects
  
  
  if(length(s) == 1){ # palmer
    s[[1]] -> s
  } else {
    s %>% 
      do.call(c, .) -> s
    # single stars object
  }
  
  if(names(st_dimensions(s))[1] == "time"){
    s %>% 
      aperm(c(2,3,1)) -> s
    # rearrange dimensions (for masking)
  }
  
  
  # MASK STARS
  print(str_glue("Masking ..."))
  s[mask] -> s
  
  
  # ROLLAPPLY
  # only palmer
  if(str_detect(variable_data, "palmer")){
    print(str_glue("Rollapplying..."))
    
    s %>%
      mutate(var = case_when(var < -10 ~ -10,
                             var > 10 ~ 10,
                             TRUE ~ var)) %>% 
      st_apply(c("lon", "lat"),
               FUTURE = T,
               rename = F,
               .fname = "time",
               function(x){
                 
                 zoo::rollmean(x,
                               k = 3,
                               na.rm = T,
                               align = "right",
                               fill = NA)
                 
               }) -> s
    
    s %>% 
      st_set_dimensions("time",
                        values = seq(as_date(str_glue("{first(all_years)}0101")), 
                                     as_date(str_glue("{last(all_years)}1201")), 
                                     by = "month")) -> s
    
  }
  
  toc() # 2
  
  print(str_glue(" "))
  
  
  
  
  # CALCULATE MEANS ------------------------------------------------------------
  
  # plan(multicore, workers = availableCores() - 1)
  # 
  # tb_mid_yr %>% 
  #   pull(deg) %>% 
  #   unique() %>%
  #   # .[c(1,3)] %>%                                                               # DELETE !!!
  #   
  #   # obtain 95th percentiles
  #   map(function(d){
  #     
  #     print(str_glue("[{func_get_time()}] Processing levels: {d} ..."))
  #     tic("Done") # 3
  #     
  #     tb_mid_yr %>% 
  #       filter(model == models[mod],
  #              deg == d) %>% 
  #       pull(mid_yr) %>% 
  #       {c(. - 10, . + 10)} -> years_lim
  #     
  #     s %>% 
  #       filter(year(time) >= years_lim[1],
  #              year(time) <= years_lim[2]) -> s_deg
  #     
  #     map(seq_len(12), function(mth){
  #       
  #       s_deg %>% 
  #         filter(month(time) == mth) %>% 
  #         st_apply(c(1,2),
  #                  FUTURE = T,
  #                  rename = F,
  #                  FUN = mean, na.rm = T) %>% 
  #         setNames(month.abb[mth])
  #       
  #     }) %>% 
  #       do.call(c, .) %>% 
  #       merge(name = "time") %>% 
  #       setNames(d)
  #     
  #   }) %>% 
  #   do.call(c, .) %>% 
  #   merge(name = "deg") -> s_mean
  
  
  # CALCULATE LEVELS -----------------------------------------------------------

  plan(multicore, workers = availableCores() - 1)
  
  # LOOP THROUGH DEGREES
  map(unique(tb_mid_yr$deg), function(d){
    
    print(str_glue("[{func_get_time()}] Processing levels: {d} ..."))
    tic("Done") # 3
    
    tb_mid_yr %>%
      filter(model == models[mod],
             deg == d) %>%
      pull(mid_yr) %>%
      {c(. - 10, . + 10)} -> years_lim
    
    s %>%
      filter(year(time) >= years_lim[1],
             year(time) <= years_lim[2]) -> s_deg
    
    # **********
    # DROUGHT
    if(variable == "extr_drought"){
      
      set.seed(123)
      s_deg %>% 
        mutate(var = jitter(var)) -> s_deg
      
      # loop through months
      map(seq_len(12), function(mth){
        
        print(str_glue("Processing {month.abb[mth]} ..."))
        
        s_deg %>%
          filter(month(time) == mth) %>%
          
          st_apply(c("lon","lat"),
                   quantile, prob = 0.05, type = 4, na.rm = T,
                   FUTURE = TRUE,
                   .fname = "func") %>%
          
          setNames(month.abb[mth])
        
      }) %>% 
        do.call(c, .) %>%
        merge(name = "time") %>% 
        setNames(d) -> quz
      
     
    # **********
    # DRY DAYS   
    } else if(variable == "extr_cdd"){
      
      # loop through months
      map(seq_len(12), function(mth){
        
        print(str_glue("Processing {month.abb[mth]} ..."))
        
        s_deg %>%
          filter(month(time) == mth) -> s_deg_mth
        
        st_get_dimension_values(s_deg_mth, "time") %>% 
          days_in_month() -> days_size
        
        s_deg_mth %>% 
          st_apply(c("lon","lat"),
                   func_binom_level_maxl, md = mod, days_size = days_size,
                   FUTURE = TRUE,
                   future.seed = NULL,
                   .fname = "func") %>%
          
          setNames(month.abb[mth]) 
        
      }) %>%
        do.call(c, .) %>%
        merge(name = "time") %>%
        setNames(d) -> quz
    
        
    # **********
    # ALL OTHER VARS
    } else {
      
      # Loop across GEV methods
      c("lmom", "maxl") %>%
        map(function(gev_meth){
          
          # loop months
          seq_len(12) %>%
            # .[c(1,5,9)] %>%                                                     # DELETE !!!
            map(function(mth){
              
              print(str_glue("Processing {month.abb[mth]} ({gev_meth}) ..."))
              
              s_deg %>%
                filter(month(time) == mth) %>%
                
                st_apply(c("lon","lat"),
                         str_glue("func_gev_level_{gev_meth}"),
                         FUTURE = TRUE,
                         .fname = "func") %>%
                
                setNames(month.abb[mth])
              
            }) %>%
            do.call(c, .) %>%
            merge(name = "time") %>%
            setNames(gev_meth)
          
        }) %>%
        do.call(c, .) %>%
        merge(name = "gev_method") %>%
        setNames(d) -> quz
      
    }
    
    toc() # 3
    print(str_glue(" "))
    
    return(quz)
  
  }) %>%
    do.call(c, .) %>%
    merge(name = "deg") -> s_level
  
  
  

  # CALCULATE EXCEEDANCE PROBS --------------------------------------------------------------------

  plan(multicore, workers = availableCores() - 1)
  
  # LOOP THROUGH DEGREES
  map(unique(tb_mid_yr$deg), function(d){
    
    print(str_glue("[{func_get_time()}] Processing exceedance: {d} ..."))
    tic("Done") # 4
    
    tb_mid_yr %>%
      filter(model == models[mod],
             deg == d) %>%
      pull(mid_yr) %>%
      {c(. - 10, . + 10)} -> years_lim
    
    s %>%
      filter(year(time) >= years_lim[1],
             year(time) <= years_lim[2]) -> s_deg
    
    
    # **********
    # DROUGHT
    if(variable == "extr_drought"){
      
      set.seed(123)
      s_deg %>% 
        mutate(var = jitter(var)) -> s_deg
      
      # loop through months
      map(seq_len(12), function(mth){
        
        print(str_glue("Processing {month.abb[mth]} ..."))
        
        s_level %>%
          filter(deg == "deg_1") %>%
          filter(time == month.abb[mth]) %>%
          adrop() -> s_level_deg1
        
        s_deg %>%
          filter(month(time) == mth) %>%
          split("time") %>%
          c(s_level_deg1) %>%
          merge() %>%
          
          st_apply(c("lon","lat"),
                   FUN = function(x){
                     
                     last(x) -> x_lev
                     x[seq_len(length(x)-1)] -> x_val
                     
                     if(is.na(x_lev) | sum(!is.na(x_val)) == 0){
                       NA
                     } else {
                     
                     ecdf(x_val)(x_lev) %>% 
                       round(2)
                     
                     }
                   },
                   
                   FUTURE = TRUE,
                   .fname = "func") %>%
          
          setNames(month.abb[mth])
        
      }) %>%
        do.call(c, .) %>%
        merge(name = "time") %>%
        setNames(d) -> quz
      
    
    # **********
    # DRY DAYS
    } else if(variable == "extr_cdd"){
      
      # loop through months
      map(seq_len(12), function(mth){
        
        print(str_glue("Processing {month.abb[mth]} ..."))
        
        s_level %>%
          filter(func == "val") %>%
          filter(deg == "deg_1") %>%
          filter(time == month.abb[mth]) %>%
          adrop() -> s_level_deg1_mm
        
        s_deg %>%
          filter(month(time) == mth) %>%
          # mutate(var = as.double(var)) %>%
          split("time") %>%
          c(s_level_deg1_mm) %>%
          merge() -> s_deg_mth_lev
        
        s_deg_mth_lev %>% 
          st_get_dimension_values(3) %>%
          .[-length(.)] %>% 
          days_in_month() -> days_size
        
        s_deg_mth_lev %>%   
          st_apply(c("lon","lat"),
                   func_binom_exceed_maxl, md = mod, days_size = days_size,
                   FUTURE = TRUE,
                   future.seed = NULL) %>%
          
          setNames(month.abb[mth])
        
      }) %>%
        do.call(c, .) %>%
        merge(name = "time") %>%
        setNames(d) -> quz
      
      
    # **********
    # OTHER VARS
    } else {
      
      # Loop across GEV methods
      c("lmom", "maxl") %>%
        map(function(gev_meth){
          
          # loop months
          seq_len(12) %>%
            # .[c(1,5,9)] %>%                                                     # DELETE !!!
            
            map(function(mth){
              
              print(str_glue("Processing {month.abb[mth]} ({gev_meth}) ..."))
              
              s_level %>%
                filter(gev_method == gev_meth) %>%
                filter(func == "val") %>%
                filter(deg == "deg_1") %>%
                filter(time == month.abb[mth]) %>%
                adrop() -> s_level_deg1_mm
              
              s_deg %>%
                filter(month(time) == mth) %>%
                split("time") %>%
                c(s_level_deg1_mm) %>%
                merge() %>%
                
                st_apply(c("lon","lat"),
                         str_glue("func_gev_exceed_{gev_meth}"),
                         FUTURE = TRUE,
                         .fname = "func") %>%
                
                setNames(month.abb[mth])
              
            }) %>%
            do.call(c, .) %>%
            merge(name = "time") %>%
            setNames(gev_meth)
          
        }) %>%
        do.call(c, .) %>%
        merge(name = "gev_method") %>%
        setNames(d) -> quz
      
    }
    
    toc() # 4
    print(str_glue(" "))
    
    return(quz)
    
  }) %>%
    do.call(c, .) %>%
    merge(name = "deg") -> s_exceed

  
  

  # ASSEMBLE + SAVE RESULTS -----------------------------------------------------
  print(str_glue("Assembling ..."))

  li <- list(levels = s_level,
             exceedance = s_exceed)

  print(str_glue("Saving ..."))
  saveRDS(li, str_glue("temp/s_non_ensemble_{variable}_{domain}_{mod}.rds"))

  print(str_glue("Deleting ..."))
  rm(li, s, s_level, s_exceed)
  gc()

  toc() # 1
  
  # # avg:
  # print(str_glue("Saving ..."))
  # saveRDS(s_mean, str_glue("output/temp/s_non_ensemble_{variable}_{domain}_{mod}.rds"))
  # 
  # print(str_glue("Deleting ..."))
  # rm(s, s_mean)
  # gc()
  
}

list.files("temp", full.names = T) %>% 
  map(readRDS) -> li

saveRDS(li, str_glue("{dir_output}/s_non_ensemble_{variable_old}_{domain}.rds"))

unlink("temp", recursive = T)

unlink(dir_down_temp, recursive = T)

rm(li)


