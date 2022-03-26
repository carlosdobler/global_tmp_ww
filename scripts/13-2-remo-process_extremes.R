
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