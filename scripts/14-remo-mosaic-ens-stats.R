
rm(mod)

# LOAD FILES --------------------------------------------------------------------------------------

list.files(dir_output) %>% 
  # .[str_detect(., variable)] %>% 
  .[str_detect(., variable_old)] %>% 
  .[str_detect(., "s_non_ensemble")] -> list_files

if(variable == "extr_precip"){
  list_files %>% 
    .[str_detect(., "avg", negate = T)] -> list_files
}

list_files %>% 
  str_split("_", simplify = T) %>% 
  .[,ncol(.)] %>% 
  str_sub(end = 3) -> doms

# Read maps
list_files %>%
  set_names(doms) %>%
  map(~readRDS(str_glue("{dir_output}/{.x}"))) %>% 
  transpose() -> foo

# foo:
# 1st level: model
# 2nd level: domain
# 3rd level: lev / exd



# MOSAIC ------------------------------------------------------------------------------------------


# MEAN VARIABLES

if(str_detect(variable, "mean")){
  
  map(seq_len(3) %>% set_names(c("Had", "MPI", "Nor")), function(mod){
    
    map(c("deg_1", "deg_1.5", "deg_2") %>% set_names(), function(degr){
      
      map(doms %>% set_names(), function(dom){
        
        foo %>% 
          pluck(mod) %>% 
          pluck(dom) -> ls_1
        
        ls_1 %>%
          filter(deg == degr) %>% 
          adrop() %>% 
          as("Raster") -> ls_x
        
        return(ls_x)
        
      }) %>%
        unname() %>% 
        do.call(terra::merge, .) %>% 
        st_as_stars() %>% 
        st_set_dimensions(3,
                          names = "time",
                          values = month.abb) %>% 
        rename("X" = 1)
      
    })
    
  }) -> bar
  
  
# **********
# DRY DAYS  
  
} else if(variable == "extr_cdd"){
  
  map(seq_len(3) %>% set_names(c("Had", "MPI", "Nor")), function(mod){
    
    map(c("deg_1", "deg_1.5", "deg_2") %>% set_names(), function(degr){
      
      map(doms %>% set_names(), function(dom){
        
        print(str_glue("Processing {mod} / {degr} / {dom}"))
        
        foo %>% 
          pluck(mod) %>% 
          pluck(dom) -> ls_1
        
        ls_x <- vector("list", 4) %>% setNames(c("lev", "exc", "gof", "ks"))
        
        ls_1 %>% 
          pluck("levels") %>% 
          filter(deg == degr) %>% 
          adrop() -> ls_lev
        
        map(seq_len(3), function(f_layer){
          
          ls_lev %>% 
            slice(func, f_layer) %>% 
            as("Raster")
          
        }) -> ls_x[c(1,3,4)]
        
        ls_1 %>% 
          pluck("exceedance") %>% 
          filter(deg == degr) %>% 
          adrop() %>% 
          as("Raster") -> ls_x[[2]]
        
        return(ls_x)
        
      }) %>% 
        transpose() %>% 
        
        map(~.x %>% 
              unname() %>% 
              do.call(terra::merge, .) %>% 
              st_as_stars() %>% 
              st_set_dimensions(3,
                                names = "time",
                                values = month.abb))
      
    })
    
  }) -> bar
  
# **********
# DROUGHT
} else if(variable == "extr_drought"){
  
  map(seq_len(3) %>% set_names(c("Had", "MPI", "Nor")), function(mod){
    
    map(c("deg_1", "deg_1.5", "deg_2") %>% set_names(), function(degr){
      
      map(doms %>% set_names(), function(dom){
        
        print(str_glue("Processing {mod} / {degr} / {dom}"))
        
        foo %>% 
          pluck(mod) %>% 
          pluck(dom) -> ls_1
        
        ls_x <- vector("list", 2) %>% setNames(c("lev", "exc"))
        
        ls_1 %>% 
          pluck("levels") %>% 
          filter(deg == degr) %>% 
          adrop() %>% 
          as("Raster") -> ls_x[[1]]
        
        ls_1 %>% 
          pluck("exceedance") %>% 
          filter(deg == degr) %>% 
          adrop() %>% 
          as("Raster") -> ls_x[[2]]
        
        return(ls_x)
        
      }) %>% 
        transpose() %>% 
        
        map(~.x %>% 
              unname() %>% 
              do.call(terra::merge, .) %>% 
              st_as_stars() %>% 
              st_set_dimensions(3,
                                names = "time",
                                values = month.abb))
      
    })
    
  }) -> bar
  
  
# **************  
# OTHER EXTREME VARS
} else {
  
  map(seq_len(3) %>% set_names(c("Had", "MPI", "Nor")), function(mod){
    
    # map(c("lmom", "maxl") %>% set_names(), function(gev_meth){
    gev_meth <- "lmom"
    
    map(c("deg_1", "deg_1.5", "deg_2") %>% set_names(), function(degr){
      
      map(doms %>% set_names(), function(dom){
        
        print(str_glue("Processing {mod} / {degr} / {dom}"))
        
        foo %>% 
          pluck(mod) %>% 
          pluck(dom) -> ls_1
        
        ls_x <- vector("list", 4) %>% setNames(c("lev", "exc", "gof", "ks"))
        
        ls_1 %>% 
          pluck("levels") %>% 
          filter(gev_method == gev_meth,
                 deg == degr) %>% 
          adrop() -> ls_lev
        
        map(seq_len(3), function(f_layer){
          
          ls_lev %>% 
            slice(func, f_layer) %>% 
            as("Raster")
          
        }) -> ls_x[c(1,3,4)]
        
        ls_1 %>% 
          pluck("exceedance") %>% 
          filter(gev_method == gev_meth,
                 deg == degr) %>% 
          adrop() %>% 
          as("Raster") -> ls_x[[2]]
        
        return(ls_x)
        
      }) %>% 
        transpose() %>% 
        
        map(~.x %>% 
              unname() %>% 
              do.call(terra::merge, .) %>% 
              st_as_stars() %>% 
              st_set_dimensions(3,
                                names = "time",
                                values = month.abb))
      
    })
    
    # })
    
  }) -> bar
  
}

# bar:
# 1 level: model
# 2 level: degrees
# 3 level: lev/gof/ks/exc



# ENSEMBLE ----------------------------------------------------------------------------------------

# plan(multicore, workers = 4)

# MEAN VARIABLES

if(str_detect(variable, "mean")){
  
  map(seq_len(3) %>% set_names(c("deg_1", "deg_1.5", "deg_2")), function(d){
    
    print(str_glue("Processing degree {d}"))
    
    bar %>%
      map(pluck, d) %>% # deg
      do.call(c, .) %>% 
      merge() %>% 
      st_apply(c("x", "y", "time"),
               mean, na.rm = T,
               # FUTURE = T,
               rename = F)
    
  }) -> s_ensemble
  
  
# **********

# EXTREME VARIABLES 
  
} else {
  
  map(seq_len(3) %>% set_names(c("deg_1", "deg_1.5", "deg_2")), function(d){
    map(c(1,2) %>% set_names(c("lev", "exc")), function(levexd){
      
      print(str_glue("Processing degree {c('deg_1', 'deg_1.5', 'deg_2')[d]} / {c('lev', 'exc')[levexd]}"))
      
      bar %>%
        map(~.x %>% 
              pluck(d) %>% # deg
              pluck(levexd) # level
        ) %>% 
        do.call(c, .) %>% 
        merge() %>% 
        st_apply(c("x", "y", "time"),
                 mean, na.rm = T,
                 # FUTURE = T,
                 rename = F)
    })
  }) -> s_ensemble
  
}

saveRDS(s_ensemble, str_glue("{dir_output}/s_ensemble_{variable}.rds"))



# STATISTICS --------------------------------------------------------------------------------------

rast_nbr_dom %>%  
  slice(band, 1) %>% 
  setNames("CODE") -> rast_nbr

rast_nbr %>% 
  as_tibble() %>%
  filter(!is.na(CODE)) %>% 
  group_by(CODE) %>% 
  count() -> tb_total_n_bin

rast_nbr %>% 
  mutate(CODE = NA) -> rast_ref

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

map(c("deg1_", "deg1.5_", "deg2_"), ~str_c(.x, vector_names)) %>% 
  flatten() %>% 
  unlist() -> vector_names

# **********

# MEAN VARIABLES

if(str_detect(variable, "mean")){
  
  map_df(c("deg_1", "deg_1.5", "deg_2"), function(d){
    map_df(seq_len(12), function(mth){
      
      print(str_glue("{d} / {mth}"))
      
      s_ensemble %>% 
        pluck(d) %>%
        slice(time, mth) -> s
      
      st_mosaic(s, rast_ref) %>% # extend extension
        c(rast_nbr, along = list(foo = c("v", "CODE"))) %>%
        split("foo") %>% 
        as_tibble() %>% 
        filter(!is.na(v)) -> tb_1
      
      # Table of n/bins
      tb_1 %>%
        group_by(CODE) %>% 
        nest() %>%
        mutate(
          quintile = map(data, function(df){
            
            # not enough cells to obtain unique bins
            if(nrow(df) < 5 ){
              
              tibble(quintile = str_c("n_", 1:5, "q"),
                     value = nrow(df)/5)
              
            # too many cells with the same values producing duplicated quintile breaks
            } else if(any(df$v %>% quantile(seq(0,1,0.2), na.rm = T) %>% duplicated() == T)){
              
              df %>% 
                summarize(c = first(CODE)) %>% 
                pull(c) %>% 
                {print(str_glue("Duplicate: {.}"))}
              
              tibble(quintile = str_c("n_", 1:5, "q"),
                     value = nrow(df)/5)
              
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
        
        mutate(month = month.abb[mth],
               deg = d)
      
    })
    
  }) -> tb
  
  # *****
  
  tb %>% 
    filter(!is.na(CODE)) -> tb
  
  tb %>% 
    filter(quintile == "perc_cov") %>% 
    group_by(CODE) %>% 
    summarize(value = sum(value)) %>% 
    filter(value == 0) %>% 
    pull(CODE) -> nbr_wo_dom
  
  tb %>% 
    filter(!CODE %in% nbr_wo_dom) -> tb
  
  # *****
  
  tb %>%
    left_join(tb_total_n_bin, by = "CODE") %>% 
    mutate(deg = str_replace(deg, "_", "")) %>% 
    pivot_wider(names_from = c(deg, month, quintile), values_from = value) %>%
    arrange(CODE) %>% 
    select(CODE, n, all_of(vector_names)) -> tb_f
  
  
# ***********
  
# EXTREME VARIABLES
  
} else {
  
  map_df(c("deg_1", "deg_1.5", "deg_2"), function(d){
    map_df(c("lev", "exc"), function(levexd){
      map_df(seq_len(12), function(mth){
        
        print(str_glue("{d} / {levexd} / {mth}"))
        
        s_ensemble %>% 
          pluck(d) %>% 
          pluck(levexd) %>%
          slice(time, mth) -> s
        
        st_mosaic(s, rast_ref) %>% # extend extent
          c(rast_nbr, along = list(foo = c("v", "CODE"))) %>%
          split("foo") %>% 
          as_tibble() %>% 
          filter(!is.na(v)) -> tb_1
        
        # Table of n/bins
        tb_1 %>%
          group_by(CODE) %>% 
          nest() %>%
          mutate(
            quintile = map(data, function(df){
              
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
                       value = nrow(df)/5)
                
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
          
          mutate(month = month.abb[mth],
                 var = levexd,
                 deg = d)
        
        
      })
    })
  }) -> tb
  
  tb %>% 
    filter(!is.na(CODE)) -> tb
  
  tb %>% 
    filter(quintile == "perc_cov",
           var == "lev") %>% 
    group_by(CODE) %>% 
    summarize(value = sum(value)) %>% 
    filter(value == 0) %>% 
    pull(CODE) -> nbr_wo_dom
  
  tb %>% 
    filter(!CODE %in% nbr_wo_dom) -> tb
  
  tb %>% 
    mutate(value = ifelse(var == "exc" & str_detect(quintile, "q(0|1)"), round(value, 2), value)) -> tb
  
  tb %>%
    left_join(tb_total_n_bin, by = "CODE") %>% 
    mutate(deg = str_replace(deg, "_", "")) %>% 
    pivot_wider(names_from = c(deg, month, quintile), values_from = value) %>%
    arrange(CODE) %>% 
    select(var, CODE, n, all_of(vector_names)) -> tb_f
  
}

write_csv(tb_f, str_glue("{dir_output}/tbl_{variable}_remo.csv"))
