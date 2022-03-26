

# SET DESTINATION OF FILES

dir_down_temp <- str_glue("{dir_disk}/{domain}/")
dir.create(dir_down_temp)

# *************************************************************************************************

# DOWNLOAD

plan(multicore, workers = availableCores() - 1)

# all variables except PDSI
if(str_detect(variable_data, "palmer", negate = T)){
  
  models %>%
    walk(function(m){
      
      print(str_glue(" "))
      print(str_glue("[{func_get_time()}] Downloading files:"))
      
      tb_mid_yr %>%
        filter(model == m) %>%
        pull(mid_yr) %>%
        map(~seq(.x - 10,
                 .x + 10)) %>%
        do.call(c, .) %>%
        unique() -> all_years
      
      tb_files %>%
        filter(model == m,
               year(as_date(t_f)) >= first(all_years),
               year(as_date(t_i)) <= last(all_years)) -> files_to_down
      
      files_to_down %>%
        mutate(across(starts_with("t_"), ~as_date(.x))) %>%
        select(-1) %>%
        print(n = nrow(files_to_down))
      
      files_to_down %>%
        pull(file) -> files_to_down
      
      tic(str_glue("Files of {m} downloaded"))
      seq_along(files_to_down) %>%
        future_walk(function(f){
          
          file.copy(str_glue("{dir_bucket_risk}/RCM_regridded_data/REMO2015/{domain}/daily/{variable_data}/{files_to_down[f]}"),
                    dir_down_temp)
          
        })
      toc()
    })
  
} else { # PDSI
  
  print(str_glue(" "))
  print(str_glue("[{func_get_time()}] Downloading files ..."))
  tic("Done")
  
  str_glue("{dir_bucket_risk}/RCM_regridded_data/REMO2015/{domain}/monthly/{variable_data}/") %>%
    list.files(full.names = T) %>%
    future_walk(file.copy, dir_down_temp)
  
  toc()
  
}




