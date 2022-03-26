
# CALCULATE MEANS ------------------------------------------------------------

plan(multicore, workers = availableCores() - 1)

tb_mid_yr %>%
  pull(deg) %>%
  unique() %>%
  # .[c(1,3)] %>%                                                               # DELETE !!!

  # obtain 95th percentiles
  map(function(d){

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

    map(seq_len(12), function(mth){

      s_deg %>%
        filter(month(time) == mth) %>%
        st_apply(c(1,2),
                 FUTURE = T,
                 rename = F,
                 FUN = mean, na.rm = T) %>%
        setNames(month.abb[mth])

    }) %>%
      do.call(c, .) %>%
      merge(name = "time") %>%
      setNames(d)

  }) %>%
  do.call(c, .) %>%
  merge(name = "deg") -> s_mean