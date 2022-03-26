
# Create a vector of dates to assign to time dimension of star objects
# vector based on model and start-end of the time-series

func_dates <- function(mod, t_i, t_f)
{
  if(str_detect(mod, "Had")){
    
    tibble(date = seq(as_date(t_i), as_date(t_f), by = "1 day")) %>%
      group_by(year = year(date), month = month(date)) %>% 
      summarise(day_max = max(day(date))) %>% 
      ungroup() %>%
      
      pmap(function(year, month, day_max)
      {
        seq(as_date(str_c(year,
                          str_pad(month, 2, "left", 0),
                          "01")),
            
            as_date(str_c(year,
                          str_pad(month, 2, "left", 0),
                          str_pad(day_max, 2, "left", 0))),
            
            length.out = 30)
      }
      ) %>% 
      do.call(c, .)
    
  } else {
    
    seq(as_date(t_i), as_date(t_f), by = "1 day")
  }
}


# *************************************************************************************************


# Fit a Generalized Extreme Value distribution to a vector of values
# and obtain the 95th percentile (maximum likelihood method). Perform 
# a Kolmogorov-Smirnov Test of the fit. Calculate the goodness of 
# fit (quantiles-vs-quantiles)

func_gev_level_maxl <- function(x)
{
  
  if(sum(!is.na(x)) == 0){
    
    results <- c(val = NA,
                 gof = NA,
                 ks = NA)
    
  } else {
    
    q <- 0.95 # 1-(1/20)
    
    if(tail_side == "left"){
      q <- 1-q
    }
    
    # obtain dist. parameters
    try(evd::fgev(x, std.err = F)$estimate %>%
          as.list() %>% as_tibble()) -> params
    
    # **********
    
    # calculate level
    if(any(class(params) == "try-error")){
      NA -> val
      
    } else {
      
      evd::qgev(q,
                loc = params$loc,
                scale = params$scale,
                shape = params$shape) -> val
      
    }
    
    # **********
    
    # calculate ks
    if(any(class(params) == "try-error")){
      NA -> ks
      
    } else {
      
      ks.test(x,
              evd::pgev,
              params$loc,
              params$scale,
              params$shape)$p.value -> ks
      
    }
    
    # **********
    
    # calculate gof
    if(any(class(params) == "try-error")){
      NA -> gof
      
    } else {
      
      # fitted
      evd::qgev(seq(0.01,0.99,0.01),
                loc = params$loc,
                scale = params$scale,
                shape = params$shape) %>% 
        quantile(probs = seq(0,1, length.out = length(x))) -> p_fitted
      
      # observed
      x %>%
        quantile(probs = seq(0,1, length.out = length(x))) -> p_observed
      # same as x %>% sort() -> p_observed
      
      # correlation
      cor(p_fitted, p_observed) -> gof
      
    }
    
    results <- c(val = val,
                 gof = gof,
                 ks = ks)
    
  }
  return(results)
}


# *************************************************************************************************

# Fit a Generalized Extreme Value distribution to a vector of values
# and obtain the 95th percentile (l-moments method). Perform 
# a Kolmogorov-Smirnov Test of the fit. Calculate the goodness of 
# fit (quantiles-vs-quantiles)

func_gev_level_lmom <- function(x)
{
  
  if(sum(!is.na(x)) == 0){
    
    results <- c(val = NA,
                 gof = NA,
                 ks = NA)
    
  } else {
    
    q <- 0.95 # 1-(1/20)
    
    if(tail_side == "left"){
      q <- 1-q
    }
    
    # obtain dist parameters
    lmom::samlmu(x) -> lmom
    try(lmom::pelgev(lmom) %>% 
          as.list() %>% 
          as_tibble() %>% 
          rename_with(~c("loc", "scale", "shape"))) -> params
    
    # **********
    
    # calculate levels
    if(any(class(params) == "try-error")){
      NA -> val
    } else {
      
      lmom::quagev(q,
                   para = c(params$loc,
                            params$scale,
                            params$shape)) -> val
    }
    
    # **********
    
    # calculate ks
    if(any(class(params) == "try-error")){
      NA -> ks
      
    } else {
      
      ks.test(x, 
              lmom::cdfgev, 
              c(params$loc, 
                params$scale, 
                params$shape))$p.value -> ks
      
    }
    
    # **********
    
    # calculate gof
    if(any(class(params) == "try-error")){
      NA -> gof
      
    } else {
      
      # fitted
      lmom::quagev(seq(0.01,0.99,0.01), c(params$loc,
                                          params$scale,
                                          params$shape)) %>%
        quantile(probs = seq(0,1, length.out = length(x))) -> p_fitted
      
      # observed
      x %>%
        quantile(probs = seq(0,1, length.out = length(x))) -> p_observed
      
      # correlation
      cor(p_fitted, p_observed) -> gof
      
    }
    
    results <- c(val = val,
                 gof = gof,
                 ks = ks)
    
  }
  
  return(results)
  
}


# *************************************************************************************************


# Obtain the Exceedance Probability of a given value in relation to a vector of values
# (maximum likelihood method). Perform a Kolmogorov-Smirnov Test of the fit. Calculate 
# the goodness of fit (quantiles-vs-quantiles)

func_gev_exceed_maxl <- function(x)
{
  
  last(x) -> x_lev
  x[seq_len(length(x)-1)] -> x_val
  
  # obtain distr. parameters
  if(is.na(x_lev) | sum(!is.na(x_val)) == 0){
    NA
  } else {
    
    try(evd::fgev(x_val, std.err = F)$estimate %>%
          as.list() %>% as_tibble()) -> params
    
    # calculate exceedance prob
    if(any(class(params) == "try-error")){
      NA
    } else {
      
      evd::pgev(x_lev, 
                loc = params$loc,
                scale = params$scale,
                shape = params$shape) -> ex
      
      if(tail_side == "right"){
        ex <- 1-ex
      }
      
      return(ex)
      
    }
  }
}


# *************************************************************************************************

# Obtain the Exceedance Probability of a given value in relation to a vector of values
# (l-moments method).

func_gev_exceed_lmom <- function(x)
{
  
  last(x) -> x_lev
  x[seq_len(length(x)-1)] -> x_val
  
  # obtain distr. parameters
  if(is.na(x_lev) | sum(!is.na(x_val)) == 0){
    NA
  } else {
    
    lmom::samlmu(x_val) -> lmom
    try(lmom::pelgev(lmom) %>% 
          as.list() %>% 
          as_tibble() %>% 
          rename_with(~c("loc", "scale", "shape"))) -> params
    
    # calculate exceedance prob
    if(any(class(params) == "try-error")){
      NA -> ex
      
    } else {
      
      lmom::cdfgev(x_lev, 
                   para = c(params$loc,
                            params$scale,
                            params$shape)) -> ex
      
      if(tail_side == "right"){
        ex <- 1-ex
      }
      
    }
    
    return(ex)
    
  }
}


# *************************************************************************************************

# Fit a Binomial distribution to a vector of values and obtain the 
# 95th percentile. Perform a Kolmogorov-Smirnov Test of the fit. 
# Calculate the goodness of fit (quantiles-vs-quantiles)

func_binom_level_maxl <- function(x, md, days_size)
{
  
  if(sum(!is.na(x)) == 0){
    
    results <- c(val = NA,
                 gof = NA,
                 ks = NA)
    
  } else {
    
    q <- 0.95 # 1-(1/20)
    
    if(tail_side == "left"){
      q <- 1-q
    }
    
    # **********
    
    # obtain distr. parameters
    
    # (no need)
    # try(
    #   fitdistrplus::fitdist(x,
    #                         dist = "binom",
    #                         fix.arg = list(size = as.vector(days_in_month(m))),
    #                         start = list(prob = mean(x/as.vector(days_in_month(m)))))$estimate,
    #   silent = T
    #   ) -> params
    
    # (same as above)
    if(md == 1){ # had: all months have 30 days
      
      mean(x/30) -> params
      
    } else { # MPI or Nor
      
      mean(x/days_size) -> params
      
    }

    
    # **********
    
    # calculate level
    
    # old approach
    # if(md == 1){ # had
    #   qbinom(q,
    #          size = 30,
    #          prob = params) -> val
    # } else { # MPI or Nor
    #   qbinom(q,
    #          size = as.vector(days_in_month(m)),
    #          prob = params) -> val
    # }
    
    # new approach (jitter)
    if(md == 1){ # had
      
      set.seed(123)
      qbinom(seq(0.01,0.99,0.01),
             30,
             params) %>%
        jitter() %>%
        quantile(q) %>% 
        unname() -> val
      
    } else {
      
      set.seed(123)
      qbinom(seq(0.01,0.99,0.01),
             max(days_size),
             params) %>%
        jitter() %>%
        quantile(q) %>% 
        unname() -> val
      
    }
    
    # **********
    
    # calculate ks
    if(md == 1){ # Had
      ks.test(x,
              pbinom,
              30,
              params)$p.value -> ks
    } else { # MPI or Nor
      ks.test(x,
              pbinom,
              max(days_size),
              params)$p.value -> ks
    }
    
    # alternative: chi square
    # tibble(z = x) %>% 
    #   count(z) %>% 
    #   {left_join(tibble(z = seq(0, days_in_month(m))), ., by = "z")} %>% 
    #   mutate(n = ifelse(is.na(n), 0, n)) %>% 
    #   pull(n) %>% 
    #   
    #   chisq.test(.,
    #              p = dbinom(seq(0, days_in_month(m)), 
    #                         days_in_month(m), 
    #                         params),
    #              simulate.p.value = T,
    #              B = 500) %>% 
    #   .$p.value -> ks
    
    # **********
    
    # calculate gof
    
    # fitted
    if(md == 1){
      qbinom(seq(0.01,0.99,0.01),
             30,
             params) %>%
        quantile(probs = seq(0,1, length.out = length(x))) -> p_fitted
    } else {
      qbinom(seq(0.01,0.99,0.01),
             max(days_size),
             params) %>%
        quantile(probs = seq(0,1, length.out = length(x))) -> p_fitted
    }
    
    # observed
    x %>%
      quantile(probs = seq(0,1, length.out = length(x))) -> p_observed
    # same as x %>% sort() -> p_observed
    
    # correlation
    cor(p_fitted, p_observed) -> gof
    
    results <- c(val = val,
                 gof = gof,
                 ks = ks)
    
  }
  return(results)
}


# *************************************************************************************************

# Obtain the Exceedance Probability of a given value in relation to a vector of values.

func_binom_exceed_maxl <- function(x, md, days_size){
  
  last(x) -> x_lev
  x[seq_len(length(x)-1)] -> x_val
  
  # **********
  
  # obtain distr. parameters
  if(is.na(x_lev) | sum(!is.na(x_val)) == 0){
    NA
  } else {
    
    if(md == 1){
      mean(x_val/30) -> params
    } else {
      mean(x_val/days_size) -> params
    }
    
    # ***********
    
    # calculate exceedance prob
    
    # old approach
    # if(md == 1){
    #   pbinom(x_lev,
    #          size = 30,
    #          prob = params) -> ex
    # } else {
    #   pbinom(x_lev,
    #          size = as.vector(days_in_month(m)),
    #          prob = params) -> ex
    # }
    # 
    # if(tail_side == "right"){
    #   ex <- 1-ex
    # }
    
    # new approach
    if(md == 1){
      
      set.seed(123)
      qbinom(seq(0.01,0.99,0.01),
             30,
             params) %>%
        jitter() %>%
        {ecdf(.)(x_lev)} %>% 
        unname() %>% 
        {1 -.} %>% 
        round(2)
      
    } else {
      
      set.seed(123)
      qbinom(seq(0.01,0.99,0.01),
             max(days_size),
             params) %>%
        jitter() %>%
        {ecdf(.)(x_lev)} %>% 
        unname() %>% 
        {1 -.} %>% 
        round(2)
      
    }
  }
}


# **************************************************************************************************

# Calculate quintile breaks and n/bins per NBR

func_nbrstats <- function(tb, tb_total_n_nbr){
  
  # Table of n/bins
  tb %>%
    group_by(CODE) %>% 
    nest() %>%
    mutate(
      quintile = map(data, function(df){
        
        # not enough cells to obtain unique bins or
        # all cells of same value (exceedance in 1 deg)
        if(nrow(df) < 5 | near(mean(df$v), 0.05, 0.009)){
          
          tibble(quintile = str_c("n_", 1:5, "q"),
                 value = nrow(df)/5)
          
          # duplicate quintiles: impossible to split  
        } else if(any(df$v %>% quantile(seq(0,1,0.2), na.rm = T) %>% duplicated() == T)){
          
          df %>% 
            summarize(c = first(CODE)) %>% 
            pull(c) %>% 
            {print(str_glue("Duplicate: {.}"))}
          
          tibble(quintile = str_c("n_", 1:5, "q"),
                 value = df %>% nrow() %>% {./5})
          
          # no issues
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
    right_join(tb_total_n_nbr, by = "CODE") %>%
    mutate(value = ifelse(is.na(value), 0, value)) %>% 
    mutate(value = value/n*100,
           quintile = "perc_cov") %>%
    select(CODE, quintile, value) -> tb_2_2
  
  # Join tables
  bind_rows(tb_2_1, tb_2_2) %>% 
    arrange(CODE, quintile) -> tb_2
  
  # Table of quintile breaks
  tb %>% 
    group_by(CODE) %>%
    summarize(value = quantile(v, probs = seq(0,1,0.2), na.rm = T),
              quintile = str_c("q", seq(0,1,0.2))) %>%
    suppressMessages() %>% 
    ungroup() -> tb_3
  
  # Final table
  bind_rows(tb_2, tb_3)
  
}


# FUNCTIONS IN NO USE ----------------------------------------------------------

# # function exceedance
# func_return <- function(x)
# {
#   
#   last(x) -> i
#   x[seq_len(length(x)-1)] -> xx
#   
#   # obtain distr. parameters
#   if(is.na(i) | sum(!is.na(xx)) == 0){
#     NA
#   } else {
#     
#     try(evd::fgev(xx, std.err = F)$estimate %>%
#           as.list() %>% as_tibble()) -> params
#     
#     # calculate new exceedance prob.
#     if(any(class(params) == "try-error")){
#       NA
#     } else if(ks.test(xx, evd::pgev, params$loc, params$scale, params$shape)$p.value < 0.05) {
#       NA 
#     } else {
#       
#       evd::pgev(i, 
#                 loc = params$loc,
#                 scale = params$scale,
#                 shape = params$shape) -> val
#       
#       if(tail_side == "right"){
#         val <- 1-val
#       }
#       
#       return(val)
#     }
#   }
# }

# # function event
# func_event <- function(x)
# {
#   if(sum(!is.na(x)) == 0){
#     
#     results <- c(val = NA,
#                  se = NA)
#     
#   } else {
# 
#     seq_len(500) %>% 
#       map_dbl(~sample(x, 21, replace = T) %>% {ecdf(.)(-3)}) %>% 
#       # map_dbl(~x %>% sample(21, replace = T) %>% {sum(. <= -3)} %>% {./21}) %>% 
#       as_tibble() %>% 
#       summarize(val = mean(value),
#                 se = sd(value)) %>% 
#       as.list() %>% 
#       unlist() -> results
#     
#     # sample(x, 1000, replace = T) %>%                                           # test for Zach
#     #   sum(. < -3)/1000
#   }
#   
#   return(results)
#   
# }
# 
# 
# # function return event
# func_return_event <- function(x)
# {
#   
#   last(x) -> i
#   x[seq_len(length(x)-1)] -> xx
#   
#   # obtain return
#   if(is.na(i) | sum(!is.na(xx)) == 0){
#     
#     results <- c(val = NA,
#                  se = NA)
#     
#   } else if(round(21*i) < 1){
#     
#     results <- c(val = -3,
#                  se = 0)
#     
#   } else {
#     seq_len(500) %>% 
#       map_dbl(~xx %>% sample(21, replace = T) %>% quantile(probs = i)) %>% 
#       # map_dbl(~xx %>% sample(21, replace = T) %>% sort() %>% .[round(21*i)]) %>% 
#       as_tibble() %>% 
#       summarize(val = mean(value),
#                 se = sd(value)) %>% 
#       as.list() %>% 
#       unlist() -> results
#   }
#   
#   return(results)
#   
# }
# 
# # *********************************************************
# 
# # function level non dist
# func_level_nondist <- function(x)
# {
#   if(sum(!is.na(x)) == 0){
#     
#     results <- c(mean = NA,
#                  se = NA)
#     
#   } else {
#     
#     seq_len(500) %>%
#       map_dbl(~sample(x, 21, replace = T) %>% {sort(.)[round(21*0.05)]}) %>%
#       as_tibble() %>% 
#       summarize(mean = mean(value),
#                 se = sd(value)) %>% 
#       as.list() %>% 
#       unlist() -> results
#     
#     # sample(x, 1000, replace = T) %>% 
#     #   sum(. < -3)/1000
#   }
#   
#   return(results)
#   
# }
# 
# # function return non dist
# func_return_nondist <- function(x)
# {
#   
#   last(x) -> i
#   x[seq_len(length(x)-1)] -> xx
#   
#   # obtain return
#   if(is.na(i) | sum(!is.na(xx)) == 0){
#     NA
#   } else {
#     
#     seq_len(500) %>% 
#       map_dbl(~sample(xx, 21, replace = T) %>% sort() %>% {which.min(abs(.-i))} %>% {./21}) %>% 
#       mean()
#       
#   }
# }