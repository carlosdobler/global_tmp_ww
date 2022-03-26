
variable <- c("mean_temp",
              "mean_precip",
              "extr_precip",
              "extr_maxtemp",
              "extr_wetbulbtemp",
              "extr_fwi",
              "extr_cdd")[7]

# ******************************************************************************

# related variable names
variable_data <- func_var_def(variable)["name_data"]
variable_old <- func_var_def(variable)["name_old"]

library(colorspace)
library(patchwork)

# ************

# PER PIXEL
# ^^^^^^^^^

# LOAD FILES

readRDS(str_glue("{dir_output}/s_ensemble_{variable}.rds")) %>% 
  pluck("deg_1") -> s_remo
  
if(str_detect(variable, "mean", negate = T)){
  s_remo %>% 
    pluck("lev") -> s_remo
}

file_era <- case_when(variable == "extr_precip" ~ "s_era_pr_max_levels_lmom.RDS",
                      variable == "extr_maxtemp" ~ "s_era_tasmax_max_levels_lmom.RDS",
                      variable == "extr_fwi" ~ "s_era_fwi_levels.RDS",
                      variable == "extr_wetbulbtemp" ~ "s_era_wetbulb_max_levels_lmom.RDS",
                      TRUE ~ as.character(str_glue("s_era_{variable}_levels.RDS")))

readRDS(str_glue("{dir_output}/{file_era}")) -> s_era 
  
if(str_detect(variable, "mean", negate = T)){
  s_era %>% 
    slice(func, 1) -> s_era
}

if(variable == "extr_fwi") s_era %>% slice(gev_method, 1) -> s_era


# COMBINE AND TURN INTO TABLES
# SLICED BY MONTH

map_df(seq_len(12), function(mth){
  
  s_remo %>% 
    slice(time, mth) -> s_1
  
  s_era %>% 
    slice(time, mth) %>% 
    st_warp(s_1) -> s_2
  
  c(s_1, s_2) %>% 
    setNames(c("remo", "era")) %>% 
    as_tibble() %>% 
    filter(!is.na(remo) & !is.na(era)) %>% 
    mutate(month = factor(month.abb[mth], levels = month.abb))
  
}) -> tb

# CALCULATE CORRELATIONS + MAD

tb %>% 
  group_by(month) %>% 
  nest() %>% 
  mutate(stats = map(data, function(df){
    
    cor(df$remo, df$era) %>% 
      round(3) -> corr
    
    df %>% 
      mutate(dev = abs(remo - era)) %>% 
      summarize(md = mean(dev)) %>% 
      pull(md) %>% 
      round(2) -> md
    
    tibble(cor = corr, md = md)
    
  })) -> miau

miau %>% 
  unnest(stats) %>% 
  select(-data) -> stats

# PLOT

range(tb$remo) %>% 
  {(.[2] - .[1])*0.98} %>% 
  {min(tb$remo) + .} -> x_pos

range(tb$era) %>% 
  {(.[2] - .[1])*1/5} %>% 
  {min(tb$era) + .} -> y_pos

ggplot(data = tb, aes(x = remo, y = era)) +
  geom_hex(show.legend = F) +
  scale_fill_continuous_sequential("Plasma", begin = 0.1, trans = "log", rev = F) +
  geom_abline(color = "black", size = 0.5, linetype = "2222") +
  geom_smooth(method = "lm", color = "black", size = 0.5) +
  facet_wrap(~month, ncol = 4) +
  geom_label(data = stats, x = x_pos, y = y_pos, aes(label = str_glue("r = {cor}\nmad = {md}")), hjust = 1, size = 2.7, alpha = 0.6, label.size = 0) +
  labs(x = "REMO2015", y = "ERA5",
       subtitle = "Comparison between REMO2015 and ERA5 per-cell results") -> p2

# ggsave(str_glue("{dir_output}/fig_{variable}_removsera_cell.png"), width = 5.5, height = 7)

# ggplot(data = tb %>% slice_sample(prop = 0.1), aes(x = remo, y = era)) +
#   geom_hex(show.legend = F, bins = 90) +
#   scale_fill_continuous_sequential("viridis", begin = 0.1) +
#   geom_abline() +
#   geom_smooth(method = "lm", color = "black", linetype = "2222", size = 0.5) +
#   facet_wrap(~month, ncol = 3) +
#   labs(x = "REMO2015", y = "ERA5") +
#   coord_equal(xlim = c(0,100), ylim = c(0,100))
# 
# ggsave(str_glue("{dir_output}/fig_{variable}_removsera_cell_zoom.png"), width = 5.5, height = 7)

# ************

# PER NBR
# ^^^^^^^

# LOAD TABLES

# REMO
str_glue("{dir_output}/tbl_{variable}_remo.csv") %>% 
  read_csv() -> tb_remo

if(str_detect(variable, "mean", negate = T)){
  tb_remo %>% 
    filter(var == "lev") -> tb_remo
}

tb_remo %>% 
  select(CODE, starts_with("deg1_")) %>% 
  select(-contains("_n_"),
         -ends_with("_cov")) %>% 
  rename_with(.cols = -1, ~str_replace(.x, "deg1_", "")) -> tb_remo
  
tb_remo %>% 
  pivot_longer(-CODE,
               names_to = c("month", "quantile"),
               names_pattern = "(.*)_q(.*)",
               values_to = "val"
  ) %>% 
  mutate(data = "remo") -> tb_remo

if(variable == "extr_fwi") tb_remo %>% filter(!is.na(val)) -> tb_remo



# ERA
file_era <- case_when(variable == "extr_fwi" ~ "tbl_fire_weather_index_era.csv",
                      variable == "extr_maxtemp" ~ "tbl_maximum_temperature_era.csv",
                      variable == "extr_wetbulbtemp" ~ "tbl_extr_wetbulbtemp_era.csv",
                      TRUE ~ as.character(str_glue("tbl_{variable}_era.csv")))

str_glue("{dir_output}/{file_era}") %>% 
  read_csv() -> tb_era

if(str_detect(variable, "mean", negate = T)){
  tb_era %>% 
    filter(var == "lev") %>% 
    select(-var) -> tb_era
}

tb_era %>% 
  select(-n,
         -contains("_n_"),
         -ends_with("_cov")) %>% 
  
  pivot_longer(-CODE,
               names_to = c("month", "quantile"),
               names_pattern = "(.*)_q(.*)",
               values_to = "val"
  ) %>% 
  mutate(data = "era") -> tb_era

# JOIN
inner_join(tb_remo, tb_era, by = c("CODE", "month", "quantile")) -> tb_j

tb_j %>% 
  mutate(month = factor(month, levels = month.abb)) -> tb_j

# CALCULATE CORR + MAD

tb_j %>% 
  group_by(month, quantile) %>% 
  nest() %>% 
  mutate(stats = map(data, function(df){
    
    cor(df$val.x, df$val.y) %>% 
      round(3) -> corr
    
    df %>% 
      mutate(dev = abs(val.x - val.y)) %>% 
      summarize(md = mean(dev)) %>% 
      pull(md) %>% 
      round(2) -> md
    
    tibble(cor = corr, md = md)
  })) -> guau

guau %>% 
  unnest(stats) %>% 
  select(-data) -> stats_2

# PLOT

# range(tb_j$val.x) %>% 
#   {(.[2] - .[1])*0.98} %>% 
#   {min(tb_j$val.x) + .} -> x_pos
# 
# range(tb_j$val.y) %>% 
#   {(.[2] - .[1])*1/4} %>% 
#   {min(tb_j$val.y) + .} -> y_pos
# 
# ggplot(tb_j, aes(x = val.x, y = val.y)) +
#   
#   # geom_point(size = 0.1) +
#   
#   geom_hex(show.legend = F, bins = 15) +
#   scale_fill_continuous_sequential("viridis", begin = 0.1, trans = "log") +
#   
#   facet_grid(month~quantile) +
#   geom_abline(color = "red", linetype = "2222", size = 0.5) +
#   geom_smooth(method = "lm", color = "red", size = 0.5) +
#   labs(x = "REMO2015", y = "ERA5") +
#   geom_label(data = stats, x = x_pos, y = y_pos, aes(label = str_glue("r = {cor}\nmad = {md}")), hjust = 1, size = 2.7, alpha = 0.6, label.size = 0) +
#   coord_equal()
# 
# ggsave(str_glue("{dir_output}/fig_{variable}_removsera_nbr.png"), width = 7, height = 12)

case_when(variable == "mean_temp" ~ "1. Mean temperature",
          variable == "mean_precip" ~ "2. Mean precipitation",
          variable == "extr_precip" ~ "3. Extreme precipitation",
          variable == "extr_maxtemp" ~ "4. Extreme maximum temperature",
          variable == "extr_wetbulbtemp" ~ "5. Extreme wetbulb temperature",
          variable == "extr_fwi" ~ "6. Extreme fire conditions",
          variable == "extr_cdd" ~ "7. Extreme count of dry days",
          variable == "extr_drought" ~ "8. Extreme drought") -> variable_title

ggplot(stats_2, aes(y = quantile, x = month, fill = cor)) +
  geom_tile() +
  scale_fill_continuous_sequential(palette = "Viridis", name = "Pearson's r") +
  theme(axis.title.x = element_blank()) +
  labs(title = variable_title, 
       subtitle = "Correlation coef. between REMO2015 and ERA5 per-NBR results") -> p1

layout <- "
AAAA#
BBBBB
BBBBB"

patchwork::wrap_plots(p1, p2, design = layout)

ggsave(str_glue("{dir_output}/fig_{variable}_removsera.png"), width = 6.2, height = 7.6)
