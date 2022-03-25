
# DROUGHT ----

# Focusing in EUR, month of December
# First a pixel that has 0 as exceedance in deg_2
# Many pixels in same condition: cause quintiles to be duplicated (e.g. CODE = 100)

# tm_shape(t) +
#   tm_raster(palette = colorspace::sequential_hcl(n = 10, "Viridis", rev = T),
#             breaks = c(0, 0.001, 0.04, 0.05, 0.06, 0.1, 0.5, 1)) +
#   tm_mouse_coordinates()

mod = 1

# obtain s
source(textConnection(readLines("scripts/13-remo-process.R")[92:352]))

d = "deg_1"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_1

d = "deg_2"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_2


s_deg_1 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - 19.1278))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - 47.0708))) %>% 
  as_tibble() %>% 
  pull(var) -> x_1

x_1 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_1_5th # -3.841165

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # same as x_1_5th!


s_deg_2 %>% # s_deg in 2 deg with jitter
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - 19.1278))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - 47.0708))) %>% 
  as_tibble() %>% 
  pull(var) -> x_2

x_2 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_2_5th # -2.864459

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # # same as x_2_5th!

# exceedances

ecdf(x_1)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # same! (0.05, which is correct)

ecdf(x_2)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # same! (0, which means x_1_5th is outside of range of data in a 2 degree scenario)

# now let's try with:
mod = 3

# obtain s
source(textConnection(readLines("scripts/13-remo-process.R")[92:352]))

d = "deg_1"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_1

d = "deg_2"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_2


s_deg_1 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - 19.1278))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - 47.0708))) %>% 
  as_tibble() %>% 
  pull(var) -> x_1

x_1 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_1_5th # -4.3613

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # same as x_1_5th!


s_deg_2 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - 19.1278))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - 47.0708))) %>% 
  as_tibble() %>% 
  pull(var) -> x_2

x_2 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_2_5th # -3.616184

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # # same as x_2_5th!

# exceedances

ecdf(x_1)(x_1_5th) %>% round(2)

bar %>% 
  pluck(1) %>% # model
  pluck("deg_1") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # same! (0.05, which is correct)

ecdf(x_2)(x_1_5th) %>% round(2)

bar %>% 
  pluck(1) %>% # model
  pluck("deg_2") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - 19.1278))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - 47.0708))) %>% 
  pull(1) # same! (0, which means x_1_5th is outside of range of data in a 2 degree scenario)
# final exceedance of this pixel in a 2 deg for Dec is 0 because in all three models was 0


# *******************

# now let's try with a pixel that had an exceedance of over 0.5

lo <- 6.1212
la <- 43.7368

mod = 1

# obtain s
source(textConnection(readLines("scripts/13-remo-process.R")[92:352]))

d = "deg_1"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_1

d = "deg_2"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_2


s_deg_1 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_1

x_1 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_1_5th # -4.845103

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same as x_1_5th!


s_deg_2 %>% # s_deg in 2 deg with jitter
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_2

x_2 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_2_5th # -8.885582

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # # same as x_2_5th!

# exceedances

ecdf(x_1)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same! (0.05, which is correct)

ecdf(x_2)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same! 0.43

# ****************

# now let's try with:
mod = 2

# obtain s
source(textConnection(readLines("scripts/13-remo-process.R")[92:352]))

d = "deg_1"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_1

d = "deg_2"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_2


s_deg_1 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_1

x_1 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_1_5th # -4.601509

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same as x_1_5th!


s_deg_2 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_2

x_2 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_2_5th # -10.00171 

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # # same as x_2_5th!

# exceedances

ecdf(x_1)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same! (0.05, which is correct)

ecdf(x_2)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same! 0.95 (correct given the range of data)

# ****************

# now let's try with:
mod = 3

# obtain s
source(textConnection(readLines("scripts/13-remo-process.R")[92:352]))

d = "deg_1"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_1

d = "deg_2"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) -> s_deg

set.seed(123)
s_deg %>% 
  mutate(var = jitter(var)) -> s_deg_2


s_deg_1 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_1

x_1 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_1_5th # -5.861172

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same as x_1_5th!


s_deg_2 %>%
  filter(month(time) == 12) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_2

x_2 %>% quantile(prob = 0.05, type = 4, na.rm = T) -> x_2_5th # -7.2766

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("lev") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # # same as x_2_5th!

# exceedances

ecdf(x_1)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_1") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same! (0.05, which is correct)

ecdf(x_2)(x_1_5th) %>% round(2)

bar %>% 
  pluck(mod) %>% # model
  pluck("deg_2") %>% 
  pluck("exc") %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same! 0.29 (correct given the range of data)

# is the ensemble correct?
mean(c(-4.845103, -4.601509, -5.861172))
s_ensemble$deg_1$lev %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same!
  
mean(c(-8.885582, -10.00171, -7.2766))
s_ensemble$deg_2$lev %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same!

mean(c(0.43, 0.95, 0.29))
s_ensemble$deg_2$exc %>% 
  slice(time, 12) %>% 
  slice(x, which.min(abs(st_get_dimension_values(., "x") - lo))) %>% 
  slice(y, which.min(abs(st_get_dimension_values(., "y") - la))) %>% 
  pull(1) # same!




# DRY DAYS ----

# Focusing on CAM
# First just want to check calculations are alright, using the Had model in January

# One pixel with lower exceedances in deg 2
lo <- -72.4428
la <- 1.2130

d = "deg_1"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) %>% 
  filter(month(time) == 1) -> s_deg_1


d = "deg_2"

tb_mid_yr %>%
  filter(model == models[mod],
         deg == d) %>%
  pull(mid_yr) %>%
  {c(. - 10, . + 10)} -> years_lim

s %>%
  filter(year(time) >= years_lim[1],
         year(time) <= years_lim[2]) %>% 
  filter(month(time) == 1) -> s_deg_2


s_deg_1 %>%
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_1

mean(x_1/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  quantile(0.95) %>% 
  unname() -> lev_1 # 3.1548

s_level %>% 
  slice(func,1) %>% 
  slice(deg, 1) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same as lev_1

s_deg_2 %>%
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_2, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_2, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_2

mean(x_2/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  quantile(0.95) %>% 
  unname() -> lev_2 # 1.94979

s_level %>% 
  slice(func,1) %>% 
  slice(deg, 3) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same as lev_2


# exceedances

mean(x_1/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  {ecdf(.)(lev_1)} %>% 
  unname() %>% 
  {1 -.} %>% 
  round(2) # 0.05 = correct!

s_exceed %>% 
  slice(deg,1) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same!

mean(x_2/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  {ecdf(.)(lev_1)} %>% 
  unname() %>% 
  {1 -.} %>% 
  round(2) # 0 : 95th perc (lev_1) is out of range (of x_2)

s_exceed %>% 
  slice(deg,3) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same!


# One pixel with exceedance of over 0.5
lo <- -66.7323
la <- -7.2639

s_deg_1 %>%
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_1, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_1, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_1

mean(x_1/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  quantile(0.95) %>% 
  unname() -> lev_1 # 0.01767484 (should be 0)

s_level %>% 
  slice(func,1) %>% 
  slice(deg, 1) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same as lev_1

s_deg_2 %>%
  slice(lon, which.min(abs(st_get_dimension_values(s_deg_2, "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(s_deg_2, "lat") - la))) %>% 
  as_tibble() %>% 
  pull(var) -> x_2

mean(x_2/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  quantile(0.95) %>% 
  unname() -> lev_2 # 1.113154

s_level %>% 
  slice(func,1) %>% 
  slice(deg, 3) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same as lev_2


# exceedances

mean(x_1/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  {ecdf(.)(lev_1)} %>% 
  unname() %>% 
  {1 -.} %>% 
  round(2) # 0.05 = correct!

s_exceed %>% 
  slice(deg,1) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same!

mean(x_2/30) -> params

set.seed(123)
qbinom(seq(0.01,0.99,0.01),
       30,
       params) %>%
  jitter() %>%
  {ecdf(.)(lev_1)} %>% 
  unname() %>% 
  {1 -.} %>% 
  round(2) # 0.55 : 95th perc (lev_1) is basically 0, so anything above will increase exceedance considerably

s_exceed %>% 
  slice(deg,3) %>% 
  slice(time,1) %>% 
  slice(lon, which.min(abs(st_get_dimension_values(., "lon") - lo))) %>% 
  slice(lat, which.min(abs(st_get_dimension_values(., "lat") - la))) %>% 
  pull(1) # same!

