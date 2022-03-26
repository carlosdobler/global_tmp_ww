
# Script to identify the CORDEX domain to which each NBR belongs based on proximity of centroids

library(tidyverse)
library(stars)
# sf_use_s2(FALSE)

# REMO SECTION -----------------------------------------------------------------
# Reference rasters
c(st_point(c(-179.9,-89.9)),
  st_point(c(179.9,89.9))) %>% 
  st_bbox() %>% 
  st_set_crs(4326) %>% 
  st_as_stars(dx = 0.05, dy = 0.05, values = -9999) -> rast_reference_0.05

c(st_point(c(-179.9,-89.9)),
  st_point(c(179.9,89.9))) %>% 
  st_bbox() %>%
  st_set_crs(4326) %>% 
  st_as_stars(dx = 0.2, dy = 0.2, values = -9999) -> rast_reference_remo

# Import nbrs
"data/OE_Bioregions_int_gadm36_0.shp" %>% 
  st_read() -> vect_nbr

# Rasterize nbr
vect_nbr %>% 
  select(CODE) %>% 
  st_rasterize(rast_reference_0.05) -> rast_nbr_0.05

# based on area
rast_nbr_0.05 %>% 
  st_warp(rast_reference_remo, use_gdal = T, method = "mode") -> rast_nbr_remo

rast_nbr_remo[rast_nbr_remo == -9999] <- NA

# Polygonize
rast_nbr_0.05[rast_nbr_0.05 == -9999] <- NA
rast_nbr_0.05 %>% 
  st_as_sf(as_points = F, merge = T) -> vect_nbr_2

# Dissolve contiguous polygons
sf_use_s2(FALSE)
vect_nbr_2 %>%
  group_by(CODE) %>% 
  summarize() %>% 
  suppressMessages() -> vect_nbr_2

st_write(vect_nbr_2, "data/nbr_grid_0.05.gpkg", append = F)

# Table of domains
c("SAM", "CAM", "NAM", "EUR", "AFR", "WAS", "EAS", "CAS", "AUS", "SEA") %>%
  sort() %>% 
  {tibble(dom_f = .,
          dom_id = seq_along(.))} -> tb_doms

# # Download nc files to obtain domain boundaries
# "/home/cdobler/Documents/bucket_mnt/RCM_regridded_data/REMO2015/" -> dir
# 
# c("SAM", "CAM", "NAM", "EUR", "AFR", "WAS", "EAS", "CAS", "AUS", "SEA") %>%
#     walk(function(dom){
# 
#         str_glue("{dir}{dom}/daily/maximum_temperature/") %>%
#             list.files(full.names = T) %>%
#             .[str_detect(., "_1980")] %>%
#             .[str_detect(., "Had")] %>%
#             .[1] -> f
# 
#         print(f)
# 
#         file.copy(f,
#                   "data/down2")
#     })

# Obtain boundaries
tb_doms$dom_f %>%
  map_dfr(function(dom){
    
    "data/down2" %>%
      list.files(full.names = T) %>%
      .[str_detect(., dom)] -> f
    
    print(f)
    
    read_ncdf(f, make_time = F, make_units = F,
              ncsub = cbind(start = c(1,1,1),
                            count = c(NA, NA, 1))) %>%
      adrop() -> s
    
    s[!is.na(s)] <- 1
    
    # Regularize grid (old approach)
    # s %>% st_set_dimensions("lon") %>% st_set_dimensions("lat") -> s_d
    # 
    # # re-dimension
    # st_dimensions(s_d)$lon$offset <- round(st_get_dimension_values(s, "lon", center = F)[1], 1)
    # st_dimensions(s_d)$lat$offset <- round(st_get_dimension_values(s, "lat", center = F)[1], 1)
    # st_dimensions(s_d)$lon$delta <- 0.2
    # st_dimensions(s_d)$lat$delta <- 0.2
    # st_crs(s_d) <- st_crs(s)
    
    # Regularize grid (new approach)
    s %>%
      st_get_dimension_values("lon") %>% # center
      {c(first(.), last(.))} %>%
      round(1) %>%
      {. - 0.1} %>% # shift half cell to the west
      {seq(.[1], .[2], 0.2)} %>%
      {st_set_dimensions(s, "lon", values = .)} -> s
    
    s %>%
      st_get_dimension_values("lat") %>%
      {c(first(.), last(.))} %>%
      round(1) %>%
      {. - 0.1} %>% # shift half cell to the south
      {seq(.[1], .[2], 0.2)} %>%
      {st_set_dimensions(s, "lat", values = .)} -> s
    
    s %>%
      st_set_crs(4326) -> s
    
    s %>%
      st_as_sf(as_points = F, merge = T) -> v
    
    v %>%
      mutate(dom = dom) %>%
      select(-1)
    
  }) -> domain_lims

st_write(domain_lims, "data/domain_lims.gpkg", append = T)
domain_lims <- st_read("data/domain_lims.gpkg")


# *****************************************************************************
# *****************************************************************************
# By what domains are nbrs *fully* covered?
st_covered_by(vect_nbr_2, domain_lims, sparse = F) %>% 
  as_tibble() %>% 
  rename_with(~domain_lims$dom) %>% 
  bind_cols(vect_nbr_2) %>% 
  st_sf() %>% 
  st_collection_extract("POLYGON") -> vect_nbr_dom_coveredby

# Count domains/nbr
vect_nbr_dom_coveredby %>% 
  rowwise() %>% 
  mutate(n_covered = sum(c_across(domain_lims$dom))) %>% 
  ungroup() -> vect_nbr_dom_coveredby

# ******************************************************************************
# Only one domain covers completely
vect_nbr_dom_coveredby %>% 
  filter(n_covered == 1) -> vect_nbr_dom_fullycov_1

vect_nbr_dom_fullycov_1 %>% 
  st_drop_geometry() %>% 
  select(-n_covered) %>% 
  pivot_longer(-CODE, names_to = "dom_f", values_to = "v") %>% 
  filter(v == TRUE) %>%
  right_join(vect_nbr_dom_fullycov_1) %>%
  st_sf() %>% 
  select(CODE, dom_f) %>% 
  mutate(type = "fully cov by 1 dom") -> f_nbr_dom_fullycov_1

# ******************************************************************************
# Several domains cover each the domain completely
# Assign domain based on distance to centroid
vect_nbr_dom_coveredby %>%
  filter(n_covered > 1) %>% 
  select(-n_covered) -> vect_nbr_dom_fullycov_over1

# Distance
tribble(
  ~domain, ~dom, ~lon,  ~lat,
  "s_ame", "SAM", 299.7, -21.11,
  "c_ame", "CAM", 287.29, 10.20,
  "n_ame", "NAM", 263.0, 47.28,
  "euro", "EUR", 9.75, 49.68,
  "afr", "AFR", 17.60, -1.32,
  "s_asia", "WAS", 67.18, 16.93,
  "e_asia", "EAS", 116.57, 34.40,
  "c_asia", "CAS", 74.64, 47.82,
  "aus", "AUS", 147.63, -24.26,
  "se_asia", "SEA", 118.04, 6.5 
) %>% 
  mutate(lon = ifelse(lon > 180, lon-360, lon)) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) -> dom_centroids

seq_len(nrow(vect_nbr_dom_fullycov_over1)) %>% 
  map_dfr(function(i){
    
    vect_nbr_dom_fullycov_over1 %>% 
      slice(i) -> v 
    
    v %>%
      st_drop_geometry() %>% 
      pivot_longer(-CODE, names_to = "dom", values_to = "v") %>% 
      filter(v == TRUE) %>% 
      pull(dom) -> d
    
    dom_centroids %>% 
      filter(dom %in% d) %>% 
      
      mutate(value = st_distance(., v)[,1]) %>%
      st_drop_geometry() %>% 
      
      filter(value == min(value)) %>% 
      bind_cols(v) %>%
      st_sf() %>%
      rename(dom_f = dom) %>% 
      select(CODE, dom_f)
    
  }) %>% 
  mutate(type = "fully cov by 1+ dom") -> f_nbr_dom_fullycov_over1


# *****************************************************************************
# *****************************************************************************
# *Partially* covered section
vect_nbr_dom_coveredby %>% 
  filter(n_covered == 0) %>% 
  select(CODE) -> vect_nbr_dom_partcov

vect_nbr_dom_partcov %>%
  st_intersects(domain_lims, sparse = F) %>% 
  as_tibble() %>% 
  rename_with(~domain_lims$dom) %>%
  mutate(CODE = vect_nbr_dom_partcov$CODE) %>% 
  rowwise() %>% 
  mutate(n_intersected = sum(c_across(domain_lims$dom))) %>%
  ungroup() %>%
  filter(n_intersected != 0) %>% # orphans: remove
  right_join(vect_nbr_dom_partcov) %>% 
  st_sf() -> vect_nbr_dom_partcov

# *****************************************************************************
# Only one domains covers partially
vect_nbr_dom_partcov %>% 
  filter(n_intersected == 1) -> vect_nbr_dom_partcov_1

vect_nbr_dom_partcov_1 %>% 
  st_drop_geometry() %>% 
  select(-n_intersected) %>% 
  pivot_longer(-CODE, names_to = "dom_f", values_to = "v") %>% 
  filter(v == TRUE) %>%
  right_join(vect_nbr_dom_partcov_1) %>%
  st_sf() %>% 
  select(CODE, dom_f) -> f_nbr_dom_partcov_1

# Cut
st_intersection(f_nbr_dom_partcov_1, domain_lims) %>% 
  st_collection_extract("POLYGON") %>% 
  select(-dom) %>% 
  group_by(CODE, dom_f) %>% 
  summarize() %>% 
  ungroup() %>% 
  mutate(type = "part cov by 1 dom")-> f_nbr_dom_partcov_1

# *****************************************************************************
# Partially covered by more than one domain
# Assign domain based on distance
vect_nbr_dom_partcov %>% 
  filter(n_intersected > 1) -> vect_nbr_dom_partcov_over1

# Intersect domains
st_intersection(domain_lims) -> domain_lims_intersected

# Split nbrs across domains
st_intersection(vect_nbr_dom_partcov_over1 %>% 
                  select(CODE),
                domain_lims_intersected) %>% 
  st_collection_extract("POLYGON") %>% 
  
  group_by(origins, CODE) %>% # intersection splitted detached pieces
  summarize() %>% 
  ungroup() -> vect_nbr_dom_partcov_over1_2

seq_len(nrow(vect_nbr_dom_partcov_over1_2)) %>% 
  map_dfr(function(i){
    
    vect_nbr_dom_partcov_over1_2 %>% 
      slice(i) %>%
      .$origins %>% 
      unlist() -> orig
    
    # only one intersection?
    if(length(orig) == 1){
      
      vect_nbr_dom_partcov_over1_2 %>% 
        slice(i) %>% 
        mutate(dom_f = domain_lims$dom[orig]) %>% 
        select(CODE, dom_f)
      
      # more than one intersection?
    } else {
      
      vect_nbr_dom_partcov_over1_2 %>% 
        slice(i) -> v 
      
      dom_centroids %>% 
        filter(dom %in% domain_lims$dom[orig]) %>%
        
        mutate(value = st_distance(., v)[,1]) %>% 
        st_drop_geometry() %>% 
        
        filter(value == min(value)) %>% 
        bind_cols(v) %>%
        st_sf() %>% 
        rename(dom_f = dom) %>%
        select(CODE, dom_f)
      
    }
    
  }) %>%
  mutate(type = "part cov by +1 dom") -> f_nbr_dom_partcov_over1

bind_rows(f_nbr_dom_fullycov_1,
          f_nbr_dom_fullycov_over1,
          f_nbr_dom_partcov_1,
          f_nbr_dom_partcov_over1) -> nbr_domains

nbr_domains %>% 
  group_by(CODE, dom_f, type) %>% 
  summarize() %>% 
  ungroup() -> nbr_domains

st_write(nbr_domains, "data/nbr_domains.gpkg", append = F)
st_read("data/nbr_domains.gpkg") -> nbr_domains

# Rasterize
tb_doms %>% 
  right_join(nbr_domains, by = "dom_f") %>% 
  st_sf() %>% 
  select(dom_id) %>%
  st_rasterize(rast_reference_0.05) -> rast_nbr_domains_0.05

rast_nbr_domains_0.05 %>% 
  st_warp(rast_reference_remo, use_gdal = T, method = "mode") -> rast_domains_remo
rast_domains_remo[rast_domains_remo == -9999] <- NA

c(rast_nbr_remo, rast_domains_remo) %>% 
  setNames(c("nbr", "domain")) %>%
  merge() %>% 
  write_stars("data/rast_nbr_dom_remo_res.tif")



# ERA SECTION ------------------------------------------------------------------

st_bbox() %>%
  st_set_crs(4326) %>% 
  st_as_stars(dx = 0.05, dy = 0.05, values = -9999) -> rast_reference_0.05

st_bbox() %>%
  st_set_crs(4326) %>%
  st_as_stars(dx = 0.25, dy = 0.25, values = -9999) -> rast_reference_era

# Import nbrs
"data/OE_Bioregions_int_gadm36_0.shp" %>% 
  st_read() -> vect_nbr

# Rasterize nbr
vect_nbr %>% 
  select(CODE) %>% 
  st_rasterize(rast_reference_0.05) -> rast_nbr_0.05

rast_nbr_0.05 %>% 
  st_warp(rast_reference_era, use_gdal = T, method = "mode") -> rast_nbr_era
rast_nbr_era[rast_nbr_era == -9999] <- NA

rast_nbr_era %>% 
  write_stars("data/rast_nbr_era_res.tif")











# Raster with REMO resolution **************************************************

st_bbox() %>%
  st_set_crs(4326) %>%
  st_as_stars(dx = 0.2, dy = 0.2) -> rast_reference

# Rasterize nbr
vect_nbr %>% 
  mutate(CODE = ifelse(CODE == 0, 9999, CODE)) %>% # ocean will turn into 0
  select(CODE) %>% 
  st_rasterize(rast_reference) -> rast_nbr

rast_nbr[rast_nbr == 0] <- NA # ocean
rast_nbr[rast_nbr == 9999] <- 0 # original 0

rast_nbr %>% 
  as_tibble() %>%
  group_by(CODE) %>%
  summarize(n = n()) %>%  
  ungroup() %>% 
  write_csv("data/total_n_per_nbr_remo.csv")


# Raster with ERA resolution **************************************************

st_bbox() %>%
  st_set_crs(4326) %>%
  st_as_stars(dx = 0.25, dy = 0.25) -> rast_reference

# Rasterize nbr
vect_nbr %>% 
  mutate(CODE = ifelse(CODE == 0, 9999, CODE)) %>% # ocean will turn into 0
  select(CODE) %>% 
  st_rasterize(rast_reference) -> rast_nbr

rast_nbr[rast_nbr == 0] <- NA # ocean
rast_nbr[rast_nbr == 9999] <- 0 # original 0

rast_nbr %>% 
  as_tibble() %>%
  group_by(CODE) %>%
  summarize(n = n()) %>%  
  ungroup() %>% 
  write_csv("data/total_n_per_nbr_era.csv")



