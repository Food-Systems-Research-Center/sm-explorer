
# Example Data ------------------------------------------------------------


pacman::p_load(
  dplyr,
  sf,
  tidyr,
  stringr
)

dat <- readRDS('dev/data/aggregated_data.rds') %>% 
  mutate(
    across(c('county_name', 'state_name'), ~ str_to_title(.)),
    across(where(is.numeric), ~ round(., 3))
    )

counties_2021 <- readRDS('dev/data/ne_counties_2021.RDS') %>% 
  st_transform(4326)
counties_2024 <- readRDS('dev/data/ne_counties_2024.RDS') %>% 
  st_transform(4326)

get_str(dat)


# Test Join and Map -------------------------------------------------------


dat$variable_name %>% 
  unique %>% 
  sort

pre <- dat %>% 
  filter(
    str_detect(variable_name, 'number_food_hubs')
  )

get_str(pre)

# usethis::use_data(dat, overwrite = TRUE)
# usethis::use_data(counties_2021)
# usethis::use_data(counties_2024)


# Updating Data -----------------------------------------------------------


pacman::p_load(
  dplyr,
  sf
)

# Check old dat
get_str(dat)

# Need fips, county_name, state_name, year, variable_name, value,
# dimension, index, indicator


dat <- readRDS('dev/data/aggregated_metrics.rds')
meta <- readRDS('dev/data/aggregated_meta.rds')
counties <- readRDS('dev/data/ne_counties_2024.RDS')
fips_key <- readRDS('dev/data/fips_key.RDS')


get_str(dat)
get_str(meta)


# have to join to fips key, then meta to get dimensions
dat <- inner_join(dat, fips_key, by = 'fips') %>% 
  left_join(meta, by = 'variable_name') %>% 
  select(-c(scope:last_col())) %>% 
  filter(!is.na(dimension)) %>% 
  mutate(value = as.numeric(value))
    
get_str(dat)

usethis::use_data(dat, overwrite = TRUE)
