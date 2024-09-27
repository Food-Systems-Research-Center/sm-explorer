
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

# usethis::use_data(dat)
# usethis::use_data(counties_2021)
# usethis::use_data(counties_2024)
