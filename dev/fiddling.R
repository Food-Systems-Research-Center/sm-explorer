
# Example Data ------------------------------------------------------------


pacman::p_load(
  dplyr,
  sf,
  tidyr,
  stringr
)

dat <- readRDS('ne_data.rds')
counties <- readRDS('counties.RDS')

get_str(dat)
get_str(counties)

# Get rid of rando columns
counties <- counties %>%
  select(
    geoid, 
    county = namelsad,
    aland,
    awater,
    geometry
  )
get_str(counties)

# Same with dat
dat$variable_name %>% unique
dat <- dat %>% 
  filter(!str_detect(variable_name, '^total')) %>% 
  pivot_wider(names_from = 'variable_name') %>% 
  unnest(cols = everything())
get_str(dat)

# join data with counties
# dat <- counties %>%
#   inner_join(dat, by = join_by(geoid == fips)) %>% 
#   select(-county_name)

get_str(dat)
get_size(dat)
dat

map_dat <- dat %>% 
  filter(!is.na(food_hardship), year == 2020) %>% 
  group_by(county_name, fips) %>% 
  summarize(hardship = mean(food_hardship, na.rm = TRUE)) %>% 
  left_join(counties, by = join_by(fips == geoid)) %>% 
  st_as_sf()
get_str(map_dat)
get_size(map_dat)

mapview::mapview(map_dat, zcol = 'hardship')

# Will have to have reactives to pick a variable, then pick a year from available years
# or just get rid of all years but the last?

# Let's use map_dat and also full dat
usethis::use_data(dat)
usethis::use_data(map_dat)
