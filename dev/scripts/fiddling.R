
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



# Correlation plot --------------------------------------------------------


pacman::p_load(
  GGally,
  ggplot2,
  ggcorrplot,
  tidyr
)

load('data/dat.rda')
get_str(dat)

test <- dat %>% 
  group_by(fips, county_name, state_name) %>% # Group by columns that uniquely identify each record
  filter(year == max(year, na.rm = TRUE)) %>% # Keep only the latest year for each group
  ungroup() %>% 
  select(-year) %>% 
  unique() %>% 
  pivot_wider(names_from = 'variable_name', values_from = 'value') %>% 
  unnest()
get_str(test)
glimpse(test)
test %>% 
  select(where(is.numeric))







dat <- dat %>% 
  unique() %>% 
  pivot_wider(names_from = c('variable_name', 'year'))
get_str(dat)

# corr plot
dat %>% 
  select((where(is.numeric)))
get_str(dat)
test <- cor(dat)
ggcorrplot::ggcorrplot(dat)
?ggcorrplot

