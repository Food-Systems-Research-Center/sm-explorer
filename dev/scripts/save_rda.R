sm_data <- readRDS('dev/data/sm_data.rds')

# sm_data$metrics %>% 
#   filter(
#     variable_name == 'annualAvgEstabs111NAICS',
#     fips == '25009'
#   )

get_str(sm_data$metadata)
sm_data$metadata %>% 
  filter(variable_name == 'gini') %>% 
  pull(definition)

sm_data$metadata$definition[sm_data$metadata$variable_name == 'gini'] <- 
  'Gini Index of income inequality. 0 is perfect equality, while 1 is perfect inequality'


usethis::use_data(sm_data, overwrite = TRUE)
