sm_data <- readRDS('dev/data/sm_data.rds')

# sm_data$metrics %>% 
#   filter(
#     variable_name == 'annualAvgEstabs111NAICS',
#     fips == '25009'
#   )

usethis::use_data(sm_data, overwrite = TRUE)
