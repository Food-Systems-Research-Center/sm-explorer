load('data/sm_data.rda')

get_str(sm_data$metrics)

sm_data$metrics$variable_name %>% 
  unique %>% 
  sort 

pattern <- '^oty|NAICS$|avgEmpLvl|nAvgEstab|^lq|Naics'
sm_data$metrics <- sm_data$metrics %>% 
  filter(str_detect(variable_name, pattern, negate = TRUE))

# Check
sm_data$metrics$variable_name %>% 
  unique %>% 
  sort 

# Metadata too
get_str(sm_data$metadata)
sm_data$metadata <- sm_data$metadata %>% 
  filter(str_detect(variable_name, pattern, negate = TRUE))
sm_data$metadata$variable_name %>% 
  unique %>% 
  sort

# Fix gini index definition
# test <- sm_data$metadata
# test$definition[test$variable_name == 'gini'] <- 
#   'Gini Index of income inequality'
# test$definition


usethis::use_data(sm_data, overwrite = TRUE)
