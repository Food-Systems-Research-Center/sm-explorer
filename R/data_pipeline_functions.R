# Data Pipeline Functions
# 2024-10-04

# Several functions to help in wrangle aggregated long data and prepare for 
# mapping and analysis.

pacman::p_load(
  dplyr,
  tidyr,
  stringr
)

# Convenience function to filter so that each variable_name only has latest year
get_latest_year <- function(df, var_col = 'variable_name', year_col = 'year'){
  
  # Make sure year column is numeric
  df <- mutate(df, {{ year_col }} := as.numeric(.data[[year_col]]))
  
  # Get unique vector of variables from variable_name column
  vars <- unique(df[[var_col]])
  
  # Get new df with only the latest year of each variable
  # Map over each variable_name
  filtered_df <- map(vars, \(var) {
    
    # Get all unique years
    unique_years <- df %>%
      filter(.data[[var_col]] == var) %>%
      pull({{ year_col }}) %>% 
      unique()
    
    # Filter to the lastest unique year for each variable
    out <- df %>% 
      filter(.data[[var_col]] == var, .data[[year_col]] == max(unique_years))
    return(out)
  }) %>% 
    
    # Put each variable back together
    bind_rows()
  
  return(filtered_df)
}

# Pivot wider and add year to the variable name
make_wider <- function(df, 
                       var_col = 'variable_name', 
                       year_col = 'year', 
                       val_col = 'value',
                       id_col = 'fips') {
  out <- df %>% 
    mutate(
      {{ var_col }} := paste0(.data[[var_col]], '_', .data[[year_col]])
      # .keep = 'unused'
    ) %>% 
    pivot_wider(
      # id_cols = {{ id_col }},
      names_from = {{ var_col }},
      values_from = {{ val_col }}
    )
}

# Reduce to counties
filter_to_counties <- function(df, 
                               fips_path = '5_objects/fips_all.rds',
                               fips_col = 'fips') {
  # If CT has both counties and regions, remove it from analysis and give warning
  if (any(str_detect(df[[fips_col]], '^09[0-9]{2}0$')) 
      & any(str_detect(df[[fips_col]], '^09[0-9]{2}[1-9]$'))) {
    df <- df %>% filter(str_detect(.data[[fips_col]], '^09', negate = TRUE))
    warning(
      'Connecticut data contain both counties and governance regions. ',
      'These geographies changed in 2022, and data cannot be compared before and after that time. ',
      'All data from Connecticut have been removed from the data frame.',
      call. = FALSE
    )
  }
  
  fips_all <- readRDS(fips_path)
  out <- df %>% 
    filter(.data[[fips_col]] %in% fips_all)
  
  return(out)
}

