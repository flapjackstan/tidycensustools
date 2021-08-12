# Various functions to access census variables and use them in
# in various analyses
#
# Todo:
#   - Heterogeneity
#   - Majority Function


#' @title
#' Census Table Time Series
#'
#' @description
#' This function returns a dataframe of specified census table within designated areas and years
#'
#' @section Features to Implement:
#' Todo list of things to add
#'
#' \itemize{
#'   \item Grab geographies from multiple states/counties
#'   \item Grab variables from different tables
#'   \item Return object in long format
#' }
#'
#' @param census_table character strings of census table id
#'
#' @param geography character string of geography that the data is available at (tract, county, zcta, etc...)
#'
#' @param state character string of state study areas are in (without this, downloads all counties in US)
#'
#' @param study_areas vector of character strings of areas that the table allows for
#'
#' @param years vector of numeric type for each year desired
#'
#' @return Returns: tibble dataframe in wide format
#'#'
#' @examples
#' census_table_timeseries("B25014", "county", "CA", ("Los Angeles", "Orange"), (2019,2018,2017))
#'
#' @export

census_table_timeseries <- function(census_table, geography, state, study_areas, years) {

  df <- tibble()

  for(i in years){

    print(paste("Downloading data for year:", i))

    temp <- get_acs(geography = geography,
                    table = census_table,
                    state = state,
                    county = study_areas,
                    year = i,
                    output = "tidy",
                    geometry = FALSE)

    temp <- temp %>% mutate(year = i)

    df <- rbind(df,temp)
  }

  # grab names of variables to join
  var_names <- load_variables(years[1], 'acs5', cache = TRUE)

  # merge names
  df <- merge(df,var_names, by.x = "variable", by.y = "name")

  # remove unnecessary variables
  df_wide <- df %>% select(-1, -5, -8)

  df_wide <- df_wide %>%  pivot_wider(names_from = label, values_from = estimate)

  return(df_wide)
}


#' @title
#' Race and Ethnicity Table (B03002)
#'
#' @description
#' This function returns a dataframe of the race and ethnicity table that includes percentages and an option for
#' homogeneity and majority demographic for the geography. (Note that majority returns largest percentage and not
#' whether the group is over a threshhold of the geography. eg: does not return if group is >50% of the geography)
#'
#' @section Features to Implement:
#' Todo list of things to add
#'
#' \itemize{
#'   \item Option for percentage thresh hold -> binary variable for is_over_fifty_percent_of_tract
#' }
#'
#' @param geography character string of geography that the data is available at (tract, county, zcta, etc...)
#'
#' @param state character string of state study areas are in (without this, downloads all counties in US)
#'
#' @param county character string of county study areas are in (without this, downloads all counties in US)
#'
#' @param homogeneity binary to return numeric value of homogeneity (closer to 1 the more homogenous and of one race)
#'
#' @param majority binary to return character string of largest group percentage
#'
#' @return Returns: tibble dataframe in wide format
#'#'
#' @examples
#' race_ethnicity("tract", "CA", "Los Angeles", homogeneity = TRUE, majority = FALSE)
#' race_ethnicity("tract", "CA", "Los Angeles", TRUE, FALSE)
#'
#' @export

race_ethnicity <- function(geography, state, county, homogeneity = TRUE, majority = TRUE) {


}
