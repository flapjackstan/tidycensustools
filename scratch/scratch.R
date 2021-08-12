library(tidyverse)
library(tidycensus)


census_api_key("6ed2c84b691cee46900c6dc2d03ed90d8d4db051", overwrite=TRUE)

# temp <- get_estimates(
#   geography = "county",
#   product = "characteristics",
#   breakdown = c("RACE"),
#   breakdown_labels = TRUE,
#   year = 2019,
#   state = "CA",
#   county = "Los Angeles",
#   output = "wide",
#   geometry = FALSE,
#   show_call = TRUE
# )

temp <- get_acs(geography = "tract",
                table = "B03002",
                state = "CA",
                county = "Los Angeles",
                output = "tidy",
                geometry = FALSE)

# grab names of variables to join
var_names <- load_variables(2019, 'acs5', cache = TRUE)

# merge names
la <- merge(temp,var_names, by.x = "variable", by.y = "name")

# remove unnecessary variables
la_wide <- la %>% select(-1, -5, -7)

la_wide <- la_wide %>%  pivot_wider(names_from = label, values_from = estimate)

#write_csv(tibble(names(la_wide)), "../../census.csv")

data("race_by_hispanic_rename_vars")

names(la_wide) <- race_by_hispanic_rename_vars$desired_name

la_wide <- la_wide %>% select(-Drop)

# Need to incorporate this somehow https://censusreporter.org/tables/B03001/

# create percentages for every variables
la_wide <- la_wide %>% mutate_at(vars(NH_Total:H_Multiple), funs(pcnt = ./la_wide$Total))

#write_csv(tibble(la_wide), "../data/census_pcnt.csv")

la_wide <- la_wide %>% mutate(homogeneity = (NH_White_pcnt^2 + NH_Black_pcnt^2 + NH_AIAN_pcnt^2 +
                                                      NH_Asian_pcnt^2 + NH_NHPI_pcnt^2 + NH_Other_pcnt^2 +
                                                      NH_Multiple_pcnt^2 + H_White_pcnt^2 + H_Black_pcnt^2 +
                                                      H_AIAN_pcnt^2 + H_Asian_pcnt^2 + H_NHPI_pcnt^2 + H_Other_pcnt^2 +
                                                      H_Multiple_pcnt^2))


homo_la <- la_wide %>% filter(all_race_homogeneity >= .80)


la_long <- la_wide %>% pivot_longer(cols=3:36, names_to = "variable", values_to = "estimate")

# remove variables that contain word total, then remove variables that dont contain the word pcnt,
# then groupby id, then assign majority the first categorical variable when estimate is sorted in descending order
summary_table <- la_long %>% filter(!str_detect(variable, 'Total')) %>%
                             filter(str_detect(variable, 'pcnt')) %>%
                             group_by(GEOID) %>% summarise(majority = variable[order(-estimate)[1]])

la_wide <- merge(la_wide, summary_table, by="GEOID")
