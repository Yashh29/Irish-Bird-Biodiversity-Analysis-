# 02 - Initial Data Exploration


library(dplyr)
library(readr)

# Load dataset
birds <- read_tsv(
  "data/BirdsOfIreland.txt",
  show_col_types = FALSE
)


# 1. Dataset size


dim(birds)



# 2. Number of unique species


birds %>%
  summarise(
    scientific_species = n_distinct(TaxonName, na.rm = TRUE),
    common_species = n_distinct(`Common name`, na.rm = TRUE)
  )



# 3. Number of counties


birds %>%
  summarise(
    counties = n_distinct(County, na.rm = TRUE)
  )



# 4. Date range


birds %>%
  summarise(
    earliest_date = min(StartDate, na.rm = TRUE),
    latest_date = max(StartDate, na.rm = TRUE)
  )



# 5. Missing values


missing_values <- data.frame(
  variable = names(birds),
  missing = sapply(birds, function(x) sum(is.na(x))),
  missing_percent = sapply(
    birds,
    function(x) mean(is.na(x)) * 100
  )
)

missing_values



# 6. Most frequently recorded species


birds %>%
  count(`Common name`, sort = TRUE) %>%
  head(20)



# 7. Records by county


birds %>%
  count(County, sort = TRUE) %>%
  head(20)



# 8. Habitat information


birds %>%
  count(`Habitat description`, sort = TRUE) %>%
  head(20)



# 9. Type of sighting


birds %>%
  count(`Type of sighting`, sort = TRUE) %>%
  head(20)



# 10. Survey names


birds %>%
  count(`Survey name`, sort = TRUE) %>%
  head(20)

