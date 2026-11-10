
# 06_geographic_analysis.R


library(dplyr)
library(ggplot2)


# 1. Bird Records by County


county_records <- birds_clean %>%
  filter(!is.na(County)) %>%
  group_by(County) %>%
  summarise(
    records = n(),
    unique_species = n_distinct(TaxonName, na.rm = TRUE),
    .groups = "drop"
  )

county_records



# 2. Top 20 Counties by Number of Records


top_20_counties_records <- county_records %>%
  arrange(desc(records)) %>%
  head(20)

top_20_counties_records



# 3. Top 20 Counties by Species Richness


top_20_counties_species <- county_records %>%
  arrange(desc(unique_species)) %>%
  head(20)

top_20_counties_species



# 4. Plot Top 20 Counties by Records


ggplot(
  top_20_counties_records,
  aes(
    x = reorder(County, records),
    y = records
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Irish Counties by Bird Records",
    subtitle = "National Biodiversity Data Centre records",
    x = "County",
    y = "Number of Bird Records"
  ) +
  theme_minimal()



# 5. Plot Top 20 Counties by Species Richness


ggplot(
  top_20_counties_species,
  aes(
    x = reorder(County, unique_species),
    y = unique_species
  )
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Top 20 Irish Counties by Bird Species Diversity",
    subtitle = "National Biodiversity Data Centre records",
    x = "County",
    y = "Number of Unique Bird Species"
  ) +
  theme_minimal()



# 6. Relationship Between Records and Species Richness


county_correlation <- cor(
  county_records$records,
  county_records$unique_species,
  use = "complete.obs"
)

county_correlation



# 7. Recording Activity vs Species Richness


ggplot(
  county_records,
  aes(
    x = records,
    y = unique_species
  )
) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Recording Activity and Bird Species Diversity by County",
    subtitle = "National Biodiversity Data Centre records",
    x = "Number of Bird Records",
    y = "Number of Unique Species"
  ) +
  theme_minimal()



# 8. County-Level Statistical Model


county_model <- lm(
  unique_species ~ records,
  data = county_records
)

summary(county_model)



# 9. County Ranking Summary


county_summary <- county_records %>%
  arrange(desc(unique_species))

head(county_summary, 20)

