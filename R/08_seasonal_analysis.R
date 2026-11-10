
# 08_seasonal_analysis.R


library(dplyr)
library(ggplot2)
library(lubridate)



# 1. Create month variable


birds_clean <- birds_clean %>%
  mutate(
    Month = month(StartDate),
    Month_Name = month(StartDate, label = TRUE, abbr = FALSE)
  )

head(birds_clean)



# 2. Number of bird records by month


monthly_records <- birds_clean %>%
  filter(!is.na(Month)) %>%
  count(Month, Month_Name, name = "records") %>%
  arrange(Month)

monthly_records



# 3. Plot bird records by month


ggplot(
  monthly_records,
  aes(
    x = Month_Name,
    y = records,
    fill = Month_Name
  )
) +
  geom_col(width = 0.75) +
  scale_fill_viridis_d(
    option = "turbo",
    guide = "none"
  ) +
  labs(
    title = "Bird Recording Activity by Month",
    subtitle = "National Biodiversity Data Centre records",
    x = "Month",
    y = "Number of Bird Records"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "grey40"
    ),
    axis.title = element_text(face = "bold"),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1
    ),
    panel.grid.major.x = element_blank()
  )



# 4. Unique species recorded by month


monthly_species <- birds_clean %>%
  filter(!is.na(Month)) %>%
  group_by(Month, Month_Name) %>%
  summarise(
    unique_species = n_distinct(TaxonName, na.rm = TRUE),
    records = n(),
    .groups = "drop"
  ) %>%
  arrange(Month)

monthly_species



# 5. Plot monthly species richness


ggplot(
  monthly_species,
  aes(x = Month_Name, y = unique_species)
) +
  geom_col() +
  labs(
    title = "Bird Species Richness by Month",
    subtitle = "Number of unique species recorded in each month",
    x = "Month",
    y = "Number of Unique Species"
  ) +
  theme_minimal()



# 6. Monthly recording effort vs species richness


monthly_correlation <- cor(
  monthly_species$records,
  monthly_species$unique_species,
  use = "complete.obs"
)

monthly_correlation



# 7. Relationship between monthly effort and richness


ggplot(
  monthly_species,
  aes(
    x = records,
    y = unique_species
  )
) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  labs(
    title = "Monthly Recording Effort and Species Richness",
    subtitle = "Relationship between number of records and unique species",
    x = "Number of Bird Records",
    y = "Number of Unique Species"
  ) +
  theme_minimal()



# 8. Seasons


birds_clean <- birds_clean %>%
  mutate(
    Season = case_when(
      Month %in% c(12, 1, 2) ~ "Winter",
      Month %in% c(3, 4, 5) ~ "Spring",
      Month %in% c(6, 7, 8) ~ "Summer",
      Month %in% c(9, 10, 11) ~ "Autumn",
      TRUE ~ NA_character_
    )
  )



# 9. Records by season


seasonal_records <- birds_clean %>%
  filter(!is.na(Season)) %>%
  count(Season, name = "records")

seasonal_records



# 10. Unique species by season


seasonal_species <- birds_clean %>%
  filter(!is.na(Season)) %>%
  group_by(Season) %>%
  summarise(
    unique_species = n_distinct(TaxonName, na.rm = TRUE),
    records = n(),
    .groups = "drop"
  )

seasonal_species



# 11. Plot seasonal species richness


ggplot(
  seasonal_species,
  aes(x = Season, y = unique_species)
) +
  geom_col() +
  labs(
    title = "Bird Species Richness by Season",
    subtitle = "Number of unique species recorded in each season",
    x = "Season",
    y = "Number of Unique Species"
  ) +
  theme_minimal()



# 12. Most frequently recorded species in each season


top_species_season <- birds_clean %>%
  filter(!is.na(Season)) %>%
  count(Season, TaxonName, name = "records") %>%
  group_by(Season) %>%
  slice_max(
    order_by = records,
    n = 5,
    with_ties = FALSE
  ) %>%
  arrange(Season, desc(records)) %>%
  ungroup()

top_species_season



# 13. Seasonal summary


seasonal_summary <- seasonal_species %>%
  arrange(desc(unique_species))

seasonal_summary