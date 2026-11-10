# 04 - Species Analysis


library(tidyverse)


# 1. Number of records for each species


species_records <- birds_clean %>%
  count(TaxonName, sort = TRUE)

species_records

#Displaying common names of Bird instead of Scientific name
species_records <- birds_clean %>%
  filter(!is.na(`Common name`), `Common name` != "") %>%
  count(`Common name`, sort = TRUE)

species_records


# View top 20
head(species_records, 20)

n_distinct(birds_clean$TaxonName)


# 2. Top 20 most recorded species

top_20_species <- species_records %>%
  slice_head(n = 20)

ggplot(
  top_20_species,
  aes(
    x = reorder(`Common name`, n),
    y = n,
    fill = `Common name`
  )
) +
  geom_col(width = 0.8) +
  coord_flip() +
  scale_fill_viridis_d(option = "turbo", guide = "none") +
  labs(
    title = "Top 20 Most Frequently Recorded Bird Species",
    subtitle = "National Biodiversity Data Centre records",
    x = "Bird Species",
    y = "Number of Records"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 9),
    plot.subtitle = element_text(size = 10, colour = "grey40"),
    axis.text = element_text(size = 10),
    axis.title = element_text(face = "bold"),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(15, 25, 15, 15)
  )


# 7. Species diversity by county


county_species <- birds_clean %>%
  filter(!is.na(County), !is.na(TaxonName)) %>%
  group_by(County) %>%
  summarise(
    records = n(),
    unique_species = n_distinct(TaxonName),
    .groups = "drop"
  ) %>%
  arrange(desc(unique_species))

# View results
county_species

# Top 20 counties by species diversity
top_20_counties <- county_species %>%
  slice_head(n = 20)

head(top_20_counties, 20)


# 8. Plot species diversity by county


ggplot(
  top_20_counties,
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


