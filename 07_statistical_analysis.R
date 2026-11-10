# 07_statistical_analysis.R
library(dplyr)
library(ggplot2)
library(MASS)
library(car)


# 1. PREPARE DATA


model_data <- yearly_richness %>%
  dplyr::filter(
    !is.na(Year),
    !is.na(unique_species),
    !is.na(total_records)
  ) %>%
  dplyr::arrange(Year)


# 2. MODEL 1: YEAR VS SPECIES RICHNESS


richness_model <- MASS::glm.nb(
  unique_species ~ Year,
  data = model_data
)

summary(richness_model)


# 3. MODEL 2: RECORDING EFFORT VS SPECIES RICHNESS


effort_model <- MASS::glm.nb(
  unique_species ~ total_records,
  data = model_data
)

summary(effort_model)


# 4. CORRELATION ANALYSIS


richness_correlation <- cor(
  model_data$Year,
  model_data$unique_species,
  use = "complete.obs"
)

effort_correlation <- cor(
  model_data$total_records,
  model_data$unique_species,
  use = "complete.obs"
)

richness_correlation
effort_correlation


# 5. MODEL 3: YEAR + RECORDING EFFORT


adjusted_model <- MASS::glm.nb(
  unique_species ~ Year + total_records,
  data = model_data
)

summary(adjusted_model)

# 6. MODEL COMPARISON USING AIC AND BIC


nb_model_comparison <- data.frame(
  Model = c(
    "Year only",
    "Recording effort only",
    "Year + Recording effort"
  ),
  
  AIC = c(
    AIC(richness_model),
    AIC(effort_model),
    AIC(adjusted_model)
  ),
  
  BIC = c(
    BIC(richness_model),
    BIC(effort_model),
    BIC(adjusted_model)
  )
)

print(nb_model_comparison)


# 7. PREDICTIONS FROM ALL THREE MODELS


model_data$year_only_predicted <- predict(
  richness_model,
  newdata = model_data,
  type = "response"
)

model_data$effort_only_predicted <- predict(
  effort_model,
  newdata = model_data,
  type = "response"
)

model_data$combined_predicted <- predict(
  adjusted_model,
  newdata = model_data,
  type = "response"
)

head(model_data, 20)


# 8. GRAPH 1: YEAR VS SPECIES RICHNESS
# NEGATIVE BINOMIAL YEAR-ONLY MODEL


year_grid <- data.frame(
  Year = seq(
    min(model_data$Year),
    max(model_data$Year),
    length.out = 200
  )
)

year_prediction <- predict(
  richness_model,
  newdata = year_grid,
  type = "link",
  se.fit = TRUE
)

year_grid$predicted <- exp(year_prediction$fit)

year_grid$lower <- exp(
  year_prediction$fit -
    1.96 * year_prediction$se.fit
)

year_grid$upper <- exp(
  year_prediction$fit +
    1.96 * year_prediction$se.fit
)

graph1 <- ggplot() +
  
  geom_ribbon(
    data = year_grid,
    aes(
      x = Year,
      ymin = lower,
      ymax = upper
    ),
    fill = "#B7E4C7",
    alpha = 0.35
  ) +
  
  geom_line(
    data = model_data,
    aes(
      x = Year,
      y = unique_species,
      colour = "Observed"
    ),
    linewidth = 1.2
  ) +
  
  geom_point(
    data = model_data,
    aes(
      x = Year,
      y = unique_species,
      colour = "Observed"
    ),
    size = 2.5
  ) +
  
  geom_line(
    data = year_grid,
    aes(
      x = Year,
      y = predicted,
      colour = "Predicted"
    ),
    linewidth = 1.3,
    linetype = "dashed"
  ) +
  
  scale_colour_manual(
    values = c(
      "Observed" = "#087F5B",
      "Predicted" = "#E09F3E"
    )
  ) +
  
  labs(
    title = "Observed vs Predicted Bird Species Richness",
    subtitle = "Negative Binomial Model based on Year",
    x = "Year",
    y = "Number of Unique Species",
    colour = "Data"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "darkgreen"
    ),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

print(graph1)


# 9. GRAPH 2: RECORDING EFFORT VS SPECIES RICHNESS
# NEGATIVE BINOMIAL RECORDING-EFFORT MODEL


effort_grid <- data.frame(
  total_records = seq(
    min(model_data$total_records),
    max(model_data$total_records),
    length.out = 200
  )
)

effort_prediction <- predict(
  effort_model,
  newdata = effort_grid,
  type = "link",
  se.fit = TRUE
)

effort_grid$predicted <- exp(effort_prediction$fit)

effort_grid$lower <- exp(
  effort_prediction$fit -
    1.96 * effort_prediction$se.fit
)

effort_grid$upper <- exp(
  effort_prediction$fit +
    1.96 * effort_prediction$se.fit
)

graph2 <- ggplot() +
  
  geom_ribbon(
    data = effort_grid,
    aes(
      x = total_records,
      ymin = lower,
      ymax = upper
    ),
    fill = "#B8C9E8",
    alpha = 0.4
  ) +
  
  geom_point(
    data = model_data,
    aes(
      x = total_records,
      y = unique_species
    ),
    colour = "#555555",
    size = 2.5,
    alpha = 0.85
  ) +
  
  geom_line(
    data = effort_grid,
    aes(
      x = total_records,
      y = predicted
    ),
    colour = "darkgreen",
    linewidth = 1.3
  ) +
  
  labs(
    title = "Relationship Between Recording Activity and Species Richness",
    subtitle = "Negative Binomial Model based on Recording Effort",
    x = "Number of Bird Records",
    y = "Number of Unique Species"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 09
    ),
    plot.subtitle = element_text(
      size = 11,
      colour = "darkgreen"
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(graph2)


# 10. GRAPH 3: OBSERVED VS PREDICTED SPECIES RICHNESS
# COMBINED NEGATIVE BINOMIAL MODEL


graph3 <- ggplot(
  model_data,
  aes(
    x = unique_species,
    y = combined_predicted
  )
) +
  
  geom_point(
    colour = "#087F5B",
    size = 3,
    alpha = 0.8
  ) +
  
  geom_abline(
    intercept = 0,
    slope = 1,
    colour = "#E09F3E",
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Observed vs Predicted Bird Species Richness",
    subtitle = "Combined Negative Binomial Model: Year + Recording Effort",
    x = "Observed Species Richness",
    y = "Predicted Species Richness"
  ) +
  
  theme_minimal(base_size = 13) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 14
    ),
    plot.subtitle = element_text(
      size = 10,
      colour = "grey40"
    ),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(graph3)


# 11. ANNUAL CHANGE IN SPECIES RICHNESS


yearly_change <- yearly_richness %>%
  dplyr::arrange(Year) %>%
  dplyr::mutate(
    change_in_species =
      unique_species - dplyr::lag(unique_species)
  )

head(yearly_change, 10)


# 12. YEARS WITH LARGEST INCREASES


largest_increases <- yearly_change %>%
  dplyr::filter(!is.na(change_in_species)) %>%
  dplyr::arrange(desc(change_in_species)) %>%
  head(10)

print(largest_increases)


# 13. YEARS WITH LARGEST DECREASES


largest_decreases <- yearly_change %>%
  dplyr::filter(!is.na(change_in_species)) %>%
  dplyr::arrange(change_in_species) %>%
  head(10)

print(largest_decreases)


# 14. PLOT OF ANNUAL CHANGES


graph_annual_change <- ggplot(
  yearly_change,
  aes(
    x = Year,
    y = change_in_species
  )
) +
  
  geom_col(
    fill = "#087F5B",
    na.rm = TRUE
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = "dashed"
  ) +
  
  labs(
    title = "Annual Change in Recorded Bird Species",
    subtitle = "Change compared with the previous recorded year",
    x = "Year",
    y = "Change in Number of Species"
  ) +
  
  theme_minimal()

print(graph_annual_change)


# 15. MODEL DIAGNOSTICS


par(mfrow = c(2, 2))
plot(adjusted_model)
par(mfrow = c(1, 1))

# Dispersion statistics

nb_dispersion_results <- data.frame(
  Model = c(
    "Year only",
    "Recording effort only",
    "Year + Recording effort"
  ),
  
  Dispersion = c(
    sum(residuals(richness_model, type = "pearson")^2) /
      df.residual(richness_model),
    
    sum(residuals(effort_model, type = "pearson")^2) /
      df.residual(effort_model),
    
    sum(residuals(adjusted_model, type = "pearson")^2) /
      df.residual(adjusted_model)
  )
)

print(nb_dispersion_results)


# 16. CONFIDENCE INTERVALS AND INCIDENCE RATE RATIOS


confint(adjusted_model)

exp(coef(adjusted_model))

exp(confint(adjusted_model))


# 17. MULTICOLLINEARITY CHECK


vif(adjusted_model)


# 18. STATISTICAL MODEL SUMMARY


deviance_explained <- function(model) {
  1 - (deviance(model) / model$null.deviance)
}

model_results <- data.frame(
  Model = c(
    "Year only",
    "Recording effort only",
    "Year + Recording effort"
  ),
  
  Deviance_Explained = c(
    deviance_explained(richness_model),
    deviance_explained(effort_model),
    deviance_explained(adjusted_model)
  ),
  
  AIC = c(
    AIC(richness_model),
    AIC(effort_model),
    AIC(adjusted_model)
  ),
  
  BIC = c(
    BIC(richness_model),
    BIC(effort_model),
    BIC(adjusted_model)
  )
)

print(model_results)


# 19. PREDICTION PERFORMANCE COMPARISON


prediction_comparison <- data.frame(
  Model = c(
    "Year only",
    "Year + Recording effort"
  ),
  
  MAE = c(
    mean(abs(
      model_data$unique_species -
        model_data$year_only_predicted
    )),
    
    mean(abs(
      model_data$unique_species -
        model_data$combined_predicted
    ))
  ),
  
  RMSE = c(
    sqrt(mean(
      (model_data$unique_species -
         model_data$year_only_predicted)^2
    )),
    
    sqrt(mean(
      (model_data$unique_species -
         model_data$combined_predicted)^2
    ))
  )
)

print(prediction_comparison)


# 20. RECORDING EFFORT OVER TIME


effort_time_model <- MASS::glm.nb(
  total_records ~ Year,
  data = model_data
)

summary(effort_time_model)

effort_year_grid <- data.frame(
  Year = seq(
    min(model_data$Year),
    max(model_data$Year),
    length.out = 200
  )
)

effort_year_grid$predicted <- predict(
  effort_time_model,
  newdata = effort_year_grid,
  type = "response"
)

graph_effort_time <- ggplot() +
  
  geom_point(
    data = model_data,
    aes(x = Year, y = total_records),
    colour = "#087F5B",
    size = 2
  ) +
  
  geom_line(
    data = effort_year_grid,
    aes(x = Year, y = predicted),
    colour = "#E09F3E",
    linewidth = 1.2
  ) +
  
  labs(
    title = "Recording Effort in Ireland Over Time",
    subtitle = "Negative Binomial Model of Annual Recording Activity",
    x = "Year",
    y = "Total Bird Records"
  ) +
  
  theme_minimal()

print(graph_effort_time)

effort_year_correlation <- cor(
  model_data$Year,
  model_data$total_records,
  use = "complete.obs"
)

effort_year_correlation

# 21. CORRELATION MATRIX


correlation_matrix <- cor(
  model_data %>%
    dplyr::select(
      Year,
      total_records,
      unique_species
    ),
  use = "complete.obs"
)

print(correlation_matrix)


# 22. LARGEST PREDICTION ERRORS


prediction_errors <- model_data %>%
  dplyr::mutate(
    prediction_error =
      unique_species - combined_predicted,
    
    absolute_error =
      abs(prediction_error)
  ) %>%
  dplyr::arrange(desc(absolute_error)) %>%
  dplyr::select(
    Year,
    unique_species,
    total_records,
    combined_predicted,
    prediction_error,
    absolute_error
  ) %>%
  head(10)

print(prediction_errors)


# 23. EXPORT STATISTICAL RESULTS


write.csv(
  model_results,
  "model_comparison.csv",
  row.names = FALSE
)

write.csv(
  nb_model_comparison,
  "negative_binomial_model_comparison.csv",
  row.names = FALSE
)

write.csv(
  nb_dispersion_results,
  "model_dispersion_results.csv",
  row.names = FALSE
)

write.csv(
  prediction_comparison,
  "prediction_performance.csv",
  row.names = FALSE
)

write.csv(
  largest_increases,
  "largest_species_increases.csv",
  row.names = FALSE
)

write.csv(
  largest_decreases,
  "largest_species_decreases.csv",
  row.names = FALSE
)

write.csv(
  prediction_errors,
  "largest_prediction_errors.csv",
  row.names = FALSE
)


# 24. SAVE ALL GRAPHS


if (!dir.exists("figures")) {
  dir.create("figures")
}

ggsave(
  "figures/graph1_year_negative_binomial.png",
  graph1,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "figures/graph2_effort_negative_binomial.png",
  graph2,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "figures/graph3_combined_observed_predicted.png",
  graph3,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "figures/graph_annual_change.png",
  graph_annual_change,
  width = 10,
  height = 6,
  dpi = 300
)

ggsave(
  "figures/graph_recording_effort_over_time.png",
  graph_effort_time,
  width = 10,
  height = 6,
  dpi = 300
)
