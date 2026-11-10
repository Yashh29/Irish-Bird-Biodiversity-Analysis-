
# IRISH BIRD BIODIVERSITY EXPLORER


library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)


# 1. FIND PROJECT ROOT


find_data_file <- function() {
  
  possible_files <- c(
    file.path("data", "BirdsOfIreland.txt"),
    file.path("..", "data", "BirdsOfIreland.txt"),
    file.path("../..", "data", "BirdsOfIreland.txt")
  )
  
  found <- possible_files[file.exists(possible_files)]
  
  if (length(found) == 0) {
    
    stop(
      paste0(
        "\nCould not find BirdsOfIreland.txt.\n\n",
        "Expected structure:\n",
        "Irish Biodiversity Analysis/\n",
        "├── data/\n",
        "│   └── BirdsOfIreland.txt\n",
        "└── shiny/\n",
        "    └─ app.R\n"
      )
    )
  }
  
  found[1]
}

data_file <- find_data_file()



# 2. LOAD DATA


birds <- read.delim(
  data_file,
  header = TRUE,
  sep = "\t",
  stringsAsFactors = FALSE,
  check.names = FALSE
)



# 3. CHECK REQUIRED COLUMNS


required_columns <- c(
  "Date",
  "TaxonName",
  "County"
)

missing_columns <- setdiff(
  required_columns,
  names(birds)
)

if (length(missing_columns) > 0) {
  
  stop(
    paste(
      "Missing required columns:",
      paste(missing_columns, collapse = ", ")
    )
  )
}



# 4. CREATE YEAR FROM DATE


birds$Date <- suppressWarnings(
  parse_date_time(
    as.character(birds$Date),
    orders = c(
      "ymd",
      "dmy",
      "mdy",
      "ymd HMS",
      "dmy HMS",
      "mdy HMS"
    )
  )
)

birds$Year <- year(birds$Date)



# 5. CLEAN DATA


birds <- birds %>%
  
  mutate(
    
    TaxonName = trimws(
      as.character(TaxonName)
    ),
    
    County = trimws(
      as.character(County)
    )
    
  ) %>%
  
  filter(
    !is.na(Year)
  )



# 6. CREATE SEASON


birds <- birds %>%
  
  mutate(
    
    Season = case_when(
      
      month(Date) %in% c(12, 1, 2) ~ "Winter",
      
      month(Date) %in% c(3, 4, 5) ~ "Spring",
      
      month(Date) %in% c(6, 7, 8) ~ "Summer",
      
      month(Date) %in% c(9, 10, 11) ~ "Autumn",
      
      TRUE ~ NA_character_
      
    )
    
  )



# 7. BASIC YEAR RANGE


year_min <- min(
  birds$Year,
  na.rm = TRUE
)

year_max <- max(
  birds$Year,
  na.rm = TRUE
)



# 8. COLOUR PALETTE


forest_green <- "#145A45"
deep_green   <- "#0B3D2E"
soft_green   <- "#E8F3EE"

teal <- "#2A8C82"

gold <- "#D4A72C"
light_gold <- "#F3E4B2"

orange <- "#D97941"

cream <- "#F7F9F6"

text_dark <- "#18332A"
text_soft <- "#6B7C74"



# 9. GLOBAL GRAPH THEME


theme_bird <- function() {
  
  theme_minimal(
    base_size = 13
  ) +
    
    theme(
      
      plot.title = element_text(
        size = 17,
        face = "bold",
        colour = deep_green,
        margin = margin(
          b = 6
        )
      ),
      
      plot.subtitle = element_text(
        size = 11.5,
        colour = text_soft,
        margin = margin(
          b = 14
        )
      ),
      
      axis.title = element_text(
        size = 11.5,
        face = "bold",
        colour = text_dark
      ),
      
      axis.text = element_text(
        colour = text_dark
      ),
      
      panel.grid.minor = element_blank(),
      
      panel.grid.major = element_line(
        colour = "#E5EBE7",
        linewidth = 0.35
      ),
      
      plot.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      panel.background = element_rect(
        fill = "white",
        colour = NA
      ),
      
      legend.title = element_text(
        face = "bold",
        colour = text_dark
      ),
      
      legend.text = element_text(
        colour = text_dark
      )
    )
}



# 10. UI


ui <- fluidPage(
  
  tags$head(
    
    tags$style(
      
      HTML(
        
        paste0(
          
          "

          body {
            background: ", cream, ";
            color: ", text_dark, ";
            font-family: 'Segoe UI', Arial, sans-serif;
          }

          .app-header {
            background: linear-gradient(
              135deg,
              ", deep_green, ",
              ", forest_green, "
            );

            color: white;
            padding: 30px 42px 27px 42px;
            margin-bottom: 0;
            border-bottom: 4px solid ", gold, ";
          }

          .app-title {
            font-size: 34px;
            font-weight: 600;
            letter-spacing: 0.2px;
            margin: 0;
          }

          .app-subtitle {
            font-size: 15px;
            margin-top: 7px;
            color: #E7F2ED;
          }

          .bird-mark {
            color: ", gold, ";
            font-size: 20px;
            margin-right: 8px;
          }

          .nav-tabs {
            border-bottom: 1px solid #DDE7E1;
            background: white;
            padding-left: 25px;
          }

          .nav-tabs > li > a {
            color: ", forest_green, ";
            border: none;
            padding: 15px 20px;
            font-size: 14px;
          }

          .nav-tabs > li > a:hover {
            background: ", soft_green, ";
            border: none;
            color: ", deep_green, ";
          }

          .nav-tabs > li.active > a,
          .nav-tabs > li.active > a:hover,
          .nav-tabs > li.active > a:focus {
            background: ", forest_green, ";
            color: white;
            border: none;
            border-radius: 7px 7px 0 0;
          }

          .page-title {
            color: ", deep_green, ";
            font-size: 27px;
            font-weight: 600;
            margin-bottom: 5px;
          }

          .page-description {
            color: ", text_soft, ";
            font-size: 14px;
            margin-bottom: 22px;
          }

          .stat-card {
            background: white;
            border-radius: 12px;
            padding: 18px 20px;
            margin-bottom: 20px;
            border-top: 3px solid ", gold, ";
            box-shadow: 0 2px 10px rgba(20,90,69,0.07);
            min-height: 110px;
          }

          .stat-label {
            color: ", text_soft, ";
            font-size: 12px;
            text-transform: uppercase;
            letter-spacing: 0.7px;
          }

          .stat-value {
            color: ", forest_green, ";
            font-size: 28px;
            font-weight: 600;
            margin-top: 7px;
          }

          .stat-small {
            color: ", text_soft, ";
            font-size: 12px;
            margin-top: 3px;
          }

          .chart-card {
            background: white;
            border-radius: 12px;
            padding: 20px 22px 15px 22px;
            margin-bottom: 22px;
            box-shadow: 0 2px 10px rgba(20,90,69,0.06);
          }

          .chart-title {
            color: ", deep_green, ";
            font-size: 17px;
            font-weight: 600;
            margin-bottom: 3px;
          }

          .chart-description {
            color: ", text_soft, ";
            font-size: 12.5px;
            margin-bottom: 8px;
          }

          .control-card {
            background: ", soft_green, ";
            border-radius: 12px;
            padding: 18px 22px 8px 22px;
            margin-bottom: 22px;
            border-left: 4px solid ", gold, ";
          }

          .control-label {
            color: ", deep_green, ";
            font-weight: 600;
          }

          .insight-card {
            background: white;
            border-radius: 12px;
            padding: 20px 23px;
            margin-bottom: 20px;
            border-left: 4px solid ", gold, ";
            box-shadow: 0 2px 9px rgba(20,90,69,0.06);
          }

          .insight-number {
            color: ", gold, ";
            font-size: 22px;
            font-weight: 600;
          }

          .insight-title {
            color: ", deep_green, ";
            font-size: 17px;
            font-weight: 600;
            margin-bottom: 5px;
          }

          .insight-text {
            color: ", text_soft, ";
            line-height: 1.6;
          }

          .section-heading {
            color: ", deep_green, ";
            font-size: 19px;
            font-weight: 600;
            margin: 8px 0 15px 0;
          }

          .county-highlight {
            background: linear-gradient(
              135deg,
              ", deep_green, ",
              ", forest_green, "
            );
            color: white;
            border-radius: 12px;
            padding: 22px;
            margin-bottom: 22px;
            border-bottom: 3px solid ", gold, ";
          }

          .county-name {
            font-size: 25px;
            font-weight: 600;
          }

          .county-number {
            font-size: 29px;
            color: ", light_gold, ";
            font-weight: 600;
          }

          .county-label {
            font-size: 12px;
            color: #DDECE5;
            text-transform: uppercase;
            letter-spacing: 0.6px;
          }

          .selectize-input {
            border-radius: 7px !important;
            border: 1px solid #CBDCD3 !important;
          }

          .irs-bar {
            background: ", forest_green, " !important;
            border-top: 1px solid ", forest_green, " !important;
            border-bottom: 1px solid ", forest_green, " !important;
          }

          .irs-handle {
            border: 2px solid ", gold, " !important;
          }

          .irs-from,
          .irs-to,
          .irs-single {
            background: ", forest_green, " !important;
          }

          "
          
        )
        
      )
      
    )
    
  ),
  
  
  
  # HEADER
  
  
  div(
    class = "app-header",
    
    div(
      class = "app-title",
      
      span(
        class = "bird-mark",
        "🇮🇪"
      ),
      
      "Irish Bird Biodiversity"
    ),
    
    div(
      class = "app-subtitle",
      
      "Exploring species richness, recording effort and geographic patterns across Ireland"
    )
    
  ),
  
  
  
  # NAVIGATION + CONTENT
  
  
  navbarPage(
    
    title = NULL,
    
    id = "main_nav",
    
    inverse = FALSE,
    
    
    # OVERVIEW
    
    
    tabPanel(
      
      "Overview",
      
      br(),
      
      div(
        class = "container-fluid",
        
        div(
          class = "page-title",
          "Ireland's Bird Biodiversity"
        ),
        
        div(
          class = "page-description",
          "A visual exploration of recorded bird species, recording effort, seasonality and geographic patterns."
        ),
        
        
        # STATISTICS
        
        
        fluidRow(
          
          column(
            3,
            
            div(
              class = "stat-card",
              
              div(
                class = "stat-label",
                "Years of records"
              ),
              
              div(
                class = "stat-value",
                textOutput(
                  "stat_years",
                  inline = TRUE
                )
              ),
              
              div(
                class = "stat-small",
                "Available observation period"
              )
              
            )
            
          ),
          
          column(
            3,
            
            div(
              class = "stat-card",
              
              div(
                class = "stat-label",
                "Unique species"
              ),
              
              div(
                class = "stat-value",
                textOutput(
                  "stat_species",
                  inline = TRUE
                )
              ),
              
              div(
                class = "stat-small",
                "Distinct taxa recorded"
              )
              
            )
            
          ),
          
          column(
            3,
            
            div(
              class = "stat-card",
              
              div(
                class = "stat-label",
                "Counties"
              ),
              
              div(
                class = "stat-value",
                textOutput(
                  "stat_counties",
                  inline = TRUE
                )
              ),
              
              div(
                class = "stat-small",
                "Counties represented in records"
              )
              
            )
            
          ),
          
          column(
            3,
            
            div(
              class = "stat-card",
              
              div(
                class = "stat-label",
                "Bird records"
              ),
              
              div(
                class = "stat-value",
                textOutput(
                  "stat_records",
                  inline = TRUE
                )
              ),
              
              div(
                class = "stat-small",
                "Total observations"
              )
              
            )
            
          )
          
        ),
        
        
        # GRAPHS
        
        
        fluidRow(
          
          column(
            
            6,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Species richness through time"
              ),
              
              div(
                class = "chart-description",
                "Annual number of distinct bird species recorded."
              ),
              
              plotOutput(
                "overview_time",
                height = "370px"
              )
              
            )
            
          ),
          
          column(
            
            6,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Recording effort"
              ),
              
              div(
                class = "chart-description",
                "Annual number of bird records."
              ),
              
              plotOutput(
                "overview_effort",
                height = "370px"
              )
              
            )
            
          )
          
        )
        
      )
      
    ),
    
    
    
    # OVER TIME
    
    
    tabPanel(
      
      "Over Time",
      
      br(),
      
      div(
        class = "container-fluid",
        
        div(
          class = "page-title",
          "Biodiversity over time"
        ),
        
        div(
          class = "page-description",
          "Explore how observed species richness changes across the available recording period."
        ),
        
        div(
          class = "control-card",
          
          sliderInput(
            
            "year_range",
            
            "Select year range",
            
            min = year_min,
            
            max = year_max,
            
            value = c(
              year_min,
              year_max
            ),
            
            step = 1,
            
            sep = ""
            
          )
          
        ),
        
        fluidRow(
          
          column(
            
            8,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Annual species richness"
              ),
              
              div(
                class = "chart-description",
                "Unique species recorded within the selected period."
              ),
              
              plotOutput(
                "time_plot",
                height = "450px"
              )
              
            )
            
          ),
          
          column(
            
            4,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Recording effort"
              ),
              
              div(
                class = "chart-description",
                "Total bird records by year."
              ),
              
              plotOutput(
                "time_records",
                height = "450px"
              )
              
            )
            
          )
          
        )
        
      )
      
    ),
    
    
    
    # SEASONS
    
    
    tabPanel(
      
      "Seasons",
      
      br(),
      
      div(
        class = "container-fluid",
        
        div(
          class = "page-title",
          "Seasonal biodiversity"
        ),
        
        div(
          class = "page-description",
          "Compare observed species richness and recording effort across the four seasons."
        ),
        
        fluidRow(
          
          column(
            
            6,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Species richness by season"
              ),
              
              div(
                class = "chart-description",
                "Number of unique species recorded within each season."
              ),
              
              plotOutput(
                "season_species",
                height = "390px"
              )
              
            )
            
          ),
          
          column(
            
            6,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Recording effort by season"
              ),
              
              div(
                class = "chart-description",
                "Total bird records within each season."
              ),
              
              plotOutput(
                "season_records",
                height = "390px"
              )
              
            )
            
          )
          
        )
        
      )
      
    ),
    
    
    
    # COUNTIES
    
    
    tabPanel(
      
      "Counties",
      
      br(),
      
      div(
        class = "container-fluid",
        
        div(
          class = "page-title",
          "County biodiversity"
        ),
        
        div(
          class = "page-description",
          "Explore observed bird species richness across Irish counties."
        ),
        
        div(
          class = "control-card",
          
          selectInput(
            
            "selected_county",
            
            "Select a county",
            
            choices = c(
              "All counties",
              sort(
                unique(
                  na.omit(
                    birds$County
                  )
                )
              )
            ),
            
            selected = "All counties"
            
          )
          
        ),
        
        uiOutput(
          "county_highlight"
        ),
        
        fluidRow(
          
          column(
            
            7,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Species richness by county"
              ),
              
              div(
                class = "chart-description",
                "Counties with the highest observed species richness."
              ),
              
              plotOutput(
                "county_species",
                height = "500px"
              )
              
            )
            
          ),
          
          column(
            
            5,
            
            div(
              class = "chart-card",
              
              div(
                class = "chart-title",
                "Recording effort and richness"
              ),
              
              div(
                class = "chart-description",
                "Relationship between annual recording effort and observed species richness."
              ),
              
              plotOutput(
                "county_effort",
                height = "500px"
              )
              
            )
            
          )
          
        )
        
      )
      
    ),
    
    
    
    # ADVANCED ANALYSIS
    
    
    tabPanel(
      "Advanced Analysis",
      br(),
      div(
        class = "container-fluid",
        div(class = "page-title", "Advanced Analysis"),
        div(
          class = "page-description",
          paste(
            "Explore statistical relationships, the most frequently",
            "recorded bird species, and monthly biodiversity patterns."
          )
        ),
        div(
          class = "chart-card",
          div(class = "chart-title", "Statistical Model Comparison"),
          div(
            class = "chart-description",
            paste(
              "Compare how year and recording effort are associated",
              "with observed annual species richness."
            )
          ),
          uiOutput("advanced_model_summary"),
          h4("Model performance"),
          tableOutput("model_comparison_table"),
          br(),
          plotOutput("advanced_model_plot", height = "400px")
        ),
        div(
          class = "chart-card",
          div(class = "chart-title", "Top 20 Most Frequently Recorded Bird Species"),
          div(
            class = "chart-description",
            paste(
              "Species are displayed using common names where available.",
              "If a common name is missing, the taxon name is shown instead."
            )
          ),
          plotOutput("top_species_plot", height = "650px")
        ),
        div(
          class = "chart-card",
          div(class = "chart-title", "Monthly Biodiversity Patterns"),
          div(
            class = "chart-description",
            paste(
              "Compare total records and distinct species",
              "recorded in each calendar month across all years."
            )
          ),
          plotOutput("monthly_patterns_plot", height = "430px")
        )
      )
    ),
    
    
    # KEY FINDINGS
    
    
    tabPanel(
      
      "Key Findings",
      
      br(),
      
      div(
        class = "container-fluid",
        
        div(
          class = "page-title",
          "Key findings"
        ),
        
        div(
          class = "page-description",
          "Summary of the main patterns identified in the analysis."
        ),
        
        uiOutput(
          "finding_time"
        ),
        
        uiOutput(
          "finding_effort"
        ),
        
        uiOutput(
          "finding_season"
        ),
        
        uiOutput(
          "finding_county"
        )
        
      )
      
    )
    
  )
  
)



# SERVER


server <- function(
    
  input,
  output,
  session
  
) {
  
  
  
  # OVERVIEW STATISTICS
  
  
  output$stat_years <- renderText({
    
    length(
      unique(
        birds$Year
      )
    )
    
  })
  
  
  output$stat_species <- renderText({
    
    comma(
      n_distinct(
        birds$TaxonName,
        na.rm = TRUE
      )
    )
    
  })
  
  
  output$stat_counties <- renderText({
    
    comma(
      n_distinct(
        birds$County[
          !is.na(birds$County) &
            birds$County != ""
        ]
      )
    )
    
  })
  
  
  output$stat_records <- renderText({
    
    comma(
      nrow(birds)
    )
    
  })
  
  
  
  # YEARLY SUMMARY
  
  
  yearly_data <- reactive({
    
    birds %>%
      
      group_by(
        Year
      ) %>%
      
      summarise(
        
        unique_species =
          n_distinct(
            TaxonName,
            na.rm = TRUE
          ),
        
        total_records =
          n(),
        
        .groups = "drop"
        
      ) %>%
      
      arrange(
        Year
      )
    
  })
  
  
  
  # OVERVIEW TIME GRAPH
  
  
  output$overview_time <- renderPlot({
    
    ggplot(
      yearly_data(),
      aes(
        x = Year,
        y = unique_species
      )
    ) +
      
      geom_area(
        fill = soft_green,
        alpha = 0.9
      ) +
      
      geom_line(
        colour = forest_green,
        linewidth = 1.2
      ) +
      
      geom_point(
        colour = deep_green,
        size = 2.2
      ) +
      
      geom_smooth(
        method = "lm",
        se = FALSE,
        colour = gold,
        linetype = "dashed",
        linewidth = 1
      ) +
      
      labs(
        x = "Year",
        y = "Unique species"
      ) +
      
      theme_bird()
    
  })
  
  
  
  # OVERVIEW EFFORT GRAPH
  
  
  output$overview_effort <- renderPlot({
    
    ggplot(
      yearly_data(),
      aes(
        x = Year,
        y = total_records
      )
    ) +
      
      geom_area(
        fill = "#DCEDE8",
        alpha = 0.9
      ) +
      
      geom_line(
        colour = teal,
        linewidth = 1.2
      ) +
      
      geom_point(
        colour = forest_green,
        size = 2.2
      ) +
      
      labs(
        x = "Year",
        y = "Bird records"
      ) +
      
      scale_y_continuous(
        labels = comma
      ) +
      
      theme_bird()
    
  })
  
  
  
  # FILTERED YEAR DATA
  
  
  filtered_yearly <- reactive({
    
    req(
      input$year_range
    )
    
    yearly_data() %>%
      
      filter(
        
        Year >= input$year_range[1],
        
        Year <= input$year_range[2]
        
      )
    
  })
  
  
  
  # MAIN TIME GRAPH
  
  
  output$time_plot <- renderPlot({
    
    ggplot(
      filtered_yearly(),
      aes(
        x = Year,
        y = unique_species
      )
    ) +
      
      geom_area(
        fill = soft_green,
        alpha = 0.9
      ) +
      
      geom_line(
        colour = forest_green,
        linewidth = 1.3
      ) +
      
      geom_point(
        colour = deep_green,
        size = 2.5
      ) +
      
      geom_smooth(
        method = "lm",
        se = TRUE,
        colour = gold,
        linewidth = 1.1,
        linetype = "dashed"
      ) +
      
      labs(
        x = "Year",
        y = "Unique species"
      ) +
      
      theme_bird()
    
  })
  
  
  
  # TIME RECORDS
  
  
  output$time_records <- renderPlot({
    
    ggplot(
      filtered_yearly(),
      aes(
        x = Year,
        y = total_records
      )
    ) +
      
      geom_col(
        fill = forest_green,
        alpha = 0.88
      ) +
      
      labs(
        x = "Year",
        y = "Bird records"
      ) +
      
      scale_y_continuous(
        labels = comma
      ) +
      
      theme_bird()
    
  })
  
  
  
  # SEASON SUMMARY
  
  
  seasonal_data <- reactive({
    
    birds %>%
      
      filter(
        !is.na(Season)
      ) %>%
      
      group_by(
        Season
      ) %>%
      
      summarise(
        
        unique_species =
          n_distinct(
            TaxonName,
            na.rm = TRUE
          ),
        
        records =
          n(),
        
        .groups = "drop"
        
      ) %>%
      
      mutate(
        
        Season = factor(
          Season,
          levels = c(
            "Winter",
            "Spring",
            "Summer",
            "Autumn"
          )
        )
        
      )
    
  })
  
  
  
  # SEASON SPECIES
  
  
  output$season_species <- renderPlot({
    
    season_cols <- c(
      
      "Winter" = "#5D8196",
      
      "Spring" = "#5B9B69",
      
      "Summer" = gold,
      
      "Autumn" = orange
      
    )
    
    ggplot(
      seasonal_data(),
      aes(
        x = Season,
        y = unique_species,
        fill = Season
      )
    ) +
      
      geom_col(
        width = 0.62,
        show.legend = FALSE
      ) +
      
      geom_text(
        aes(
          label = comma(unique_species)
        ),
        vjust = -0.4,
        size = 4,
        colour = text_dark
      ) +
      
      scale_fill_manual(
        values = season_cols
      ) +
      
      scale_y_continuous(
        expand = expansion(
          mult = c(
            0,
            0.12
          )
        )
      ) +
      
      labs(
        x = NULL,
        y = "Unique species"
      ) +
      
      theme_bird()
    
  })
  
  
  
  # SEASON RECORDS
  
  
  output$season_records <- renderPlot({
    
    season_cols <- c(
      
      "Winter" = "#5D8196",
      
      "Spring" = "#5B9B69",
      
      "Summer" = gold,
      
      "Autumn" = orange
      
    )
    
    ggplot(
      seasonal_data(),
      aes(
        x = Season,
        y = records,
        fill = Season
      )
    ) +
      
      geom_col(
        width = 0.62,
        show.legend = FALSE
      ) +
      
      geom_text(
        aes(
          label = comma(records)
        ),
        vjust = -0.4,
        size = 3.7,
        colour = text_dark
      ) +
      
      scale_fill_manual(
        values = season_cols
      ) +
      
      scale_y_continuous(
        labels = comma,
        expand = expansion(
          mult = c(
            0,
            0.12
          )
        )
      ) +
      
      labs(
        x = NULL,
        y = "Bird records"
      ) +
      
      theme_bird()
    
  })
  
  
  
  # COUNTY SUMMARY
  
  
  county_data <- reactive({
    
    birds %>%
      
      filter(
        !is.na(County),
        County != ""
      ) %>%
      
      group_by(
        County
      ) %>%
      
      summarise(
        
        records = n(),
        
        unique_species =
          n_distinct(
            TaxonName,
            na.rm = TRUE
          ),
        
        .groups = "drop"
        
      )
    
  })
  
  
  
  # COUNTY HIGHLIGHT
  
  
  output$county_highlight <- renderUI({
    
    req(
      input$selected_county
    )
    
    if (
      input$selected_county == "All counties"
    ) {
      
      return(NULL)
      
    }
    
    selected <- county_data() %>%
      
      filter(
        County == input$selected_county
      )
    
    if (
      nrow(selected) == 0
    ) {
      
      return(NULL)
      
    }
    
    div(
      
      class = "county-highlight",
      
      div(
        class = "county-name",
        selected$County[1]
      ),
      
      br(),
      
      fluidRow(
        
        column(
          
          6,
          
          div(
            class = "county-label",
            "Unique species"
          ),
          
          div(
            class = "county-number",
            comma(
              selected$unique_species[1]
            )
          )
          
        ),
        
        column(
          
          6,
          
          div(
            class = "county-label",
            "Bird records"
          ),
          
          div(
            class = "county-number",
            comma(
              selected$records[1]
            )
          )
          
        )
        
      )
      
    )
    
  })
  
  
  
  # COUNTY SPECIES GRAPH
  
  
  output$county_species <- renderPlot({
    
    selected <- input$selected_county
    
    if (
      selected == "All counties"
    ) {
      
      plot_data <- county_data() %>%
        
        arrange(
          desc(unique_species)
        ) %>%
        
        slice_head(
          n = 15
        ) %>%
        
        arrange(
          unique_species
        )
      
    } else {
      
      plot_data <- county_data() %>%
        
        filter(
          County == selected
        )
      
    }
    
    plot_data$County <- factor(
      plot_data$County,
      levels = plot_data$County
    )
    
    ggplot(
      plot_data,
      aes(
        x = unique_species,
        y = County
      )
    ) +
      
      geom_col(
        fill = forest_green,
        width = 0.68
      ) +
      
      geom_text(
        aes(
          label = comma(unique_species)
        ),
        hjust = -0.2,
        size = 3.5,
        colour = text_dark
      ) +
      
      scale_x_continuous(
        expand = expansion(
          mult = c(
            0,
            0.12
          )
        )
      ) +
      
      labs(
        x = "Unique species",
        y = NULL
      ) +
      
      theme_bird()
    
  })
  
  
  
  # COUNTY EFFORT GRAPH
  
  
  output$county_effort <- renderPlot({
    
    ggplot(
      county_data(),
      aes(
        x = records,
        y = unique_species
      )
    ) +
      
      geom_point(
        colour = forest_green,
        size = 3.5,
        alpha = 0.8
      ) +
      
      geom_smooth(
        method = "lm",
        se = TRUE,
        colour = gold,
        linewidth = 1.1
      ) +
      
      labs(
        x = "Bird records",
        y = "Unique species"
      ) +
      
      scale_x_continuous(
        labels = comma
      ) +
      
      theme_bird()
    
  })
  
  
  
  # ADVANCED ANALYSIS
  
  
  # 1. STATISTICAL MODEL COMPARISON
  
  advanced_models <- reactive({
    
    dat <- yearly_data() %>%
      filter(
        !is.na(Year),
        !is.na(unique_species),
        !is.na(total_records)
      ) %>%
      arrange(Year)
    
    validate(
      need(
        nrow(dat) >= 5,
        "Not enough annual observations for model comparison."
      )
    )
    
    model_year <- MASS::glm.nb(
      unique_species ~ Year,
      data = dat
    )
    
    model_effort <- MASS::glm.nb(
      unique_species ~ total_records,
      data = dat
    )
    
    model_combined <- MASS::glm.nb(
      unique_species ~ Year + total_records,
      data = dat
    )
    
    models <- list(
      "Year only" = model_year,
      "Recording effort only" = model_effort,
      "Year + recording effort" = model_combined
    )
    
    predictions <- lapply(
      models,
      function(model) {
        predict(model, newdata = dat, type = "response")
      }
    )
    
    model_metrics <- data.frame(
      Model = names(models),
      AIC = sapply(models, AIC),
      BIC = sapply(models, BIC),
      MAE = sapply(
        predictions,
        function(pred) mean(abs(dat$unique_species - pred))
      ),
      RMSE = sapply(
        predictions,
        function(pred) sqrt(mean((dat$unique_species - pred)^2))
      ),
      Pearson_Dispersion = sapply(
        models,
        function(model) {
          sum(residuals(model, type = "pearson")^2) /
            df.residual(model)
        }
      ),
      check.names = FALSE
    )
    
    dat$nb_predicted <- predict(
      model_combined,
      newdata = dat,
      type = "response"
    )
    
    list(
      data = dat,
      models = models,
      metrics = model_metrics
    )
  })
  
  
  output$advanced_model_summary <- renderUI({
    
    result <- advanced_models()
    comparison <- result$metrics
    
    best_model <- comparison$Model[
      which.min(comparison$AIC)
    ]
    
    best_fit <- result$models[[best_model]]
    
    div(
      class = "insight-card",
      div(
        class = "insight-title",
        "Negative binomial model with the lowest AIC"
      ),
      div(
        class = "insight-text",
        paste0(
          "The best-fitting model among those compared is '",
          best_model,
          "' (AIC = ",
          round(AIC(best_fit), 2),
          "). The negative binomial model allows for overdispersion ",
          "in count data. A lower AIC indicates a better relative fit ",
          "while accounting for model complexity."
        )
      )
    )
  })
  
  
  output$model_comparison_table <- renderTable({
    
    result <- advanced_models()
    
    result$metrics %>%
      mutate(
        AIC = round(AIC, 2),
        BIC = round(BIC, 2),
        MAE = round(MAE, 2),
        RMSE = round(RMSE, 2),
        Pearson_Dispersion = round(Pearson_Dispersion, 3)
      ) %>%
      rename(
        `Pearson dispersion` = Pearson_Dispersion
      )
    
  }, striped = TRUE, bordered = FALSE, spacing = "s")
  
  
  output$advanced_model_plot <- renderPlot({
    
    dat <- advanced_models()$data
    
    ggplot(
      dat,
      aes(
        x = unique_species,
        y = nb_predicted
      )
    ) +
      geom_point(
        colour = forest_green,
        size = 3,
        alpha = 0.8
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        colour = gold,
        linewidth = 1.2,
        linetype = "dashed"
      ) +
      labs(
        title = "Observed vs Predicted Bird Species Richness",
        subtitle = "Combined negative binomial model: Year + Recording Effort",
        x = "Observed species richness",
        y = "Predicted species richness"
      ) +
      theme_bird()
  })
  
  
  # 2. TOP 20 MOST RECORDED SPECIES (COMMON NAMES WHERE AVAILABLE)
  output$top_species_plot <- renderPlot({
    # The dataset's "Common name" field is incomplete, so use a common
    # name whenever one exists for a taxon and fall back to TaxonName.
    species_source <- birds %>%
      mutate(
        CommonName = if ("Common name" %in% names(birds)) {
          trimws(as.character(.data[["Common name"]]))
        } else {
          NA_character_
        },
        CommonName = na_if(CommonName, "")
      ) %>%
      filter(!is.na(TaxonName), trimws(TaxonName) != "")
    
    species_data <- species_source %>%
      group_by(TaxonName) %>%
      summarise(
        records = n(),
        common_name = {
          available_names <- CommonName[!is.na(CommonName)]
          if (length(available_names) > 0) available_names[1] else NA_character_
        },
        .groups = "drop"
      ) %>%
      mutate(
        Species = if_else(
          !is.na(common_name) & common_name != "",
          common_name,
          TaxonName
        )
      ) %>%
      arrange(desc(records)) %>%
      slice_head(n = 20) %>%
      arrange(records) %>%
      mutate(Species = factor(Species, levels = unique(Species)))
    
    validate(need(nrow(species_data) > 0, "No species records are available."))
    
    ggplot(species_data, aes(x = records, y = Species)) +
      geom_col(fill = forest_green, width = 0.72) +
      geom_text(
        aes(label = comma(records)),
        hjust = -0.15, size = 3.3, colour = text_dark
      ) +
      scale_x_continuous(
        labels = comma,
        expand = expansion(mult = c(0, 0.15))
      ) +
      labs(x = "Number of records", y = NULL) +
      theme_bird()
  })
  
  # 3. MONTHLY BIODIVERSITY PATTERNS
  output$monthly_patterns_plot <- renderPlot({
    monthly_data <- birds %>%
      filter(
        !is.na(Date),
        !is.na(TaxonName),
        trimws(TaxonName) != ""
      ) %>%
      mutate(Month_Number = month(Date)) %>%
      group_by(Month_Number) %>%
      summarise(
        records = n(),
        unique_species = n_distinct(TaxonName),
        .groups = "drop"
      )
    
    validate(
      need(nrow(monthly_data) > 0, "No valid dated species records are available.")
    )
    
    monthly_data <- data.frame(Month_Number = 1:12) %>%
      left_join(monthly_data, by = "Month_Number") %>%
      mutate(Month = factor(month.abb[Month_Number], levels = month.abb))
    
    monthly_data$records[is.na(monthly_data$records)] <- 0
    monthly_data$unique_species[is.na(monthly_data$unique_species)] <- 0
    
    plot_data <- rbind(
      data.frame(
        Month = monthly_data$Month,
        Measure = "Total records",
        Value = monthly_data$records
      ),
      data.frame(
        Month = monthly_data$Month,
        Measure = "Unique species",
        Value = monthly_data$unique_species
      )
    )
    
    ggplot(plot_data, aes(x = Month, y = Value, fill = Measure)) +
      geom_col(width = 0.7, show.legend = FALSE) +
      facet_wrap(~ Measure, scales = "free_y", ncol = 1) +
      scale_fill_manual(values = c(
        "Total records" = forest_green,
        "Unique species" = gold
      )) +
      scale_y_continuous(
        labels = comma,
        expand = expansion(mult = c(0, 0.1))
      ) +
      labs(x = "Month", y = NULL) +
      theme_bird()
  })
  
  # ==========================================================
  # KEY FINDING 1
  # ==========================================================
  
  output$finding_time <- renderUI({
    
    dat <- yearly_data()
    
    highest <- dat %>%
      
      slice_max(
        unique_species,
        n = 1,
        with_ties = FALSE
      )
    
    lowest <- dat %>%
      
      slice_min(
        unique_species,
        n = 1,
        with_ties = FALSE
      )
    
    div(
      
      class = "insight-card",
      
      div(
        class = "insight-number",
        "01"
      ),
      
      div(
        class = "insight-title",
        "Long-term species richness"
      ),
      
      div(
        
        class = "insight-text",
        
        paste0(
          
          "The highest observed annual species richness was ",
          
          comma(
            highest$unique_species
          ),
          
          " species in ",
          
          highest$Year,
          
          ". The lowest observed value was ",
          
          comma(
            lowest$unique_species
          ),
          
          " species in ",
          
          lowest$Year,
          
          "."
          
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # KEY FINDING 2
  # ==========================================================
  
  output$finding_effort <- renderUI({
    
    dat <- yearly_data()
    
    correlation <- suppressWarnings(
      
      cor(
        dat$total_records,
        dat$unique_species,
        use = "complete.obs"
      )
      
    )
    
    div(
      
      class = "insight-card",
      
      div(
        class = "insight-number",
        "02"
      ),
      
      div(
        class = "insight-title",
        "Recording effort and observed richness"
      ),
      
      div(
        
        class = "insight-text",
        
        paste0(
          
          "Across years, recording effort and observed species richness have a correlation of ",
          
          round(
            correlation,
            3
          ),
          
          ". This describes an association between the number of records and the number of species observed."
          
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # KEY FINDING 3
  # ==========================================================
  
  output$finding_season <- renderUI({
    
    dat <- seasonal_data()
    
    highest <- dat %>%
      
      slice_max(
        unique_species,
        n = 1,
        with_ties = FALSE
      )
    
    div(
      
      class = "insight-card",
      
      div(
        class = "insight-number",
        "03"
      ),
      
      div(
        class = "insight-title",
        "Seasonal variation"
      ),
      
      div(
        
        class = "insight-text",
        
        paste0(
          
          highest$Season,
          
          " had the highest observed species richness, with ",
          
          comma(
            highest$unique_species
          ),
          
          " unique species recorded."
          
        )
        
      )
      
    )
    
  })
  
  
  # ==========================================================
  # KEY FINDING 4
  # ==========================================================
  
  output$finding_county <- renderUI({
    
    dat <- county_data()
    
    highest <- dat %>%
      
      slice_max(
        unique_species,
        n = 1,
        with_ties = FALSE
      )
    
    div(
      
      class = "insight-card",
      
      div(
        class = "insight-number",
        "04"
      ),
      
      div(
        class = "insight-title",
        "Geographic variation"
      ),
      
      div(
        
        class = "insight-text",
        
        paste0(
          
          highest$County,
          
          " had the highest observed species richness among the counties represented in the dataset, with ",
          
          comma(
            highest$unique_species
          ),
          
          " unique species."
          
        )
        
      )
      
    )
    
  })
  
}


# ============================================================
# RUN APPLICATION
# ============================================================

shinyApp(
  ui = ui,
  server = server
)