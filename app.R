# Shiny application
# install.packages(c("shiny", "sf", "ggplot2", "dplyr", "rnaturalearth", "rnaturalearthdata", "kableExtra"))

# Load libraries
library(shiny)
library(sf)
library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(kableExtra)

# UI for the Shiny app
ui <- fluidPage(
  titlePanel("Fiji: Insights and Projections"),
  
  # Use a tabbed layout for navigation
  tabsetPanel(
    tabPanel("World Map",
             plotOutput("world_map")
    ),
    tabPanel("Island Map",
             plotOutput("island_map")
    ),
    tabPanel("Key Facts",
             tableOutput("key_facts")
    ),
    tabPanel("GDP Projection",
             plotOutput("gdp_projection")
    ),
    tabPanel("Comparison with Island States",
             plotOutput("comparison_plot"),
             plotOutput("population_vs_area"),
             plotOutput("tourism_plot"),
             tableOutput("island_table")
    ),
    tabPanel("SWOT Analysis",
             tableOutput("swot_table")
    )
  )
)

# Server logic for the Shiny app
server <- function(input, output) {
  # Load world and Fiji data
  world <- ne_countries(scale = "medium", returnclass = "sf")
  fiji_coords <- tibble(name = "Fiji", lat = -17.7134, long = 178.0650)
  fiji_shapefile <- st_read("gadm41_FJI_0.shp")
  
  # GDP data
  gdp_data <- data.frame(
    year = 2000:2023,
    gdp = c(1.68, 1.65, 1.83, 2.30, 2.71, 2.98, 3.08, 3.38, 3.52, 2.87,
            3.14, 3.78, 3.97, 4.19, 4.86, 4.68, 4.93, 5.35, 5.58, 5.44,
            4.43, 4.31, 4.98, 5.49)
  )
  
  # Island states comparison data
  island_states <- tibble(
    country = c("Fiji", "Vanuatu", "Tonga", "Samoa", "Solomon Islands"),
    gdp = c(5.49, 0.99, 0.52, 0.90, 1.55),
    population = c(889950, 334506, 100209, 222382, 707851),
    land_area = c(18274, 12189, 747, 2842, 28896),
    tourists = c(800000, 300000, 90000, 150000, 30000)
  )
  
  # SWOT analysis data
  swot <- data.frame(
    Category = c("Strengths", "Weaknesses", "Opportunities", "Threats"),
    Details = c(
      "Thriving tourism, rich biodiversity, stable democracy, cultural heritage.",
      "Natural disaster vulnerability, reliance on tourism, limited industrial base.",
      "Eco-tourism growth, renewable energy, blue economy opportunities.",
      "Climate change, rising sea levels, external shocks (e.g., pandemics)."
    )
  )
  
  # Output: World map
  output$world_map <- renderPlot({
    ggplot(data = world) +
      geom_sf(fill = "lightblue", color = "gray") +
      geom_point(data = fiji_coords, aes(x = long, y = lat), color = "red", size = 3) +
      labs(title = "Location of Fiji in the World", x = "Longitude", y = "Latitude") +
      theme_minimal()
  })
  
  # Output: Island map
  output$island_map <- renderPlot({
    ggplot(data = fiji_shapefile) +
      geom_sf(fill = "lightgreen", color = "darkgreen") +
      coord_sf(xlim = c(176, 180), ylim = c(-20, -16)) +
      labs(title = "Map of Fiji (Zoomed)", x = "Longitude", y = "Latitude") +
      theme_minimal()
  })
  
  # Output: Key facts
  output$key_facts <- renderTable({
    tibble(
      Category = c("Government", "Economy", "Population", "Natural Environment", "History"),
      Details = c(
        "Parliamentary Democracy",
        "Tourism, Agriculture, and Fisheries",
        "889,950",
        "Coral reefs and tropical forests",
        "Independence: October 10, 1970"
      )
    )
  })
  
  # Output: GDP projection
  output$gdp_projection <- renderPlot({
    gdp_model <- lm(gdp ~ year, data = gdp_data)
    future_years <- data.frame(year = 2024:2030)
    future_gdp <- predict(gdp_model, newdata = future_years)
    gdp_projection <- rbind(
      gdp_data,
      data.frame(year = future_years$year, gdp = future_gdp)
    )
    
    ggplot(gdp_projection, aes(x = year, y = gdp)) +
      geom_line(color = "blue", size = 1) +
      geom_point(data = gdp_data, aes(x = year, y = gdp), color = "red", size = 2) +
      labs(title = "Historical and Projected GDP of Fiji (2000-2030)", x = "Year", y = "GDP (in billions USD)") +
      theme_minimal()
  })
  
  # Output: Comparison plots
  output$comparison_plot <- renderPlot({
    ggplot(island_states, aes(x = reorder(country, gdp), y = gdp, fill = country)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "GDP of South Pacific Island States (2023)", x = "Country", y = "GDP (in billions USD)") +
      theme_minimal()
  })
  
  output$population_vs_area <- renderPlot({
    ggplot(island_states, aes(x = land_area, y = population, label = country)) +
      geom_point(size = 4, color = "blue") +
      geom_text(vjust = -0.5, hjust = 0.5, size = 3) +
      labs(title = "Population vs. Land Area (2023)", x = "Land Area (sq km)", y = "Population") +
      theme_minimal()
  })
  
  output$tourism_plot <- renderPlot({
    ggplot(island_states, aes(x = reorder(country, tourists), y = tourists, fill = country)) +
      geom_col(show.legend = FALSE) +
      coord_flip() +
      labs(title = "Tourist Arrivals (2023)", x = "Country", y = "Tourist Arrivals") +
      theme_minimal()
  })
  
  output$island_table <- renderTable({
    island_states
  })
  
  # Output: SWOT analysis
  output$swot_table <- renderTable({
    swot
  })
}

# Run the application
shinyApp(ui = ui, server = server)
