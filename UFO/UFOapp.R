#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(readr)

# Load the UFO dataset
ufo_data <- read_csv("UFO/complete_UFO.csv", 
                     col_types = cols(datetime = col_datetime(format = "%m/%d/%Y")))

# Extract year and filter out rows with missing countries
ufo_data <- ufo_data |>
  mutate(Year = as.integer(format(datetime, "%Y"))) |>
  filter(!is.na(country))  # Remove rows with missing country data

# Define the UI
ui <- fluidPage( # Creates a responsive layout that adjusts to different screen sizes;
  titlePanel("UFO Sightings by Year"), # Adds a title;
  sidebarLayout(
    sidebarPanel( #includes the input controls: country selection box; 
      checkboxGroupInput("countries",  # A checkbox group that allows multiple countries to be selected;
                         "Select Countries:", 
                         choices = unique(ufo_data$country),
                         selected = unique(ufo_data$country))  # All countries selected by default
    ),
    mainPanel( # This panel is where the output (plot) will be displayed; 
      plotOutput("ufoPlot") # The plot is rendered using plotOutput("ufoPlot");
    )
  )
)

# Define the server
server <- function(input, output) { # Defines the server-side logic for the app.
  filtered_data <- reactive({ #A reactive expression that automatically updates when the input changes. 
    ufo_data |> filter(country %in% input$countries) # Filters the ufo_data dataset to only include the countries selected by the user in input$countries
  })
  
  output$ufoPlot <- renderPlot({ # Renders the plot output.
    data <- filtered_data() |> # Calls the reactive expression to get the filtered data
      group_by(Year, country) |>
      summarise(Sightings = n(), .groups = "drop") # Summarizes the data by counting the number of sightings per country and year.
    # .groups = "drop": Drops the grouping structure after summarization.
    ggplot(data, aes(x = Year, y = Sightings, color = country)) +
      geom_line(size = 1) +
      labs(title = "UFO Sightings by Year",
           x = "Year",
           y = "Number of Sightings",
           color = "Country") +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# Run the app
shinyApp(ui, server)