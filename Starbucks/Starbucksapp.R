#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(leaflet)
library(dplyr)

# Load the Starbucks data
starbucks_data <- read.csv("starbucksMA.csv")

# Define the UI
ui <- fluidPage( #Defines the overall layout of the app, which is responsive to different screen sizes.
  titlePanel("Starbucks Locations in MA - Gross Profit"), #Adds a title to the app
  sidebarLayout( #Creates a layout with a sidebar (for inputs) and a main panel (for outputs).
    sidebarPanel( #Adds the slider input to control the range of
      sliderInput("profitRange", #The input ID for the slider
                  "Gross Profit Range:", #Label for the slider
                  min = min(starbucks_data$gross_profit, na.rm = TRUE), #Define the range of values based on the particular variable
                  max = max(starbucks_data$gross_profit, na.rm = TRUE),
                  value = c(min(starbucks_data$gross_profit, na.rm = TRUE), #the default range selected when the app loads
                            max(starbucks_data$gross_profit, na.rm = TRUE)),
                  step = 1000) #Size for each of the slider increments (1000 in this case)
    ),
    mainPanel(
      leafletOutput("map") #render te interactive map using leaflet
    )
  )
)

# Define the server
server <- function(input, output, session) {#Defines the server logic for the app.
  filtered_data <- reactive({ #Creates a reactive expression to filter the data based on the slider input (input$profitRange).
    starbucks_data |>
      filter(gross_profit >= input$profitRange[1], #filters gross_profit is within the selected range
             gross_profit <= input$profitRange[2])
  })
  
  output$map <- renderLeaflet({ #defining the map output by Leaflet map
    leaflet() |> #creates a blank map
      addTiles() |> #adds a default basemap layer
      setView(lng = mean(starbucks_data$Longitude, na.rm = TRUE), #Sets the initial view of the map to the center of MA
              lat = mean(starbucks_data$Latitude, na.rm = TRUE), 
              zoom = 10) #default view of the map: zoom level of 10
  })
  
  observe({ #Dynmamically updates the map as the slider input changes;
    data <- filtered_data()
    leafletProxy("map", data = data)|> #Updates the existing map;
      clearMarkers() |> #Removes existing markers before adding new ones; 
      addCircleMarkers(lng = ~Longitude, #adds circle markers to the map for the filtered data;
                       lat = ~Latitude, 
                       popup = ~paste0("<b>Name:</b> ", Name, "<br>", #Adds a popup for each marker displying the store nam and gross profit
                                       "<b>Gross Profit:</b> $", gross_profit),
                       radius = 5, #marker size
                       color = "blue", #define the marker's color
                       fillOpacity = 0.7) #define the marker's apperance
  })
}

# Run the app
shinyApp(ui, server) #Combines the ui and server components into a runnable Shiny app.
