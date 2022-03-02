library(shiny)
library(shinythemes)
library(data.table)
library(RCurl)
library(ggplot2)

# stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations$FUEL_TYPE_CODE <- as.factor(stations$FUEL_TYPE_CODE)
levels(stations$FUEL_TYPE_CODE)[1] <- "Biodiesel (B20 and above)"
levels(stations$FUEL_TYPE_CODE)[2] <- "Compressed Natural Gas (CNG)"
levels(stations$FUEL_TYPE_CODE)[3] <- "Electric"
levels(stations$FUEL_TYPE_CODE)[4] <- "Ethanol (E85)"
levels(stations$FUEL_TYPE_CODE)[5] <- "Hydrogen"
levels(stations$FUEL_TYPE_CODE)[6] <- "Liquefied Natural Gas (LNG)"
levels(stations$FUEL_TYPE_CODE)[7] <- "Propane (LPG)"


stations$STATUS_CODE <- as.factor(stations$STATUS_CODE)
levels(stations$STATUS_CODE)[1] <- "Available"
levels(stations$STATUS_CODE)[2] <- "Planned"
levels(stations$STATUS_CODE)[3] <- "Temporarily Unavailable"

stations$CITY

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

stations$OWNER_TYPE_CODE

ui <- fluidPage(theme = shinytheme("journal"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Analysing US Alternative Fuel Stations",
                  tabPanel("Basic Data Table",
                           fluidRow(
                             column(4,
                                    selectInput("fuel",
                                                "Type of Alternative Fuel:",
                                                c("All",
                                                  unique(as.character(stations$FUEL_TYPE_CODE))))
                             ),
                             column(4,
                                    selectInput("state",
                                                "US or Canadian State",
                                                c("All",
                                                  unique(as.character(stations$STATE))))
                             ),
                             column(4,
                                    selectInput("avbl",
                                                "Availability",
                                                c("All",
                                                  unique(as.character(stations$STATUS_CODE))))
                             )
                           ),
                           # Create a new row for the table.
                           DT::dataTableOutput("table")),
                  tabPanel("Visualisation of the Stations on the Map", fluid = TRUE, icon = icon("globe-americas"),
                           # Sidebar layout with a input and output definitions
                           sidebarLayout(
                             sidebarPanel(
                               
                               titlePanel("Station Features"),
                               #shinythemes::themeSelector(),
                               fluidRow(column(3,
                                               
                                               # Select which Gender(s) to plot
                                               checkboxGroupInput(inputId = "AvailabilityFinder",
                                                                  label = "Select Availability:",
                                                                  choices = c("Available" = "Available", "Planned" = "Planned", "Temporarily Unavailable" = "Temporarily Unavailable"),
                                                                  selected = "All")
                               ),
                               column(6, offset = 2,
                                      # Select which Region(s) to plot
                                      checkboxGroupInput(inputId = "CountryFinder",
                                                         label = "Select Country(s):",
                                                         choices = c("Canada" = "CA", "United States" = "US"),
                                                         selected = "All")
                               )),
                               hr(),
                               # Select Event
                               selectInput(inputId = "TypeFinder",
                                           label = "Select Type",
                                           choices = c("Biodiesel (B20 and above)", "Compressed Natural Gas (CNG)", "Electric", "Ethanol (E85)", 
                                                       "Hydrogen", "Liquefied Natural Gas (LNG)", "Propane (LPG)"),
                                           selected = "All",
                                           width = "220px"
                               ),
                               hr(),
                               titlePanel("Owner Type"),
                               # Select which School Type to plot
                               checkboxGroupInput(inputId = "OwnerFinder",
                                                  label = "Select Owner Type(s):",
                                                  choices = c("Federal Government" = "FG", "Jointly Owned" = "J", "Local/Municipal Government" = "LG", 
                                                              "Privately Owned" = "P", "State/Provincial Government" = "SG", "Utility Owned" = "T"),
                                                  selected = "All")),
                             mainPanel(
                               fluidRow(
                                 column(3, offset = 9,
                                        
                                        radioButtons(inputId = "show_NamesFinder",
                                                     label = "Display:",
                                                     choices = c("School Names", "City Names", "Neither"),
                                                     selected = "School Names")
                                 )),
                               # hr(),
                               withSpinner(plotOutput(outputId = "scatterplotFinder", click = "click_plotFinder"
                               )),
                               hr(),
                               fluidRow(column(7,
                                               helpText("Tip: Click locations to populate table below with information on schools in a specific area")
                                               #actionButton(inputId = "draw", label = "Input Event and Times")
                                               
                               ),
                               column(width = 2, offset = 2, conditionalPanel(
                                 condition = "output.schoolstableFinder",
                                 actionButton(inputId = "FinderClear", label = "Clear Table")))),
                               br(),
                               fluidRow(
                                 withSpinner(dataTableOutput(outputId = "schoolstableFinder"))))
                           )
                  ),
                  tabPanel("Navbar 3", "This panel is intentionally left blank"))) 

# Define server function  
server <- function(input, output) {
  
  # Filter data based on selections
  output$table <- DT::renderDataTable(DT::datatable({
    data <- stations
    if (input$fuel != "All") {
      data <- data[data$FUEL_TYPE_CODE == input$fuel,]
    }
    if (input$state != "All") {
      data <- data[data$STATE == input$state,]
    }
    if (input$avbl != "All") {
      data <- data[data$STATUS_CODE == input$avbl,]
    }
    data
  }))
  
}

# Create Shiny object
shinyApp(ui = ui, server = server)