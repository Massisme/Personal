# ============================================================================
# MTCARS INTERACTIVE DASHBOARD - SHINY APP
# ============================================================================
# This app demonstrates core Shiny concepts:
# - UI/Server architecture
# - Input controls (slider, dropdown menus)
# - Output rendering (plots, tables)
# - Reactivity and reactive expressions
# - Dynamic data filtering and column selection
# ============================================================================

# Load required package
library(shiny)

# ============================================================================
# UI (USER INTERFACE)
# ============================================================================
# The UI defines what users see and interact with
# Everything here creates the visual layout and input controls

ui <- fluidPage(
  
  # Browser tab title (not visible in the app itself)
  title = "mtcars Explorer",
  
  # Main heading displayed at top of app
  h1("mtcars Dataset Dashboard"),
  
  # ------------------------------------------------------------
  # INPUT 1: Range Slider for filtering by MPG
  # ------------------------------------------------------------
  sliderInput(
    inputId = "mpg_filter",        # ID to access this input in server
    label = "Filter by MPG:",      # Text label shown to user
    min = 10,                      # Minimum slider value
    max = 35,                      # Maximum slider value
    value = c(15, 30)              # Initial range [15, 30] - creates two handles
  ),
  
  # ------------------------------------------------------------
  # INPUT 2: Dropdown for selecting X-axis variable
  # ------------------------------------------------------------
  selectInput(
    inputId = "x_var",             # ID to access this input in server
    label = "X-axis variable:",    # Text label shown to user
    choices = c(                   # Dropdown options
      "Weight" = "wt",             # "Display Name" = "actual_column_name"
      "Horsepower" = "hp", 
      "Displacement" = "disp",
      "Quarter Mile Time" = "qsec"
    ),
    selected = "wt"                # Default selection when app starts
  ),
  
  # ------------------------------------------------------------
  # INPUT 3: Dropdown for selecting Y-axis variable
  # ------------------------------------------------------------
  selectInput(
    inputId = "y_var",             # ID to access this input in server
    label = "Y-axis variable:",    # Text label shown to user
    choices = c(                   # Dropdown options
      "Miles Per Gallon" = "mpg",
      "Horsepower" = "hp",
      "Weight" = "wt",
      "Displacement" = "disp"
    ),
    selected = "mpg"               # Default selection when app starts
  ),
  
  # ------------------------------------------------------------
  # OUTPUT 1: Placeholder for plot
  # ------------------------------------------------------------
  plotOutput("cars_plot"),         # Creates space for plot with ID "cars_plot"
  
  # ------------------------------------------------------------
  # OUTPUT 2: Placeholder for table
  # ------------------------------------------------------------
  tableOutput("cars_table")        # Creates space for table with ID "cars_table"
)

# ============================================================================
# SERVER (APPLICATION LOGIC)
# ============================================================================
# The server processes inputs and generates outputs
# It contains the "brain" of the app

server <- function(input, output) {
  
  # ------------------------------------------------------------
  # REACTIVE EXPRESSION: Filtered Data
  # ------------------------------------------------------------
  # This creates a "smart variable" that:
  # - Automatically recalculates when input$mpg_filter changes
  # - Can be reused by multiple outputs (efficient!)
  # - Must be called with () when used: filtered_data()
  
  filtered_data <- reactive({
    # Filter mtcars to only include rows where:
    # - mpg >= left slider handle (input$mpg_filter[1])
    # - mpg <= right slider handle (input$mpg_filter[2])
    mtcars[mtcars$mpg >= input$mpg_filter[1] & 
           mtcars$mpg <= input$mpg_filter[2], ]
  })
  
  # ------------------------------------------------------------
  # OUTPUT 1: Render the scatter plot
  # ------------------------------------------------------------
  # This connects to plotOutput("cars_plot") in the UI
  # Automatically re-runs when any input it uses changes
  
  output$cars_plot <- renderPlot({
    
    # Create scatter plot with user-selected variables
    plot(
      # X-axis: Use [[ ]] to access column by variable name
      filtered_data()[[input$x_var]], 
      
      # Y-axis: Use [[ ]] to access column by variable name
      filtered_data()[[input$y_var]],
      
      # Dynamic labels based on user selections
      xlab = input$x_var,                           # X-axis label
      ylab = input$y_var,                           # Y-axis label
      main = paste(input$y_var, "vs", input$x_var), # Plot title
      
      # Visual styling
      pch = 19,                                     # Point style (filled circles)
      col = "blue"                                  # Point color
    )
  })
  
  # ------------------------------------------------------------
  # OUTPUT 2: Render the data table
  # ------------------------------------------------------------
  # This connects to tableOutput("cars_table") in the UI
  # Automatically re-runs when inputs change
  
  output$cars_table <- renderTable({
    
    # Get filtered data and store in local variable
    data <- filtered_data()
    
    # Add car names as a column (they're currently row names)
    data$car_name <- rownames(data)
    
    # Determine which columns to display:
    # - Always show: car_name, mpg
    # - Dynamically show: whatever user selected for X and Y axes
    # - unique() removes duplicates (e.g., if user selects mpg for Y-axis)
    columns_to_show <- unique(c("car_name", "mpg", input$x_var, input$y_var))
    
    # Return only the selected columns
    data[, columns_to_show]
    
  }, rownames = FALSE)  # Don't show row names (we added car_name as column)
}

# ============================================================================
# RUN THE APP
# ============================================================================
# This line launches the app by connecting UI and Server

shinyApp(ui = ui, server = server)

# ============================================================================
# KEY CONCEPTS DEMONSTRATED:
# ============================================================================
#
# 1. UI/SERVER SEPARATION
#    - UI defines layout and appearance
#    - Server defines logic and computations
#
# 2. INPUT → SERVER → OUTPUT FLOW
#    - User interacts with inputs (slider, dropdowns)
#    - Values stored in input$inputId
#    - Server processes and creates outputs
#    - Outputs displayed via output$outputId
#
# 3. REACTIVITY
#    - Outputs automatically update when inputs change
#    - No manual "refresh" needed
#
# 4. REACTIVE EXPRESSIONS
#    - Created with reactive({ })
#    - Called with ()
#    - Eliminate code duplication
#    - Improve efficiency
#
# 5. DYNAMIC COLUMN ACCESS
#    - Use [[ ]] when column name is in a variable
#    - Example: data[[input$x_var]] instead of data$x_var
#
# 6. ID MATCHING
#    - UI: plotOutput("cars_plot")
#    - Server: output$cars_plot <- renderPlot({ })
#    - IDs must match exactly!
#
# ============================================================================
