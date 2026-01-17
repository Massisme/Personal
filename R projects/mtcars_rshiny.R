# Project Overview
# An interactive dashboard to explore the mtcars dataset with dynamic filtering, data visualization, and tabular display.

# ======================================================================

#library & dependencies
setwd("G:/Programming/Rshiny project1/")
library(shiny)

# ==============  UI  ================ 

ui <- fluidPage(
  title = "mtcars explorer",
  h1("mtcars Dataset Dashboard"),

  sliderInput(
  inputId = "mpg_filter",
  label = "filter by MPG:",
  min = 10, 
  max = 34,
  value = c(15,30)
  ),

tableOutput("cars_table"),

selectInput(
  inputId = "x_var",
  label = "X-axis variable:",
  choices = c( "weight" = "wt",
               "HP" = "hp",
               "displacement" = "disp",
               "quarter mile time" = "qsec"
               ),
  selected = "wt"
),

selectInput(
  inputId = "y_var",
  label = "Y-axis variable:",
  choices = c( "MPG" = "mpg",
               "HP" = "hp",
               "displacement" = "disp",
               "weight" = "wt"
               ),
  selected = "mpg"
),

plotOutput("cars_plot")

)

# ============== SERVER  =====================
server <- function(input, output) {
  
  filtered_data <- reactive({
    mtcars[mtcars$mpg >= input$mpg_filter[1] &
             mtcars$mpg <= input$mpg_filter[2], ]
    })

  output$cars_table <- renderTable({
    data <- filtered_data()
    data$car_name <- rownames(data)
    
    columns_visual <- unique(c("car_name", "cyl", "gear", input$x_var, input$y_var))
    
    data[ , columns_visual]
    }, rownames = FALSE)

  output$cars_plot <- renderPlot({
    plot(filtered_data()[[input$x_var]], filtered_data()[[input$y_var]],
         xlab = input$x_var,
         ylab = input$y_var,
         main = paste(input$y_var, "vs", input$x_var),
         pch = 19,
         col = "red"
         )
  })
  
  
}  
  
shinyApp(ui = ui, server = server)


### first push was commited. here is a new version with this comment added. to attempt another push to understand versions and control.
### first attempt didnt work, I think. 2nd try.

### attempt successful. now to understand pulling. made this chage to see if I can pull the file to see this change. goodluck to me!





