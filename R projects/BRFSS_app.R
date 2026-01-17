# App overview
# I want to create an app that lets me explore the BRFSS 2011-present prevalence dataset.

# =======================================================================================

#library & dependencies
library(shiny)
library(tidyverse)

# Load data
brfss<- read_csv("G:/Programming/BRFSS/Data-2024/2011 pervalence & trends/Behavioral_Risk_Factor_Surveillance_System_(BRFSS)_Prevalence_Data_(2011_to_present)_20260115.csv")

# ============================= UI level ================================================

ui <- fluidPage(
  title = "BRFSS prevalence Explorer",
  h1("BRFSS Prevalence Data (2011 to present) Dataset Dashboard"),
  tableOutput("table")
)


# =========================== SERVER level ================================================

server <- function(input, output) {
  
  filtered_data() <- head(brfss, 10)
    
  output$table <- renderTable({
  filtered_data()  
  })
}
    


# =========================== App Wrap ===================================================
shinyApp(ui = ui, server = server)
