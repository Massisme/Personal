# App overview
# I want to create an app that lets me explore the BRFSS 2011-present prevalence dataset.

# =======================================================================================

#library & dependencies
library(shiny)
library(tidyverse)

# Load data
brfss<- read_csv("G:/Programming/BRFSS/Data-2024/2011 pervalence & trends/Behavioral_Risk_Factor_Surveillance_System_(BRFSS)_Prevalence_Data_(2011_to_present)_20260115.csv")

# Initial operations
brfss_dat <- brfss %>% separate(GeoLocation,
                   into = c("latitude", "longitude"),
                   sep = ",",
                   remove = FALSE)  %>%
  mutate(
    latitude = as.numeric(str_extract(latitude, "-?\\d+\\.\\d+")),
    longitude = as.numeric(str_extract(longitude, "-?\\d+\\.\\d+")),
  ) %>%
  
  filter(latitude >= 24,          # Excludes Hawaii and southern territories
         latitude <= 50,          # Excludes Alaska
         longitude >= -125,       # Excludes far western territories
         longitude <= -65)        # Excludes eastern territories



# #column names for table reference
# "Year"                       "Locationabbr"               "Locationdesc"               "Class"                     
# "Topic"                      "Question"                   "Response"                   "Break_Out"                 
# "Break_Out_Category"         "Sample_Size"                "Data_value"                 "Confidence_limit_Low"      
# "Confidence_limit_High"      "Display_order"              "Data_value_unit"            "Data_value_type"           
# "Data_Value_Footnote_Symbol" "Data_Value_Footnote"        "DataSource"                 "ClassId"                   
# "TopicId"                    "LocationID"                 "BreakoutID"                 "BreakOutCategoryID"        
# "QuestionID"                 "ResponseID"                 "GeoLocation"       "latitude" "longitude"

# Break out category names for reference,
#  "Overall"            "Sex"                "Age Group"          "Race/Ethnicity"     "Education Attained" "Household Income" 

# ============================= UI level ================================================

ui <- fluidPage(
  title = "BRFSS prevalence Explorer",
  h1("BRFSS Prevalence Data (2011 to present) Dataset Dashboard"),

# Let's set up some dropdown menu to change the display data. Beginning with a smaller scale with fewer choices. I also want them spread horizontally. time to learn fluidRow() and column()

fluidRow(
  # year filter
    column(4, #takes upto 6 out of 12 columns
  selectInput(
    inputId = "year_filt",
    label = "Which Year?",
    choices = c( "2024" = "2024",
                 "2023" = "2023",
                 "2022" = "2022",
                 "2021" = "2021",
                 "2020" = "2020",
                 "2019" = "2019",
                 "2018" = "2018",
                 "2017" = "2017",
                 "2016" = "2016",
                 "2015" = "2015",
                 "2014" = "2014",
                 "2013" = "2013",
                 "2012" = "2012",
                 "2011" = "2011"
    ),
    selected = "2024"
    )
    ),
# state filter  
column(4, #takes upto 6 out of 12 columns
  selectInput(
    inputId = "state_filt",
    label = "Which State?",
    choices = c( "Alaska" = "AK",
                 "N. Carolina" = "NC",
                 "Minnesota" = "MN",
                 "Florida" = "FL"
    ),
    selected = "FL"
  )
  ),
# break out category filter  
column(4, #takes upto 6 out of 12 columns
       selectInput(
         inputId = "bocat_filt",
         label = "Which Category?",
         choices = c( "Overall" = "Overall",
                      "Sex" = "Sex",
                      "Age Group" = "Age Group",
                      "Race/Ethnicity" = "Race/Ethnicity",
                      "Education Attained" = "Education Attained",
                      "Household Income" ="Household Income" 
         ),
         selected = "Overall"
       )
)
),
  tableOutput("table")
)



# =========================== SERVER level ================================================

server <- function(input, output) {
  
# set up a reactive data for the table display. we are filtering by the above inputs.  
  filtered_data <- reactive({
  
    brfss_dat %>% filter(Year == input$year_filt, Locationabbr == input$state_filt, Break_Out_Category == input$bocat_filt)
      })
  
  output$table <- renderTable({
# For ease of view, reduce the number of columns down by relevance. 
  columns_vis <- unique(c("Year", "Locationabbr", "Question", "Response" , "Data_value", "Break_Out" ,"Break_Out_Category"))  
    
  filtered_data()[ , columns_vis]
  })
}


# =========================== App Wrap ===================================================
shinyApp(ui = ui, server = server)



# ========================== NOTES ======================================================
# I am still having issue understanding expression vs function, e.g, table and table() and which is applicable when.
# Need to formalise a plan. right now, I want dropdown filters 


