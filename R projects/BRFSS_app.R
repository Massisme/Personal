# App overview
# I want to create an app that lets me explore the BRFSS 2011-present prevalence dataset.

# =======================================================================================

#library & dependencies
library(shiny)
library(tidyverse)
library(DT)

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
  ) 
# %>%
#   filter(latitude >= 24,          # Excludes Hawaii and southern territories
#          latitude <= 50,          # Excludes Alaska
#          longitude >= -125,       # Excludes far western territories
#          longitude <= -65)        # Excludes eastern territories



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

# Question topics for reference, Need to select only a few good topics to look into.
# [1] "Depression"              "Age"                     "Disability status"      
# [4] "COPD"                    "Kidney"                  "Other Cancer"           
# [7] "Skin Cancer"             "Last Checkup"            "Cardiovascular Disease" 
# [10] "Hearing"                 "Diabetes"                "Alcohol Consumption"    
# [13] "E-Cigarette Use"         "Education"               "Employment"             
# [16] "Overall Health"          "HIV Test"                "Income"                 
# [19] "Marital Status"          "Health Care Cost"        "Personal Care Provider" 
# [22] "Race"                    "Sex"                     "Smokeless Tobacco"      
# [25] "Veteran Status"          "All Teeth Removed"       "BMI Categories"         
# [28] "Asthma"                  "Number of Children"      "USPSTF Recommendations" 
# [31] "Arthritis"               "Teeth Removed"           "Flu Shot"               
# [34] "Under 65 Coverage"       "Health Care Coverage"    "Had CAT/CT Chest Scan"  
# [37] "Mammogram"               "Healthy Days"            "Pneumonia Vaccination"  
# [40] "Binge Drinking"          "Blood Stool Test"        "Heavy Drinking"         
# [43] "Fair or Poor Health"     "Current Smoker Status"   "Smoker Status"          
# [46] "Exercise"                "Cholesterol Checked"     "Drink and Drive"        
# [49] "Aerobic Activity"        "Physical Activity Index" "Strength Activity"      
# [52] "Cholesterol High"        "High Blood Pressure"     "Seatbelt Use"           
# [55] "Dental Visit"            "Rent/Own Home"           "Fruit Consumption"      
# [58] "Vegetable Consumption"   "Pap Test"                "PSA Test"               
# [61] "Tetanus Shot"            "Colonoscopy"             "Sigmoidoscopy"          
# [64] "Shingle Vaccination"     "Internet"                "Vision"  

# ============================= UI level ================================================

ui <- fluidPage(
  title = "BRFSS prevalence Explorer",
  h1("BRFSS Prevalence Data (2011 to present) Dataset Dashboard"),
  h2("Prevalence Data Table"),

# let's set up some dropdown menu to change the display data. Beginning with a smaller scale with fewer choices. I also want them spread horizontally. time to learn fluidRow() and column()

fluidRow(
  # year filter
    column(3, #takes upto 6 out of 12 columns
  selectInput(
    inputId = "year_filt",
    label = "Which Year?",
    choices = c( "2024" = "2024", "2023" = "2023", "2022" = "2022", "2021" = "2021", "2020" = "2020",
                 "2019" = "2019", "2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2015",
                 "2014" = "2014", "2013" = "2013", "2012" = "2012", "2011" = "2011"
                ),
    selected = "2024"
    )
    ),
# state filter  
column(3, #takes upto 6 out of 12 columns
  selectInput(
    inputId = "state_filt",
    label = "Which State?",
    choices = c( "Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR", "Arizona" = "AZ", "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "District of Columbia" = "DC", 
                 "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Iowa" = "IA", "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Kansas" = "KS",
                 "Kentucky" = "KY", "Louisiana" = "LA", "Massachusetts" = "MA", "Maryland" = "MD", "Maine" = "ME", "Michigan" = "MI", "Minnesota" = "MN", "Missouri" = "MO", "Mississippi" = "MS",
                 "Montana" = "MT", "North Carolina" = "NC", "North Dakota" = "ND", "Nebraska" = "NE", "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "Nevada" = "NV", "New York" = "NY",
                 "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD", "Texas" = "TX",
                 "All States, DC and Territories (median)" = "US", "Utah" = "UT", "All States and DC (median)" = "UW", "Virginia" = "VA", "Virgin Islands" = "VI", "Vermont" = "VT", "Washington" = "WA",
                 "Wisconsin" = "WI", "West Virginia" = "WV", "Wyoming" = "WY", "Tennessee" = "TN"
    ),
    selected = "US"
  )
  ),
# break out category filter  
column(3, #takes upto 6 out of 12 columns
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
),
# topic filter  
column(3, #takes upto 6 out of 12 columns
       selectInput(
         inputId = "topic_filt",
         label = "Select topic ",
         choices = c( "Depression" = "Depression",
                      "COPD" = "COPD",
                      "Skin Cancer" = "Skin Cancer",
                      "Diabetes" = "Diabetes",
                      "High Blood Pressure" = "High Blood Pressure",
                      "Cardiovascular Disease" = "Cardiovascular Disease" ,
                      "Cholesterol High" = "Cholesterol High",
                      "Smoker Status" = "Smoker Status",
                      "Heavy Drinking" = "Heavy Drinking",
                      "Overall Health" = "Overall Health"
                    ),
         selected = "Overall Health"
       )
),
),
  tableOutput("table"),

fluidRow(
  column(3, #takes upto 6 out of 12 columns
       sliderInput(
         inputId = "year_filter",
         label = "Year range",
         min = 2011, 
         max = 2024,
         value = c(2011,2024),
         sep = ""
       ),

),
column(3, #takes upto 6 out of 12 columns
       selectInput(
         inputId = "state_filter",
         label = "Which State?",
         choices = c( "Alaska" = "AK", "Alabama" = "AL", "Arkansas" = "AR", "Arizona" = "AZ", "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "District of Columbia" = "DC", 
                      "Delaware" = "DE", "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Iowa" = "IA", "Idaho" = "ID", "Illinois" = "IL", "Indiana" = "IN", "Kansas" = "KS",
                      "Kentucky" = "KY", "Louisiana" = "LA", "Massachusetts" = "MA", "Maryland" = "MD", "Maine" = "ME", "Michigan" = "MI", "Minnesota" = "MN", "Missouri" = "MO", "Mississippi" = "MS",
                      "Montana" = "MT", "North Carolina" = "NC", "North Dakota" = "ND", "Nebraska" = "NE", "New Hampshire" = "NH", "New Jersey" = "NJ", "New Mexico" = "NM", "Nevada" = "NV", "New York" = "NY",
                      "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", "South Dakota" = "SD", "Texas" = "TX",
                      "All States, DC and Territories (median)" = "US", "Utah" = "UT", "All States and DC (median)" = "UW", "Virginia" = "VA", "Virgin Islands" = "VI", "Vermont" = "VT", "Washington" = "WA",
                      "Wisconsin" = "WI", "West Virginia" = "WV", "Wyoming" = "WY", "Tennessee" = "TN"
         ),
         selected = "US"
       )
),
# break out category filter  
column(3, #takes upto 6 out of 12 columns
       selectInput(
         inputId = "bocat_filter",
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
),
# topic filter  
column(3, #takes upto 6 out of 12 columns
       selectInput(
         inputId = "topic_filter",
         label = "Select topic ",
         choices = c( "Depression" = "Depression",
                      "COPD" = "COPD",
                      "Skin Cancer" = "Skin Cancer",
                      "Diabetes" = "Diabetes",
                      "High Blood Pressure" = "High Blood Pressure",
                      "Cardiovascular Disease" = "Cardiovascular Disease" ,
                      "Cholesterol High" = "Cholesterol High",
                      "Smoker Status" = "Smoker Status",
                      "Heavy Drinking" = "Heavy Drinking",
                      "Overall Health" = "Overall Health"
         ),
         selected = "Overall Health"
       )
),
plotOutput("plot")
),
)


# =========================== SERVER level ================================================

server <- function(input, output) {
  
# set up a reactive data for the table display. we are filtering by the above inputs.  
  filtered_data <- reactive({
      brfss_dat %>% filter(Year == input$year_filt, Locationabbr == input$state_filt, Break_Out_Category == input$bocat_filt, Topic == input$topic_filt )
      })
  
  output$table <- renderTable({
# for ease of view, reduce the number of columns down by relevance.
  columns_vis <- unique(c("Year", "Locationabbr", "Question", "Response" , "Data_value", "Break_Out" ,"Break_Out_Category"))

  filtered_data()[ , columns_vis]
  })

# let's put the reactive data for the plots
  plot_data <- reactive({
    brfss_dat %>% filter(Year >= input$year_filter[1], Year <= input$year_filter[2], Locationabbr == input$state_filter, Break_Out_Category == input$bocat_filter, Topic == input$topic_filter,
                         Response == "Good")
  })
# render the plot  
  output$plot <- renderPlot({
    data <- plot_data()

    plot(data$Year, data$Data_value,
         type = "b",
         pch = 19,
         col = "red",
         lwd = 2,
         xlab = "Year",
         ylab = "prevalence (%)",
         main = paste(input$topic_filter, "-response:Good"))
    
  })  
  
}


# =========================== App Wrap ===================================================
shinyApp(ui = ui, server = server)

# ========================== NOTES ======================================================
# I am still having issue understanding expression vs function, e.g, table and table() and which is applicable when.
# Need to formalise a plan. right now, I want dropdown filters 
# The table needed pagination. Implemented DT library and input output to created paged tables.
# Now that I have also filtered by question topic. the tables don't need pagination. but atleast learned the option exists. Pasting the sections down here
# In UI:
# DTOutput("table")
# In server: 
# output$table <- renderDT({
#   columns_vis <- unique(c("Year", "Locationabbr", "Question", "Response" , "Data_value", "Break_Out" ,"Break_Out_Category"))
#   filtered_data()[ , columns_vis]
#   }, options = list(
#   pageLength = 10,
#   lengthMenu = c(10,20,30,50)
# )
# )
# MAYBE, we can use it again later for a different display, or a complete page in a sideboard option.

# Next step is to begin working on the plot section. Let's just try to setup the UI elements now first.
# I am considering showing prevalence with CI bars over year range.
# A year range slider, topic, dropdown, state, breakout category for UI elements, 
# but then generate a plot for each response level for the question.

# Had another idea, what if we just plot all the response levels in the same graphic, instead of one for each. although, showing CI might be problematic. 
# Maybe we can create a way to select the option. if we can get one and both ready in time. 



