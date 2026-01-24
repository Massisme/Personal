# App overview
# I want to create an app that lets me explore the BRFSS 2011-present prevalence dataset.

# =======================================================================================

#library & dependencies
library(shiny)
library(tidyverse)
library(DT)
library(maps)
library(sf)
library(usmap)

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
  
  # ================== DATA TABLES =========================
# let's set up some dropdown menu to change the display data. Beginning with a smaller scale with fewer choices. I also want them spread horizontally. time to learn fluidRow() and column()
  h2("Prevalence Data Table"),
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

# ================== TREND CHARTS =========================
  h2("Prevalence Trends visualisations"),
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

      checkboxInput(
      inputId = "show_cl",
      label = "Show Confidence Limits",
      value = TRUE    # TRUE = checked by default
      ),
plotOutput("plot")
),

# ================== CHOROPLETH MAPS=========================
  h2("Prevalence Choropleth Map"),

fluidRow(
  # year filter
  column(3, #takes upto 3 out of 12 columns
         selectInput(
           inputId = "year_map",
           label = "Which Year?",
           choices = c( "2024" = "2024", "2023" = "2023", "2022" = "2022", "2021" = "2021", "2020" = "2020",
                        "2019" = "2019", "2018" = "2018", "2017" = "2017", "2016" = "2016", "2015" = "2015",
                        "2014" = "2014", "2013" = "2013", "2012" = "2012", "2011" = "2011"
           ),
           selected = "2024"
         ),
 
          selectInput(
           inputId = "topic_map",
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
         ),
 
         selectInput(
           inputId = "response_map",
           label = "Select response",
           choices = c("placeholder01" = "ph1"
           ),
           selected = NULL
         )
  ),
  column(10,
plotOutput("map")
),
),
)


# =========================== SERVER level ================================================

server <- function(input, output, session) {
  
# set up a reactive data for the table display. we are filtering by the above inputs.  
  filtered_data <- reactive({
      brfss_dat %>% filter(Year == input$year_filt, Locationabbr == input$state_filt, Break_Out_Category == input$bocat_filt, Topic == input$topic_filt )
      })
  
  output$table <- renderTable({
# for ease of view, reduce the number of columns down by relevance.
  columns_vis <- unique(c("Year", "Locationabbr", "Question", "Response" , "Data_value", "Confidence_limit_Low" , "Confidence_limit_High", "Break_Out", "Break_Out_Category"))

  filtered_data()[ , columns_vis]
  })

# let's put the reactive data for the plots
  plot_data <- reactive({
    brfss_dat %>% filter(Year >= input$year_filter[1], Year <= input$year_filter[2], Locationabbr == input$state_filter,
                         Break_Out_Category == input$bocat_filter, Topic == input$topic_filter
                         )
  })
# render the plot  
  output$plot <- renderPlot({
    data <- plot_data()

#There is an issue where there are no data (eg, FL in 2011), so we should include a check if there is any data,
    if(nrow(data)==0) {
      ggplot() + 
        annotate("text", x = 1, y= 1,
                 label = "No data available for selection") + 
        theme_void()
    } else {
#create the ggplot      
    ggplot(data, 
           aes(x = Year, y = Data_value, color = Response, fill = Response, group = Response)) +
# add the CI in ribbons
{if(input$show_cl) geom_ribbon(aes(ymin = Confidence_limit_Low ,
                        ymax = Confidence_limit_High),
                    alpha = 0.2)} +
           geom_line(linewidth  =1.1) +
            geom_point(size=2.5) +
             labs(
               title = paste(input$topic_filter, "prevalence trends", "(", input$year_filter[1], "—", input$year_filter[2], ")"),
               subtitle = paste(input$bocat_filter, "—", input$state_filter),
               x = "Year",
               y = "Prevalence (%)",
               color = "Response"
             ) +
        facet_wrap(~ Break_Out, ncol=2)
         }
    })

# set up the data for the choropleth map  
  observeEvent(input$topic_map, {
  # get unique response levels fro selected topic
      responses <- brfss_dat %>% filter(Topic == input$topic_map) %>% 
        pull(Response) %>% unique()
      
  updateSelectInput(session,
                    inputId = "response_map",
                    choices = responses,
                    selected = responses[1]) #select the first response as default displayed   
  })
  
    map_data <- reactive({
    brfss_dat %>% filter(Year == input$year_map, Break_Out_Category == "Overall",
                         Topic == input$topic_map,
                         Response == input$response_map,
                         # We should filter out some extranuous locations in Locationabbr US, UW, GU, PR, VI
                         !Locationabbr %in% c("US", "UW", "GU", "PR", "VI")
                         )
    })

    
    
# Map portion
    
    output$map <- renderPlot({
    data <- map_data()
        if (nrow(data) == 0) {
      ggplot() + 
        annotate("text", x = 1, y= 1,
                 label = "No data available for selection") + 
        theme_void()
    } else {
      data <- data %>% rename(state = Locationabbr)
      
      plot_usmap(data = data, values = "Data_value", color = "black") +
        scale_fill_continuous(low = "white",
                              high = "darkred",
                              name = "Prevalence (%)"
                              ) +
        labs(title = paste(input$topic_map, "prevalence Choropleth", input$year_map),
             subtitle = paste("Breakout Category: Overall", "-", "responded:", input$response_map)
              )+
        theme(
          legend.position = "left",        # Try: "right", "left", "top", "bottom"
          legend.key.width = unit(0.75, "cm"),  # Adjust width
          legend.key.height = unit(2, "cm") # Adjust height
        )
      }
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
# stopping here for today.
# 1/18/2026 - 03.36

# implemented plotting, include with a checkbox for CL visual. facetting to show breakout levels within BO categories.
# additionally, all response levels are plotted in one line. Seems like a good place to take a break for tonight, but I do want to implment a 
# heatmap visual (completed during the staging ground) instead of points though, a full fill of colour for each state.
# 1/19/2026 - 01.40


# Started on the map section. the UI section is done. The plan is to include options for Year, topic, and responses, with breakout category fixed at Overall.
# one issue is on how to make the response dropdown list reactive to the topic. will need to think for a moment.
# It is tricky since I may have to make the list in server then work it back into UI somehow. Unsure how to proceed. 
# Maybe employ a full result to input work, as in make one plot, then expand back.
# Taking a break here since I need to think 01/19/2026 - 23.15

#map completed. changed the fluid row columns to accommodate the maps on the side. This should be the final version of
# things I wanted on display. next would be polishing. maybe get some ideas. 01/ 24/2026 - 01.30 
