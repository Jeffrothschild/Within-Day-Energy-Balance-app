library(flexdashboard)
library(dplyr)
library(ggplot2)
library(shiny)
library(forcats)
library(shinyWidgets)
library(rhandsontable)
# library(shinyscreenshot)
library(downloadthis)


ui <- fluidPage(
  titlePanel("Within-Day Energy Balance Calculation App"),
  sidebarLayout(
    sidebarPanel(
      br(),
      numericInput("weight", "Enter body weight", 70, min = 30, max = 300),
      radioGroupButtons(inputId = "weight_unit", label = "Body weight unit", 
                        choices = c("kg", "lb"), selected = "kg"),
      hr(),
      numericInput("height", "Enter height", 180, min = 1, max = 250),
      radioGroupButtons(inputId = "height_unit", label = "Height unit", 
                        choices = c("cm", "in"), selected = "cm"),
      hr(),
      numericInput("age", "Enter age", 30, min = 15, max = 100),
      hr(),
      radioGroupButtons(inputId = "sex", label = "Sex", 
                        choices = c("Male", "Female"), selected = "Male"),
      br(), hr(),
      h4("Area below -300 kcal"),
      h5("Aim to minimize this number!"),
      textOutput("aucText")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Intake", rHandsontableOutput("table")),
        tabPanel("Exercise", rHandsontableOutput("table_exer")),
        tabPanel("Within-day energy balance", plotOutput("energyBalancePlot")),
        tabPanel("Estimated Daily Energy Expenditure", textOutput("dailyEnergyExpenditure")),
        tabPanel("Download Summary", uiOutput("downloadUI"))
      )
    )
  )
)

server <- function(input, output) {
  # Example reactive expression for weight conversion
  weight_kg <- reactive({
    if(input$weight_unit == "kg") {
      as.numeric(input$weight)
    } else {
      as.numeric(input$weight) / 2.20462 # Convert lbs to kg
    }
  })
  
  # Placeholder for AUC calculation output
  output$aucText <- renderText({
    paste("AUC Placeholder")
  })
  
  output$table_exer <- renderRHandsontable({
    rhandsontable(exercise_df, rowHeaders = NULL) %>%  ## need to call converter
      hot_col("Hour", readOnly = TRUE,  colWidths= 75) %>% 
      hot_col("Kcal burn", colWidths= 120, halign = "htCenter")# %>%   
    # hot_cell(1, 2, "For exercise lasting longer than 1 hr, enter your kcal burn for each hour rather than the full workout kcal burn into one hour.")
  })
  
  
  column(12, fluidRow(rHandsontableOutput("table_exer")))
  
  
  
  joined_tbl <- reactive({
    hot_to_r(req(input$table_exer)) %>% as_tibble()  %>% 
      left_join(intake_tbl(), by = "Hour") %>% 
      rename("exercise_calorie_burn" = "Kcal burn",
             "food_intake_calories" = "Kcal intake",
             "hour" = "Hour"
      ) %>% 
      mutate(
        hour = fct_inorder(hour),
        across(exercise_calorie_burn:food_intake_calories, ~as.numeric(.)),
        nonexercise_burn = c(
          rep(baseline_tbl()$hourly_nonexercise_burn * .9, 5),
          rep(baseline_tbl()$hourly_nonexercise_burn, 17),
          rep(baseline_tbl()$hourly_nonexercise_burn * .9, 2)
        ),
        epoc = dplyr::lag(exercise_calorie_burn) * .05,
        epoc2 = dplyr::lag(exercise_calorie_burn, 2) * .03,
        across(exercise_calorie_burn:epoc2, ~ ifelse(is.na(.), 0, .)),
        nonexercise_burn = ifelse(exercise_calorie_burn > 0, 0, nonexercise_burn),
        EB  = 0 - nonexercise_burn - exercise_calorie_burn + food_intake_calories - epoc - epoc2,
        cumulative = 100 + cumsum(EB),
        hour2 = seq(0,23,1)
      )
  })
  
  
  total_needs <- reactive({
    round((sum(joined_tbl()$nonexercise_burn, na.rm = T)) + (sum(joined_tbl()$exercise_calorie_burn)) -+ 
            (sum(joined_tbl()$epoc, na.rm = T)) +   (sum(joined_tbl()$epoc2, na.rm = T)) )
  })
  
  
  net_kcal <- reactive({
    (sum(joined_tbl()$food_intake_calories, na.rm = T)) - (sum(joined_tbl()$nonexercise_burn)) - 
      (sum(joined_tbl()$exercise_calorie_burn, na.rm = T)) 
  })
  
  br()
  
  
  # Extend this section with your app's logic, including table rendering and plot generation
}
# Run the application 
shinyApp(ui = ui, server = server)
