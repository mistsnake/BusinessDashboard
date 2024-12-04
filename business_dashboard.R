library(tidyverse)
library(DT)
library(shiny)
library(purrr)
library(ggplot2)
library(plotly)
library(dplyr)
library(shinycssloaders)
library(shinyWidgets)
library(shinythemes)

# Load dataset
ncvt <- readRDS(gzcon(url("https://uwmadison.box.com/shared/static/o0jesak9enlsuy0l7u29ao07q72raoay.rds")))

# Make note of columns
columns <- colnames(ncvt)

# Save the unique column values for the dropdown menus
col_vals <- lapply(names(ncvt), function(col) {
  sort(unique(ncvt[[col]]))
})

# Assign column names to the column values
names(col_vals) <- names(ncvt)

# Define permanent columns required for plots
permanent_columns <- c("Performance_Score", "Job_Title", "Team_Size", "Projects_Handled", "Department", "Monthly_Salary", "Resigned", "Employee_Satisfaction_Score", "Promotions", "Overtime_Hours", "Work_Hours_Per_Week", "Education_Level")

# Define optional columns for dynamic dropdown
optional_columns <- setdiff(names(ncvt), permanent_columns)

# Generate display-friendly column names
display_names <- gsub("_", " ", optional_columns)  # Replace underscores with spaces
display_names <- tools::toTitleCase(display_names)  # Convert to title case
name_map <- setNames(display_names, optional_columns)  # Map original to display-friendly names

# Function to create dropdowns with server-side processing
create_dropdowns <- function(selected_cols) {
  tagList(
    lapply(selected_cols, function(col) {
      if (col == "Employee_ID") {
        selectizeInput(
          inputId = paste0("filter_", col),
          label = "Select value for Employee ID",
          choices = NULL,  # Dynamically populated
          selected = "All",
          multiple = TRUE,
          options = list(placeholder = "Start typing to search...")
        )
      } else if (col == "Gender") {
        radioButtons(
          inputId = paste0("filter_", col),
          label = "Select value for Gender",
          choices = c("All", "Male", "Female", "Other"),
          selected = "All",
          inline = TRUE
        )
      } else if (col == "Age") {
        sliderInput(
          inputId = paste0("filter_", col),
          label = "Select Age Range",
          min = min(ncvt[[col]], na.rm = TRUE),
          max = max(ncvt[[col]], na.rm = TRUE),
          value = c(min(ncvt[[col]], na.rm = TRUE), max(ncvt[[col]], na.rm = TRUE)),  # Default: full range
          step = 1
        )
      } else if (col == "Hire_Date") {
        dateRangeInput(
          inputId = paste0("filter_", col),
          label = "Select Hire Date Range",
          start = min(as.Date(ncvt[[col]]), na.rm = TRUE),
          end = max(as.Date(ncvt[[col]]), na.rm = TRUE),  # Default end date
          min = min(as.Date(ncvt[[col]], na.rm = TRUE)),  # Minimum selectable date
          max = max(as.Date(ncvt[[col]], na.rm = TRUE)) 
        )
      } else if (col == "Years_At_Company") {
        sliderInput(
          inputId = paste0("filter_", col),
          label = "Select Years at Company",
          min = min(ncvt[[col]], na.rm = TRUE),
          max = max(ncvt[[col]], na.rm = TRUE),
          value = c(min(ncvt[[col]], na.rm = TRUE), max(ncvt[[col]], na.rm = TRUE)),  # Default: full range
          step = 1
        )
      } else if (col == "Sick_Days") {
        sliderInput(
          inputId = paste0("filter_", col),
          label = "Select Sick Days",
          min = min(ncvt[[col]], na.rm = TRUE),
          max = max(ncvt[[col]], na.rm = TRUE),
          value = c(min(ncvt[[col]], na.rm = TRUE), max(ncvt[[col]], na.rm = TRUE)), # Default: full range
          step = 1
        )
      } else if (col == "Remote_Work_Frequency") {
        selectInput(
          inputId = paste0("filter_", col),
          label = "Select value for Remote Work Frequency",
          choices = c("All", "Never", "Occasionally", "Frequently", "Always"),
          selected = "All",
          multiple = TRUE
        )
      } else if (col == "Training_Hours") {
        sliderInput(
          inputId = paste0("filter_", col),
          label = "Select Training Hours Range",
          min = min(ncvt[[col]], na.rm = TRUE),
          max = max(ncvt[[col]], na.rm = TRUE),
          value = c(min(ncvt[[col]], na.rm = TRUE), max(ncvt[[col]], na.rm = TRUE)),  # Default: full range
          step = 1
        )
      }
    })
  )
}


# Shiny app
ui <- fluidPage(
  theme = shinytheme('simplex'),
  
  titlePanel("Employee Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_cols", 
                  label = "Select Columns for Datatable", 
                  choices = setNames(optional_columns, display_names),  # Use display-friendly names
                  selected = optional_columns, 
                  multiple = TRUE),
      withSpinner(uiOutput("dynamic_dropdowns"))
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Graphs", 
                 fluidRow(
                   column(12, withSpinner(plotOutput("work_hours_plot"))),
                   style = "margin-top: 8%; margin-bottom: 8%;"
                 ),
                 fluidRow(
                   column(12, withSpinner(plotOutput("projects_plot"))),
                   style = "margin-bottom: 8%;"
                 ),
                 fluidRow(
                   column(12, withSpinner(plotOutput("dept_trends_plot"))),
                   style = "margin-bottom: 8%;"
                 ),
                 fluidRow(
                   column(12, withSpinner(plotOutput("career_progression_plot"))),
                   style = "margin-bottom: 8%;"
                 ),
                 fluidRow(
                   column(12, withSpinner(plotlyOutput("correlation_plot"))),
                   style = "margin-bottom: 8%;"
                 )
        ),
        tabPanel("Table", DTOutput("filtered_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically render dropdowns
  output$dynamic_dropdowns <- renderUI({
    req(input$selected_cols)
    create_dropdowns(input$selected_cols)
  })
  
  # Observe and update dropdown choices dynamically
  observe({
    req(input$selected_cols)
    for (col in input$selected_cols) {
      updateSelectizeInput(
        session,
        inputId = paste0("filter_", col),
        choices = c("All", col_vals[[col]]),  # Provide unique values dynamically
        server = TRUE
      )
    }
    
    
  })
  
  # Reactive dataframe based on filters
  
  filtered_data <- reactive({
    data <- ncvt  # Start with the full dataset
    req(input$selected_cols)  # Ensure selected columns are not NULL
    
    # Apply filters dynamically based on menu input
    for (col in input$selected_cols) {
      filter_values <- input[[paste0("filter_", col)]]
      
      if (!is.null(filter_values) && !"All" %in% filter_values) {
        if (col == "Employee_ID") {
          # Selectize input for Employee ID
          data <- data %>% filter(Employee_ID %in% filter_values)
        } else if (col == "Gender") {
          # Radio buttons for Gender
          if (filter_values != "All") {
            data <- data %>% filter(Gender %in% filter_values)
          }
        } else if (col == "Age") {
          # Slider input for Age range
          data <- data %>% filter(Age >= filter_values[1] & Age <= filter_values[2])
        } else if (col == "Hire_Date") {
          # Date range input for Hire Date
          data <- data %>% filter(Hire_Date >= as.Date(filter_values[1]) & Hire_Date <= as.Date(filter_values[2]))
        } else if (col == "Years_At_Company") {
          # Slider input for Years at Company range
          data <- data %>% filter(Years_At_Company >= filter_values[1] & Years_At_Company <= filter_values[2])
        } else if (col == "Sick_Days") {
          # Slider input for Sick Days range
          data <- data %>% filter(Sick_Days >= filter_values[1] & Sick_Days <= filter_values[2])
        } else if (col == "Remote_Work_Frequency") {
          # Select input for Remote Work Frequency
          data <- data %>% filter(Remote_Work_Frequency %in% filter_values)
        } else if (col == "Training_Hours") {
          # Slider input for Training Hours range
          data <- data %>% filter(Training_Hours >= filter_values[1] & Training_Hours <= filter_values[2])
        }
      }
    }
    
    return(data)  # Return the filtered dataset
  })
  
  
  
  # Graph: Correlation Bar Chart (Plotly)
  output$correlation_plot <- renderPlotly({
    df <- filtered_data()
    
    # Selecting relevant columns
    data_subset <- df %>%
      select(Performance_Score, Education_Level, Job_Title, Age, Monthly_Salary, 
             Remote_Work_Frequency, Overtime_Hours, Training_Hours, Employee_Satisfaction_Score)
    
    # Convert categorical variables to dummy variables
    data_subset <- data_subset %>%
      mutate(Education_Level = as.factor(Education_Level),
             Job_Title = as.factor(Job_Title)) %>%
      model.matrix(~ . - 1, data = .) %>%
      as.data.frame()
    
    # Calculate correlation matrix
    cor_matrix <- cor(data_subset, use = "complete.obs")
    
    # Extract correlations with Performance_Score
    performance_corr <- data.frame(
      Factor = colnames(cor_matrix),
      Correlation = cor_matrix[,"Performance_Score"]
    )
    
    # Remove Performance_Score itself and sort by absolute correlation
    performance_corr <- performance_corr %>%
      filter(Factor != "Performance_Score") %>%
      arrange(desc(abs(Correlation)))
    
    performance_corr$Color <- ifelse(performance_corr$Correlation >= 0, 'skyblue', '#ff5050')
    
    # Plot bar chart
    plot_ly(
      data = performance_corr,
      x = ~reorder(Factor, abs(Correlation)),
      y = ~Correlation,
      type = "bar",
      marker = list(color = ~Color)
    ) %>%
      layout(
        title = "Relevance of Factors to Performance Score",
        xaxis = list(title = "Factor", tickangle = -45),
        yaxis = list(title = "Correlation", range = c(-0.01, 0.02)),
        margin = list(b = 150)
      )
    
  })
  
  # Graph: Performance vs Work Hours
  output$work_hours_plot <- renderPlot({
    data <- filtered_data()
    employeeData <- data %>% drop_na(Performance_Score, Job_Title, Monthly_Salary, Team_Size, Projects_Handled)
    
    employeeData$Job_Title <- as.factor(employeeData$Job_Title)
    employeeData$Team_Size <- as.numeric(employeeData$Team_Size)
    employeeData$Performance_Score <- as.numeric(employeeData$Performance_Score)
    
    employeeData <- employeeData %>%
      mutate(Team_Size_Range = case_when(
        Team_Size >= 1 & Team_Size <= 5 ~ "1-5 Team Members",
        Team_Size >= 6 & Team_Size <= 10 ~ "6-10 Team Members",
        Team_Size >= 11 & Team_Size <= 15 ~ "11-15 Team Members",
        Team_Size >= 16 & Team_Size <= 20 ~ "16-20 Team Members",
        TRUE ~ "21+"
      ),
      Team_Size_Range = factor(Team_Size_Range, levels = c("1-5 Team Members", "6-10 Team Members", "11-15 Team Members", "16-20 Team Members", "21+ Team Members")))
    
    
    bar_employeeData <- employeeData %>%
      group_by(Job_Title, Team_Size_Range) %>%
      summarise(Average_Performance = mean(Performance_Score, na.rm = TRUE))
    
    ggplot(bar_employeeData, aes(x = reorder(Job_Title, -Average_Performance), y = Average_Performance, fill = Job_Title)) +
      geom_bar(stat = "identity") +
      labs(title = "Average Performance Score by Job Title and Team Sizes",
           x = "Job Title",
           y = "Average Performance Score") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none") +
      coord_cartesian(ylim = c(2.92, 3.05)) +
      facet_wrap(~ Team_Size_Range, scales = "free_x")

  })
  
  # Graph: Performance by Projects Handled
  output$projects_plot <- renderPlot({
    
    employee_data <- filtered_data() %>%
      
      # Work Hours Per Week Group
      mutate(Work_Hours_Per_Week_Group = case_when(
        Work_Hours_Per_Week <= 20 ~ "<20",
        Work_Hours_Per_Week <= 30 ~ "20-30",
        Work_Hours_Per_Week <= 40 ~ "30-40",
        Work_Hours_Per_Week <= 50 ~ "40-50",
        Work_Hours_Per_Week <= 60 ~ "50-60",
        TRUE ~ "60+"
      )) %>%
      
      # Projects Handled Group
      mutate(Projects_Handled_Group = case_when(
        Projects_Handled <= 5 ~ "<5",
        Projects_Handled <= 10 ~ "5-10",
        Projects_Handled <= 20 ~ "10-20",
        Projects_Handled <= 30 ~ "20-30",
        Projects_Handled <= 40 ~ "30-40",
        TRUE ~ "40+"
      )) %>%
      
      # Overtime Hours Group
      mutate(Overtime_Hours_Group = case_when(
        Overtime_Hours <= 0 ~ "0-10",
        Overtime_Hours <= 20 ~ "10-20",
        Overtime_Hours <= 30 ~ "20-30",
        Overtime_Hours <= 40 ~ "30-40",
        TRUE ~ "40+"
      ))
    
    dept_proj_perf <- employee_data %>%
      group_by(Department, Projects_Handled) %>%
      summarise(Avg_Performance = mean(Performance_Score, na.rm = TRUE), .groups = "drop")
    
    ggplot(dept_proj_perf, aes(Projects_Handled, Avg_Performance)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(Department ~ .) +
      theme_minimal() +
      labs(
        x = "Number of Projects Handled",
        y = "Average Performance Score",
        title = "Average Performance vs Projects Handled by Department"
      )
  })
  
  # Graph: Departmental Trends
  output$dept_trends_plot <- renderPlot({
    employee_data <- filtered_data() %>%
      
      # Work Hours Per Week Group
      mutate(Work_Hours_Per_Week_Group = case_when(
        Work_Hours_Per_Week <= 20 ~ "<20",
        Work_Hours_Per_Week <= 30 ~ "20-30",
        Work_Hours_Per_Week <= 40 ~ "30-40",
        Work_Hours_Per_Week <= 50 ~ "40-50",
        Work_Hours_Per_Week <= 60 ~ "50-60",
        TRUE ~ "60+"
      )) %>%
      
      # Projects Handled Group
      mutate(Projects_Handled_Group = case_when(
        Projects_Handled <= 5 ~ "<5",
        Projects_Handled <= 10 ~ "5-10",
        Projects_Handled <= 20 ~ "10-20",
        Projects_Handled <= 30 ~ "20-30",
        Projects_Handled <= 40 ~ "30-40",
        TRUE ~ "40+"
      )) %>%
      
      # Overtime Hours Group
      mutate(Overtime_Hours_Group = case_when(
        Overtime_Hours <= 0 ~ "0-10",
        Overtime_Hours <= 20 ~ "10-20",
        Overtime_Hours <= 30 ~ "20-30",
        Overtime_Hours <= 40 ~ "30-40",
        TRUE ~ "40+"
      ))
    
    dept_hours_perf <- employee_data %>%
      group_by(Department, Work_Hours_Per_Week) %>%
      summarise(Avg_Performance = mean(Performance_Score))
    
    ggplot(dept_hours_perf, aes(Work_Hours_Per_Week, Avg_Performance)) +
      geom_point() +
      geom_smooth(method = "lm") +
      facet_wrap(Department ~ .) +
      theme_minimal() +
      labs(
        x = "Work Hours Per Week",
        y = "Average Performance Score",
        title = "Average Performance vs Work Hours Per Week by Department"
      )
    
    
  })
  
  # Graph: Career Progression
  output$career_progression_plot <- renderPlot({
    
    employee_data <- filtered_data()
    # Create career progression analysis
    career_analysis <- employee_data %>%
      mutate(
        experience_band = cut(Years_At_Company,
                              breaks = c(-1, 2, 5, 10, Inf),
                              labels = c("New (<2 yrs)", 
                                         "Established (2-5 yrs)",
                                         "Experienced (5-10 yrs)", 
                                         "Veteran (10+ yrs)")),
        promotion_rate = Promotions / Years_At_Company,
        # Categorize performance for clearer insights
        performance_level = case_when(
          Performance_Score >= 4 ~ "High Performer",
          Performance_Score >= 3 ~ "Average Performer",
          TRUE ~ "Below Average"
        )
      ) %>%
      group_by(experience_band, Education_Level) %>%
      summarize(
        avg_salary = mean(Monthly_Salary),
        retention_rate = (1 - mean(Resigned == "True")) * 100,
        promotion_rate = mean(promotion_rate),
        avg_satisfaction = mean(Employee_Satisfaction_Score),
        n_employees = n(),
        pct_high_performers = mean(performance_level == "High Performer") * 100
      )
    
    # Create the visualization
    ggplot(career_analysis, 
           aes(x = experience_band, y = avg_salary/1000)) +
      geom_col(aes(fill = retention_rate)) +
      geom_text(aes(label = sprintf("n=%d\n%.1f%% High Perf", 
                                    n_employees, pct_high_performers)),
                position = position_stack(vjust = 0.5)) +
      facet_wrap(~Education_Level) +
      scale_fill_gradient(low = "#FF6B6B", high = "#4169E1") +
      labs(title = "Career Progression by Education Level",
           subtitle = "Numbers show employee count and % high performers | Color indicates retention rate",
           x = "Years of Experience",
           y = "Average Monthly Salary (K$)",
           fill = "Retention Rate (%)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold")
      )
  })
  
  # Render filtered datatable
  output$filtered_table <- renderDT({
    req(filtered_data())  # Ensure the filtered data is not NULL
    filtered_data()
  }, options = list(pageLength = 10))
}

shinyApp(ui, server)
