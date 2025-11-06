# ==========================================================
# Mobile Device Usage Explorer
# Project 2 – Shiny App
# ==========================================================

# ---- Load packages ----
library(shiny)
library(shinycssloaders)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(rlang)

# ==========================================================
# 1. LOAD DATA
# ==========================================================

DATA_PATH <- "data/user_behavior_dataset.csv"

raw_data <- read_csv(DATA_PATH, show_col_types = FALSE)

# quick data cleaning (optional)
data <- raw_data %>%
  rename_with(~ gsub(" ", "_", .x)) %>%  # replace spaces with underscores
  mutate(across(where(is.character), as.factor))  # categorical → factor

# Identify categorical and numeric columns
cat_vars <- names(data)[sapply(data, is.factor)]
num_vars <- names(data)[sapply(data, is.numeric)]

# ==========================================================
# 2. UI
# ==========================================================
ui <- fluidPage(
  titlePanel("📱 Mobile Device Usage Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Filters"),
      helpText("Select categories and numeric ranges, then click 'Apply Filters'."),
      
      # Categorical filters
      selectInput("cat1", "Select Gender:", choices = NULL, multiple = TRUE),
      selectInput("cat2", "Select Education Level:", choices = NULL, multiple = TRUE),
      
      # Numeric variable filters
      selectInput("num1", "Numeric Variable 1:", choices = NULL),
      uiOutput("num1_range"),
      
      selectInput("num2", "Numeric Variable 2:", choices = NULL),
      uiOutput("num2_range"),
      
      actionButton("apply", "Apply Filters", class = "btn btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("About",
                 h3("About This App"),
                 # apology section ---------------------------------
                 uiOutput("apology_section"),
                 tags$hr(),
                 p("This interactive dashboard explores patterns in mobile device usage
                   based on demographics and behavior."),
                 h4("Dataset"),
                 p("Source: ",
                   a("Mobile Device Usage and User Behavior Dataset (Kaggle)",
                     href = "https://www.kaggle.com/datasets/valakhorasani/mobile-device-usage-and-user-behavior-dataset")),
                 h4("Instructions"),
                 tags$ul(
                   tags$li("Use sidebar filters to choose subsets of the data."),
                   tags$li("Navigate tabs to view data, summaries, and visualizations."),
                   tags$li("Click 'Apply Filters' to update the view.")
                 ),
                 br(),
                 img(src = "https://media1.giphy.com/media/v1.Y2lkPTc5MGI3NjExOXcyOWp0dnlrbGE5YnRhZG40MGxsczV4OTVzMzF3OGh1OHhzcGRhdSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/3XRbEorYTwU0nNyQvu/giphy.gif",
                     height = "300px")
        ),
        
        tabPanel("Data Download",
                 h3("Preview and Download Data"),
                 DTOutput("data_table") %>% withSpinner(),
                 br(),
                 downloadButton("download_data", "Download Filtered CSV",
                                class = "btn btn-success")
        ),
        
        tabPanel("Data Exploration",
                 h3("Explore the Data"),
                 radioButtons("summary_type", "Choose summary type:",
                              choices = c("Categorical tables" = "cat",
                                          "Numeric summaries (by category)" = "num"),
                              inline = TRUE),
                 
                 conditionalPanel(
                   condition = "input.summary_type == 'cat'",
                   selectInput("cat_summary_var", "One-way Table Variable:", choices = NULL),
                   selectInput("cat_summary_var2", "Two-way Table Variable:", choices = NULL)
                 ),
                 
                 conditionalPanel(
                   condition = "input.summary_type == 'num'",
                   selectInput("num_summary_var", "Numeric Variable:", choices = NULL),
                   selectInput("group_var", "Group By (Categorical):", choices = NULL)
                 ),
                 
                 h4("Summary Table"),
                 DTOutput("summary_table") %>% withSpinner(),
                 
                 hr(),
                 h4("Visualizations"),
                 selectInput("plot_type", "Select Plot Type:",
                             choices = c("Histogram", "Boxplot", "Scatterplot", "Barplot", "Heatmap")),
                 uiOutput("xvar_ui"),
                 uiOutput("yvar_ui"),
                 uiOutput("color_ui"),
                 plotOutput("plot") %>% withSpinner()
        )
      )
    )
  )
)

# ==========================================================
# 3. SERVER
# ==========================================================
server <- function(input, output, session) {
  # --- apology section reactive UI ----
  output$apology_section <- renderUI({
    if (is.null(input$apology_choice)) {
      # show the gif + buttons initially
      div(
        style = "text-align:center; margin-bottom:20px;",
        img(
          src = "https://media0.giphy.com/media/v1.Y2lkPTc5MGI3NjExMTd1Z2JsY3JyZ2o3eGxuYTNkNXJhOGMxOG84YXpnaHVzNzBxZmRuNiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/8fJROjbScXV3q3KC92/giphy.gif",
          height = "150px"
        ),
        br(),
        h4("Sorry for the late submission 🙏"),
        p("I tried my best and learned a lot while completing this project."),
        br(),
        actionButton("forgive_btn", "✅ Yes, I forgive you", class = "btn btn-success"),
        actionButton("mad_btn", "😠 No, I am mad at you", class = "btn btn-danger")
      )
    } else {
      NULL  # hide after they pick
    }
  })
  
  observeEvent(input$forgive_btn, {
    output$apology_section <- renderUI({
      div(
        style = "text-align:center; color:darkgreen;",
        h4("Yay! Thank you for forgiving me 💕 Enjoy exploring the app!")
      )
    })
    later::later(function() {
      output$apology_section <- renderUI(NULL)
    }, delay = 3)
  })
  
  observeEvent(input$mad_btn, {
    output$apology_section <- renderUI({
      div(
        style = "text-align:center; color:darkred;",
        h4("Oh no 😢 I'll do better next time…"),
        p("(You can still explore the app!)")
      )
    })
    later::later(function() {
      output$apology_section <- renderUI(NULL)
    }, delay = 3)
  })
  
  
  
  # Initialize sidebar choices dynamically
  observe({
    updateSelectInput(session, "cat1", choices = unique(data[[cat_vars[1]]]),
                      selected = unique(data[[cat_vars[1]]]))
    updateSelectInput(session, "cat2", choices = unique(data[[cat_vars[2]]]),
                      selected = unique(data[[cat_vars[2]]]))
    updateSelectInput(session, "num1", choices = num_vars, selected = num_vars[1])
    updateSelectInput(session, "num2", choices = num_vars, selected = num_vars[2])
    updateSelectInput(session, "cat_summary_var", choices = cat_vars)
    updateSelectInput(session, "cat_summary_var2", choices = cat_vars)
    updateSelectInput(session, "num_summary_var", choices = num_vars)
    updateSelectInput(session, "group_var", choices = cat_vars)
  })
  
  # Dynamic sliders
  output$num1_range <- renderUI({
    req(input$num1)
    rng <- range(data[[input$num1]], na.rm = TRUE)
    sliderInput("num1_slider", paste0(input$num1, " Range:"), min = rng[1], max = rng[2],
                value = rng, step = 0.5)
  })
  
  output$num2_range <- renderUI({
    req(input$num2)
    rng <- range(data[[input$num2]], na.rm = TRUE)
    sliderInput("num2_slider", paste0(input$num2, " Range:"), min = rng[1], max = rng[2],
                value = rng, step = 0.5)
  })
  
  # Subset data on button click
  filtered <- eventReactive(input$apply, {
    df <- data
    if (!is.null(input$cat1)) df <- df[df[[cat_vars[1]]] %in% input$cat1, ]
    if (!is.null(input$cat2)) df <- df[df[[cat_vars[2]]] %in% input$cat2, ]
    if (!is.null(input$num1_slider)) {
      df <- df[df[[input$num1]] >= input$num1_slider[1] & df[[input$num1]] <= input$num1_slider[2], ]
    }
    if (!is.null(input$num2_slider)) {
      df <- df[df[[input$num2]] >= input$num2_slider[1] & df[[input$num2]] <= input$num2_slider[2], ]
    }
    df
  }, ignoreNULL = FALSE)
  
  # ---- Data Table ----
  output$data_table <- renderDT({
    datatable(filtered(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Download
  output$download_data <- downloadHandler(
    filename = function() { "filtered_data.csv" },
    content = function(file) { write_csv(filtered(), file) }
  )
  
  # ---- Summary Tables ----
  output$summary_table <- renderDT({
    df <- filtered()
    
    if (input$summary_type == "cat") {
      if (!is.null(input$cat_summary_var2) && input$cat_summary_var != input$cat_summary_var2) {
        tab <- as.data.frame.matrix(table(df[[input$cat_summary_var]], df[[input$cat_summary_var2]]))
      } else {
        tab <- as.data.frame(table(df[[input$cat_summary_var]]))
        names(tab) <- c("Category", "Count")
      }
      datatable(tab)
    } else {
      req(input$num_summary_var, input$group_var)
      tab <- df %>%
        group_by(.data[[input$group_var]]) %>%
        summarize(
          Mean = mean(.data[[input$num_summary_var]], na.rm = TRUE),
          Median = median(.data[[input$num_summary_var]], na.rm = TRUE),
          SD = sd(.data[[input$num_summary_var]], na.rm = TRUE),
          n = n(),
          .groups = "drop"
        )
      datatable(tab)
    }
  })
  
  # ---- Dynamic plotting UI ----
  output$xvar_ui <- renderUI({
    selectInput("xvar", "X variable:", choices = num_vars, selected = num_vars[1])
  })
  output$yvar_ui <- renderUI({
    selectInput("yvar", "Y variable (optional):", choices = c("None", num_vars), selected = "None")
  })
  output$color_ui <- renderUI({
    selectInput("colorvar", "Color by:", choices = c("None", cat_vars), selected = "None")
  })
  
  # ---- Plots ----
  output$plot <- renderPlot({
    df <- filtered()
    req(input$plot_type)
    
    if (nrow(df) == 0) return(NULL)
    
    p <- ggplot(df)
    
    if (input$plot_type == "Histogram") {
      p <- p + geom_histogram(aes(x = .data[[input$xvar]]), bins = 20, fill = "steelblue")
    } else if (input$plot_type == "Boxplot") {
      req(input$colorvar != "None")
      p <- p + geom_boxplot(aes(x = .data[[input$colorvar]], y = .data[[input$xvar]], fill = .data[[input$colorvar]]))
    } else if (input$plot_type == "Scatterplot") {
      req(input$yvar != "None")
      p <- p + geom_point(aes(x = .data[[input$xvar]], y = .data[[input$yvar]],
                              color = if (input$colorvar != "None") .data[[input$colorvar]] else NULL),
                          alpha = 0.7)
    } else if (input$plot_type == "Barplot") {
      req(input$colorvar != "None")
      p <- p + stat_summary(aes(x = .data[[input$colorvar]], y = .data[[input$xvar]]),
                            fun = mean, geom = "bar", fill = "darkorange")
    } else if (input$plot_type == "Heatmap") {
      corr_data <- df %>% select(where(is.numeric))
      corr <- cor(corr_data, use = "pairwise.complete.obs")
      corr_df <- as.data.frame(as.table(corr))
      names(corr_df) <- c("Var1", "Var2", "Correlation")
      p <- ggplot(corr_df, aes(Var1, Var2, fill = Correlation)) +
        geom_tile() +
        scale_fill_gradient2(low = "red", high = "blue", mid = "white", midpoint = 0) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    }
    
    p + theme_minimal() + labs(x = input$xvar, y = input$yvar)
  })
}

# ==========================================================
# 4. RUN APP
# ==========================================================
shinyApp(ui = ui, server = server)
