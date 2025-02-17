library(shiny)
library(ggplot2)
library(lubridate)
library(shinydashboard)
library(bslib)
library(shinyjs)


ui <- dashboardPage(
  dashboardHeader(title = "CSV File Viewer & Graph Builder"),
  
  # Sidebar configuration - Removed the width argument
  dashboardSidebar(
    fileInput("file", "Choose CSV File", accept = ".csv"),
    actionButton("show_summary", "Show Summary", icon = icon("info-circle")),
    
    # Space the variable selection inputs better
    uiOutput("var_select_ui"),
    
    selectInput("plot_type", "Choose Plot Type", 
                choices = c("None", "Histogram", "Bar Plot", "Scatterplot", "Mosaic Plot", "Boxplot", "Time Series"),
                selectize = TRUE),
    
    br(), # Extra space before next elements
    helpText("Choose the graph type and select the variables."),
    
    # Download button for images
    downloadButton("download_plot", "Download Plot")
  ),
  
  # Body configuration with fluid layout
  dashboardBody(
    useShinyjs(),  # Enables JavaScript functionality (e.g., hiding/showing elements)
    
    fluidRow(
      # Data Preview and Summary Table aligned in two boxes
      box(title = "Data Preview", status = "primary", solidHeader = TRUE, width = 6, 
          tableOutput("table")),
      box(title = "Summary Output", status = "info", solidHeader = TRUE, width = 6, 
          verbatimTextOutput("summary"))
    ),
    
    fluidRow(
      # Plot area taking up the entire width
      box(title = "Plot", status = "warning", solidHeader = TRUE, width = 12, 
          plotOutput("plot", height = "400px"))
    )
  )
)

server <- function(input, output, session) {
  
  data <- reactive({
    req(input$file)
    df <- read.csv(input$file$datapath)
    df[sapply(df, is.character)] <- lapply(df[sapply(df, is.character)], as.factor)
    df
  })
  
  output$table <- renderTable({ head(data(), 6) })
  
  output$summary <- renderPrint({ req(input$show_summary, data()); summary(data()) })
  
  output$var_select_ui <- renderUI({
    req(data())
    df <- data()
    numeric_vars <- names(df)[sapply(df, is.numeric)]
    factor_vars <- names(df)[sapply(df, is.factor)]
    
    tagList(
      h4("Variable Selection"),
      selectInput("single_num_var", "Numeric Variable (Single)", choices = c("None", numeric_vars)),
      selectInput("single_qual_var", "Qualitative Variable (Single)", choices = c("None", factor_vars)),
      selectInput("num_var_x", "X Numeric Variable", choices = c("None", numeric_vars)),
      selectInput("num_var_y", "Y Numeric Variable", choices = c("None", numeric_vars)),
      selectInput("qual_var_x", "X Qualitative Variable", choices = c("None", factor_vars)),
      selectInput("qual_var_y", "Y Qualitative Variable", choices = c("None", factor_vars)),
      selectInput("date_var", "Date Variable", choices = c("None", names(df))),
      selectInput("time_series_var", "Time Series Variable (Numeric)", choices = c("None", numeric_vars))
    )
  })
  
  output$plot <- renderPlot({
    req(data())
    df <- data()
    
    plot_type <- input$plot_type
    title_text <- ""
    
    if (!is.null(input$single_num_var) && input$single_num_var != "None") {
      data_range <- diff(range(df[[input$single_num_var]], na.rm = TRUE))
      n_bins <- min(30, max(15, floor(length(df[[input$single_num_var]]) / 10)))
      bin_width <- data_range / n_bins
    } else {
      bin_width <- 10
    }
    
    bar_width <- 0.7
    
    # Aesthetic modifications for plots
    plot_theme <- theme_minimal() +
      theme(text = element_text(family = "Helvetica", size = 14), 
            plot.title = element_text(size = 16, face = "bold", color = "darkblue"),
            plot.background = element_rect(fill = "white", color = NA), 
            panel.grid.major = element_line(color = "gray90", size = 0.5), 
            panel.grid.minor = element_line(color = "gray95", size = 0.25))
    
    if (plot_type == "Histogram" && input$single_num_var != "None") {
      title_text <- paste("Histogram of", input$single_num_var)
      ggplot(df, aes_string(input$single_num_var)) + 
        geom_histogram(binwidth = bin_width, fill = "#69b3a2", color = "black", alpha = 0.7) + 
        ggtitle(title_text) + plot_theme
      
    } else if (plot_type == "Bar Plot" && input$single_qual_var != "None") {
      title_text <- paste("Bar Plot of", input$single_qual_var)
      ggplot(df, aes_string(input$single_qual_var)) + 
        geom_bar(fill = "#ff7f0e", alpha = 0.7, width = bar_width) + 
        ggtitle(title_text) + plot_theme
      
    } else if (plot_type == "Scatterplot" && input$num_var_x != "None" && input$num_var_y != "None") {
      title_text <- paste("Scatterplot of", input$num_var_x, "vs", input$num_var_y)
      ggplot(df, aes_string(input$num_var_x, input$num_var_y)) + 
        geom_point(color = "#1f77b4", size = 2) + 
        ggtitle(title_text) + plot_theme
      
    } else if (plot_type == "Mosaic Plot" && input$qual_var_x != "None" && input$qual_var_y != "None") {
      title_text <- paste("Mosaic Plot of", input$qual_var_x, "by", input$qual_var_y)
      ggplot(df, aes_string(x = input$qual_var_x, fill = input$qual_var_y)) +
        geom_bar(position = "fill") +
        scale_fill_brewer(palette = "Set2") +
        ggtitle(title_text) + plot_theme
      
    } else if (plot_type == "Boxplot" && input$qual_var_x != "None" && input$num_var_y != "None") {
      title_text <- paste("Boxplot of", input$num_var_y, "by", input$qual_var_x)
      ggplot(df, aes_string(x = input$qual_var_x, y = input$num_var_y)) + 
        geom_boxplot(fill = "#9467bd", alpha = 0.7) +
        ggtitle(title_text) + plot_theme
      
    }else if (plot_type == "Time Series" && input$date_var != "None" && input$time_series_var != "None") {
      date_var <- df[[input$date_var]]
      
      if (!inherits(date_var, "Date")) {
        parsed_date <- parse_date_time(date_var, orders = c("dmy", "mdy", "ymd", "dby", "myd", "dmy"))
        
        if (any(is.na(parsed_date))) {
          ggplot() +
            annotate("text", x = 1, y = 1, label = "Date conversion failed. Please check the format.", size = 5) +
            theme_void()
          return()
        }
        
        df[[input$date_var]] <- as.Date(parsed_date)
      }
      
      df_time <- df[!is.na(df[[input$date_var]]) & !is.na(df[[input$time_series_var]]), ]
      
      ggplot(df_time, aes_string(x = input$date_var, y = input$time_series_var)) +
        geom_line(color = "#2ca02c", size = 1.2) +
        scale_x_date(date_labels = "%d %b %Y") +
        ggtitle(paste("Time Series of", input$time_series_var, "over", input$date_var)) + plot_theme
    }
  
    
  })
  
  # Save the plot for download
  output$download_plot <- downloadHandler(
    filename = function() { paste("plot_", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png", height = 6, width = 8)
    }
  )
}

shinyApp(ui, server)
