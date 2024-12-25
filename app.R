library(shiny)
library(shinydashboard)
library(readxl)
library(ggplot2)
library(scales) # For formatting y-axis labels

# Load and clean the dataset
dataset <- read_excel('Revenue Versus Headcount.xlsx')
dataset <- dataset[rowSums(is.na(dataset)) != ncol(dataset), ]

starts_with_column <- function(x) {
  all(startsWith(x, "Column"))
}

# Apply the function to each row and filter out those that satisfy the condition
dataset <- dataset[!apply(dataset, 1, starts_with_column), ]

Geoplex_overall <- dataset
Geoplex_overall <- as.data.frame(t(Geoplex_overall))
row.names(Geoplex_overall) <- NULL
colnames(Geoplex_overall) <- Geoplex_overall[1,]
Geoplex_overall <- Geoplex_overall[-1, ]

# Process the data into separate dataframes
total_rev_indices <- grep("Total Revenue", names(Geoplex_overall))
separate_dfs <- list()
df_names <- c()

for (i in seq_along(total_rev_indices)) {
  if (i == length(total_rev_indices)) {
    subset_df <- Geoplex_overall[, (total_rev_indices[i] - 1):ncol(Geoplex_overall)]
  } else {
    subset_df <- Geoplex_overall[, (total_rev_indices[i] - 1):(total_rev_indices[i + 1] - 2)]
  }
  
  subset_df[, -1] <- lapply(subset_df[, -1], as.numeric)
  first_col_name <- names(subset_df)[1]
  names(subset_df)[1] <- "Date"
  assign(first_col_name, subset_df)
  separate_dfs[[first_col_name]] <- subset_df
  df_names <- c(df_names, first_col_name)
}

# Shiny App
ui <- dashboardPage(
  dashboardHeader(title = "Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                box(width = 8, plotOutput("plot")),
                box(width = 4,
                    selectizeInput("df_select", "Select Departments:", choices = df_names, multiple = TRUE, selected = df_names[1]),
                    uiOutput("dynamic_filter1"),
                    selectInput("time_filter", "Select Time Period:", choices = c("Months", "Years"), selected = "Months")
                )
              ),
              fluidRow(
                box(width = 12, title = "Summary", status = "primary", solidHeader = TRUE, textOutput("summary"))
              )
      )
    )
  )
)


server <- function(input, output, session) {
  
  selected_dfs <- reactive({
    df_names <- input$df_select
    lapply(df_names, function(name) separate_dfs[[name]])
  })
  
  filtered_dfs <- reactive({
    dfs <- selected_dfs()
    lapply(dfs, function(df) {
      if (input$time_filter == "Years") {
        df[(nrow(df)-2):nrow(df), ][rev(1:3), ] # Filter and reverse rows
      } else {
        df[1:(nrow(df)-3), ]
      }
    })
  })
  
  output$dynamic_filter1 <- renderUI({
    dfs <- selected_dfs()
    if(length(dfs) > 0) {
      colnames <- names(dfs[[1]])
      selectInput("filter1", "Select KPI:", choices = colnames, selected = colnames[2])
    }
  })
  
  output$plot <- renderPlot({
    dfs <- filtered_dfs()
    if (length(dfs) > 0 && !is.null(input$filter1)) {
      combined_df <- do.call(rbind, lapply(1:length(dfs), function(i) {
        df <- dfs[[i]]
        df$Department <- input$df_select[i]
        
        # Ensure all dataframes have the same columns
        all_cols <- unique(unlist(lapply(dfs, colnames)))
        missing_cols <- setdiff(all_cols, colnames(df))
        df[missing_cols] <- NA
        
        df
      }))
      
      combined_df$Date <- factor(combined_df$Date, levels = unique(combined_df$Date[order(as.Date(paste0("01-", combined_df$Date), format = "%d-%b-%y"))])) # Ensure chronological order
      
      ggplot(combined_df, aes_string(x = "Date", y = paste0("`", input$filter1, "`"), group = "Department", color = "Department")) +
        geom_line(size = 1) + geom_point() + theme_bw() +
        labs(
          title = "Comparison of Departments",
          subtitle = paste("Plot of", input$filter1, "vs Date"),
          x = "Date",
          y = input$filter1
        ) +
        scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
        theme_minimal()
    }
  })
  
  output$summary <- renderText({
    dfs <- filtered_dfs()
    if (length(dfs) > 0 && !is.null(input$filter1)) {
      summaries <- lapply(1:length(dfs), function(i) {
        df <- dfs[[i]]
        kpi <- input$filter1
        department <- input$df_select[i]
        
        mean_kpi <- mean(df[[kpi]], na.rm = TRUE)
        max_kpi <- max(df[[kpi]], na.rm = TRUE)
        min_kpi <- min(df[[kpi]], na.rm = TRUE)
        
        paste(department, ":", 
              "\nMean", kpi, "=", scales::comma(mean_kpi), 
              "\nMax", kpi, "=", scales::comma(max_kpi), 
              "\nMin", kpi, "=", scales::comma(min_kpi))
      })
      
      paste(summaries, collapse = "\n\n")
    } else {
      "Please select at least one department and KPI to see the summary."
    }
  })
}

shinyApp(ui, server)
