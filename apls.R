library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(DT, warn.conflicts = FALSE)
library(scales)
options(shiny.maxRequestSize = 50 * 1024^2)


ui <- dashboardPage(
  dashboardHeader(title = "My Shinydashboard App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Upload Excel File", tabName = "upload", icon = icon("upload")),
      menuItem("Result", tabName = "result", icon = icon("table")),
      menuItem("Apl", tabName = "all", icon = icon("list")),
      menuItem("Fund Description", tabName = "categories", icon = icon("list"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "upload",
        h2("Upload Excel File"),
        fluidRow(
          column(
            width = 5,
            box(
              title = "Upload",
              status = "warning",
              solidHeader = TRUE,
              width = 8,
              fileInput("excelFile", "Choose Excel File (xlsx)", accept = c(".xlsx", ".csv")),
              br(),
              actionButton("submitBtn", "Submit")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "result",
        h2("Result"),
        fluidRow(
          column(
            width = 5,
            box(
              title = "Result Table",
              status = "warning",
              solidHeader = TRUE,
              tableOutput("resultTable")
            )
          ),
          column(
            width = 12,
            box(
              title = "Result DT",
              status = "info",
              solidHeader = TRUE,
              DTOutput("resultDT")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "all",
        h2("Apl"),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Filter by Date Range 1",
              status = "warning",
              solidHeader = TRUE,
              dateRangeInput(
                "dateRangeAll1",
                "Select Date Range 1:",
                start = Sys.Date(),
                end = Sys.Date(),
                separator = " - "
              ),
              br(),
              actionButton("filterBtn1", "Filter 1")
            )
          ),
          column(
            width = 6,
            box(
              title = "Filter by Date Range 2",
              status = "warning",
              solidHeader = TRUE,
              dateRangeInput(
                "dateRangeAll2",
                "Select Date Range 2:",
                start = Sys.Date(),
                end = Sys.Date(),
                separator = " - "
              ),
              br(),
              actionButton("filterBtn2", "Filter 2")
            )
          ),
          column(
            width = 6.5,
            box(
              title = "Filtered DT 1",
              status = "info",
              solidHeader = TRUE,
              DTOutput("filteredDT1")
            )
          ),
          column(
            width = 6.5,
            box(
              title = "Filtered DT 2",
              status = "info",
              solidHeader = TRUE,
              DTOutput("filteredDT2")
            )
          )
        )
      ),
      
      tabItem(
        tabName = "categories",
        h2("Fund Description"),
        fluidRow(
          column(
            width = 6,
            box(
              title = "Filter by Date Range 1",
              status = "warning",
              solidHeader = TRUE,
              dateRangeInput(
                "dateRangeCategories1",
                "Select Date Range 1:",
                start = Sys.Date(),
                end = Sys.Date(),
                separator = " - "
              ),
              br(),
              actionButton("filterCategoriesBtn1", "Filter 1")
            )
          ),
          column(
            width = 6,
            box(
              title = "Filter by Date Range 2",
              status = "warning",
              solidHeader = TRUE,
              dateRangeInput(
                "dateRangeCategories2",
                "Select Date Range 2:",
                start = Sys.Date(),
                end = Sys.Date(),
                separator = " - "
              ),
              br(),
              actionButton("filterCategoriesBtn2", "Filter 2")
            )
          ),
          column(
            width = 6.5,
            box(
              title = "Categories DT 1",
              status = "info",
              solidHeader = TRUE,
              DTOutput("categoriesDT1")
            )
          ),
          column(
            width = 6.5,
            box(
              title = "Categories DT 2",
              status = "info",
              solidHeader = TRUE,
              DTOutput("categoriesDT2")
            )
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  # Read the uploaded Excel file
  dat <- reactive({
    req(input$excelFile)
    inFile <- input$excelFile
    read_excel(inFile$datapath)
  })
  
  # Calculate the desired result for the table output
  resultTable <- reactive({
    dat() %>%
      group_by(Gf_CnBio_ID) %>%
      summarise(`n#` = n(), `Total Amount` = sum(Gf_Amount)) %>%
      ungroup() %>%
      summarise(`n#` = format(sum(`n#`), big.mark = ","), 
                `Total Amount` = dollar(sum(`Total Amount`), prefix = "$", big.mark = ",", decimal.mark = "."))
  })
  
  # Calculate the desired result for the DT output
  resultDT <- reactive({
    dat() %>%
      group_by(Gf_CnBio_ID) %>%
      summarise(Count = n(), Total_Amount = sum(Gf_Amount)) %>%
      mutate(
        Count = format(Count, big.mark = ","),
        Total_Amount = dollar(Total_Amount, prefix = "$", big.mark = ",", decimal.mark = ".")
      )
  })
  
  # Calculate the description counts for Date Range 1
  descriptionCounts1 <- reactive({
    req(input$dateRangeAll1)
    start_date <- as.POSIXct(input$dateRangeAll1[1]) 
    end_date <- as.POSIXct(input$dateRangeAll1[2])
    
    filtered_data <- dat() %>%
      filter(Gf_Date >= start_date, Gf_Date <= end_date)
    
    all_descriptions <- unlist(filtered_data[, c("Gf_Apls_1_01_Description", "Gf_Apls_1_02_Description", "Gf_Apls_1_03_Description")])
    description_counts <- table(all_descriptions)
    
    data.frame(Description = names(description_counts), Count = format(as.numeric(description_counts), big.mark = ","))
  })
  
  # Calculate the description counts for Date Range 2
  descriptionCounts2 <- reactive({
    req(input$dateRangeAll2)
    start_date <- as.POSIXct(input$dateRangeAll2[1])
    end_date <- as.POSIXct(input$dateRangeAll2[2])  
    
    filtered_data <- dat() %>%
      filter(Gf_Date >= start_date, Gf_Date <= end_date)
    
    all_descriptions <- unlist(filtered_data[, c("Gf_Apls_1_01_Description", "Gf_Apls_1_02_Description", "Gf_Apls_1_03_Description")])
    description_counts <- table(all_descriptions)
    
    data.frame(Description = names(description_counts), Count = format(as.numeric(description_counts), big.mark = ","))
  })
  
  # Render the result table output
  output$resultTable <- renderTable({
    resultTable()
  })
  
  # Render the result DT output
  output$resultDT <- renderDT({
    resultDT()
  }, rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Blfrtip', lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), buttons = list('excel')))
  
  # Render the filtered DT 1 output
  output$filteredDT1 <- renderDT({
    descriptionCounts1()
  }, rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Blfrtip', lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), buttons = list('excel')))
  
  # Render the filtered DT 2 output
  output$filteredDT2 <- renderDT({
    descriptionCounts2()
  }, rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Blfrtip', lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), buttons = list('excel')))
  
  # Calculate the description counts and totals for Date Range 1 in Categories tab
  descriptionCountsCategories1 <- reactive({
    req(input$dateRangeCategories1)
    start_date <- as.POSIXct(input$dateRangeCategories1[1])  
    end_date <- as.POSIXct(input$dateRangeCategories1[2])   
    
    filtered_data <- dat() %>%
      filter(Gf_Date >= start_date, Gf_Date <= end_date)
    
    all_descriptions <- unlist(filtered_data[, c("Gf_Fnds_1_01_Description", "Gf_Fnds_1_02_Description", "Gf_Fnds_1_03_Description")])
    description_counts <- table(all_descriptions)
    
    if (length(description_counts) == 0) {
      return(data.frame(Description = character(), Count = numeric(), Total_Amount = numeric()))
    }
    
    totals <- aggregate(c(Gf_Fnds_1_01_Amount, Gf_Fnds_1_02_Amount, Gf_Fnds_1_03_Amount) ~
                          c(Gf_Fnds_1_01_Description, Gf_Fnds_1_02_Description, Gf_Fnds_1_03_Description),
                        data = filtered_data, sum)
    
    colnames(totals) <- c("Description", "Total_Amount")
    description_counts_df <- data.frame(Description = names(description_counts), Count = as.numeric(description_counts))
    
    merged_data <- merge(description_counts_df, totals, by = "Description", all.x = TRUE)
    merged_data[is.na(merged_data)] <- 0  # Replace NA values with 0
    
    # Format counts with commas
    merged_data$Count <- format(merged_data$Count, big.mark = ",")
    # Format totals with dollar signs
    merged_data$Total_Amount <- dollar(merged_data$Total_Amount, prefix = "$", big.mark = ",", decimal.mark = ".")
    
    merged_data
  })
  
  # Calculate the description counts and totals for Date Range 2 in Categories tab
  descriptionCountsCategories2 <- reactive({
    req(input$dateRangeCategories2)
    start_date <- as.POSIXct(input$dateRangeCategories2[1])  
    end_date <- as.POSIXct(input$dateRangeCategories2[2]) 
    
    filtered_data <- dat() %>%
      filter(Gf_Date >= start_date, Gf_Date <= end_date)
    
    all_descriptions <- unlist(filtered_data[, c("Gf_Fnds_1_01_Description", "Gf_Fnds_1_02_Description", "Gf_Fnds_1_03_Description")])
    description_counts <- table(all_descriptions)
    
    if (length(description_counts) == 0) {
      return(data.frame(Description = character(), Count = numeric(), Total_Amount = numeric()))
    }
    
    totals <- aggregate(c(Gf_Fnds_1_01_Amount, Gf_Fnds_1_02_Amount, Gf_Fnds_1_03_Amount) ~
                          c(Gf_Fnds_1_01_Description, Gf_Fnds_1_02_Description, Gf_Fnds_1_03_Description),
                        data = filtered_data, sum)
    
    colnames(totals) <- c("Description", "Total_Amount")
    description_counts_df <- data.frame(Description = names(description_counts), Count = as.numeric(description_counts))
    
    merged_data <- merge(description_counts_df, totals, by = "Description", all.x = TRUE)
    merged_data[is.na(merged_data)] <- 0  # Replace NA values with 0
    
    # Format counts with commas
    merged_data$Count <- format(merged_data$Count, big.mark = ",")
    # Format totals with dollar signs
    merged_data$Total_Amount <- dollar(merged_data$Total_Amount, prefix = "$", big.mark = ",", decimal.mark = ".")
    
    merged_data
  })
  
  
  # Render the Categories DT 1 output
  output$categoriesDT1 <- renderDT({
    descriptionCountsCategories1()
  }, rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Blfrtip', lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), buttons = list('excel')))
  
  # Render the Categories DT 2 output
  output$categoriesDT2 <- renderDT({
    descriptionCountsCategories2()
  }, rownames = FALSE, extensions = 'Buttons', options = list(dom = 'Blfrtip', lengthMenu = list(c(10, 25, 50, -1), c('10', '25', '50', 'All')), buttons = list('excel')))

}

shinyApp(ui, server)
