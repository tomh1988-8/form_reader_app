app_ui <- function() {
  shinyUI(
    dashboardPage(
      dashboardHeader(title = "Easybook"),
      dashboardSidebar(
        sidebarMenu(
          id = "tabs",
          menuItem("Filters", tabName = "frontPage"),  # Filters tab first
          menuItem("Text Search Forms", tabName = "TextSearch")
        )
      ),
      dashboardBody(
        use_theme(my_theme),
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
          tags$style(HTML("
            body {
                font-family: 'Open Sans', sans-serif;
            }
            .large-output pre {
                height: 150px; /* Set a specific height */
                overflow-y: auto; /* Enable scrolling if content overflows */
                width: 100%; /* Ensure the pre tag takes up all horizontal space */
                font-size: 1em; /* Adjust the font size as necessary */
                white-space: pre-wrap; /* Ensures text wraps and uses all available space */
                word-break: break-word; /* Ensures long words do not cause horizontal scrolling */
                line-height: 1.4; /* Adjust the line height for better readability */
            }
          "))
        ),
        img(src = "logo.png", id = "logo"),
        
        tabItems(
          # Filters Tab (with two grouping variables)
          tabItem(
            tabName = "frontPage",
            fluidRow(
              column(
                6,
                uiOutput("variablePicker1"),  # First categorical variable selection
                uiOutput("levelPicker1")  # Level selection for the first variable
              ),
              column(
                6,
                uiOutput("variablePicker2"),  # Second categorical variable selection
                uiOutput("levelPicker2")  # Level selection for the second variable
              )
            ),
            actionButton("confirm", "Confirm Filter"),
            actionButton("reset", "Reset Filter"),
            actionButton("showDescription", "Read Me!"),
            verbatimTextOutput("filterStatus")
          ),
          
          # Text Search Tab
          tabItem(
            tabName = "TextSearch",
            tabsetPanel(
              tabPanel(
                "Keyword Search",
                style = "padding: 50px; position: relative;",
                fluidRow(
                  column(3, textInput("evidenceString1", "First keyword:")),
                  column(3, textInput("evidenceString2", "Second keyword:")),
                  column(3, selectInput("booleanOperator", "Boolean Operator:", choices = c("AND", "OR", "NOT")))
                ),
                fluidRow(
                  column(3, dateInput("startDate", "Select Start Date:", value = as.Date("2023-01-01"), format = "dd-mm-yyyy")),
                  column(3, dateInput("endDate", "Select End Date:", value = Sys.Date(), format = "dd-mm-yyyy"))
                ),
                fluidRow(
                  column(3, downloadButton("downloadExcel", icon = icon("file-excel"))),
                  column(9, actionButton("filterButton", label = "Click to start", icon = icon("search")))
                ),
                fluidRow(
                  column(12, uiOutput("searchStatus"))  # Placeholder for search completion message
                )
              ),
              
              # View Forms Tab
              tabPanel(
                "View Forms",
                fluidRow(
                  box(
                    width = 12,
                    title = "Evidence Form Details",
                    solidHeader = TRUE,
                    status = "primary",
                    fluidRow(
                      column(6, h4("Local Authority"), verbatimTextOutput("localAuthority")),
                      column(6, h4("Gender"), verbatimTextOutput("gender"))
                    ),
                    fluidRow(
                      column(6, h4("Age Group"), verbatimTextOutput("ageGroup")),
                      column(6, h4("Disability"), verbatimTextOutput("disability"))
                    ),
                    fluidRow(
                      column(12, h4("Problem"), div(verbatimTextOutput("problem"), class = "large-output")),
                      column(12, h4("Impact"), div(verbatimTextOutput("impact"), class = "large-output")),
                      column(12, h4("Number of Results"), verbatimTextOutput("numResults"))
                    ),
                    div(
                      class = "d-flex justify-content-end mt-2",
                      actionButton("prevButton", "Previous", icon = icon("arrow-left")),
                      actionButton("nextButton", "Next", icon = icon("arrow-right"))
                    )
                  )
                )
              ),
              
              # Word Cloud Tab
              tabPanel(
                "Word Cloud",
                fluidRow(
                  box(uiOutput("wordcloudOutput"), width = 6)
                ),
                fluidRow(
                  column(
                    4,
                    downloadButton("downloadclouddata", "Download Data")
                  ),
                  column(
                    4,
                    actionButton("refreshButton", "Refresh Word Cloud", class = "btn-primary",
                                 style = "background-color: white; color: black; border-color: #ccc;")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}
