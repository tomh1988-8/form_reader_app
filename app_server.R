# Define the server logic in a function
app_server <- function(input, output, session) {
  
  ############################### FILTER ###########################################################
  # Backup the original data
  EFS_original <<- EFS  # Store the original unfiltered EFS
  
  # Show the "Read Me" modal dialog with app description
  observeEvent(input$showDescription, {
    showModal(modalDialog(
      title = "Application Description",
      "This app allows for analysis of the EFS data. You can apply filters on one or both of the categorical variables on the front page. The app makes calculations using the full dataset. However, if you wish to filter, select the options on the front page and click confirm. If you want to reset to the whole dataset, click Reset.",
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  # First categorical variable selector (with Demo_AICPart1 as default)
  output$variablePicker1 <- renderUI({
    valid_choices <- names(EFS)[sapply(EFS, is.factor) & !(names(EFS) %in% c("Demo_URL", "Demo_Case.Ref"))]
    pickerInput("group_var1", "Choose first grouping variable:", choices = valid_choices, selected = "Demo_AICPart1", multiple = TRUE, options = list(`actions-box` = TRUE))
  })
  
  # First level picker
  output$levelPicker1 <- renderUI({
    req(input$group_var1)
    pickerInput("group_levels1", "Select levels:", choices = levels(EFS[[input$group_var1]]), multiple = TRUE, options = list(`actions-box` = TRUE))
  })
  
  # Second categorical variable selector (with Demo_housing_tenure as default)
  output$variablePicker2 <- renderUI({
    valid_choices <- names(EFS)[sapply(EFS, is.factor) & !(names(EFS) %in% c("Demo_URL", "Demo_Case.Ref", 
                                                                             "Demo_Key.Phrase", "Demo_Problem", "Demo_Impact"))]
    pickerInput("group_var2", "Choose second grouping variable:", choices = valid_choices, selected = "Demo_housing_tenure", multiple = TRUE, options = list(`actions-box` = TRUE))
  })
  
  # Second level picker
  output$levelPicker2 <- renderUI({
    req(input$group_var2)
    pickerInput("group_levels2", "Select levels:", choices = levels(EFS[[input$group_var2]]), multiple = TRUE, options = list(`actions-box` = TRUE))
  })
  
  
  # Front Page Filters: Apply filters when confirmed
  observeEvent(input$confirm, {
    EFS <<- EFS_original  # Reset dataset before applying new filters
    
    # Print the structure of EFS before applying any filters
    print("Original EFS structure before filtering:")
    # print(str(EFS))
    
    # Apply first grouping variable filter
    if (!is.null(input$group_var1) && !is.null(input$group_levels1)) {
      print(paste("Filtering on group_var1:", input$group_var1))
      print(paste("Selected levels for group_var1:", paste(input$group_levels1, collapse = ", ")))
      EFS <<- EFS %>% filter(get(input$group_var1) %in% input$group_levels1)
      print(paste("Rows in EFS after applying filter on", input$group_var1, ":", nrow(EFS)))
    }
    
    # Apply second grouping variable filter
    if (!is.null(input$group_var2) && !is.null(input$group_levels2)) {
      print(paste("Filtering on group_var2:", input$group_var2))
      print(paste("Selected levels for group_var2:", paste(input$group_levels2, collapse = ", ")))
      EFS <<- EFS %>% filter(get(input$group_var2) %in% input$group_levels2)
      print(paste("Rows in EFS after applying filter on", input$group_var2, ":", nrow(EFS)))
    }
    
    # Print the structure and number of rows in the final filtered dataset
    print("Filtered EFS structure after all filters:")
    # print(str(EFS))
    print(paste("Total rows in EFS after filtering:", nrow(EFS)))
    
    # Print the number of rows in both datasets
    print(paste("Rows in EFS_original (unfiltered):", nrow(EFS_original)))
    
    output$filterStatus <- renderText("Filters applied successfully!")
  })
  
  # Reset filters and restore original data
  observeEvent(input$reset, {
    EFS <<- EFS_original  # Restore original dataset
    print("EFS has been reset to its original form.")
    print(paste("Rows in EFS after reset:", nrow(EFS)))
    
    output$filterStatus <- renderText("Filters reset successfully.")
  })
  
  ######################################################################################################
  ####################### TEXT SEARCH ############################################
  # Function to search with boolean logic
  search_with_boolean <- function(df, columns, pattern1, pattern2, operator = "AND") {
    # Print the structure of the dataframe
    print("Structure of dataframe before boolean search:")
   # print(str(df))
    
    # Ensure columns exist in the dataframe before proceeding
    valid_columns <- columns[columns %in% names(df)]
    if (length(valid_columns) == 0) {
      print("No valid columns for boolean search found.")
      return(df)  # Return the dataframe as is if no valid columns
    }
    
    # Perform the filtering using sapply and grepl
    logical_matrix <- sapply(valid_columns, function(column) {
      if (operator == "AND") {
        grepl(pattern1, df[[column]], ignore.case = TRUE, perl = TRUE) &
          grepl(pattern2, df[[column]], ignore.case = TRUE, perl = TRUE)
      } else if (operator == "OR") {
        grepl(pattern1, df[[column]], ignore.case = TRUE, perl = TRUE) |
          grepl(pattern2, df[[column]], ignore.case = TRUE, perl = TRUE)
      } else if (operator == "NOT") {
        grepl(pattern1, df[[column]], ignore.case = TRUE, perl = TRUE) &
          !grepl(pattern2, df[[column]], ignore.case = TRUE, perl = TRUE)
      }
    })
    
    # Print debug information for the logical matrix
    # print("Logical matrix produced by sapply:")
    # print(logical_matrix)
    
    # Ensure that the result of sapply is appropriate for rowSums
    if (is.matrix(logical_matrix) && ncol(logical_matrix) > 0) {
      df <- df[rowSums(logical_matrix) > 0, ]  # Apply the filter
    } else {
      print("Invalid logical matrix: skipping boolean filter")
    }
    
    df  # Return the filtered dataframe
  }
  
  # Main reactive function to filter data for Text Search
  filteredDataText <- reactive({
    req(input$filterButton > 0)  # Ensure the filter button has been clicked
    
    isolate({
      withProgress(message = "Filtering data...", value = 0, {
        EFS$Date <- as.Date(EFS$Date, format = "%d/%m/%Y")
        filtered <- EFS
        
        # Apply date range filter
        if (!is.na(input$startDate) && !is.na(input$endDate)) {
          filtered <- filtered %>% filter(Date >= as.Date(input$startDate) & Date <= as.Date(input$endDate))
          incProgress(1 / 3, detail = "Applying date filter...")
        }
        
        # Apply keyword filters based on boolean operator
        if (nzchar(input$evidenceString1) || nzchar(input$evidenceString2)) {
          pattern1 <- if (nzchar(input$evidenceString1)) input$evidenceString1 else ""
          pattern2 <- if (nzchar(input$evidenceString2)) input$evidenceString2 else ""
          
          # Print debug information
          print(paste("Pattern 1:", pattern1))
          print(paste("Pattern 2:", pattern2))
          print(paste("Boolean Operator:", input$booleanOperator))
          
          # Apply search with boolean logic
          filtered <- search_with_boolean(filtered, c("Demo_Key.Phrase", "Demo_Problem", "Demo_Impact"), pattern1, pattern2, input$booleanOperator)
          incProgress(1 / 3, detail = "Applying keyword filter...")
        }
        
        # Finalization
        filtered <- distinct(filtered, Evidence.Form.Id, .keep_all = TRUE)
        incProgress(1 / 3, detail = "Finalizing...")
        
        filtered  # Return the filtered dataset
      })
    })
  })
  
  observeEvent(input$filterButton, {
    
    # Reset the search completion message at the start of a new search
    output$searchStatus <- renderUI({
      HTML("")  # Clear the message when a new search starts
    })
    
    # Trigger the filtering process and ensure it's reactive
    filtered_data <- isolate({
      filteredDataText()  # Assuming filteredDataText() returns a data frame
    })
    
    # After the filtering process is complete, show the completion message directly under the search button
    output$searchStatus <- renderUI({
      if (nrow(filtered_data) > 0) {
        HTML('<p style="padding-top: 10px; margin-top: 0; text-align: left;">Search complete. Results are available.</p>')
      } else {
        HTML('<p style="padding-top: 10px; margin-top: 0; color: red; text-align: left;">No results found.</p>')
      }
    })
  })
  
  
  
  
  
  ############################# FORM READER ####################################################

  # Number of results output
  output$numResults <- renderText({
    paste("Total number of results:", nrow(filteredDataText()))
  })

  # Download handler for Excel
  output$downloadExcel <- downloadHandler(
    filename = function() {
      paste("data-summary-", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      
      # Debugging: Print the file path to check where the file is being saved
      print(paste("Saving file to:", file))
      
      filtered_dataset <- filteredDataText()
      
      # Debugging: Check if the filtered dataset is correctly retrieved
      print(paste("Number of rows in filtered dataset:", nrow(filtered_dataset)))
      
      EFS <- readRDS(file = "EFS.rds")
      
      # Debugging: Check if the EFS dataset is correctly loaded
      print(paste("Number of rows in EFS dataset:", nrow(EFS)))

      #### Code for original chunk (unfiltered and filtered summaries)
      Number_Unfiltered <- as.data.frame(nrow(EFS)) %>%
        rename("Total_Number_of_Forms_Unfiltered" = "nrow(EFS)")
      codes1 <- as.data.frame(unique(EFS$Demo_AICPart1)) %>%
        rename("All_Part_1_Codes" = "unique(EFS$Demo_AICPart1)")
      codes2 <- as.data.frame(unique(EFS$Demo_AICPart2)) %>%
        rename("All_Part_2_Codes" = "unique(EFS$Demo_AICPart2)")
      codes3 <- as.data.frame(unique(EFS$Demo_AICPart3)) %>%
        rename("All_Part_3_Codes" = "unique(EFS$Demo_AICPart3)")

      # Code for the filtered version (similar to unfiltered summaries)
      Number_Filtered <- as.data.frame(nrow(filtered_dataset)) %>%
        rename("Total_Number_of_Forms_Filtered" = "nrow(filtered_dataset)")
      codes1_filtered <- as.data.frame(unique(filtered_dataset$Demo_AICPart1)) %>%
        rename("Filtered_Part_1_Codes" = "unique(filtered_dataset$Demo_AICPart1)")
      
      # Debugging: Confirm before writing to the Excel file
      print("Writing to Excel file...")

      # Combine all datasets into one list
      Sheet.list <- list(
        Filtered_Evidence_Forms = filtered_dataset,
        Number_Unfiltered = Number_Unfiltered,
        Number_Filtered = Number_Filtered,
        All_Part_1_Codes = codes1,
        All_Part_2_Codes = codes2,
        All_Part_3_Codes = codes3,
        Filtered_Part_1_Codes = codes1_filtered
      )

      writexl::write_xlsx(Sheet.list, file)
      
      # Debugging: Confirm file creation
      print("Excel file successfully written.")
      
    }
  )

  ########### Form viewer ###################################
  formIndex <- reactiveVal(1)  # Reactive value for index
  
  # Reactive function for accessing filtered data
  reactiveFilteredData <- reactive({
    data <- filteredDataText()  # Fetch filtered data
    if (!is.data.frame(data)) {
      data <- as.data.frame(data)  # Ensure it's a data frame
    }
    return(data)
  })
  
  # Generic function to render text output for form details
  renderFormDetail <- function(columnName) {
    renderText({
      data <- reactiveFilteredData()
      current_index <- formIndex()  # Ensure reactivity on formIndex()
      if (nrow(data) > 0 && current_index <= nrow(data)) {
        if (columnName %in% colnames(data)) {
          # Convert factor to character just for rendering
          value <- data[current_index, columnName, drop = TRUE]
          if (is.factor(value)) {
            return(as.character(value))  # Convert factor to character
          } else {
            return(value)  # Return directly if not a factor
          }
        } else {
          return(paste("Column", columnName, "not found"))
        }
      } else {
        return("No data available")
      }
    })
  }
  
  # Create text outputs for each field
  output$problem <- renderFormDetail("Demo_Problem")
  output$impact <- renderFormDetail("Demo_Impact")
  output$localAuthority <- renderFormDetail("Demo_Local.Authority")
  output$gender <- renderFormDetail("Demo_Gender")
  output$ageGroup <- renderFormDetail("Demo_Age.Group")
  output$disability <- renderFormDetail("Demo_Disability")
  
  # Observe events on the Previous button
  observeEvent(input$prevButton, {
    data <- reactiveFilteredData()
    current_index <- formIndex()  # Get the current index
    new_index <- max(1, current_index - 1)  # Decrease index, ensure it doesn't go below 1
    formIndex(new_index)  # Update the reactive value
  })
  
  # Observe events on the Next button
  observeEvent(input$nextButton, {
    data <- reactiveFilteredData()
    current_index <- formIndex()  # Get the current index
    max_index <- nrow(data)  # Get the maximum index from the dataset
    new_index <- min(max_index, current_index + 1)  # Increase index, ensure it doesn't exceed max_index
    formIndex(new_index)  # Update the reactive value
  })
  
  
  ########################## Data Preparation for Word Cloud ################################
  clouddata <- reactive({
    withProgress(message = "Preparing data...", value = 0, {
      setProgress(0.2, detail = "Starting data preparation...")
      
      filtered_dataset <- filteredDataText()
      
      Word.df <- filtered_dataset %>%
        select(Demo_Key.Phrase, Demo_Problem, Demo_Impact) %>%
        gather(key = "column", value = "text") %>%
        unnest_tokens(word, text) %>%
        group_by(word) %>%
        summarise(frequency = n(), .groups = "drop") %>%
        filter(!grepl("\\d", word)) %>%
        anti_join(stop_words)
      
      # Lemmatization
      ud_model <- udpipe_download_model(language = "english", model_dir = ".")
      model <- udpipe_load_model(ud_model$file_model)
      
      annotated <- udpipe_annotate(model, x = Word.df$word)
      annotated_df <- as.data.frame(annotated)
      
      Word.df <- Word.df %>%
        mutate(lemma = annotated_df$lemma[match(word, annotated_df$token)]) %>%
        group_by(lemma) %>%
        summarise(frequency = sum(frequency), .groups = "drop") %>%
        arrange(desc(frequency)) %>%
        filter(!is.na(lemma))
      
      ########################## Data Preparation for Word Cloud ################################
clouddata <- reactive({
  withProgress(message = "Preparing data...", value = 0, {
    setProgress(0.2, detail = "Starting data preparation...")
    
    filtered_dataset <- filteredDataText()
    
    Word.df <- filtered_dataset %>%
      select(Demo_Key.Phrase, Demo_Problem, Demo_Impact) %>%
      gather(key = "column", value = "text") %>%
      unnest_tokens(word, text) %>%
      group_by(word) %>%
      summarise(frequency = n(), .groups = "drop") %>%
      filter(!grepl("\\d", word)) %>%
      anti_join(stop_words)
    
    # Lemmatization
    ud_model <- udpipe_download_model(language = "english", model_dir = ".")
    model <- udpipe_load_model(ud_model$file_model)
    
    annotated <- udpipe_annotate(model, x = Word.df$word)
    annotated_df <- as.data.frame(annotated)
    
    Word.df <- Word.df %>%
      mutate(lemma = annotated_df$lemma[match(word, annotated_df$token)]) %>%
      group_by(lemma) %>%
      summarise(frequency = sum(frequency), .groups = "drop") %>%
      arrange(desc(frequency)) %>%
      filter(!is.na(lemma))
    
    excluded_lemmas <- c(
      "council", "tax", "reduction", "received", "application",
      "cts", "claim", "ct", "cl", "client", "financial", "clients",
      "causing", "single", "week", "client's", "paid", "time",
      "situation", "paying", "months", "receiving", "result", "local",
      "partner", "meet", "ongoing", "ca", "company", "told", "receives",
      "top", "amount", "reduce", "approached", "plan", "agents", "scheme",
      "standard", "start", "apply", "suitable", "day", "current", "pay",
      "student", "university", "finance", "benefit", "universal", "credit",
      "stop", "underlying", "payment", "issues", "left", "studying", "cl's",
      "agreement", "september", "advice", "claiming", "study", "school", "live",
      "property", "option","rate", "mean", "impact", "monthly", "month", "element",
      "fe", "include", "lead", "advise", "spend", "issue", "condition", "contribute",
      "manage", "affect", "receipt", "call", "account", "write", "offer", "continue",
      "enter", "people", "set", "provide", "explain", "form",  "receive", "request",
      "significant", "stat","additional", "recently", "check", "deal", "figure", "find",
      "require", "seek", "term", "accrue", "extra", "basic", "complete", "direct", "hour",
      "week", "year", "cause", "hold", "period", "position", "agree", "caseworker", "feel", "past",
      "person", "potential", "potentially", "push", "a", "subject", "understand", "card", "forward",
      "personal", "practice","previously", "remain", "total", "actual", "approx", "citizen", "date",
      "face", "pass", "wellbe", "forc", "adviser", "email", "attempte", "attempted", "floor", "mis",
      "ago", "whilst", "thame", "accept", "await", "aware", "building", "capability", "privately",
      "put", "regularly", "respond", "choose", "considerable", "cornwall", "daily", "directly",
      "experience", "give", "improve", "june", "january", "february", "march", "april", "may", "july",
      "august", "october", "november", "december", "list", "marston", "return", "team", "advisers",
      "carry", "arrange", "authority", "assume", "borough", "assistance", "contribution", "correct",
      "exclud", "expect",  "regular", "simply", "level", "effect", "https", "implication", "lloyd",
      "initial", "mention", "joint", "outcome", "resolve", "statement", "shropshire", "sufficient",
      "basis", "appointment", "basis", "addition", "build", "consider", "progress", "send", "occur",
      "lowell", "private", "solution", "service", "arrangement"
    )# Add more as needed
    
    Word.df <- Word.df %>%
      filter(!lemma %in% excluded_lemmas)
    
    setProgress(0.9, detail = "Finalizing data setup...")
    Word.df
  })
})

########################## Word Cloud Plot Rendering ######################################
efplot <- reactive({
  req(refreshTrigger())  # Ensure dependency on refreshTrigger
  print("efplot triggered")  # Debugging log
  
  withProgress(message = "Generating word cloud...", value = 0.9, {
    setProgress(0.95, detail = "Preparing visualization...")
    
    word_freq <- clouddata()
    plot.words <- head(word_freq, n = 150)
    
    Cab.Colours <- c("#004b88", "#fcbb69", "#9a1d4e", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde", "#d4e5ef")
    word_colors <- sample(Cab.Colours, nrow(plot.words), replace = TRUE)
    
    wordcloud2(plot.words, size = 0.7, shape = "circle", color = word_colors)
  })
})

########################## Render the Word Cloud ##########################################
output$wordcloudOutput <- renderUI({
  print("Rendering word cloud")  # Debugging log
  efplot()  # Call efplot to render the word cloud
})

########################## Refresh Mechanism for Word Cloud ###############################
refreshTrigger <- reactiveVal(0)

observeEvent(input$refreshButton, {
  refreshTrigger(refreshTrigger() + 1)
  print(paste("Refresh triggered:", refreshTrigger()))  # Debugging log
})

########################## Download Word Cloud Data #######################################
output$downloadclouddata <- downloadHandler(
  filename = function() {
    paste0("wordlist_", Sys.Date(), ".csv")
  },
  content = function(file) {
    write.csv(clouddata(), file, row.names = FALSE)  # Save the clouddata as CSV
  }
)

      Word.df <- Word.df %>%
        filter(!lemma %in% excluded_lemmas)
      
      setProgress(0.9, detail = "Finalizing data setup...")
      Word.df
    })
  })
  
  ########################## Word Cloud Plot Rendering ######################################
  efplot <- reactive({
    req(refreshTrigger())  # Ensure dependency on refreshTrigger
    print("efplot triggered")  # Debugging log
    
    withProgress(message = "Generating word cloud...", value = 0.9, {
      setProgress(0.95, detail = "Preparing visualization...")
      
      word_freq <- clouddata()
      plot.words <- head(word_freq, n = 150)
      
      Cab.Colours <- c("#004b88", "#fcbb69", "#9a1d4e", "#57486b", "#a6d6ae", "#006278", "#fcceba", "#005743", "#c2bdde", "#d4e5ef")
      word_colors <- sample(Cab.Colours, nrow(plot.words), replace = TRUE)
      
      wordcloud2(plot.words, size = 0.7, shape = "circle", color = word_colors)
    })
  })
  
  ########################## Render the Word Cloud ##########################################
  output$wordcloudOutput <- renderUI({
    print("Rendering word cloud")  # Debugging log
    efplot()  # Call efplot to render the word cloud
  })
  
  ########################## Refresh Mechanism for Word Cloud ###############################
  refreshTrigger <- reactiveVal(0)
  
  observeEvent(input$refreshButton, {
    refreshTrigger(refreshTrigger() + 1)
    print(paste("Refresh triggered:", refreshTrigger()))  # Debugging log
  })
  
  ########################## Download Word Cloud Data #######################################
  output$downloadclouddata <- downloadHandler(
    filename = function() {
      paste0("wordlist_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(clouddata(), file, row.names = FALSE)  # Save the clouddata as CSV
    }
  )
  
  
  
  
  ######################### ITS A WRAP ######################################################
  }
