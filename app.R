# Source the UI and server files
source("global.R")
source("app_ui.R")
source("app_server.R")

# Launch the app
shinyApp(ui = app_ui(), server = app_server)