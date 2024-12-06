rm(list = ls())

# Libraries galore...
library(shinydashboard)
library(shiny)
library(DT)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(reshape2)
library(janitor)
library(udpipe)
library(zoo)
library(rlang)
library(wordcloud2)
library(glue)
library(stringr)
library(zoo)
library(plotly)
library(fresh)
library(sass)
library(sf)
library(shinyWidgets)
library(writexl)

# Theme for the app
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#333333" # Dark gray for accent colors
  ),
  adminlte_global(
    content_bg = "#FFFFFF", # White background for the main body
    box_bg = "#FFFFFF", # White background for box elements
    info_box_bg = "#FFFFFF" # White background for info boxes
  ),
  adminlte_sidebar(
    dark_bg = "#333333", # Charcoal for sidebar background
    dark_color = "#FFFFFF", # White text for sidebar elements
    dark_hover_bg = "#555555", # Slightly lighter gray for hover effect
    dark_hover_color = "#FFFFFF" # White text for hover state
  )
)

# Load data
EFS <- readRDS(file = "EFS.rds")

# Ok, let's begin....
rm(list = ls())

# Libraries galore...
library(shinydashboard)
library(shiny)
library(DT)
library(tidyverse)
library(tidytext)
library(kableExtra)
library(reshape2)
library(janitor)
library(udpipe)
library(zoo)
library(rlang)
library(glue)
library(stringr)
library(plotly)
library(fresh)
library(sass)
library(sf)
library(shinyWidgets)
library(writexl)

# Theme for the app
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#333333" # Dark gray for accent colors
  ),
  adminlte_global(
    content_bg = "#FFFFFF", # White background for the main body
    box_bg = "#FFFFFF", # White background for box elements
    info_box_bg = "#FFFFFF" # White background for info boxes
  ),
  adminlte_sidebar(
    dark_bg = "#333333", # Charcoal for sidebar background
    dark_color = "#FFFFFF", # White text for sidebar elements
    dark_hover_bg = "#555555", # Slightly lighter gray for hover effect
    dark_hover_color = "#FFFFFF" # White text for hover state
  )
)


# Load data
EFS <- readRDS(file = "EFS.rds")
