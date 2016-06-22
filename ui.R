# ui.R
library(shiny)
shinyUI(pageWithSidebar(
  headerPanel(title="Input File Test"),
  sidebarPanel(
    fileInput("file", label="Input File:")
  ),
  mainPanel(
    h4("File Information:"),
    verbatimTextOutput("info"),
    h4("Contents"),
    verbatimTextOutput("file")
  )
))
