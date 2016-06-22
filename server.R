# server.R
library(shiny)
shinyServer(function(input, output) {
  output$info <- reactiveText(function(){
    file <- input$file
    if(is.null(file)) {
      "no data"
    } else {
      name <- paste("File Name: ", iconv(file$name, from="latin1", to="UTF-8"))
      size <- paste("File Size: ", file$size, "B")
      type <- paste("File Type: ", file$type)
      paste(name, size, type, sep="\n")
    }
  })
  output$file <- reactiveText(function() {
    file <- input$file
    if(is.null(file)) {
      "no data"
    } else {
      filepath <- file$datapath
      contents <- scan(file=filepath, what="character", sep="\n", encoding="UTF-8")
      paste(contents, collapse="\n")
    }
  })
})
