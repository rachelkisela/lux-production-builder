library(shiny)
library(stringr)
library(shinyWidgets)
source("lux_production_builder_program.R")
# NOTE 6/3 use showReactLog() in console to show reactive values

ui <- fluidPage(
  setBackgroundColor(color = "#fffbf0"),
  
  # Logo ----
  img(src = "luxlogo.png", height = "10%", width = "10%", 
      style = "display: block; margin-left: auto; margin-right: auto;"), # This line centers the logo
  # App title ----
  column(5, offset = 4, titlePanel("The LUX Production Builder")),
  #tags$style(HTML("The LUX Production Builder")), # LEFT OFF HERE 8/2 4:21PM
  
  
  mainPanel(
    
    HTML("<font size=+1>This app will intake Google Forms data to create downloadable production spreadsheets.<br><br>
         HOW TO USE THIS APP:</font><br>
         <b>1.</b> Go to the Google Form page for this quarter's productions<br>
         <b>2.</b> Click \"Responses\"<br>
         <b>3.</b> Click the square green Sheets icon (hover = \"View reponses in Sheets\")
                   in the upper right side of the screen<br>
         <b>4.</b> Once the Google Sheet loads, make sure there are no gaps in the rows.
                   If there are, drag up rows so there are no blank rows<br>
         <b>5.</b> Click File -> Download as -> Comma-separated values (.csv, current sheet)<br>
         <b>6.</b> Upload that file below:<br><br>"),
    
    fileInput("googleform", "Upload CSV File",
              multiple = FALSE,
              accept = (".csv")
             ),
    
    # NOTE 8/1: We will use num_productions as a global variable to allow flexibility in
    # future iterations.
    HTML("How many productions this quarter?<br>
         <i>(NOTE: As of now, this web app only works for 3 productions!
         This input box is for testing only.)</i>"), 
    numericInput("num_productions", label = NULL, value = 3, min = 1),
    HTML("<br>"),
    
    HTML("Enter the production titles below (any order):"),
    # Place to hold dynamic inputs
    uiOutput("inputGroup"),
    
    HTML("<br>"),
    downloadButton("downloadData", "Download Production Spreadsheets"),
    
    HTML("<br><br><br><br>")
    )
)

server <- function(input, output) {
  
  # ** DYNAMIC # OF INPUTS
  observeEvent(input$num_productions, {
    output$inputGroup <- renderUI({
      input_list <- lapply(1:input$num_productions, function(i) {
        # for each dynamically generated input, give a different name
        inputName <- paste("prod", i, "title", sep = "")
        textInput(inputName, label = h5("Production title:"))
    })
    do.call(tagList, input_list)
  })
  })
  # ** DYNAMIC # OF INPUTS
  
  prodmaker <- reactive({
    if (is.null(input$googleform)) {
      return(NULL)
    }
    filestr <- input$googleform
    googleformfile <- read.csv(filestr$datapath, stringsAsFactors = FALSE)
    people_placer(input$prod1title, input$prod2title, input$prod3title, googleformfile, input$num_productions)
  })

  
  output$downloadData <- downloadHandler(
    
    filename = "productions.zip",
    
    content = function(file) {
      # write all CSV files, and attach underscored file names to them.
      # note: prodmaker() returns prod. dataframes (1-3) and underscored filenames (4-6)
      fs <- c(prodmaker()[[4]], prodmaker()[[5]], prodmaker()[[6]])
      write.csv(prodmaker()[[1]], file = fs[1])
      write.csv(prodmaker()[[2]], file = fs[2])
      write.csv(prodmaker()[[3]], file = fs[3])
        
      zip(zipfile = file, files = fs)
      },
      contentType = "application/zip"
  )
}

shinyApp(ui, server)
