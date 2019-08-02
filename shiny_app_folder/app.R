library(shiny)
library(stringr)
source("lux people-placer program.R")
# NOTE 6/3 use showReactLog() in console to show reactive values

ui <- fluidPage(
  
  # App title ----
  column(5, offset = 4, titlePanel("The LUX People-Placer")),
  
  mainPanel(
    
    helpText("This app will intake Google Forms data to create 3 production spreadsheets."),
    helpText("HOW TO USE THIS APP:"),
    helpText("1. Go to the Google Form page for this quarter's productions"),
    helpText("2. Click \"Responses\""),
    helpText("3. Click the square green Sheets icon (hover = \"View reponses in Sheets\") in the upper right side of the screen"),
    helpText("4. Once the Google Sheet loads, make sure there are no gaps in the rows. If there are, drag up rows so there are no blank rows"),
    helpText("5. Click File -> Download as -> Comma-separated values (.csv, current sheet)"),
    helpText("6. Upload that file below:"),
    
    fileInput("googleform", "Upload CSV File",
              multiple = FALSE,
              accept = (".csv")
             ),
    
    # NOTE 8/1: We will use num_productions as a global variable to allow flexibility in
    # future iterations.
    helpText("How many productions this quarter?"), 
    helpText("(NOTE: As of now, this web app only works for 3 productions! This input box is for testing only.)"), 
    numericInput("num_productions", label = NULL, value = 3, min = 1),
    
    helpText("Please enter the production titles below (any order):"),
    # Place to hold dynamic inputs
    uiOutput("inputGroup"),
    
    downloadButton("downloadData", "Download Production Spreadsheets")
    )
)

server <- function(input, output) {
  
  # ** DYNAMIC # OF INPUTS
  observeEvent(input$num_productions, {
    output$inputGroup = renderUI({
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
    if(is.null(input$googleform)) {
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
      write.csv(prodmaker()[[1]], file = fs[1]) #, sep =",")
      write.csv(prodmaker()[[2]], file = fs[2]) #, sep =",")
      write.csv(prodmaker()[[3]], file = fs[3]) #, sep =",")
        
      zip(zipfile = file, files = fs)
      },
      contentType = "application/zip"
  )
}

shinyApp(ui, server)
