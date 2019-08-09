library(shiny)
library(stringr)
library(shinyWidgets)
source("lux_production_builder_program.R")
# NOTE 6/3 use showReactLog() in console to show reactive values

ui <- fluidPage(
  setBackgroundColor(color = "#fffbf0"),
  tags$head(tags$style(HTML('* {font-family: Rubik Light;}'))),
  
  # Logo ----
  img(src = "luxlogo.png", height = "10%", width = "10%", 
      style = "display: block; margin-left: auto; margin-right: auto;"), # This line centers the logo
  # App title ----
  column(5, offset = 4, titlePanel("The LUX Production Builder")),
  
  
  mainPanel(
    
    HTML("<font size=+1>This app will intake Google Forms data to create downloadable production spreadsheets.<br><br>
         HOW TO USE THIS APP:</font><br>
         <b>1.</b> Go to the Google Form page for this quarter's productions.<br>
         <b>2.</b> Click \"Responses\".<br>
         <b>3.</b> Click the square green Sheets icon in the upper right side of the screen
                   and create a spreadsheet.<br>
         <b>4.</b> Once the Google Sheet loads, make sure there are no gaps in the rows.
                   If there are, drag up rows so there are no blank rows.<br>
         <b>5.</b> Click File -> Download as -> Comma-separated values (.csv, current sheet)<br>
         <b>6.</b> Upload that file below:<br><br>"),
    
    fileInput("googleform", "Upload CSV File",
              multiple = FALSE,
              accept = (".csv")
             ),
    
    HTML("Enter the quarter and year:"),
    selectInput("quarter", "Quarter:",
                c("Autumn" = "AU",
                  "Winter" = "WI",
                  "Spring" = "SP",
                  "Summer" = "SU")),
    numericInput("year", "Year:", 19, min = 00, max = 99),
    HTML("<br>"),
    
    
    # num_productions allows us to vary the number of productions depending on the quarter
    HTML("How many productions this quarter?"),
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
  
  # "prodmaker()" calls the algorithm file
  prodmaker <- reactive({
    
    # * these lines save the google form to a variable before using it in the algorithm
    if (is.null(input$googleform)) {
      return(NULL)
    }
    filestr <- input$googleform
    googleformfile <- read.csv(filestr$datapath, stringsAsFactors = FALSE)
    # *
    
    # ** this creates a list of production titles that varies in length
    #    depending on the number of productions this quarter.
    production_titles <- sapply(1:input$num_productions, function(i) {input[[paste0("prod", i, "title")]]})

    people_placer(production_titles, googleformfile, input$num_productions)
  })

  
  output$downloadData <- downloadHandler(
    
    filename = function(){
      paste0(input$quarter, input$year, "_LUX_productions.zip")
    },
    
    content = function(file) {
      # write all CSV files, and attach underscored file names to them.
      # note: prodmaker() returns prod. dataframes (if num_productions = 3, 1 to 3)
      #       and underscored filenames (4 to 6)
      fs <- c()
      for (i in 1:(input$num_productions)) { #8/8 THIS WOULD BE +1 TO INCL. MASTER DF
        fs <- c(fs, paste0(input$quarter, input$year, "_", prodmaker()[[input$num_productions + i]])) #8/8 THIS WOULD BE +1 TO INCL. MASTER DF
        write.csv(prodmaker()[[i]], file = fs[i])
      }
      zip(zipfile = file, files = fs)
      },
      contentType = "application/zip"
  )
}

shinyApp(ui, server)
