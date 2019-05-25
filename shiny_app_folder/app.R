library(shiny)
source("lux people-placer program.R")


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
    
    helpText("Please alphabetically enter the 3 production titles below:"),      
    textInput("prod1title", label = h6("First production title:")),
    textInput("prod2title", label = h6("Second production title:")),
    textInput("prod3title", label = h6("Third production title:")),
    
    downloadButton("downloadData", "Download 3 Production Spreadsheets")
    )
)



server <- function(input, output) {
  
  # these lines require all inputs to be filled - not sure if this is necessary, I saw it online lol
  #req(input$prod1title)
  #req(input$prod2title)
  #req(input$prod3title)
  #req(input$file)
  
  
  prodinput <- reactive({
    # 5/25 i think it is a problem with this fxn. does it return a dataframe? not sure how to test in app.R. Works in
    # its own file
      people_placer(input$prod1title, input$prod2title, input$prod3title, input$googleform)
  })
  
  fakefile <- reactive({
    list1 <- c("1", "2", "3")
    list2 <- c("a", "b", "c")
    testdf <- data.frame(list1, list2)
  })
  
  output$downloadData <- downloadHandler(
    # save 3 Excel files in a ZIP folder
    
    filename = "production1.csv",
    
    content = function(file) {
      write.csv(prodinput(), file, row.names = TRUE)
      #zip(zipfile = "productions.zip", files = fs)
    }#,
   # contentType = "application/zip"
  )
  

}

shinyApp(ui, server)

