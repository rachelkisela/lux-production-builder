library(shiny)


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
    
    fileInput("file1", "Upload CSV File",
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
  
  #TODO - does the algorithm go here?
  
  
  
  
  output$downloadData <- downloadHandler(
    #TODO - save 3 Excel files in a ZIP folder
    filename = "productions.zip",
    content = zip(file, prod_df_list)
  )
  

}

shinyApp(ui, server)

