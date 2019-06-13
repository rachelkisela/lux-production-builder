library(shiny)
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
    
    helpText("Please alphabetically enter the 3 production titles below:"),      
    textInput("prod1title", label = h6("First production title:")),
    textInput("prod2title", label = h6("Second production title:")),
    textInput("prod3title", label = h6("Third production title:")),
    
    downloadButton("downloadData", "Download 3 Production Spreadsheets")
    )
)



server <- function(input, output) {
  
  prodinput <- reactive({
    # * 5/25 i think it is a problem with this fxn. does it return a dataframe? not sure how to test in app.R. Works in
    # its own file
    # * 6/3 according to showReactLog(), this fxn is running multiple times with no new input values so it returns a blank
    # file. I believe that is why "content" can't recognize file as a character string, because prodinput() is skipped
    # over b/c it is empty. MUST FIGURE OUT - WHY is it running multiple times? use isolate() to freeze input$ values?
    
    # * 6/12 - tried the following lines of code, didnt work, commented out
    # req(input$prod1title, input$prod2title, input$prod3title, input$googleform)
    # xprod1title <- get(isolate(input$prod1title))
    # xprod2title <- get(isolate(input$prod2title))
    # xprod3title <- get(isolate(input$prod3title))
    # xgoogleform <- get(isolate(input$googleform))
    # people_placer(xprod1title, xprod2title, xprod3title, xgoogleform)
    # ** BRILLIANT IDEA - we store the prod1titles as strings so we no longer deal with "input$" and it essentially
    #   functions as fakefile() - or is it an issue with "googleform"?
    
    people_placer(input$prod1title, input$prod2title, input$prod3title, input$googleform)
  })
  
  fakefile <- reactive({
    list1 <- c("1", "2", "3")
    list2 <- c("a", "b", "c")
    testdf <- data.frame(list1, list2)
  })
  
  output$downloadData <- downloadHandler(
    # future: save 3 Excel files in a ZIP folder
    filename = "production1.csv",
    
    content = function(file) {
      write.csv(prodinput(), file, row.names = TRUE)
      #zip(zipfile = "productions.zip", files = fs)
    }#
   # contentType = "application/zip"
  )
  

}

shinyApp(ui, server)

