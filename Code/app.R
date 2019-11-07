#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

mydata <- data.frame(y=c(1,2,3),x=c(2,3,4))

# Define UI for application that draws a histogram
ui <-   fluidPage(
    titlePanel("0~3ËêÓ¤¶ù/¸¸Ä¸-Ó¤Ó×¶ùÆôÃÉ½ÌÓý"),
    downloadButton('downloadData', 'Download'),
    
    fluidRow(
        DT::dataTableOutput("table")
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$table <- DT::renderDataTable(DT::datatable({
        myData
    }, rownames = FALSE))
    
    output$downloadData <- downloadHandler(
        filename = 'file.csv',
        content = function(file) {
            write.csv(table, file)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)


