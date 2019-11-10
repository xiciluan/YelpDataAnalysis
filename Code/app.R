library(shiny)
library(gsheet)
library(leaflet)

google = gsheet2tbl('https://docs.google.com/spreadsheets/d/1Kb7eFlw5oGGQogrWLUoe2iJvAJOJYMDubygoAnDFVRU/edit#gid=0')

ui <- fluidPage(
    
    titlePanel("Suggestions for Fast Food Restuarants"),
    
    sidebarLayout(
        
        sidebarPanel(
            
            
            
            # numericInput("id","please enter the id of restuarants",value = 1,min =3, max = 1),
            
            selectInput('id', 'Please enter the ids of restuarants.', c("all",google$id), multiple=TRUE, selectize=TRUE),
            
            actionButton("suggestion", "Get suggestions!")
            
            # ,
            
            # helpText("If there is no suggestions for the restuarants you want to search 
            
            #          or you have some other suggestions for the restuarants given, 
            
            #          feel free to give your suggestions!"),
            
            # actionButton("other","Give suggestions for other restuarants.")
            
        ),
        
        
        
        mainPanel(
            
            
            
            tableOutput("table"),
            leafletOutput("map")
            
            
            
        )
        
        
        
    )
    
)





server <- function(input, output, session) {
    
    # id = reactive({
    
    #     id = input$id
    
    #     return(id)
    
    # })
    
    id = eventReactive(input$suggestion,{
        return(input$id)
    })
    
    
    
    output$table <-renderTable({
        if(length(which(id()=="all"))>0){
            google$id =  as.character(google$id)
            google
        }else if(length(which(id()==google$id))==0){
                print("Sorry, we do not have data of this fast food restuarant.")
        }else{
            google$id =  as.character(google$id)
            print(google[google$id %in% id(),])
        }
    })

    output$map <- renderLeaflet({
        if("all" %in% id()){
            selected = google
        }else{
            selected = google[google$id %in% id(),]           
        }
        if(dim(selected)[1]!=0){
            (fast_map <- leaflet() %>% 
                addTiles()%>% 
                addMarkers(selected$lng, selected$lat, 
                           popup = paste0(
                                   "<div>",
                                   "<h3>",
                                   selected$id,
                                   "</h3>",
                                   "Star: ", selected$star,
                                   "<br>",
                                   "Suggestions: ",selected$proc,
                                   "</div>"
                               )
                           )
            )
        }
    })
    
    
}





shinyApp(ui = ui, server = server)