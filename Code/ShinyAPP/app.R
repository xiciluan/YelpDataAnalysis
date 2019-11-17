library(shiny)
library(gsheet)
library(leaflet)
# library(shinythemes)
library(ggplot2)
library(sp)
library(rgeos)
library(stringr)



google = gsheet2tbl('https://docs.google.com/spreadsheets/d/1Kb7eFlw5oGGQogrWLUoe2iJvAJOJYMDubygoAnDFVRU/edit#gid=0')

ui <- fluidPage(
    # shinythemes::themeSelector(),
    titlePanel("Suggestions for Fast Food Restaurants(Yelp)"),
    sidebarLayout(
        sidebarPanel(
            selectInput('name', 'Please enter the name of restaurant you wrote on Yelp.', c(google$name), multiple=FALSE, selectize=TRUE),
            selectInput('state','You can enter the state of this restaurant.',choices = "",multiple = FALSE,selectize = TRUE),
            selectInput('city','You can enter the city of this restaurant.',choices = "",multiple = FALSE,selectize = TRUE),
            selectInput('address','You can enter the address of this restaurant.',choices = "",multiple = FALSE,selectize = TRUE),
            actionButton("suggestion", "Get suggestions!"),
            helpText("If you have any problem, please contact us! Our email: hpan55@wisc.edu")
            
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Map",leafletOutput("map"),textOutput("lat_lng")),
                tabPanel("Table",tableOutput("table")),
                tabPanel("Stars",textOutput("plot_text"),plotOutput("stars"))
                
            )
        )
    )
    
)



server <- function(input, output, session) {
    
    observe({
        updateSelectInput(
            session,
            "state",
            choices = c("all",str_sort(unique(c(google[google$name == input$name,]$state))))
        )
    })
    
    observe({
        updateSelectInput(
            session,
            "city",
            # choices = c(unique(google[google$state == input$state,]$city))
            choices = ifelse(input$state=="all",list("all"),list(c("all",str_sort(unique(google[google$name == input$name & google$state == input$state,]$city)))))[[1]]
        )
    })
    
    observe({
        updateSelectInput(
            session,
            "address",
            # choices = c(unique(google[google$city == input$city,]$address))
            choices = ifelse(input$state=="all",list(c("all")),list(c("all",str_sort(unique(google[google$name == input$name & google$state == input$state & google$city == input$city,]$address)))))[[1]]
        )
    })
    
    
    
    selected = eventReactive(input$suggestion,{
        selected = google[google$name %in% input$name,]
        if(!("all" %in% input$state)){
            selected = selected[selected$state %in% input$state,]
            if(!("all" %in% input$city)){
                selected = selected[selected$city %in% input$city,]
                if(!("all" %in% input$address)){
                    selected = selected[selected$address %in% input$address,]
                }
            }
        }
        return(selected)
    })
    

    
    
    
    output$map <- renderLeaflet({
        selected = selected()
        # surround = surround()
        if(dim(selected)[1]!=0 ){
            (fast_map <- leaflet() %>% 
                 addTiles()%>% 
                 addMarkers(selected$longitude, selected$latitude,
                            popup = paste0(
                                "<div>", "<h3>",selected$name,"</h3>",
                                "<b>","Stars: ", selected$stars,"</b>","<br>",
                                "<b>",selected$address,"</b>","<br>",
                                "<b>",selected$city,", ",selected$state,selected$postal_code,"</b>","<br>",
                                "<em>","Business Parking: ","</em>",ifelse(selected$BusinessParking,"yes","no"),"<br>",
                                "<em>","Restaurants Delivery: ","</em>",ifelse(selected$RestaurantsDelivery,"yes","no"),"<br>",
                                "<em>","Restaurants Reservations: ","</em>",ifelse(selected$RestaurantsReservations,"yes","no"),"<br>",
                                "<em>","Outdoor Seating: ","</em>",ifelse(selected$OutdoorSeating,"yes","no"),"<br>",
                                "<em>","Noise Level: ","</em>",ifelse(selected$NoiseLevel,"yes","no"),"<br>",
                                "<em>","Restaurants Take Out: ","</em>",ifelse(selected$RestaurantsTakeOut,"yes","no"),"<br>",
                                "<em>","Restaurants Price Range: ","</em>",selected$RestaurantsPriceRange2,"<br>",
                                "<em>","WiFi: ","</em>",selected$WiFi,"<br>",
                                "<em>","Bike Parking: ","</em>",ifelse(selected$BikeParking,"yes","no"),"<br>",
                                "<em>","Restaurants Good For Groups: ","</em>",ifelse(selected$RestaurantsGoodForGroups,"yes","no"),"<br>",
                                "</div>"
                            ) )

            )
        }
    })
    
    output$lat_lng <- renderText({
        selected = selected()
        num_na = max(sum(is.na(selected$latitude)),sum(is.na(selected$longitude)))
        if(num_na != 0){
            paste0("There is ",num_na," NA(s) in position data.")
        }
        if(dim(selected)[1]==0){
            print("Sorry, we do not have position data of this fast food restaurant.")
        }
    })
    
    suggest = eventReactive(input$suggestion,{
        selected = selected()
        suggest = selected[,c(2)]
        suggest$location = paste(selected$address,selected$city,paste0(selected$state,", ",selected$postal_code),sep = "<br>")
        sug = function(line){
            sug_bp = "Please supply your business parking information on yelp webside."
            if(!is.na(line$BikeParking)){
                sug_bp = ifelse(line$BusinessParking,"Good to have a business parking. Keep it!","You can build a business parking.")
            }
            sug_rd = "Please supply your delivery information on yelp webside."
            if(!is.na(line$RestaurantsDelivery)){
                sug_rd = ifelse(line$RestaurantsDelivery,"Good to have a delivery system. Keep it!","You can build a delivery system.")
            }
            sug_os = "Please supply your outdoor-seating information on yelp webside."
            if(!is.na(line$OutdoorSeating)){
                sug_os = ifelse(line$OutdoorSeating,"Good to have outdoor seats. Keep it!","You can place some outdoor seats.")
            }
            sug_rto = "Please supply your take-out information on yelp webside."
            if(!is.na(line$RestaurantsTakeOut)){
                sug_rto = ifelse(line$RestaurantsTakeOut,"Good to have a takeout service. Keep it!","You may start a takeout service.")
            }
            sug_wifi = "Please supply your Wifi information on yelp webside."
            if(!is.na(line$WiFi)){
                sug_wifi = ifelse(line$WiFi=="free","Good to have free WiFi. Keep it!","You can offer free wifi.")
            }
            sug_bip = "Please supply your bike parking information on yelp webside."
            if(!is.na(line$BikeParking)){
                sug_bip = ifelse(line$BikeParking,"Good to have a bike parking. Keep it!","You can build a bike parking.")
            }
            sug_gfg = "Please supply your GoodForGroups information on yelp webside."
            if(!is.na(line$RestaurantsGoodForGroups)){
                sug_gfg = ifelse(line$RestaurantsGoodForGroups,"Your restaurant is good for groups. Keep it!","You can build an atmosphere that is good for groups.")
            }
            return(paste(sug_bp,sug_rd,sug_os,sug_rto,sug_wifi,sug_bip,sug_gfg,sep = "<br>"))
        }
        
        
        suggest$suggestions = paste("Please supply your business parking information on yelp webside.",
                                    "Please supply your delivery information on yelp webside.",
                                    "Please supply your outdoor-seating information on yelp webside.",
                                    "Please supply your take-out information on yelp webside.",
                                    "Please supply your Wifi information on yelp webside.",
                                    "Please supply your bike parking information on yelp webside.",
                                    "Please supply your GoodForGroups information on yelp webside.",
                                    sep = "<br>")
        n = nrow(suggest)
        for(i in c(1:n)){
            suggest$suggestions[i] = sug(selected[i,])
        }
        return(suggest)
    })

    
    
    
    output$table <-renderTable({
        suggest = suggest()
        if(dim(selected())[1]>0){
            print(suggest)
        }else{
            print("Sorry, we do not have data of this fast food restaurant.")
        }
    },bordered = TRUE, sanitize.text.function=identity)
    
    output$plot_text <- renderText({
        selected = selected()
        n = nrow(selected)
        if(n==0){
            print("Sorry, we do not have data of this fast food restaurant.")
        }else if(n>1){
            print("Please choose only one restaurant for its stars plot.")
        }
    })
    
    output$stars <- renderPlot({
        selected = selected()
        n = nrow(selected)
        if(n==1){
            for(i in c(1:n)){
                line = selected[i,]
                line_star=data.frame(stars = c(1,2,3,4,5),num_reviews = c(line$star1,line$star2,line$star3,line$star4,line$star5))
                plot_star<-ggplot(data=line_star, aes(x=stars, y=num_reviews,fill = stars)) +
                    geom_bar(stat="identity") +
                    # ylab("number of reviews") +
                    labs(title = paste0("Stars Distribution of ",line$name),
                         subtitle = paste(line$address,line$city,line$state,line$postal_code,sep = ", "),
                         x = "stars", y = "number of reviews",
                         caption = paste0("The average stars is ",line$stars,".")) 
                plot(plot_star)
            }
        }
    })
    
    
}


shinyApp(ui = ui, server = server)

