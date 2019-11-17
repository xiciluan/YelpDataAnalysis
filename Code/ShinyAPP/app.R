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
            # downloadButton("download", "Download"),
            helpText("If you have any problem, please contact us! Our email: hpan55@wisc.edu")
            
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel("Suggestions",tableOutput("table")),
                tabPanel("Location",leafletOutput("map"),textOutput("lat_lng")),
                tabPanel("Ratings",textOutput("plot_text"),plotOutput("stars"))

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
        
        sug_pros = function(line){
            sug_bp =""
            if(!is.na(line$BikeParking)){
                sug_bp = ifelse(line$BusinessParking,"Great to have business parking!<br>","")
            }
            sug_rd = ""
            if(!is.na(line$RestaurantsDelivery)){
                sug_rd = ifelse(line$RestaurantsDelivery,"Your delievery service improve your ratings a lot!<br>","")
            }
            sug_os = ""
            if(!is.na(line$OutdoorSeating)){
                sug_os = ifelse( line$OutdoorSeating,"Customers will enjoy your outdoor seating!<br>","")
            }
            sug_rto = ""
            if(!is.na(line$RestaurantsTakeOut)){
                sug_rto = ifelse( line$RestaurantsTakeOut,"Good to have a takeout service!<br>","")
            }
            sug_wifi = ""
            if(!is.na(line$WiFi)){
                sug_wifi = ifelse(line$WiFi=="free","Customers are happy with your free WiFi. Keep it!<br>","")
            }
            sug_bip = ""
            if(!is.na(line$BikeParking)){
                sug_bip = ifelse(line$BikeParking,"Bikers are glad that you have place bike parking place! <br>","")
            }
            sug_gfg = ""
            if(!is.na(line$RestaurantsGoodForGroups)){
                sug_gfg = ifelse(line$RestaurantsGoodForGroups,"You can serve groups of people, great!<br>","")
            }
            sug_time = ifelse(line$att_time<0.0002,"WOW! Great to find that you can serve food FAST!<br>","")
            sug_service = ifelse(line$att_service<0.0002,"Congrats! Your service is nearly perfect!<br>","")
            sug_food = ifelse(line$att_salty<0.0002 & line$att_oily<0.0002 & line$att_overcook<0.0002 & line$att_undercook<0.0002 &
                                line$att_bland<0.0002 &line$att_vinegary<0.0002,"WOW! Seldom do people blame your food!<br>","")
            # sug_restroom = ifelse(line$att_restroom<0.0002,"You have really clean restroom! Keep it!<br>","")
            # sug_floor = ifelse(line$att_floor<0.0002,"It's nice to find that your floor is always clean! <br>","")
            # sug_napkin = ifelse(line$att_napkin<0.0002,"It's great to have napkin.<br>","")
            sug_envir = ifelse(line$att_envir<0.0002,"People are having great experience given your nice environment!","")
            
            return(str_remove_all(paste(sug_bp,sug_rd,sug_os,sug_rto,sug_wifi,sug_bip,sug_gfg,sug_time,sug_service,sug_food,
                         sug_envir),"NA"))
        }
        
        
        sug_cons = function(line){
          sug_bp = ""
          if(!is.na(line$BusinessParking)){
            sug_bp = ifelse(line$BusinessParking,"","More people will come if you can solve the parking problem!<br>")
          }
          sug_rd = ""
          if(!is.na(line$RestaurantsDelivery)){
            sug_rd = ifelse(line$RestaurantsDelivery,"","Your rating will improve a lot if you have the delivery service!<br>")
          }
          # sug_os = ""
          # if(!is.na(line$OutdoorSeating)){
          #  sug_os = ifelse(line$OutdoorSeating,"","You can place some outdoor seats.<br>")
          # }  
          sug_rto = ""
          if(!is.na(line$RestaurantsTakeOut)){
            sug_rto = ifelse(line$RestaurantsTakeOut,"","You may need to start a takeout service.<br>")
          }
          sug_wifi = ""
          if(!is.na(line$WiFi)){
            sug_wifi = ifelse(line$WiFi=="free","","Can you solve many people's concern for no free WIFI? <br>")
          }
          #sug_bip = ""
          # if(!is.na(line$BikeParking)){
          #   sug_bip = ifelse(line$BikeParking,"","You can build a bike parking.<br>")
          # }
          #sug_gfg = ""
          #if(!is.na(line$RestaurantsGoodForGroups)){
          #  sug_gfg = ifelse(line$RestaurantsGoodForGroups,"","You can build an atmosphere that is good for groups.<br>")
          #}
          
          sug_time = ""
          if(line$att_time>=0.0005){
            sug_time = "Too many complaints about waiting time!<br>"
          }else if(line$att_time>=0.0002){
            sug_time = "Some people are complaining about waiting too long.<br>"
          }
          sug_service = ""
          if(line$att_service>=0.0005){
            sug_service = "Urgency! Plz improve your service ASAP!<br>"
          }else if(line$att_service>=0.0002){
            sug_service = "Some guests are complaineing about your resturant's service.<br>"
          }
          
          sug_salty = ""
          if(line$att_salty>=0.0005){
            sug_salty = "Too many complaints that your food is too salty!<br>"
          }else if(line$att_salty>=0.0002){
            sug_salty = "It would be better if you make your food less salty.<br>"
          }
          sug_oily = ""
          if(line$att_oily>=0.0005){
            sug_oily = "Too many complaints that your food is too oily!<br>"
          }else if(line$att_oily>=0.0002){
            sug_oily = "It would be better if you make your food less oily.<br>"
          }
          sug_overcook = ""
          if(line$att_overcook>=0.0005){
            sug_overcook = "Too many complaints that your food is over-cooked!<br>"
          }else if(line$att_overcook>=0.0002){
            sug_overcook = "Some guests are complaining that your food is over-cooked.<br>"
          }
          sug_undercook = ""
          if(line$att_undercook>=0.0005){
            sug_undercook = "Too many complaints that your food is under-cooked!<br>"
          }else if(line$att_undercook>=0.0002){
            sug_undercook = "Some guests are complaining that your food is under-cooked.<br>"
          }
          sug_bland = ""
          if(line$att_bland>=0.0005){
            sug_bland = "Too many complaints that your food is bland!<br>"
          }else if(line$att_undercook>=0.0002){
            sug_bland = "Some guests are complaining that your food is bland.<br>"
          }
          sug_vinegary = ""
          if(line$att_vinegary>=0.0005){
            sug_vinegary = "Too many complaints that your food is vinegary!<br>"
          }else if(line$att_vinegary>=0.0002){
            sug_vinegary = "Some guests are complaining that your food is vinegary.<br>"
          }
          sug_restroom = ""
          if(line$att_restroom>=0.0005){
            sug_restroom = "Too many people are complaining the cleaniness of your restroom!<br>"
          }else if(line$att_restroom>=0.0002){
            sug_restroom = "It would be better if you keep your restroom cleaner.<br>"
          }
          sug_floor= ""
          if(line$att_floor>=0.0005){
            sug_floor = "Please pay a lot attention to cleaning your floor!<br>"
          }else if(line$att_floor>=0.0002){
            sug_floor = "Please pay more attention to cleaning your floor.<br>"
          }
          sug_napkin= ""
          if(line$att_napkin>=0.0005){
            sug_napkin = "Please remember to offer some napkin.<br>"
          }else if(line$att_napkin>=0.0002){
            sug_napkin = "Some guests complained about your napkin.<br>"
          }
          sug_envir= ""
          if(line$att_envir>=0.0005){
            sug_envir = "Your need to improve your overall environment ASAP!"
          }else if(line$att_envir>=0.0002){
            sug_envir = "It would be better if you try to improve your dinning environment."
          }
          
          
          return(str_remove_all(paste(sug_bp,sug_rd,sug_rto,sug_wifi,sug_time,sug_service,ifelse(max(line$att_overcook,line$att_undercook)==line$att_overcook,sug_salty,sug_bland),sug_oily,
                       ifelse(max(line$att_overcook,line$att_undercook)==line$att_overcook,sug_overcook,sug_undercook),sug_vinegary,sug_restroom,sug_floor,sug_napkin,sug_envir),"NA"))
        }


        # suggest$suggestions = paste("Please supply your business parking information on yelp webside.",
        #                             "Please supply your delivery information on yelp webside.",
        #                             "Please supply your outdoor-seating information on yelp webside.",
        #                             "Please supply your take-out information on yelp webside.",
        #                             "Please supply your Wifi information on yelp webside.",
        #                             "Please supply your bike parking information on yelp webside.",
        #                             "Please supply your GoodForGroups information on yelp webside.",
        #                             sep = "<br>")
        suggest$pros = ""
        suggest$cons = ""
        
        n = nrow(suggest)
        for(i in c(1:n)){
            suggest$pros[i] = sug_pros(selected[i,])
        }
        for(i in c(1:n)){
          suggest$cons[i] = sug_cons(selected[i,])
        }
        return(suggest)
    })
    
    # output$download <- downloadHandler(
    #   filename = function() {
    #     paste("Suggestions", ".csv", sep = "")
    #   },
    #   content = function(file) {
    #     write.csv(suggest(), file, row.names = FALSE)
    #   }
    # )
    
    
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
