

trains <- read.csv("full_trains.csv", stringsAsFactors = FALSE)
airports <- read.csv("airports.csv", stringsAsFactors = FALSE, nrows = 1000)
flights <- read.csv("flights-sample.csv", stringsAsFactors = FALSE)
airlines <- read.csv("airlines.csv", stringsAsFactors = FALSE, nrows = 1000)

flights$date <- as.Date(with(flights, paste(YEAR, MONTH, DAY,sep="-")), "%Y-%m-%d")

t = select(.data = flights, origin = "ORIGIN_AIRPORT", dest = "DESTINATION_AIRPORT")
airports_new = select(.data = airports, airport = "IATA_CODE", lat = "LATITUDE", lon = "LONGITUDE")
airlines = select(.data = airlines, code = "IATA_CODE", airline = "AIRLINE")
flights_latlon <- t %>% inner_join(select(.data = airports_new, origin = airport, origin_lat = lat, origin_lon = lon), 
                                   by = "origin") %>% 
  inner_join(select(.data = airports_new, dest = airport, dest_lat = lat, dest_lon = lon), 
             by = "dest")
flights$AIRLINES <- airlines$airline[match(flights$AIRLINE, airlines$code)]
flights$city <- airports$CITY[match(flights$DESTINATION_AIRPORT, airports$IATA_CODE)]

flightss <- flights
flightss$ARRIVAL_TIME <- format(strptime(sprintf('%04d', flightss$ARRIVAL_TIME), format='%H%M'), '%H:%M')
flightss$DEPARTURE_TIME <- format(strptime(sprintf('%04d', flightss$DEPARTURE_TIME), format='%H%M'), '%H:%M')
flightss$ARRIVAL_TIME <- as.POSIXlt(as.character(flightss$ARRIVAL_TIME), format = "%H:%M")
flightss$DEPARTURE_TIME <- as.POSIXlt(as.character(flightss$DEPARTURE_TIME), format = "%H:%M")

flights_latlon <- head(flights_latlon,100)

trains_without_NA <- trains %>% drop_na()


server <- function(input, output) {
  
  output$nb_trains <- renderValueBox({
    data <- sum(trains_filter()$total_num_trips)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Number of trains", icon("train"))
    #print(as.character(tot_filter()[tot_filter()$year == input$yearInput,]["tot_trains"]))
  })
  
  output$nb_delay_depart <- renderValueBox({
    data <- sum(trains_filter()[!is.na(trains_filter()$num_late_at_departure),]$num_late_at_departure)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Number of delayed trains at departure", icon("clock"))
    #print(as.character(tot_filter()[tot_filter()$year == input$yearInput,]["tot_delay_depart"]))
  })
  
  output$nb_delay_arrival <- renderValueBox({
    data <- sum(trains_filter()[!is.na(trains_filter()$num_arriving_late),]$num_arriving_late)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Number of delayed trains at arrival", icon("clock"))
    #print(as.character(tot_filter()[tot_filter()$year == input$yearInput,]["tot_delay_arrival"]))
  })
  
  output$avg_delay_arrival <- renderValueBox({
    data <- mean(trains_filter()[!is.na(trains_filter()$num_arriving_late),]$num_arriving_late)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average number of delayed trains at arrival", icon("clock"))
    #print(as.character(avg_filter()[avg_filter()$year == input$yearInput,]["avg_delay_arrival"]))
  })
  
  output$avg_delay_depart <- renderValueBox({
    data <- mean(trains_filter()[!is.na(trains_filter()$num_late_at_departure),]$num_late_at_departure)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average number of delayed trains at departure", icon("clock"))
    #print(as.character(avg_filter()[avg_filter()$year == input$yearInput,]["avg_delay_depart"]))
  })
  
  output$avg_delay_all_depart <- renderValueBox({
    data <- mean(trains_filter()[!is.na(trains_filter()$avg_delay_all_departing),]$avg_delay_all_departing)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average departure delay time of all trains", icon("clock"))
    #print(as.character(tot_filter()[tot_filter()$year == input$yearInput,]["avg_delay_all_depart"]))
  })
  
  output$avg_delay_all_arriving <- renderValueBox({
    data <- mean(trains_filter()[!is.na(trains_filter()$avg_delay_all_arriving),]$avg_delay_all_arriving)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average arrival delay time of all trains", icon("clock"))
    #print(as.character(tot_filter()[tot_filter()$year == input$yearInput,]["avg_delay_all_arriving"]))
  })
  
  output$avg_delay_late_depart <- renderValueBox({
    data <- mean(trains_filter()[!is.na(trains_filter()$avg_delay_late_at_departure),]$avg_delay_late_at_departure)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average departure delay time of delayed trains", icon("clock"))
    #print(as.character(avg_filter()[avg_filter()$year == input$yearInput,]["avg_delay_late_depart"]))
  })
  
  output$avg_delay_late_arrival <- renderValueBox({
    data <- mean(trains_filter()[!is.na(trains_filter()$avg_delay_late_on_arrival),]$avg_delay_late_on_arrival)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average arrival delay time of delayed trains", icon("clock"))
    #print(as.character(avg_filter()[avg_filter()$year == input$yearInput,]["avg_delay_late_arrival"]))
  })
  
  output$tot_num_cancel_trains <- renderValueBox({
    data <- sum(trains_filter()[!is.na(trains_filter()$num_of_canceled_trains),]$num_of_canceled_trains)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Number of cancelled trains", icon("window-close"))
    #print(as.character(tot_filter()[tot_filter()$year == input$yearInput,]["nb_cancel_trains"]))
  })
  
  output$perc_cancel_trains <- renderValueBox({
    data <- sum(trains_filter()[!is.na(trains_filter()$num_of_canceled_trains),]$num_of_canceled_trains)
    data <- data / sum(trains_filter()$total_num_trips)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data,3)
    data %>%
      valueBox(subtitle = "Proportion of cancelled trains", icon("percentage"))
    #print(as.character(round((tot_filter()[tot_filter()$year==input$yearInput,]["nb_cancel_trains"]/tot_filter()[tot_filter()$year==2015,]["tot_trains"])*100,2)))
  })
  
  output$list <- renderUI({
    selectInput("trainsInput", "Choose :",
                sort(unique(trains[,input$choice_t]))
    )
  })
  
  output$year <- renderUI({
    selectInput("choice_t", "Aggregate on",
                choices = list("year","departure_station"))
  })
  
  trains_filter <- reactive({
    if(input$choice_t == "year")
      trains <- subset(trains, year == input$trainsInput)
    else if(input$choice_t == "departure_station")
      trains <- subset(trains, departure_station == input$trainsInput)
  })
  
  
  
  output$mymap <- renderLeaflet({
    m <- leaflet() %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      addCircleMarkers(lng=airports$LONGITUDE, lat=airports$LATITUDE, radius = 1, label = airports$AIRPORT) %>%
      addPolylines(weight = 0.3, opacity = 0.5, color = "#820a0a", lat=c(flights_latlon$origin_lat,flights_latlon$dest_lat), 
                   lng=c(flights_latlon$origin_lon, flights_latlon$dest_lon))
  })
  
  output$tot_flights <- renderValueBox({
    nrow(flights_filter()) %>%
      valueBox("Total number of flights", icon("plane"))
  })
  
  dist_airline <- reactive({
    data <- flights %>% inner_join(select(.data = airlines, AIRLINE = code, airline = airline), 
                                   by = "AIRLINE")
    data <- aggregate(data$DISTANCE, by=list(Category=data$airline), FUN=sum)
    data <- setNames(data,c("Airline","Distance (km)"))
  })
  
  output$tot_dist_airline <- renderValueBox({
    data <- sum(flights_filter()$DISTANCE)
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Total distance covered in km", icon("tachometer-alt"))
  })
  
  avg_duration <- reactive({
    data <- flights_filter()$ARRIVAL_TIME-flights_filter()$DEPARTURE_TIME
    data <- mean(as.numeric(data[!is.na(data)]))
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
  })
  
  output$avg_flight_duration <- renderValueBox({
    avg_duration() %>%
      valueBox(subtitle = "Average flight duration in minutes", icon("hourglass-half"))
  })
  
  output$avg_flight_distance <- renderValueBox({
    data <- mean(flights_filter()$DISTANCE[!is.na(flights_filter()$DISTANCE)])
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average flight distance in km",icon("tachometer-alt"))
  })
  
  output$avg_depart_delay <- renderValueBox({
    data <- mean(flights_filter()$DEPARTURE_DELAY[!is.na(flights_filter()$DEPARTURE_DELAY)])
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average departure delay in minutes", icon("clock"))
  })
  
  output$avg_arrival_delay <- renderValueBox({
    data <- mean(flights_filter()$ARRIVAL_DELAY[!is.na(flights_filter()$ARRIVAL_DELAY)])
    if(data == "NaN")
      data = "Unknown"
    else
      data = round(data)
    data %>%
      valueBox(subtitle = "Average arrival delay in minutes", icon("clock"))
  })
  
  output$tot_nb_delay_flights <- renderValueBox({
    data <- flights_filter()[!is.na(flights_filter()$DEPARTURE_DELAY),]
    data <- data[!is.na(data$ARRIVAL_DELAY),]
    data <- sum(data$DEPARTURE_DELAY > 0 | data$ARRIVAL_DELAY > 0)
    data %>%
      valueBox(subtitle = "Total flights delayed", icon("clock"))
  })
  
  output$perc_cause_delay <- renderPlot({
    ext = mean(trains_filter()[!is.na(trains_filter()$delay_cause_external_cause),]$delay_cause_external_cause)
    rail = mean(trains_filter()[!is.na(trains_filter()$delay_cause_rail_infrastructure),]$delay_cause_rail_infrastructure)
    traffic = mean(trains_filter()[!is.na(trains_filter()$delay_cause_traffic_management),]$delay_cause_traffic_management)
    rolling = mean(trains_filter()[!is.na(trains_filter()$delay_cause_rolling_stock),]$delay_cause_rolling_stock)
    station = mean(trains_filter()[!is.na(trains_filter()$delay_cause_station_management),]$delay_cause_station_management)
    travelers = mean(trains_filter()[!is.na(trains_filter()$delay_cause_travelers),]$delay_cause_travelers)
    myPalette <- brewer.pal(5, "Set2") 
    
    slices <- c(ext, rail, traffic, rolling, station, travelers)
    lbls <- c("External", "Rail Infrastructure", "Traffic management", "Rolling stock", "Station management", "Travelers")
    pie(slices, labels = lbls, main="Proportion of delay causes", col=myPalette, border="white")
  },bg="#ecf0f5")
  
  output$list_flights <- renderUI({
    selectInput("flightsInput", "Choose :",
                sort(unique(flights[,input$choice_f]))
    )
  })
  
  output$choice_flights <- renderUI({
    selectInput("choice_f", "Aggregate on",
                choices = list("AIRLINES", "ORIGIN_AIRPORT") )
  })
  
  flights_filter <- reactive({
    if(input$choice_f == "AIRLINES")
      flights <- subset(flights, AIRLINES == input$flightsInput)
    else if(input$choice_f == "ORIGIN_AIRPORT")
      flights <- subset(flights, ORIGIN_AIRPORT == input$flightsInput)
  })
  
  output$title_cloud <- renderText({
    print("Top 10 destinations")
  })
  
  output$cloud <- renderPlot({
    set.seed(155864)
    temp <- table(trains$arrival_station)
    wordcloud(names(temp),as.numeric(temp), max.words = 10, colors = brewer.pal(8, "Dark2"), scale=c(3,.5))
  }, bg = "#ecf0f5")
  
  output$bar <- renderPlot({
    ggplot(data = trains, aes(x=departure_station, y=journey_time_avg, fill=departure_station)) +
      geom_bar( stat="identity",show.legend = FALSE) + theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
  })
  
  output$cloud_f <- renderPlot({
    set.seed(1552131)
    temp <- table(flights$city)
    wordcloud(names(temp),as.numeric(temp), max.words = 10, colors = brewer.pal(8, "Dark2"), scale=c(3,.5))
  }, bg = "#ecf0f5")
  
  output$title_stack <- renderText({
    print("Stacked bar evolution of distance by date")
  })
  
  output$title_cloud_f <- renderText({
    print("Top 10 destinations")
  })
  
  output$title_box <- renderText({
    print("Box plot evolution of departure delay by date")
  })
  
  output$stack <- renderPlot({
    
    ggplot(data=flights, mapping = aes(x = date)) + 
      geom_bar(aes(y=DISTANCE, fill=AIRLINES), stat="identity",width=1)
  })
  
  output$box <- renderPlot({
    boxplot(flights$DEPARTURE_DELAY ~ reorder(format(flights$date,'%b %y'),flights$date), outline = FALSE)
  })
}

