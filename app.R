#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shinydashboard)
library(shiny)
library(plyr)
library(tidyverse)
library(tibble)
library(ggplot2)
library(sf)
library(mapview)
library(leaflet)


ui <- navbarPage("Data Analytics Project",
                 tabPanel("SNFC", 
                          sidebarPanel(selectInput("variable_one","Aggregate By:",
                                                   c("Year"="year",
                                                     "Station"="departure_station"
                                                   ), selected = "departure_station")),         
                          mainPanel(
                          plotOutput("firstPlot"),
                          plotOutput("secondPlot"),
                          plotOutput("thirdPlot"),
                          plotOutput("forthPlot"),
                          plotOutput("fifthPlot"),
                          plotOutput("sixthPlot"),
                          plotOutput("seventhPlot"),
                          plotOutput("eigthPlot"),
                          plotOutput("ninethPlot"),
                          plotOutput("tenthPlot"),
                          plotOutput("eleventhPlot"),
                          plotOutput("twelvethPlot")
                 )),
                 tabPanel("US Flights",
                          
                          sidebarPanel(selectInput("variable_two","Aggregate By:",
                                                   c("Airline"="AIRLINE",
                                                     "Departure"="ORIGIN_AIRPORT"
                                                   ), selected = "AIRLINE")), 
                          mainPanel(
                          leafletOutput("mapplot"),
                          plotOutput("secondTabPlot"),
                          plotOutput("thirdTabPlot"),
                          plotOutput("forthTabPlot"),
                          plotOutput("fifthTabPlot"),
                          plotOutput("sixthTabPlot"),
                          plotOutput("seventhTabPlot"),
                          plotOutput("eigthTabPlot")
                          
                 ))
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    trains <- read.csv(file="full_trains.csv")
    airports <- read.csv(file="airports.csv")
    flights <- read.csv(file="flights.csv")
    airlines <- read.csv(file="airlines.csv")
    
    output$firstPlot <- renderPlot({
        trains_late_at_departure=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = sum(num_late_at_departure, na.rm = TRUE))
        trains_late_at_departure=data.frame(column_to_rownames(trains_late_at_departure,var=input$variable_one))
      
        
       barplot(t(as.matrix(trains_late_at_departure)),
               main="Number of delayed trains at departure",
               xlab=input$variable_one,
               ylab="Total",
               col="blue",
               las=2,
               cex.names=1,
               beside=TRUE)
    })


    output$secondPlot <- renderPlot({
  
        trains_late_at_arrival=trains %>%
             group_by_(input$variable_one) %>%
            summarize(result = sum(num_arriving_late, na.rm = TRUE))
        trains_late_at_arrival=data.frame(column_to_rownames(trains_late_at_arrival,var=input$variable_one))
    
        barplot(t(as.matrix(trains_late_at_arrival)),
                main="Number of delayed trains on arrival",
                xlab=input$variable_one,
                ylab="Total",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
})
    
    output$thirdPlot <- renderPlot({
        trains_average_departure=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = mean(num_late_at_departure, na.rm = TRUE))
        
        trains_average_departure=data.frame(column_to_rownames(trains_average_departure,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_average_departure)),
                main="Average number of delayed trains at departure",
                xlab=input$variable_one,
                ylab="Average",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    output$forthPlot <- renderPlot({
        trains_average_arrival=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = mean(num_arriving_late, na.rm = TRUE))
        
        trains_average_arrival=data.frame(column_to_rownames(trains_average_arrival,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_average_arrival)),
                main="Average number of delayed trains on arrival",
                xlab=input$variable_one,
                ylab="Average",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    output$fifthPlot <- renderPlot({
        trains_canceled=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = sum(num_of_canceled_trains, na.rm = TRUE))
        
        trains_canceled=data.frame(column_to_rownames(trains_canceled,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_canceled)),
                main="Canceled trains",
                xlab=input$variable_one,
                ylab="Total",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    output$sixthPlot <- renderPlot({
        
        trains_total_delay_all_departing=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = sum(avg_delay_all_departing, na.rm = TRUE))
        
        
        trains_total_delay_all_departing=data.frame(column_to_rownames(trains_total_delay_all_departing,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_total_delay_all_departing)),
                main="Total average departure of all departing",
                xlab=input$variable_one,
                ylab="Total",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    output$seventhPlot <- renderPlot({
        
        trains_total_delay_all_arriving=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = sum(avg_delay_all_arriving, na.rm = TRUE))
        
        
        trains_total_delay_all_arriving=data.frame(column_to_rownames(trains_total_delay_all_arriving,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_total_delay_all_arriving)),
                main="Total average departure of all arriving",
                xlab=input$variable_one,
                ylab="Average",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    output$eigthPlot <- renderPlot({
        
        
        trains_avg_delay_of_delayed_dep=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = mean(avg_delay_late_at_departure, na.rm = TRUE))
        
        
        trains_avg_delay_of_delayed_dep=data.frame(column_to_rownames(trains_avg_delay_of_delayed_dep,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_avg_delay_of_delayed_dep)),
                main="Total average departure of delayed departing",
                xlab=input$variable_one,
                ylab="Average",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    
    output$ninethPlot <- renderPlot({
        
        
        
        
        trains_avg_delay_of_delayed_arrival=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = mean(avg_delay_late_on_arrival, na.rm = TRUE))
        
        trains_avg_delay_of_delayed_arrival=data.frame(column_to_rownames(trains_avg_delay_of_delayed_arrival,var=input$variable_one))
        

        barplot(t(as.matrix(trains_avg_delay_of_delayed_arrival)),
                main="Total average departure of delayed departing",
                xlab=input$variable_one,
                ylab="Average",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    
    output$tenthPlot <- renderPlot({
        
        
        
        
        trains_percentage_canceled=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = sum(num_of_canceled_trains*100/sum(total_num_trips), na.rm = TRUE))
        
        
        trains_percentage_canceled=data.frame(column_to_rownames(trains_percentage_canceled,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_percentage_canceled)),
                main="Proportion of canceled trains",
                xlab=input$variable_one,
                ylab="Percentage",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })
    
    
    output$eleventhPlot <- renderPlot({
        
        
        
        
        trains_carried_out=trains %>%
            group_by_(input$variable_one) %>%
            summarize(result = sum(total_num_trips)-sum(num_of_canceled_trains), na.rm = TRUE)

    trains_carried_out=data.frame(column_to_rownames(trains_carried_out,var=input$variable_one))
        
        
        barplot(t(as.matrix(trains_carried_out)),
                main="Trains carried out",
                xlab=input$variable_one,
                ylab="Total",
                col="blue",
                las=2,
                cex.names=1,
                beside=TRUE)
        
    })

    output$twelvethPlot <- renderPlot({
    
        toExclude <- names(trains)[1:17]
        toExcludeToo <- names(trains)[24:27]
        trains2 <- gather(trains,causes,percentages,-toExclude,-toExcludeToo)
      
       causes_reasons=trains2 %>%
            group_by_(input$variable_one) %>%
            count(causes)
            
        causes_reasons_portion=trains2 %>%
              group_by_(input$variable_one,"causes") %>%
                 summarize(result = (sum(percentages,na.rm=TRUE)/1344)*100)
       

        if(input$variable_one == "year"){
        
        plot <- ggplot(causes_reasons_portion, aes(x = year, y = result,fill=causes)) +
            geom_bar(stat='identity')
        plot
        }else if(input$variable_one == "departure_station"){
          plot <- ggplot(causes_reasons_portion, aes(x = departure_station, y = result,fill=causes)) +
            geom_bar(stat='identity')
          plot
        }
     
      
})
    
    output$mapplot <- renderLeaflet({
      
     
      airports <- airports[complete.cases(airports), ]
      airports_sf <- st_as_sf(airports, coords = c("LONGITUDE", "LATITUDE"), crs = 4326)
      m <- mapview(airports_sf)
      m@map
    })
    
    
    output$secondTabPlot <- renderPlot({
      flights_grouped = flights %>% 
                            group_by_(input$variable_two) %>% 
                            summarise(n = n())
      print(flights_grouped)
      flights_grouped=data.frame(column_to_rownames(flights_grouped,var=input$variable_two))
    
      
      barplot(t(as.matrix(flights_grouped)),
              main="Number of Flights",
              xlab=input$variable_two,
              ylab="Total",
              col="blue",
              las=2,
              cex.names=1,
              beside=TRUE)
      
      
    })
    
    
    
    output$thirdTabPlot <- renderPlot({
      flights_delayed=flights %>% 
             group_by_(input$variable_two) %>% 
             summarise(num_pos = sum(DEPARTURE_DELAY > 0, na.rm=TRUE))
      flights_delayed=data.frame(column_to_rownames(flights_delayed,var=input$variable_two))
      
      
      barplot(t(as.matrix(flights_delayed)),
              main="Number of delayed flights",
              xlab=input$variable_two,
              ylab="Total",
              col="black",
              las=2,
              cex.names=1,
              beside=TRUE)
      
      
    })
    
    
    output$forthTabPlot <- renderPlot({
      flights_average_duration=flights %>% 
        group_by_(input$variable_two) %>% 
        summarise(result = mean(AIR_TIME, na.rm=TRUE))
      flights_average_duration=data.frame(column_to_rownames(flights_average_duration,var=input$variable_two))
      
      
      barplot(t(as.matrix(flights_average_duration)),
              main="Average Duration",
              xlab=input$variable_two,
              ylab="Minutes",
              col="blue",
              las=2,
              cex.names=1,
              beside=TRUE)
      
      
    })
    
    output$fifthTabPlot <- renderPlot({
      flights_average_distance=flights %>% 
        group_by_(input$variable_two) %>% 
        summarise(result = mean(DISTANCE, na.rm=TRUE))
      flights_average_distance=data.frame(column_to_rownames(flights_average_distance,var=input$variable_two))
      
      
      barplot(t(as.matrix(flights_average_distance)),
              main="Average Distance",
              xlab=input$variable_two,
              ylab="Distance",
              col="blue",
              las=2,
              cex.names=1,
              beside=TRUE)
      
      
    })
    
    output$sixthTabPlot <- renderPlot({
      flights_total_distance=flights %>% 
        group_by_(input$variable_two) %>% 
        summarise(result = sum(DISTANCE, na.rm=TRUE))
      flights_total_distance=data.frame(column_to_rownames(flights_total_distance,var=input$variable_two))
      
      
      barplot(t(as.matrix(flights_total_distance)),
              main="Total Distance",
              xlab=input$variable_two,
              ylab="Distance",
              col="yellow",
              las=2,
              cex.names=1,
              beside=TRUE)
      
      
    })
    
    output$seventhTabPlot <- renderPlot({
      flights_average_dep_delay=flights %>% 
        group_by_(input$variable_two) %>% 
        summarise(result = mean(DEPARTURE_DELAY, na.rm=TRUE))
      flights_average_dep_delay=data.frame(column_to_rownames(flights_average_dep_delay,var=input$variable_two))
      
      
      barplot(t(as.matrix(flights_average_dep_delay)),
              main="Average Departure Delay",
              xlab=input$variable_two,
              ylab="Minutes",
              col="red",
              las=2,
              cex.names=1,
              beside=TRUE)
      
      
    })
    
    output$eigthTabPlot <- renderPlot({
      flights_average_arr_delay=flights %>% 
        group_by_(input$variable_two) %>% 
        summarise(result = mean(ARRIVAL_DELAY, na.rm=TRUE))
      flights_average_arr_delay=data.frame(column_to_rownames(flights_average_arr_delay,var=input$variable_two))
      
      
      barplot(t(as.matrix(flights_average_arr_delay)),
              main="Average Arrival Delay",
              xlab=input$variable_two,
              ylab="Minutes",
              col="red",
              las=2,
              cex.names=1,
              beside=TRUE)
      
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
