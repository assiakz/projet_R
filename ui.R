library(shinydashboard)
library(shiny)
library(dplyr)
library(tm)
library(stringr)
library(tidyr)
library(leaflet)
library(plotrix)
library(ggplot2)
library(wordcloud)

ui <- dashboardPage(
  dashboardHeader(title = "Project"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Trains", tabName = "trains", icon = icon("dashboard"),
               menuItem("Basic analysis", tabName = "Agg_year"),
               menuItem("More statistics", tabName = "stats")),
      menuItem("Flights", tabName = "flights", icon = icon("dashboard"),
               menuItem("Basic analysis", tabName = "basic"),
               menuItem("More statistics", tabName = "stats_f")
      )
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Agg_year",
              fluidRow(
                uiOutput("year"),
                uiOutput("list"),
                valueBoxOutput("nb_trains", width = 6), 
                valueBoxOutput("nb_delay_depart", width = 6),
                valueBoxOutput("nb_delay_arrival", width = 6),
                valueBoxOutput("avg_delay_depart", width = 6),
                valueBoxOutput("avg_delay_arrival", width = 6),
                valueBoxOutput("avg_delay_all_depart", width = 6),
                valueBoxOutput("avg_delay_all_arriving", width = 6),
                valueBoxOutput("avg_delay_late_depart", width = 6),
                valueBoxOutput("avg_delay_late_arrival", width = 6),
                valueBoxOutput("tot_num_cancel_trains", width = 6),
                valueBoxOutput("perc_cancel_trains", width = 6),
                plotOutput("perc_cause_delay"),
                h2(textOutput("Top 10 destinations"))
              )
      ),
      
      tabItem(tabName = "stats",
              fluidRow(
                h2(textOutput("title_cloud"), align = "center"),
                plotOutput("cloud"),
                plotOutput("bar")
              )),
      
      
      tabItem(tabName = "basic",
              fluidRow(
                leafletOutput("mymap"),br(),
                uiOutput("choice_flights"),
                uiOutput("list_flights"),
                valueBoxOutput("tot_dist_airline", width = 5),
                valueBoxOutput("tot_flights", width = 5),
                valueBoxOutput("avg_flight_duration", width = 5),
                valueBoxOutput("avg_flight_distance", width = 5),
                valueBoxOutput("avg_depart_delay", width = 5),
                valueBoxOutput("avg_arrival_delay", width = 5),
                valueBoxOutput("tot_nb_delay_flights", width = 5)
              )),
      
      tabItem(tabName = "stats_f",
              fluidRow(
                h2(textOutput("title_cloud_f"), align = "center"),
                plotOutput("cloud_f"),
                h2(textOutput("title_stack"), align = "center"),
                plotOutput("stack"),
                h2(textOutput("title_box"), align = "center"),
                plotOutput("box")
              ))
      
      
      
    )
  )
)