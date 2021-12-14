## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('functions/helpers.R')

genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          dashboardSidebar(sidebarMenu(
            
            id = "tabs",
            menuItem("Recommender by Genre", tabName = "Genre", icon = icon("film")),
            menuItem("Recommender by Rating", icon = icon("star"), tabName = "Rating")
            )
          ),

          dashboardBody(
            tabItems(
              tabItem(tabName = "Genre",
                      includeCSS("css/movies.css"),
                      fluidRow(
                        box(width = 12, title = "Step 1: Select a Genre", status = "info", solidHeader = TRUE, collapsible = FALSE,
                            # div(class = "rateitems",
                            #     uiOutput('genres')
                            # ),
                            selectInput('g', 'Genre', genre_list)
                        )
                      ),
                      fluidRow(
                        useShinyjs(),
                        box(
                          width = 12, status = "info", solidHeader = TRUE,
                          title = "Step 2: Discover movies of that Genre",
                          br(),
                          withBusyIndicatorUI(
                            actionButton("btn1", "Click here to get genre recommendations", class = "btn-warning")
                          ),
                          br(),
                          tableOutput("results1")
                        )
                      )
              ),
              tabItem(tabName = "Rating",
                includeCSS("css/movies.css"),
                fluidRow(
                    box(width = 12, title = "Step 1: Rate at least 3 movies", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                  ),
                fluidRow(
                    useShinyjs(),
                    box(
                      width = 12, status = "info", solidHeader = TRUE,
                      title = "Step 2: Discover movies you might like",
                      br(),
                      withBusyIndicatorUI(
                        actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                      ),
                      br(),
                      tableOutput("results2")
                  )
               )
              )
          )
    )
  )
)