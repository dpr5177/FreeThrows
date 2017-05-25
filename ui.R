library(shiny)
library(shinydashboard)

playerdata<-read.csv("NBA1617E.csv",header=TRUE)
#PlayerNames<-read.csv("NBA1617E.csv",header=TRUE)[,1]
#index1 = which((playerdata$FGA>0)==1)
index1 = which(((playerdata$FTA >= 1)*(playerdata$FTA<=1000))==1)
playerdata2 = playerdata[index1,]
PlayerNames<-playerdata2[,1]

dashboardPage(skin="blue",
              
              #Title
              dashboardHeader(title="Hypothesis Testing with NBA data",titleWidth=450),
              
              #Sidebar
              dashboardSidebar(
                width = 260,
                sidebarMenu(
                  
                  menuItem("Overview", tabName = "over", icon = icon("university")),
                  menuItem("Part 1", tabName = "first", icon = icon("pencil-square")),
                  menuItem("Part 2", tabName = "second", icon = icon("pencil-square")),
                  menuItem("Table", tabName = "third", icon = icon("table"))
                )),
              
              #define the body of the app
              dashboardBody(
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            column(12,
                                   h3("About"),
                                   h4("In this app you will explore data about free throw percentage for NBA players in the 2016-17 season."),
                                   h4("In Part 1 you will look at the distribution of all the players free throw percentages."),
                                   h4("In Part 2 you will explore hypothesis tests about and individual player's free throw percentage"),
                                   
                                   br(),
                                   br(),
                                   img(src="fthrow2.png",height = 250,width =650,algin = "middle")
                                   
                            )
                            
                            
                          )
                  ),
                  
                  tabItem(tabName = "first",
                          fluidRow(
                            #add in latex functionality
                            withMathJax(),
                            
                            #two columns for each of the two items
                            
                            column(4,
                                   
                                   selectInput("filtertype",h2("Select how you would like to filter"),choices = c(GamesPlayed= "games", FreeThrowAttempts="FTA")),
                                   
                                   conditionalPanel("input.filtertype == 'games'",
                                                    sliderInput(inputId = "gamesplayed",
                                                                "Filter on number of games played:",
                                                                min = 0,
                                                                max = 82,
                                                                value = c(0,85)
                                                    )),
                                   conditionalPanel("input.filtertype == 'FTA'",
                                                    sliderInput(inputId = "FTA1",
                                                                "Filter on number of free throws attempted:",
                                                                min = 0,
                                                                max = 881,
                                                                value = c(0,881)
                                                    )),
                                   img(src="Giannis.png",height = 219,width =300,align = "middle")
     
                            ),
                            
                            column(8,
                                   #continuation
                                   h1("Histogram"),
                                   
                                   plotOutput("histogram")
                            ),
                            
                            box(width = 12,background = "blue", h4("Try to think about what the median and mean of FT% are and where what range you might expect most of the players to fall in. "))
                           
                          )
                          
                  ),
                  
                  tabItem(tabName = "second",
                          fluidRow(
                            column(4,
                                   selectInput(inputId = "howToChoose","Would you like to choose a random player or select your own", choices = c(Random = "rand", Select = "sel")),
                                   conditionalPanel("input.howToChoose == 'sel'",
                                                    selectizeInput(inputId = "player","Select your player from the drop down list below:",choices=PlayerNames,multiple=FALSE)
                                                    ),
                                   
                                   numericInput("null.val","Select a value for the null hypothesis. ",min = 0,max = 1,value = 0.74, step = 0.01),
                                   textOutput("text3"),
                                   
                                   h4("Simulate your player shooting free throws and guess whether or not we can reject the null hypothesis"),
                                   numericInput("samp.size",h4("Input the number of shots in the sample:  " ),min = 0,max = 1000,value = 5, step = 5),
                                   textOutput("text2"),
                                   checkboxInput("true", h6("Show the true free throw percentage")),
                                   conditionalPanel("input.true==true",
                                                    textOutput("text1")
                                   ),
                                   img(src = "fthrow.png", height = 150, width =100)

                              
                            ),
                            column(8,
                                   conditionalPanel("input.true==false",
                                                    plotOutput("proportion")
                                                    ),
                                   conditionalPanel("input.true==true",
                                                    plotOutput("proportion2")
                                                    ),
                                   h4("The red line shows the proportion from the null hypothesis"),
                                   h4("The green line shows the sample proportion"),
                                   conditionalPanel("input.true==true",
                                                    h4("The blue line shows the players actual free throw proportion from the 2016-17 season")
                                                    )

                                   )
                            
                            
                          )
                          
                          ),

                  tabItem(tabName = "third",
                          fluidRow(
                            
                            column(6,
                                   h3("Data"),
                                   tableOutput("samp.table")
                            )
                          )
                  )
                )
              )
)




