library(shiny)
library(shinydashboard)


#read in the data set
playerdata<-read.csv("NBA1617E.csv",header=TRUE)

#Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
index1 = which(((playerdata$FTA >= 1)*(playerdata$FTA<=1000))==1)
playerdata2 = playerdata[index1,]
#create a list of just the players names to be selected from later 
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
              
              #Content within the tabs
              dashboardBody(
                tabItems(
                  
                  tabItem(tabName = "over",
                          
                          fluidRow(
                            #column of length 12 which is the whole width
                            #I include everthing in a column though because this way there are margins and it looks better
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
                  
                  #Define the content contained within part 1 ie. tabname "first"
                  tabItem(tabName = "first",
                          fluidRow(
                            #Include LaTeX functioality although I don't think I used it for this
                            withMathJax(),
                            
                            #Column 1 has inputs for how to filter and is of width 4
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
                            
                            #Column two displays the Histogram of the distrubition of the free throw attempts
                            column(8,
                                   h1("Histogram"),
                                   
                                   plotOutput("histogram")
                            ),
                            
                            #A box with information to get students thinking and transitioning into part 2
                            box(width = 12,background = "blue", h4("Try to think about what the median and mean of FT% are and where what range you might expect most of the players to fall in. "))
                           
                          )
                          
                  ),
                  
                  #Define Content in tab 2
                  tabItem(tabName = "second",
                          fluidRow(
                            
                            column(4,
                                   #Conditional based on how the user would like to select a player for the hypothesis test
                                   selectInput(inputId = "howToChoose","Would you like to choose a random player or select your own", choices = c(Random = "rand", Select = "sel")),
                                   conditionalPanel("input.howToChoose == 'sel'",
                                                    selectizeInput(inputId = "player","Select your player from the drop down list below:", choices=PlayerNames, multiple=FALSE, options = list(placeholder = 'Select Your Player'),selected = NULL)
                                                    ),
                                   
                                   #The H0 value the user would like to test against
                                   numericInput("null.val","Select a value for the null hypothesis. ",min = 0,max = 1,value = 0.74, step = 0.01),
                                   
                                   #This is a text output that displays what the hypothesis is they are testing and who the player is
                                   textOutput("text3"),
                                   
                                   #User now selects what their sample size would be ie how many shots they are simulating for the player
                                   #simulates shots based on the players actual FT%
                                   h4("Simulate your player shooting free throws and guess whether or not we can reject the null hypothesis"),
                                   numericInput("samp.size",h4("Input the number of shots in the sample:  " ),min = 0,max = 1000,value = 5, step = 5),
                                   
                                   #this text output show what the proportion of free throws made is for their player 
                                   textOutput("text2"),
                                   
                                   #Conditional using checkbox if they want to see the true population proportion is for their player
                                   checkboxInput("true", h6("Show the true free throw percentage")),
                                   conditionalPanel("input.true==true",
                                                    textOutput("text1")
                                   ),
                                   #include an image
                                   img(src = "fthrow.png", height = 150, width =100)

                              
                            ),
                            
                            column(8,
                                   #Conditionally select what to output based on what the user wants to show
                                   #There is a better way to do this that I have specified in the server file
                                   conditionalPanel("input.true==false",
                                                    plotOutput("proportion")
                                                    ),
                                   conditionalPanel("input.true==true",
                                                    plotOutput("proportion2")
                                                    ),
                                   
                                   #Output some info about the graphs and the conditional part
                                   h4("The red line shows the proportion from the null hypothesis"),
                                   h4("The green line shows the sample proportion"),
                                   conditionalPanel("input.true==true",
                                                    h4("The blue line shows the players actual free throw proportion from the 2016-17 season")
                                                    )

                                   )
                            
                            
                          )
                          
                          ),
                  
                  
                  #The third tab just shows all of the data that I used
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




