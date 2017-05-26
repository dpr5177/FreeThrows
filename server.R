

library(shiny)


playerdata<-read.csv("NBA1617E.csv",header=TRUE)


shinyServer(function(input, output,session) {
  
  #This is a reactive element for filtering the data in part 1 for the histogram
  dataFilter<- reactive({
    
    games<-input$gamesplayed

    gameindex<-which(((playerdata$G >= games[1])*(playerdata$G<=games[2]))==1)
    
    fta<-input$FTA1
    index<-which(((playerdata$FTA >= fta[1])*(playerdata$FTA<=fta[2]))==1)
    playerdata<-playerdata[index,]
    bballdata<-playerdata[gameindex,]
 
  })
  
  #This is a reactive element for selecting a player based on the user's choices
  # **Part of what can be modified to make plot function simpler
  player.select <-reactive({
    #Filter the player data so that it does not choose a player who has no free throw attempts => no free throw %
    index1 = which(((playerdata$FTA >= 1)*(playerdata$FTA<=1000))==1)
    playerdata2 = playerdata[index1,]
    
    #Randomly select a player if it is random
    decision = input$howToChoose
    if(decision == "rand"){
      s1 = playerdata2[sample(nrow(playerdata2), 1), ]
      name = s1$Player
    }
    else{
      name = input$player
    }
    
    #If it is not random use the player that the user selected
    index<-which(playerdata2$Player == name)
    namedata<-playerdata2[index,]
  })
  
  #This is a reactive element for how many shots will be simulated
  n <- reactive({
    return(input$samp.size)
  })
  
  #This is a reactive element for what the user chooses for the null value
  h <- reactive({
    return(input$null.val)
  })

  #Output text for what the free throw percentage is for the player
  output$text1 <- renderText({
    namedata <-player.select()
    ftp = namedata$FT/namedata$FTA

    paste("The free throw proportion for ",namedata$Player, "is",ftp)
  })
  
  # Output text for the sample proportion
  output$text2 <- renderText({
    namedata<-player.select()
    ftp = namedata$FT/namedata$FTA
    n1 <-n()
    phat = 0
    #simulate the data based on number of shots the user selects and the players free throw percentage
    sim1 = rbinom(n = n1,size = 1, prob = ftp)
    #Find the sample proportion
    for(i in 1:n1){
      if(sim1[i]==1){
        phat = phat+1
      }
      else{
        phat = phat
      }
    }
    phat = phat/n1
    paste("The sample proportion of shots made is  ", phat)
  })
  
  #Output text for the null hypothesis
  output$text3 <- renderText({
    namedata <-player.select()
    h1 <- h()
    paste("Test the hypothesis that the free throw percentage for ",namedata$Player, "is not equal to",h1)
    
    
  })
  
  #Output the histogram in Part 1 
  output$histogram<-renderPlot({
    bballdata<-dataFilter()
   
    n = nrow(bballdata)
    y = numeric(length = n)
    
    #Calculates the free throw percentages for all the players and puts it into a variable
    #Produces NAN's for players that haven't taken any free throws
    #Doesn't matter for the Histogram though because the Histogram won't display the NaN's 
    #I take out the NaN's in a different part where it is needed
    for(i in 1:n){
      
      y[i] = bballdata$FT[i]/bballdata$FTA[i]
    }

    #The actual histogram
    hist(y,xlab = "Free Throw Proportion",main = "Histogram of Free Throw Proportion")
  })


  
  #output the plot for the proportion of free throws made
  output$proportion <-renderPlot({
    h2 <-h()
    namedata<-player.select()
    ftp = namedata$FT/namedata$FTA
    n1 <-n()
    phat = 0
    #simulate the data based on number of shots the user selects and the players free throw percentage
    sim1 = rbinom(n = n1,size = 1, prob = ftp)
    #Find the sample proportion
    for(i in 1:n1){
      if(sim1[i]==1){
        phat = phat+1
      }
      else{
        phat = phat
      }
    }
    phat = phat/n1
    #Plot it
    plot(x=NULL,
         y=NULL,
         xlim=c(0, 1),
         ylim=c(0, 1),
         ylab = "Proportion",
         xlab = "",
         xaxt = "n"
    )
    abline(h = h2, col = "red", lwd = 3)
    abline(h = phat, col = "green", lwd = 3)

  })
  #
  #
  #     v
  #     v
  #     v
  #     v
  #
  # PLOT for conditional panel
  # A more efficient way to handle this would have been to create a reactive element for the checkbox and then use an if statement in the first plot function
  #
  #     ^
  #     ^
  #     ^
  #     ^
  #
  #
  output$proportion2 <-renderPlot({
    h3<-h()
    namedata<-player.select()
    ftp = namedata$FT/namedata$FTA
    n1 <-n()
    phat = 0
    #simulate the data based on number of shots the user selects and the players free throw percentage
    sim1 = rbinom(n = n1,size = 1, prob = ftp)
    #Find the sample proportion
    for(i in 1:n1){
      if(sim1[i]==1){
        phat = phat+1
      }
      else{
        phat = phat
      }
    }
    phat = phat/n1
    #Plot it
    plot(x=NULL,
         y=NULL,
         xlim=c(0, 1),
         ylim=c(0, 1),
         ylab = "Proportion",
         xlab = "",
         xaxt = "n"
    )

    abline(h = h3, col = "red", lwd = 3)
    abline(h = phat, col = "green", lwd = 3)
    abline(h=ftp,col = "blue", lwd =3)
    
  })
  
  #Output the table of data. It is called a sample table but note that it is not a sample and is actually the entire population of NBA players in the 2016-17 season
  output$samp.table<- renderTable({

    sample1 <- playerdata
    
  })
  
})

