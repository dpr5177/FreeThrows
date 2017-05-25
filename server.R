

library(shiny)


playerdata<-read.csv("NBA1617E.csv",header=TRUE)


shinyServer(function(input, output,session) {
  dataFilter<- reactive({
    
    games<-input$gamesplayed

    gameindex<-which(((playerdata$G >= games[1])*(playerdata$G<=games[2]))==1)
    
    fta<-input$FTA1
    index<-which(((playerdata$FTA >= fta[1])*(playerdata$FTA<=fta[2]))==1)
    playerdata<-playerdata[index,]
    bballdata<-playerdata[gameindex,]
 
  })
  
  player.select <-reactive({
    index1 = which((playerdata$FGA>0)==1)
    playerdata2 = playerdata[index1,]
    
    decision = input$howToChoose
    if(decision == "rand"){
      s1 = playerdata2[sample(nrow(playerdata2), 1), ]
      name = s1$Player
    }
    else{
      name = input$player
    }
    
    index<-which(playerdata2$Player == name)
    namedata<-playerdata2[index,]
  })
  
  n <- reactive({
    return(input$samp.size)
  })
  
  h <- reactive({
    return(input$null.val)
  })

  output$text1 <- renderText({
    namedata <-player.select()
    ftp = namedata$FT/namedata$FTA

    paste("The free throw percentage for ",namedata$Player, "is",ftp)
  })
  
  output$histogram<-renderPlot({
    bballdata<-dataFilter()
   
    n = nrow(bballdata)
    y = numeric(length = n)
    #Produces NAN's for players that haven't taken any free throws
    for(i in 1:n){
      
      y[i] = bballdata$FT[i]/bballdata$FTA[i]
    }

    
    hist(y,xlab = "Free Throw Proportion",main = "Histogram of Free Throw Proportion")
  })

  
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
  
  output$text3 <- renderText({
    namedata <-player.select()
    h1 <- h()
    paste("Test the hypothesis that the free throw percentage for ",namedata$Player, "is not equal to",h1)
    
    
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
    abline(h = h2, col = "red")
    abline(h = phat, col = "green")

  })
  #PLOT for conditional panel
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
    abline(h = h3, col = "red")
    abline(h = phat, col = "green")
    abline(h=ftp,col = "blue")
    
  })
  
  output$samp.table<- renderTable({

    sample1 <- playerdata
    
  })
  
})

