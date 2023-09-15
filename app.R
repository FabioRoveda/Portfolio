library(shiny)
library(ggplot2)
library(hrbrthemes)

ui <- fluidPage(
    titlePanel("CONCENTRAZIONI DI MERCATO"),
    HTML("<br>"),
    selectInput("Anno", "Seleziona un Anno:",
                 c("2000","2001","2002","2003","2004",
                  "2005","2006","2007","2008","2009",
                  "2010","2011","2012","2013","2014",
                  "2015","2016","2017","2018","2019",
                  "2020","2021","2022")),
     
    plotOutput(outputId = "Lorenz", width = 1100, height =  650),
    HTML("<br>"),
    textOutput("text"),
    HTML("<br>"),
    plotOutput(outputId = "Gini", width = 1100, height =  650)
    
)

server <- function(input, output) {
    
  #Import dei dati e creazione di un database per l'elaborazione dell'applicazione
  library(readxl)
  Quote_marchi <- read_excel("Quote marchi.xlsx")
  quote<-round(Quote_marchi[,c(2:24)], 2)
  DATI<- cbind(Quote_marchi[,1], quote)
  rm(Quote_marchi,quote)
  
  #Costruzione della curva di Lorenz
  output$Lorenz <- renderPlot({
    #Calcolo delle quote di ciascun marchio
    anno<- as.numeric(input$Anno)
    X<-anno-1998
    quote<- as.numeric(DATI[,X])
    quote_sort<- sort(quote, decreasing = TRUE)
    sum<- cumsum(quote_sort)
    retta<- cumsum(rep((sum[27]/27),27))
    Gini<-round(2*(sum((sum-retta))/26)/100,4)
    plot(sum, main = "Frequenza cumulata quote - Curva Lorenz",
         xlab= "Numero aziende", ylab= "Quote cumulate",  ylim=c(0,100))
    lines(sum, pch=19)
    lines(retta, col="red")
    })
  
  #Calcolo dell'indice di Gini
  output$text <- renderText({
    anno<- as.numeric(input$Anno)
    X<-anno-1998
    quote<- as.numeric(DATI[,X])
    quote_sort<- sort(quote, decreasing = TRUE)
    sum<- cumsum(quote_sort)
    retta<- cumsum(rep((sum[27]/27),27))
    Gini<-round(2*(sum((sum-retta))/26)/100,4)
    testo <- sprintf("L'indice di Gini per l'anno di riferimento vale %.2f", Gini)
    return(testo)
  })
  
  #Serie storica dell'indice di Gini
  output$Gini <- renderPlot({
    G<-c()
    for (i in 1:23) {
      X<-i+1
      quote<- as.numeric(DATI[,X])
      quote_sort<- sort(quote, decreasing = TRUE)
      sum<- cumsum(quote_sort)
      retta<- cumsum(rep((sum[27]/27),27))
      Gini<-round(2*(sum((sum-retta))/26)/100,4)
      G[i]<-Gini
    }
    anni<-2000:2022
    plot(G~anni, main = "Indice di Gini per le concentrazoni di mercato",
         xlab= "Anni", ylab= "Indice di Gini",  ylim=c(0.3,0.7), pch=16, type= "l")
  })
  
}

# Run dell'applicazione 
shinyApp(ui = ui, server = server)

