library(shiny)
library(ggplot2)
library(hrbrthemes)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("IRIS"),
    checkboxGroupInput("checkGroup", label = h3("Seleziona la Specie"), 
                       choices = list("Setosa" = "setosa", "Versicolor" = "versicolor", "Virginica" = "virginica"), selected = c("setosa", "versicolor","virginica")),
    plotOutput(outputId = "DPlot")
)

server <- function(input, output) {
  
  output$DPlot <- renderPlot({
    
    data<-iris
    righe<- which(data$Species== input$checkGroup)
    dati_input<-data[righe,]

    ggplot(dati_input, aes(x=Sepal.Length, y=Sepal.Width, color=Species,  shape=Species)) + geom_point(size=6) + theme_ipsum()+ labs(x = "Sepal Lenght", y = "Sepal Width")
 })
}

# Run the application 
shinyApp(ui = ui, server = server)
