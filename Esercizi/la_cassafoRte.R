library(shinythemes)
library(shiny)

shinyApp(
  ui = fluidPage(
    theme = shinytheme("superhero"),  # <--- Add this somewhere in the UI
    sidebarPanel(
      tags$h3("La cassafoRte"),
      hr(),
      tags$h4("Benvenut* nella cassafoRte."),
      tags$h4("In questa Shiny App, il tuo compito sarà di inserire i risultati degli esercizi."),
      tags$h4("Se i valori saranno giusti, la cassafoRte si aprirà, e riceverai un premio."),
      tags$h4("Altrimenti, hai sbagliato qualocsa, quindi ritenta (e studia)!")
 
    ),
    mainPanel(
      column(3,numericInput("num1", label = h3("Valore 1"), value = 99)),
      column(3,numericInput("num2", label = h3("Valore 2"), value = 99)),
      column(3,numericInput("num3", label = h3("Valore 2"), value = 99)),
      column(6,offset = 3,
      conditionalPanel(
        hr(),
        tags$h3("Ben fatto!"),
        condition = "input.num1 == 7.56 && input.num2 == 5.10 && input.num3 == 6.54",
        verbatimTextOutput("winner")
      )
      )
    )
  ),
  server = function(input, output) {
    output$winner=renderText({"https://www.youtube.com/watch?v=0qanF-91aJo"})
  }
)
