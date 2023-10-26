library(shinythemes)
library(shiny)

shinyApp(
  ui = fluidPage(
    theme = shinytheme("superhero"),  # <--- Add this somewhere in the UI
    sidebarPanel(
      tags$h3("La cassafoRte del 24 ottobre 2023 - Alpha"),
      hr(),
      tags$h4("Benvenut* nella cassafoRte."),
      tags$h4("In questa Shiny App, il tuo compito sarà di inserire i risultati degli esercizi sull'Aplha del 15 aprile 2023."),
      tags$h4("I valori di alpha vanno arrotondati a due decimali dopo la virgola "),
      tags$h4("Se i valori saranno giusti, la cassafoRte si aprirà, e riceverai un premio."),
      tags$h4("Altrimenti, hai sbagliato qualocsa, quindi ritenta (e studia)!"), 
 
    ),
    mainPanel(
      column(3,numericInput("num1", label = h3("Valore 1"), value = "")),
      column(3,numericInput("num2", label = h3("Valore 2"), value = "")),
      column(3,numericInput("num3", label = h3("Valore 3"), value = "")),
      column(6,offset = 3,
      conditionalPanel(
        hr(),
        tags$h3("Ben fatto!"),
        condition = "input.num1 == .81 && input.num2 == .83 && input.num3 == .51",
        verbatimTextOutput("winner")
      )
      )
    )
  ),
  server = function(input, output) {
    output$winner=renderText({"https://www.youtube.com/watch?v=Dsg8JccRZCw&ab_channel=AnneIndergaard"}) # corgi
      }
)

