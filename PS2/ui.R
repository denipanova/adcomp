

shinyUI(fluidPage( #how the page to llok like
  
  titlePanel('Loan Data'), #title
  
  tabsetPanel(
    tabPanel("Plot of Accepted and Denied Loans",plotOutput("plot1",height="auto")),
    tabPanel("Confusion Matrix",tableOutput("confmatrix"))
  ),
  
  hr(),
  
  fluidRow(
    column(3,
           h3("Solvency of Accepted"),
           numericInput("muXA", label = h3("Mean"),4),
           numericInput("sdXA", label = h3("SD"),1,min=0)
           ),
    column(3,
           h3("PI ratio of Accepted"),
           numericInput("muYA", label = h3("Mean"),150),
           numericInput("sdYA", label = h3("SD"),20,min=0)
    ),
    column(3,
           h3("Solvency of Denied"),
           numericInput("muXD", label = h3("Mean"),10),
           numericInput("sdXD", label = h3("SD"),2,min=0)
    ),
    column(3,
           h3("PI ratio of Denied"),
           numericInput("muYD", label = h3("Mean"),100),
           numericInput("sdYD", label = h3("SD"),30,min=0)
    )
  )
    
    
   
))

