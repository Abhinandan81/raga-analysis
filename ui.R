shinyUI(fluidPage(
  titlePanel("Raga Analysis"),
  
  sidebarLayout(
    sidebarPanel(
    helpText("This application focuses on the RAGA Analysis "),
    
    fileInput('raga_file', 'Choose wav or mp3 file', multiple = FALSE, accept=c('.wav', '.mp3')),
    tags$hr(),
    sliderInput("number_of_clusters", "Number of clusters", 2, 10,2,1)
    ),
    
    mainPanel(
      plotOutput("mfcc_plot"),
      plotOutput("zcr_plot")
    )
  )
))
