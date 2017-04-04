shinyUI(fluidPage(
  titlePanel("Raga Analysis"),
  
  sidebarLayout(
    sidebarPanel(
    helpText("This application focuses on the RAGA Analysis "),
    
    fileInput('raga_file', 'Choose wav or mp3 file', multiple = FALSE, accept=c('.wav', '.mp3')),
    tags$hr()
    ),
    
    mainPanel(
      plotOutput("mfcc_plot"),
      plotOutput("zcr_plot")
    )
  )
))
