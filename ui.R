library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("paper"),

                  # HTML head section to import css
                  tags$head(
                    tags$title("Raga Analysis"),
                    tags$link(rel = "stylesheet", type = "text/css", href = "custom_style.css")
                  ),
                  
                  # Load loading spinner image 
                  tags$img(src = "images/loading.gif", style = "display: none"),
                  
                  navbarPage(
                    "Raga Analysis",
                    
                    tabPanel("Home",
                             sidebarLayout(
                               sidebarPanel(
                                 fileInput(
                                   'raga_file',
                                   'Choose wav or mp3 file',
                                   multiple = FALSE,
                                   accept = c('.wav', '.mp3')
                                 ),
                                 tags$hr(),
                                 sliderInput("number_of_clusters", "Number of clusters", 1, 5, 1, 1)
                               ),
                               
                               mainPanel(
                                 tabsetPanel(
                                   
                                   tabPanel("K-Means cluster Graph",
                                            plotOutput("kmeansPlot"),
                                            value = 1),
                                   tabPanel("Cluster information",
                                            dataTableOutput("clusterTable"),
                                   value = 2),
                                   id="tabs1")
                             ))
                  ))))