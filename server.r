library(shiny)
library(shinydashboard)
library(RODBC)

#####A login page function and a fluid page function
uiLogin <- function(){
  tagList(
    div(id = "logindiv",
        wellPanel(
          tags$b("Please enter your Netezza username and password"),
          tags$br(),tags$br(),
          textInput("userName","Username"),
          passwordInput("passWord","Password"),
          tags$br(),
          actionButton("loginbutton","Log in")
        )),
      tags$style(HTML("#logindiv {font-size:12px; text-align:left; position:absolute; top:40%; left:50%; margin-top:-100px;margin-left:-150px;}"
                      )))
}

uiDashboard <- function(){
  pageWithSidebar(
    headerPanel('Iris k-means clustering'),
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)),
      selectInput('ycol', 'Y Variable', names(iris),
                  selected=names(iris)[[2]]),
      numericInput('clusters', 'Cluster count', 3,
                   min = 1, max = 9)
    ),
    mainPanel(
      plotOutput('plot1')
    )
  )
}

#####Use reactive value Logged and input$loginbutton to facilitate the login process#####
user <- reactiveValues(Logged = FALSE)
server <- function(input, output){
  observe({
    if (user$Logged == FALSE){
      if (!is.null(input$loginbutton)){
        if (input$loginbutton>0){
          Username <- as.character(isolate(input$userName))
          Password <- as.character(isolate(input$passWord))
          if (Username==Password){
            user$Logged <- TRUE
          }
#           These three lines of code below are to test the connection status. 
#           If the correct authorization is given, then connection ch is not -1. 
#           NZSQL is specific to IBM Netezza server
#           ch<-odbcConnect("NZSQL",uid=Username,pwd=Password)
#           if ( ifelse(ch==-1,0,1)  ){
#             user$Logged <- TRUE
#           }
        }
      }
    }
  })
  observe({
    if (user$Logged == FALSE){
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",uiLogin())))
      })
    }
    if (user$Logged == TRUE){
      output$page <- renderUI({
        div(class="outer",do.call(bootstrapPage,c("",uiDashboard())))
      })
    }
  })

####Run a Shiny Gallery Example http://shiny.rstudio.com/gallery/kmeans-example.html####
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
}
