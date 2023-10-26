## Update Add Tabsets on 01-Aug-2022
library(shiny)
library(shinyFeedback)
library(tidyverse)
library(rclipboard)
options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  titlePanel("InHouse 23238 EDC Review"),
  useShinyFeedback(),
  a(span("Please email Alvin Lin if you run into any issues",style = "color:black"),href = "mailto:alvin.lin@abbott.com"),
  fluidRow(column(width = 12,
                  textInput("study",h6("Please enter InHouse study",value = ""),width = "400px"),
                  verbatimTextOutput("text"),
                  selectInput("label","Column Label",choices = c(TRUE,FALSE),selected = FALSE),
                  rclipboardSetup(),
                  uiOutput("clip"))),
                  br(),
  
  tabsetPanel(
            tabPanel(p("Adverse Event Report",style = "font-size:20px;"),
              fileInput("ae1","Please Upload AE1",accept = ".sas7bdat",width = "400px"),
              fileInput("ae2","Please Upload AE2",accept = ".sas7bdat",width = "400px")),
  
            tabPanel(p("Device Incident Report",style = "font-size:20px;"),
              fileInput("de","Please Upload DE",accept = ".sas7bdat",width = "400px")),
            
            tabPanel(p("Subject Characteristics",style = "font-size:20px;"),
                     fileInput("dm","Please Upload DM",accept = ".sas7bdat",width = "400px"),
                     fileInput("vs","Please Upload VS",accept = ".sas7bdat",width = "400px"),
                     fileInput("ie","Please Upload IE",accept = ".sas7bdat",width = "400px"),
                     fileInput("mh","Please Upload MH",accept = ".sas7bdat",width = "400px")),
            
            tabPanel(p("Sensor Use",style = "font-size:20px;"),
                     fileInput("du1","Please Upload DU1",accept = ".sas7bdat",width = "400px"),
                     fileInput("du2","Please Upload DU2",accept = ".sas7bdat",width = "400px"),
                     fileInput("du3","Please Upload DU3",accept = ".sas7bdat",width = "400px"),
                     fileInput("du4","Please Upload DU4",accept = ".sas7bdat",width = "400px"),
                     fileInput("du5","Please Upload DU5",accept = ".sas7bdat",width = "400px")),
            
            tabPanel(p("Protocol Deviation Report",style = "font-size:20px;"),
                     fileInput("pd","Please Upload PD",accept = ".sas7bdat",width = "400px")),
            
            tabPanel(p("Skin Assessment",style = "font-size:20px;"),
                     fileInput("sk1","Please Upload SK1",accept = ".sas7bdat",width = "400px"),
                     fileInput("sk2","Please Upload SK2",accept = ".sas7bdat",width = "400px")),
            
            tabPanel(p("Adhesive Log",style = "font-size:20px;"),
                     fileInput("dt2","Please Upload DT2",accept = ".sas7bdat",width = "400px"),
                     fileInput("dt3","Please Upload DT3",accept = ".sas7bdat",width = "400px"),
                     fileInput("dt4","Please Upload DT4",accept = ".sas7bdat",width = "400px")),
            
            tabPanel(p("Daily Activity Log",style = "font-size:20px;"),
                     fileInput("dl2","Please Upload DL2",accept = ".sas7bdat",width = "400px")),
            
            tabPanel(p("Meal and Activity Log",style = "font-size:20px;"),
                     fileInput("ml2","Please Upload ML2",accept = ".sas7bdat",width = "400px"))      
            
            ),
  


   fluidRow(column(width = 12, downloadButton("download","Download Report",class = "btn-block", style = "width:100%;")))
  
)


server <- function(input, output, session) {
  
  # output$text1 <- renderText({input$ae1$datapath})
  
  ## EDC File Path
  text <- reactive({
    req(input$study)
    
    exists <- input$study %in% c(str_c("00",as.character(seq(1,5))))
    feedbackWarning("study",!exists,"Unkown Study")
    req(exists,cancelOutput = FALSE)
    
    # if (input$study %in% paste0("0",as.character(seq(47,91)))) {
    cat(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-23238","\\","OpenClinicaExtract","\\","Current","\\",input$study))
    # }
  })
  
  output$text <-  renderPrint({text()})
  
  ## Copy button
  
  output$clip <- renderUI({
    rclipButton(
      inputId = "clipbtn",
      label = "Copy Path",
      clipText = str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-23238","\\","OpenClinicaExtract","\\","Current","\\",input$study), 
      icon = icon("clipboard")
    )
  })
  
  if (interactive()){
    observeEvent(input$clipbtn, clipr::write_clip(str_c("\\\\","wf00168p",".","oneabbott",".","com","\\","data1","\\","CDM","\\","ADC-US-RES-23238","\\","OpenClinicaExtract","\\","Current","\\",input$study)))
  }
  
  
  ## Download Report
  output$download <- downloadHandler(
    filename = function(){
      str_c("InHouse 23238 ",input$study," EDC Review Report ",Sys.Date(),".html")
    }, #"InHouse 16157 EDC Review Report.html",
    content = function(file){
      params <- list(Study = input$study,
                     label = input$label,
                     data1 = input$ae1$datapath,
                     data2 = input$ae2$datapath,
                     data4 = input$de$datapath,
                     data5 = input$dm$datapath,
                     data6 = input$du1$datapath,
                     data7 = input$du2$datapath,
                     data8 = input$du3$datapath,
                     data11 = input$du4$datapath,
                     data12 = input$du5$datapath,
                     data13 = input$ie$datapath,
                     data14 = input$mh$datapath,
                     data9 = input$pd$datapath,
                     data10 = input$vs$datapath,
                     data15 = input$dt2$datapath,
                     data16 = input$dt3$datapath,
                     data17 = input$dt4$datapath,
                     data18 = input$dl2$datapath,
                     data19 = input$ml2$datapath,
                     data20 = input$sk1$datapath,
                     data21 = input$sk2$datapath
      )
      id <- showNotification(
        "Rendering Report...",
        duration = NULL,
        closeButton = FALSE
      )
      on.exit(removeNotification(id), add = TRUE)
      
      rmarkdown::render("EDC-Shiny-Review.Rmd",
                        output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))
    }
  )
  
}

shinyApp(ui, server)