#Load Packages ----------------------------------------------------------------
library(shiny)

#UI ---------------------------------------------------------------------------
ui<-fluidPage(
  radioButtons(inputId='institution',
               label='Choose Big 10 Academic Alliance Institution',
               choices=c('University of Illinois',
                         'Indiana University',
                         'University of Iowa',
                         'University of Maryland',
                         'University of Michigan',
                         'Michigan State University',
                         'University of Minnesota',
                         'University of Nebraska-Lincoln',
                         'Northwestern University',
                         'Ohio State University',
                         'Pennsylvania State University',
                         'Purdue University',
                         'Rutgers University-New Brunswick',
                         'University of Wisconsin-Madision')
               ),
  verbatimTextOutput(outputId='message')
)

#Server -----------------------------------------------------------------------
server<-function(input,output){
output$message<-renderText({
    input$institution
  })
}

#Call of Shiny App function ---------------------------------------------------
shinyApp(ui,server)
