#Load Packages ----------------------------------------------------------------
options('repos')
library(shiny)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(DT)
library(graphics)

#Load Data --------------------------------------------------------------------
data<-read.csv('sim_data_for_demo.csv')

#UI ---------------------------------------------------------------------------

ui<-navbarPage("Equity and Inclusion in Science Courses",
               tabPanel("Course Letter Grades",
                        fluidPage(theme=shinytheme("cerulean"),
                                  sidebarLayout(
                                    sidebarPanel(
                                      checkboxGroupInput(inputId="course",
                                                         label="Science Course",
                                                         choices=unique(data$COURSE),
                                                         selected=unique(data$COURSE)
                                      ),
                                      radioButtons(inputId="grades",
                                                   label="Letter Grade Granularity",
                                                   choices=c('Letter Grade Detail',
                                                             'Letter Grade Category')),
                                      selectInput(inputId="breakdown",
                                                  label="Target Equity Population:",
                                                  choices=c('Gender',
                                                            'Underrep. Student of Color Status',
                                                            'First Generation Status',
                                                            'Low Income Status'),
                                                  selected='Low Income Status')
                                    ),
                                    mainPanel(
                                      dataTableOutput('gradedist'),
                                      plotOutput('residplot'),
                                      verbatimTextOutput('chisq')
                                    )
                                    
                                  )
                        )
               )
)

#Server -----------------------------------------------------------------------

server<-function(input,output){
  data_update<-reactive({
    tmp<-data[data$COURSE %in% input$course,]
    grades_var<-if(input$grades=='Letter Grade Detail'){
      'LETTER_GRADE'
    } else{
      'LETTER_GRADE_CATEGORY'
    }
    breakdown_var<-if(input$breakdown=='Gender'){
      'GENDER'
    } else if(input$breakdown=='Underrep. Student of Color Status'){
      'UR_STUDENT_OF_COLOR'
    } else if(input$breakdown=='First Generation Status'){
      'FIRST_GENERATION'
    } else{
      'LOW_INCOME'
    }
    tmp<-data.frame(tmp[,c(grades_var,breakdown_var)])
    names(tmp)<-c('GRADES','BREAKDOWN')
    tmp
  })
  output$gradedist<-DT::renderDataTable({
    tmp<-data_update()
    tmp_table1<-data.frame(table(tmp$BREAKDOWN,tmp$GRADES))
    tmp_table2<-data.frame(round(prop.table(table(tmp$BREAKDOWN,tmp$GRADES),margin=1)*100,1))
    names(tmp_table2)<-c('Var1','Var2','FreqPercnt')
    tmp_table<-tmp_table1%>%
      inner_join(tmp_table2,by=c('Var1','Var2'))%>%
      dplyr::mutate(FreqConcat=paste(as.character(Freq),'(',as.character(FreqPercnt),'%)'))%>%
      dplyr::select(Var1,Var2,FreqConcat)
    tmp_table<-pivot_wider(tmp_table,names_from="Var2",values_from="FreqConcat")
    names(tmp_table)[1]<-input$breakdown
    datatable(tmp_table,rownames=FALSE)
  })
  output$residplot<-renderPlot({
    tmp<-data_update()
    mosaicplot(~ BREAKDOWN + GRADES,
               data=tmp,
               main='Expected Grade Distribution Frequencies',
               sub='Standardized Residuals < - 2 indicate fewer than expected; Standardized Residuals > 2 indicate more than expected',
               xlab='',
               ylab='',
               shade=TRUE
    )
  })
  output$chisq<-renderPrint({
    data<-data_update()
    chisq.test(data$BREAKDOWN,data$GRADES)
  })
  
}

#Call of shinyApp Function ----------------------------------------------------
shinyApp(ui,server)