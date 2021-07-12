
library(shiny)
library(DT)
library(httr)
library(gsheet)
library(plotrix)
library(dplyr)
library(ggplot2)

#script for writing a blank data and saving it
start_<-function(){
  start_data<-data.frame(matrix(0,1,2))
  colnames(start_data)=c("Event","Time")
  start_data$Event[1:1]<-"Start"
  start_data$Time[1:1]<-round(as.numeric(as.POSIXct(Sys.time())),0)
  write.csv(start_data,"data.csv",row.names = FALSE)
}

#script for writing the action x and saving it
action_<-function(x){
    all_data<-read.csv("data.csv")
    action_data<-t(data.frame(c(x,round(as.numeric(as.POSIXct(Sys.time())),0))))
    colnames(action_data)=c("Event","Time")
    csvdata<-rbind(all_data,action_data)
    if (any(csvdata=="Finish")){
    
    }else{
      write.csv(csvdata,"data.csv",row.names = FALSE)
    }
  }

finish_<-function(){
  all_data<-read.csv("data.csv")
  if (any(all_data=="Finish")){
  }else{
  action_data<-t(data.frame(c("Finish",round(as.numeric(as.POSIXct(Sys.time())),0))))
  colnames(action_data)=c("Event","Time")
  csvdata<-rbind(all_data,action_data)
  write.csv(csvdata,"data.csv",row.names = FALSE)
  }
}
  
op<-function(){
  
  df<-read.csv("data.csv")
  df$duration<- diff(c(0,df$Time))
  
  if((nrow(df))>2){
    list_actions<-df$Event[2:(nrow(df)-1)]
    list_duration<-df$duration[3:nrow(df)]
    op<-data.frame(list_actions,list_duration)
  }
    else {
      list_actions<-c("Activated")
      list_duration<-c(0)
      op<-data.frame(list_actions,list_duration)
      
    }
  colnames(op)<-c("Behaviour","Duration")
    op
  }



ui <- fluidPage(
  
  titlePanel("Teaching Act Dashboard"),
  
  h4("Management (M) – related to class business unrelated to instructional activity"),
  h4("Transition (T) – managerial and organisational activities related to instruction"),
  h4("Waiting (W) – completed a task, period of no activity and no movement between activities"),
  h4("Knowledge (K) – listening to instructions, watching a demonstration, questioning"),
  h4("Activity (A) – engaged in motor activity, actively responding, actively supporting"),
  h4("Off Task (OT)"),
  

  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start",icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      br(),
      
      actionButton("action1", "Management"),
      br(),
      br(),
      
      actionButton("action2", "Transition"),
      br(),
      br(),
      
      actionButton("action3", "Waiting"),
      br(),
      br(),
      
      actionButton("action4", "Knowledge"),
      br(),
      br(),
      
      actionButton("action5", "Activity"),
      br(),
      br(),
      
      
      actionButton("action6", "Off Task"),
      br(),
      br(),
      
      
      
      
      actionButton("finish", "Finish",icon("stop-circle"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      br(),
  downloadButton("downloadData", "Download")
    ),
    mainPanel(
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"),
                 

      plotOutput('plot'),
      dataTableOutput("df")
    )
  ))
   


server <- function(input, output,session) {
  
  
  
  observeEvent(input$start, {
    start_()
  })
  
  observeEvent(input$action1, {
    action_("Management")
  })
  
  observeEvent(input$action2, {
    action_("Transition")
  })
  
  observeEvent(input$action3, {
    action_("Waiting")
  })
  
  observeEvent(input$action4, {
    action_("Knowledge")
  })
  
  observeEvent(input$action5, {
    action_("Activity")
  })
  
  observeEvent(input$action6, {
    action_("Off Task")
  })
  
  observeEvent(input$finish, {
    finish_()
  })
  
  output$df<-renderDataTable ({
    input$action1
    input$action2
    input$action3
    input$action4
    input$action5
    input$action6
    input$start
    input$finish
    op()
  })
  
 

  output$plot<-renderPlot({
  # 
    input$start
    input$action1
    input$action2
    input$action3
    input$action4
    input$action5
    input$action6
    input$finish
    op<-op()
    op<-aggregate(op$Duration, by=list(event=op$Behaviour), FUN=sum)
    op<-data.frame(op)
    
    piep<-pie(op$x,labels=op$event,
              main="Proportion of teaching behavior")

  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      "download.csv"
    },
    content = function(file) {
     data<-op()
            write.csv(data, file, row.names = F)
    }
  )
  
}


shinyApp(ui = ui, server = server)




