
library(shiny)
library(DT)
library(httr)
library(gsheet)
library(plotrix)
library(dplyr)
library(ggplot2)


start_<-function(){
  start_data<-data.frame(matrix(0,1,2))
  colnames(start_data)=c("Event","Time")
  start_data$Event[1:1]<-"Start"
  start_data$Time[1:1]<-round(as.numeric(as.POSIXct(Sys.time())),0)
  write.csv(start_data,"data.csv",row.names = FALSE)
}

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



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Teaching Act Dashboard"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      actionButton("start", "Start",icon("paper-plane"), 
                   style="color: #fff; background-color: #337ab7; border-color: #2e6da4"),
      br(),
      br(),
      
      actionButton("action1", "Behavior 1"),
      br(),
      br(),
      
      actionButton("action2", "Behavior 2"),
      br(),
      br(),
      
      actionButton("action3", "Behavior 3"),
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
                 
      #dataTableOutput("mytable"),
      plotOutput('plot'),
      dataTableOutput("df")
    )
  ))
   


# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  
  observeEvent(input$start, {
    start_()
  })
  
  observeEvent(input$action1, {
    action_("action1")
  })
  
  observeEvent(input$action2, {
    action_("action2")
  })
  
  observeEvent(input$action3, {
    action_("action3")
  })
  
  observeEvent(input$finish, {
    finish_()
  })
  
  output$df<-renderDataTable ({
    input$action1
    input$action2
    input$action3
    input$start
    input$finish
    op()
  })
  
 

  output$plot<-renderPlot({
  # 
    req(input$action1)
    input$action2
    input$action3
    input$finish
    op<-op()
  #   colnames(df)<-c("Behaviour","Duration")
  #   df<-aggregate(df$Duration, by=list(event=df$Behaviour), FUN=sum)
  #   # op<-data.frame(op)
  #   bp<- ggplot(df, aes(x="", y=Duration, fill=Behaviour))+
  #     geom_bar(width = 1, stat = "identity")
  #   
  #   pie <- bp + coord_polar("y", start=0)
  #   pie
  #   
  #   
    op<-aggregate(op$Duration, by=list(event=op$Behaviour), FUN=sum)
     op<-data.frame(op)
    
    
#     piep<-pie(op$x,labels=op$Behaviour,col=rainbow(length(op$Behaviour)*100),
     piep<-pie(op$x,labels=op$Behaviour,
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

# Run the application 
shinyApp(ui = ui, server = server)

