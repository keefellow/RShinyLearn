library(shiny)
library(dplyr)
library(DT)

get<-function(x){
    csvdata<-read.csv("data.csv")
    new_data<-x
    new_data<-data.frame(new_data)
    colnames(new_data)=c("Type")
    colnames(csvdata)=c("Type")
    csvdata<-rbind(csvdata,new_data)
    write.csv(csvdata,"data.csv",row.names = FALSE)
    csvdata
}

csvdata<-read.csv("data.csv")

ui <- fluidPage(

    # Application title
    titlePanel("Submit and update csv"),

    sidebarLayout(
        sidebarPanel(
            textInput("type","Type",value = ""),
            br(),
            actionButton("submit", label = "Submit"),
            
    ),

        mainPanel(
        downloadButton('downloadData', 'Download data'),
        br(),
        br(),
        dataTableOutput("data")
        )
    )
)

server <- function(input, output) {
    update <- eventReactive(input$submit, {
        get(input$type)
    })
    output$data<-renderDataTable({
        update()
        })
    output$downloadData <- downloadHandler(
        filename = "dataOP.csv",
        content = function(file) {
            write.csv(read.csv("data.csv"), file,row.names = FALSE)
        })
}

# Run the application 
shinyApp(ui = ui, server = server)
