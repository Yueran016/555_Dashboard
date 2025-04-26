install.packages("plotly")
install.packages("shiny")
library(plotly)
library(shiny)

# change ggplot to plotly
plist_interactive <- lapply(p, plotly::ggplotly)
tlist_interactive <- lapply(t, plotly::ggplotly)
slist_interactive <- lapply(s, plotly::ggplotly)

# UI
ui <- fluidPage(
  titlePanel("Interactive Plot Selector"),
  
  sidebarLayout(
    sidebarPanel(
      # panel
      selectInput("data_choice", "Select data group:",
                  choices = c("original data" = "p", 
                              "10% zero data" = "t", 
                              "25% zero data" = "s")),
      
      # buton
      actionButton("plot1", "sum intensity"),
      actionButton("plot2", "PC1 vs PC2"),
      actionButton("plot3", "PC1"),
      actionButton("plot4", "PC2")
    ),
    
    mainPanel(
      # display
      plotlyOutput("selected_plot")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # store via reactiveVal
  current_plot <- reactiveVal(1)
  
  # 根据选择的数据组，更新当前显示的图
  selected_data <- reactive({
    if(input$data_choice == "p") {
      return(p)
    } else if(input$data_choice == "t") {
      return(t)
    } else {
      return(s)
    }
  })
  
  # upload chosen plot
  observeEvent(input$plot1, {
    current_plot(1)
  })
  
  observeEvent(input$plot2, {
    current_plot(2)
  })
  
  observeEvent(input$plot3, {
    current_plot(3)
  })
  
  observeEvent(input$plot4, {
    current_plot(4)
  })
  
  
  output$selected_plot <- renderPlotly({
    selected_data()[[current_plot()]]
  })
}

# run shinny
shinyApp(ui = ui, server = server)
library(htmlwidgets)

# save HTML
rmarkdown::run("app.R", shiny_args = list(launch.browser = FALSE))
