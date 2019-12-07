library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly)
library(DT)
library(ptdsAlpha)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(side = "left",
                   sidebarMenu(
                     menuItem(tabName = "line_plot_tab", text = "Line chart", icon = icon("cog", lib = "glyphicon")),
                     menuItem(tabName = "bubble_tab", text = "Bubble chart", icon = icon("cog", lib = "glyphicon")),
                     menuItem(tabName = "data_tab", text = "Data Table", icon = icon("cog", lib = "glyphicon"))
                     )
                   ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "line_plot_tab",
              fluidRow("Line graph"),
              fluidRow(
                box(title = "Plot line", color = "blue", width = 11,
                    selectInput(inputId =  "variable1", choices = unique(data$country),
                                label = "Select first variable", selected = "Switzerland"),
                    selectInput(inputId =  "variable2", choices =  unique(data$variable),
                                label = "Select second variable", selected = "gdp"),
                    plotlyOutput("plot_country")),
                tabBox(title = "Comments", color = "blue", width = 5,
                       collapsible = TRUE,
                       tabs = list(
                         list(menu = "First Tab", content = "nksn<ak\nhasfhsilahf\nfukhfknksn<ak\nhasfhsilahf\nfukhfknksn<ak\nhasfhsilahf\nfukhfk")

                       )))),
      tabItem(tabName = "bubble_tab",
              fluidRow("Buble graph"),
              fluidRow(
                box(title = "Plot line", color = "blue", width = 11,
                    selectInput(inputId =  "variable3", choices = unique(data$variable),
                                label = "Select first variable", selected = "gdp"),
                    selectInput(inputId =  "variable4", choices =  unique(data$variable),
                                label = "Select second variable", selected = "productivity"),
                    plotlyOutput("plot_bubble")),
                tabBox(title = "Comments", color = "blue", width = 5,
                       collapsible = TRUE,
                       tabs = list(
                         list(menu = "First Tab", content = "nksn<ak\nhasfhsilahf\nfukhfknksn<ak\nhasfhsilahf\nfukhfknksn<ak\nhasfhsilahf\nfukhfk")

                       )))),

      tabItem(tabName = "data_tab",
              fluidRow("Data Table"),
              fluidRow(
                box(title = "Data table", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 14,
                    tags$div(
                      dataTableOutput("data_table")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                )))
      )
  ), theme = "flatly"
)

server <- function(input, output) {

  output$plot_country <- renderPlotly(plot(country_function(input$variable1,input$variable2)))
  output$plot_bubble <- renderPlotly((bubble_function(input$variable3,input$variable4)))

  output$data_table <- renderDataTable(data, options = list(dom = 't'))


}

shinyApp(ui, server)