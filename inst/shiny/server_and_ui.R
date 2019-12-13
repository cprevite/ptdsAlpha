library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly)
library(DT)
library(ptdsAlpha)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(side = "left",
                   sidebarMenu(
                     menuItem(
                       tabName = "data_tab",
                       text = "Data Table",
                       icon = icon("cog", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "ranktable_tab",
                       text = "Rank",
                       icon = icon("cog", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "maps_tab",
                       text = "Maps",
                       icon = icon("cog", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "line_plot_tab",
                       text = "Line chart",
                       icon = icon("cog", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "bubble_tab",
                       text = "Bubble chart",
                       icon = icon("cog", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "Top_10",
                       text = "Top 10",
                       icon = icon("cog", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "comparable_tab",
                       text = "Comparable maps",
                       icon = icon("cog", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "forecast_tab",
                       text = "Forecasting",
                       icon = icon("cog", lib = "glyphicon")
                     )

                   )),

  dashboardBody(tabItems(
    tabItem(tabName = "data_tab",
            fluidRow("Data Table"),
            fluidRow(
              box(
                title = "Data table",
                color = "blue",
                ribbon = FALSE,
                title_side = "top left",
                width = 14,
                tags$div(
                  dataTableOutput("data_table"),
                  style = paste0("color:", semantic_palette[["blue"]], ";")
                )
              )
            )),
    tabItem(tabName = "ranktable_tab",
            fluidRow("Rank Table"),
            fluidRow(
              box(
                title = "Rank table",
                color = "blue",
                ribbon = FALSE,
                title_side = "top left",
                width = 14,
                height =16,
                selectInput(
                  inputId =  "variable12",
                  choices = unique(data$Year),
                  multiple = TRUE,
                  label = "Select first variable",
                  selected = "1992"
                ),
                selectInput(
                  inputId =  "variable13",
                  choices =  unique(data$Country),
                  label = "Select second variable",
                  selected = "Switzerland"
                ), selectInput(
                  inputId =  "variable14",
                  choices = colnames(data)[3:19],
                  label = "Select first variable",
                  selected = "GDP"
                ),
                tags$div(
                  htmlOutput("rank_table"),
                  style = paste0("color:", semantic_palette[["blue"]], ";")
                )
              )
            )),
    tabItem(tabName = "maps_tab",
            fluidRow(),
            fluidRow(
              box(
                title = "Maps",
                color = "blue",
                width = 11,
                selectInput(
                  inputId =  "variable11",
                  choices = colnames(data)[3:19],
                  label = "Select first variable",
                  selected = "GDP"
                ),
                sliderInput(
                  "slider",
                  "Speed",
                  min = 1,
                  max = 80,
                  value = 30,
                  animate = animationOptions(
                    interval = 30,
                    loop = FALSE,
                    playButton = "Play",
                    pauseButton = "Stop"
                  ))
                )),
            fluidRow(
              box(

                width = 11,
                imageOutput("plot_map") %>% withSpinner(color = "#6d84ab"
                  ))
              ),

            fluidRow(),
            fluidRow(

              box(
                title = "Barchart",
                color = "blue",
                width = 11,
                selectInput(
                  inputId =  "variable8",
                  choices = colnames(data)[3:10],
                  label = "Select a pollution variable",
                  selected = "CO2 Emissions"
                ),
                selectInput(
                  inputId =  "variable9",
                  choices =  colnames(data)[11:19],
                  label = "Select an economic variable",
                  selected = "Productivity"
                ),
                selectInput(
                  inputId =  "variable99",
                  choices =  unique(data$Year),
                  label = "Select a year",
                  selected = 2000
                ),

                plotlyOutput("plot_barchart"),
                '* the economic variable as color fill of the bars', br(),
                br(),
                'Units:', br(), '- GDP: USD million', br(),
                '- Productivity: USD per person employed', br(),
                '- CO2 and Gas Emissions: 000 tonnes'
              ),


              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "First Tab",
                       content = 'The height of the bars represent', br(),
                       'the level of pollution.', br(),
                       'The color of the bars represent the level of
                       economic indicator')

                )
              )
            )),



    tabItem(tabName = "line_plot_tab",
            fluidRow("Line graph"),
            fluidRow(
              box(
                title = "Plot line",
                color = "blue",
                width = 11,
                selectInput(
                  inputId =  "variable1",
                  choices = unique(data$Country),
                  multiple = TRUE,
                  label = "Select first variable",
                  selected = "Switzerland"
                ),
                selectInput(
                  inputId =  "variable2",
                  choices =  colnames(data)[3:19],
                  multiple = TRUE,
                  label = "Select second variable",
                  selected = "GDP"
                ),
                plotlyOutput("plot_country")
              ),
              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "First Tab",
                       content = "test")

                )
              )
            )),


    tabItem(tabName = "bubble_tab",
            fluidRow("Buble graph"),
            fluidRow(
              box(
                title = "Plot line",
                color = "blue",
                width = 11,
                selectInput(
                  inputId =  "variable3",
                  choices = colnames(data)[3:19],
                  label = "Select first variable",
                  selected = "GDP"
                ),
                selectInput(
                  inputId =  "variable4",
                  choices =  colnames(data)[3:19],
                  label = "Select second variable",
                  selected = "Productivity"
                ),
                plotlyOutput("plot_bubble") %>%
                  withSpinner(type = 3 ,
                              color.background = "#6d84ab",
                              color = "#6d84ab")
              ),
              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "First Tab",
                       content = "test")

                )
              )
            )),


    tabItem(tabName = "Top_10",
            fluidRow(),
            fluidRow(
              box(
                title = "Top 10",
                color = "blue",
                width = 16,
                selectInput(
                  inputId =  "variable10",
                  choices = colnames(data)[3:9],
                  label = "Select first variable",
                  selected = "CO2 Emissions"
                ),
                imageOutput("plot_top_10") %>% withSpinner(color = "#6d84ab")
              ),
              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "First Tab",
                       content = "test")

                )
              )
            )),

    tabItem(tabName = "comparable_tab",
            fluidRow("Comparable graph"),
            fluidRow(
              box(
                title = "Comparable",
                color = "blue",
                width = 16,
                selectInput(
                  inputId =  "variable15",
                  choices = unique(data$Year),
                  label = "Select first year",
                  selected = "2000"
                ),
                selectInput(
                  inputId =  "variable16",
                  choices = unique(data$Year),
                  label = "Select second year",
                  selected = "2010"
                ),
                selectInput(
                  inputId =  "variable17",
                  choices = colnames(data)[3:19],
                  label = "Select first variable",
                  selected = "GDP"
                ),
                selectInput(
                  inputId =  "variable18",
                  choices = colnames(data)[3:19],
                  label = "Select first variable",
                  selected = "GDP"
                )


              )),
            fluidRow(column(8, leafletOutput("plot_comparable1", width = "550px", height = "300px")),
                     column(8,leafletOutput("plot_comparable2", width="550px",height="300px"))),
            fluidRow(column(8,plotlyOutput("plot_barchart1", width="550px",height="300px")),
                     column(8,plotlyOutput("plot_barchart2", width="550px",height="300px")))
            ),

    tabItem(tabName = "forecast_tab",
            fluidRow("Forecasting"),
            fluidRow(
              box(
                title = "Forecasting",
                color = "blue",
                width = 16,
                selectInput(
                  inputId =  "variable5",
                  choices = unique(data$Country),
                  label = "Select a country",
                  selected = "Switzerland"
                ),
                selectInput(
                  inputId =  "variable6",
                  choices =  colnames(data)[3:19],
                  label = "Select a variable you want to forecast",
                  selected = "GDP"
                ),
                sliderInput(
                  inputId = "variable7",
                  label = "Select the forecast length",
                  min = 0,
                  max = 50,
                  value = 5
                ),
                dygraphs::dygraphOutput("plot_forecast")
              ),
              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "First Tab",
                       content = "test")

                )
              )
            ))


  )
  ), theme = "flatly"
)

server <- function(input, output) {

  output$plot_country <-
    renderPlotly(plot(country_function(dataset = data,
                                       ctry = input$variable1,
                                       var = input$variable2)))

  output$plot_bubble <-
    renderPlotly((bubble_function(
      dataset = data,
      x_var = input$variable3,
      y_var = input$variable4)))

  output$data_table <-

    renderDataTable(
      data %>%
        gather(key = "Variable", value = "Value", c(3:19)),
                caption = htmltools::tags$caption(style = "caption-side: top; text-align: left;",
                "Note. ", htmltools::em("Please select the variables of interest")),
      filter = list(position = 'top',
                    clear = TRUE,
                    plain = FALSE),
      options = list(scrollX = TRUE)
    )

  output$plot_forecast <-
    dygraphs::renderDygraph(
      forecast_function(
        dataset = data,
        geo = input$variable5,
        variable = input$variable6,
        fc_length = input$variable7
      )[[3]]
    )


  output$plot_barchart1 <-
    renderPlotly(plot(
      barchart_function(
        dataset = data,
        pol_var = input$variable17,
        eco_var = input$variable18
      )
    ))

  output$plot_barchart2 <-
    renderPlotly(plot(
      barchart_function(
        dataset = data,
        pol_var = input$variable8,
        eco_var = input$variable9,
        yrs = input$variable99

      )
    ))


  output$plot_comparable1 <-
    renderLeaflet(
      comparison_function (
        dataset = data,
        yrs = input$variable15,
        eco_var  = input$variable17,
        pol_var = input$variable18

      )
    )

  output$plot_comparable2 <-
    renderLeaflet(
      comparison_function (
        dataset = data,
        yrs = input$variable16,
        eco_var  = input$variable17,
        pol_var = input$variable18

      )
    )



  output$rank_table <-
    renderText(
      ranktable_function(
        dataset = data,
        yrs = input$variable12,
        ctry = input$variable13,
        var = input$variable14
      )
    )



  output$plot_top_10 <-
    renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext='.gif')

      # now make the animation
      gganimate::anim_save("outfile.gif",
                           animated_top10(dataset = data,
                                          var = input$variable10))



      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif',
           width = 500,
           height = 450,
           alt = "This is alternate text"
      )})


  output$plot_map <- renderImage({
    # A temp file to save the output.
    # This file will be removed later by renderImage
    outfile2 <- tempfile(fileext='.gif')

    # now make the animation

    gif=gif_function(dataset = data,
                 var = input$variable11)


    tmap::tmap_animation(
      gif,
      filename = 'outfile2.gif',
      delay = 30,
      width = 1000,
      height = 800
    )



    # Return a list containing the filename
    list(src = "outfile2.gif",
         contentType = 'image/gif',
         width = 500,
         height = 450,
         alt = "This is alternate text"
    )})


  output$slider <- renderUI({
    sliderInput("slider",
                "Speed:",
                min = 1,
                max = 80,
                value = 30,
                animate = animationOptions(
                  interval = 30,
                  loop = FALSE,
                  playButton = "Play",
                  pauseButton = "Stop"
                ))
  })

}


shinyApp(ui, server)
