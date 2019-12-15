library(shiny)
library(shiny.semantic)
library(semantic.dashboard)
library(plotly)
library(DT)
library(ptdsAlpha)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(shinyjs)


ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(side = "left",
                   size ="wide",
                   sidebarMenu(
                     menuItem(
                       tabName = "welcome",
                       text = "Welcome",
                       icon = icon("file", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "data_tab",
                       text = "Data Table",
                       icon = icon("file", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "ranktable_tab",
                       text = "Line graph and rank table ",
                       icon = icon("bookmark", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "Top_10",
                       text = "Top 10 countries",
                       icon = icon("bookmark", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "maps_tab",
                       text = "Interactive graphs",
                       icon = icon("bookmark", lib = "glyphicon")
                     ),

                     menuItem(
                       tabName = "comparable_tab",
                       text = "Comparable maps",
                       icon = icon("bookmark", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "bubble_tab",
                       text = "Bubble chart",
                       icon = icon("bookmark", lib = "glyphicon")
                     ),

                     menuItem(
                       tabName = "forecast_tab",
                       text = "Forecasting graph",
                       icon = icon("bookmark", lib = "glyphicon")
                     ),
                     menuItem(
                       tabName = "about_us",
                       text = "Meet the team",
                       icon = icon("bookmark", lib = "glyphicon")
                     )

                   )),

  dashboardBody(tabItems(

    tabItem(tabName = "welcome",

            fluidRow(
                imageOutput("welcome_page"),
                'The main goal of our project is to display', br(),
                'economic and pollution indicators of European ', br(),
                'countries in the last four decades to check  ', br(),
                'specific trends and correlations.', br(), br(),
                'In this app, you can see:', br(), br(),
                ' A continuous animation of the top 10 ranked', br(),
                ' countries by economic and pollution indicators', br(), br(),
                '- Animated GIFs showing the overall evolution of ', br(),
                'economic and pollution indicators from  European ', br(),
                'countries through a period of 40 years', br(), br(),
                '- An interactive map of Europe in order to provide', br(),
                ' a visual and informative comparison between the ', br(),
                'pollution and economic indicator by year', br(), br(),
                '- A bubble graph which shows the movement of', br(),
                'countries across indicators in the past 40 years', br(), br(),
                '- Various maps, graphs, barcharts and tables ', br(),
                'to get an effective view of the data and trends', br(), br(),
                '- Forecasts of each variable future values with', br(),
                'specific time series models'

            )
    ),

    tabItem(tabName = "data_tab",
            fluidRow(
              box(
                title = "Data table",
                color = "blue",
                ribbon = TRUE,
                title_side = "top left",
                width = 14,
                tags$div(
                  dataTableOutput("data_table"))))),


    tabItem(tabName = "ranktable_tab",
            fluidRow(
              box(
                title = "Filter data",
                color = "blue",
                ribbon = FALSE,
                title_side = "top left",
                width = 16,
                height = 16,
                sidebarPanel(
                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    selectInput(
                      inputId =  "variable1",
                      choices = unique(get_data()$Year),
                      multiple = TRUE,
                      label = "Select Year",
                      selected = "1992"
                    )
                  ),

                  div(style = "display: inline-block;vertical-align:top; width: 100px;", HTML("<br>")),

                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    selectInput(
                      inputId =  "variable2",
                      choices =  unique(get_data()$Country),
                      multiple = TRUE,
                      label = "Select Country",
                      selected = "Switzerland"
                    )
                  ),

                  div(style = "display: inline-block;vertical-align:top; width: 100px;", HTML("<br>")),

                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    selectInput(
                      inputId =  "variable3",
                      choices = colnames(get_data())[3:19],
                      label = "Select Variable",
                      selected = "GDP"
                    )
                  )
                )
              )
            ),

            fluidRow(
              box(
                title = "Line plot",
                color = "blue",
                ribbon = FALSE,
                title_side = "top left",
                width = 16,
                height = 16,
                plotlyOutput("plot_country", width = "100%", height = "80%") %>%withSpinner(color = "#6d84ab", size = 1, proxy.height="300px" )
              )
            ),

            fluidRow(
              box(
                title = "Rank table",
                color = "blue",
                ribbon = FALSE,
                title_side = "top left",
                width = 16,
                height = 16,
                dataTableOutput("rank_table")%>%withSpinner(color = "#6d84ab", size = 1, proxy.height="300px" )
              )
            )),

    tabItem(tabName = "Top_10",
            fluidRow(),
            fluidRow(
              box(
                title = "Filters",
                color = "blue",
                width = 11,
                selectInput(
                  inputId =  "variable4",
                  choices = colnames(get_data())[3:9],
                  label = "Select first variable",
                  selected = "CO2 Emissions"
                )
              ),
              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "Description",
                       content = "Ranking of countries by economic and pollution
                       indicators")
                )
              )
            ),
            fluidRow(box(
              title = "Top 10",
              color = "blue",
              width = 11,
              imageOutput("plot_top_10" , width = "100%", height = "90%")
              %>% withSpinner(
                color = "#6d84ab",
                size = 1,
                proxy.height = "300px"
              )
            ))),


    tabItem(tabName = "maps_tab",
            fluidRow(),
            fluidRow(
              box(
                title = "Filters",
                color = "blue",
                width = 11,
                selectInput(
                  inputId =  "variable5",
                  choices = colnames(get_data())[3:19],
                  label = "Select first variable",
                  selected = "GDP"
                )),
                tabBox(
                  title = "Comments",
                  color = "blue",
                  width = 5,
                  collapsible = TRUE,
                  tabs = list(
                    list(menu = "Description",
                         content = "Evolution of
economic and pollution indicators over 40 years")


            ))),
            fluidRow(box(
              width = 11,
              imageOutput("plot_map",  width = "100%", height = "80%") %>% withSpinner(
                color = "#6d84ab",
                size = 1,
                proxy.height = "300px"
              )
            )))
    ,




    tabItem(tabName = "comparable_tab",
            fluidRow("Comparable graph"),
            fluidRow(
              box(
                title = "Filters",
                color = "blue",
                width = 16,
                sidebarPanel(
                  div(
                    style = "display: inline-block;vertical-align:top; width: 150px;",
                    selectInput(
                      inputId =  "variable6",
                      choices = unique(get_data()$Year),
                      label = "Select first year",
                      selected = "2000"
                    )
                  ),

                  div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),

                  div(
                    style = "display: inline-block;vertical-align:top; width: 150px;",
                    selectInput(
                      inputId =  "variable7",
                      choices = unique(get_data()$Year),
                      label = "Select second year",
                      selected = "2010"
                    )
                  ),

                  div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),

                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    selectInput(
                      inputId =  "variable8",
                      choices = colnames(get_data())[11:19],
                      label = "Select economic variable",
                      selected = "GDP"
                    )
                  ),

                  div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),

                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",

                    selectInput(
                      inputId =  "variable9",
                      choices = colnames(get_data())[3:10],
                      label = "Select pollution variable",
                      selected = "GDP"
                    )
                  )

                )
              )
            ),
            tabBox(
              title = "Comments",
              color = "blue",
              width = 16,
              collapsible = TRUE,
              tabs = list(
                list(
                  menu = "Description",
                  content = 'The height of the bars represent \nthe level of pollution.
                    \nThe color of the bars represent the level of
                       economic indicator.'
                )

              )
            ),
            fluidRow(column(8, leafletOutput("plot_comparable1", width = "550px", height = "300px")%>%withSpinner(color = "#6d84ab", size = 1, proxy.height="300px" )),
                     column(8,leafletOutput("plot_comparable2", width="550px",height="300px")%>%withSpinner(color = "#6d84ab", size = 1, proxy.height="300px" ))),
            fluidRow(column(8,plotlyOutput("plot_barchart1", width="550px",height="300px")),
                     column(8,plotlyOutput("plot_barchart2", width="550px",height="300px")))
    ),


    tabItem(tabName = "bubble_tab",
            fluidRow("Filters"),
            fluidRow(
              box(
                title = "Plot line",
                color = "blue",
                width = 16,
                style="height:200px;",

                sidebarPanel(
                  div(
                    style = "display: inline-block;vertical-align:top; width: 300px;",
                    useShinyjs(),
                    id = "form",
                    pickerInput(
                      inputId =  "variable10",
                      label = 'Select country',
                      multiple = TRUE,
                      choices = as.character(unique(na.omit(get_data()$Country))),
                      options = pickerOptions(
                        actionsBox = TRUE,
                        virtualScroll = TRUE,
                        size = 5
                      )
                    )
                  ),

                  div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),

                  div(
                    style = "display: inline-block;vertical-align:top; width: 200px;",
                    selectInput(
                        inputId =  "variable11",
                        choices = colnames(get_data())[11:19],
                        label = "Select first variable",
                        selected = "GDP"
                      ))


               ,
               div(style = "display: inline-block;vertical-align:top; width: 50px;", HTML("<br>")),


               div(
                 style = "display: inline-block;vertical-align:top; width: 200px;",
                 selectInput(
                   inputId =  "variable12",
                   choices =  colnames(get_data())[3:10],
                   label = "Select second variable",
                   selected = "CO2 Emissions"
                 )
               ))
              ),
              actionButton("resetAll", "Reset all")),

            fluidRow(
              box(
                title = "Plot line",
                color = "blue",
                width = 16,
                plotlyOutput("plot_bubble") %>%
                  withSpinner(
                    color = "#6d84ab",
                    size = 1,
                    proxy.height = "300px"
                  )
              )),
              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "Info",
                       content = "Movement of countries across the indicators
                       over the past 10 years.")

                )
              )
            ),
    tabItem(tabName = "forecast_tab",
            fluidRow("Forecasting"),
            fluidRow(
              box(
                title = "Forecasting",
                color = "blue",
                width = 16,
                selectInput(
                  inputId =  "variable13",
                  choices = unique(get_data()$Country),
                  label = "Select a country",
                  selected = "Switzerland"
                ),
                selectInput(
                  inputId =  "variable14",
                  choices =  colnames(get_data())[3:19],
                  label = "Select a variable you want to forecast",
                  selected = "GDP"
                ),
                sliderInput(
                  inputId = "variable15",
                  label = "Select the forecast length",
                  min = 0,
                  max = 50,
                  value = 5
                ),
                dygraphs::dygraphOutput("plot_forecast")%>%withSpinner(color = "#6d84ab", size = 1, proxy.height="300px")
              ),
              tabBox(
                title = "Comments",
                color = "blue",
                width = 5,
                collapsible = TRUE,
                tabs = list(
                  list(menu = "Info",
                       content = "Forecasts of each variable's future values
                       with specific time series models")

                )
              )
            )),

    tabItem(tabName = "about_us",


            fluidRow(
              box(
                title = "Pierre-Emmanuel Got",
                color = "blue",
                ribbon = TRUE,
                title_side = "top left",
                width = 10,
                imageOutput("us_photos1"),
                style = "height:400px"
              ),
              box(
                title = "Claudio Previte",
                color = "blue",
                ribbon = TRUE,
                title_side = "top left",
                width = 10,
                imageOutput("us_photos2"),
                style = "height:400px"
              ),
              box(
                title = "Ana Casian",
                color = "blue",
                ribbon = TRUE,
                collapsible = TRUE,
                title_side = "top left",
                width = 10,
                imageOutput("us_photos3"),
                style = "height:400px"),

              box(
                title = "Redwan Hasan",
                color = "blue",
                ribbon = TRUE,
                title_side = "top left",
                width = 10,
                imageOutput("us_photos4"),
                style = "height:400px"
              ),


              box(
                title = "Jeremy Choppe",
                color = "blue",
                ribbon = TRUE,
                title_side = "top left",
                width = 10,
                imageOutput("us_photos5"),
                style = "height:400px"
              )

            )
    )


  )
  ), theme = "flatly"
)

server <- function(input, output) {

  output$welcome_page<- renderImage({

    list(
      src = c("pollution.jpg"),
      contentType = 'image/gif',
      alt = "This is alternate text",
      width = "70%",
      align = "right"
    )
  },deleteFile = FALSE)


  output$data_table <-

    renderDataTable(
      data %>%
        tidyr::gather(key = "Variable", value = "Value", c(3:19)),
      caption = htmltools::tags$caption(style = "caption-side: top; text-align: left;",
                                        "Note. ", htmltools::em("Please select the variables of interest")),
      filter = list(position = 'top',
                    clear = TRUE,
                    plain = FALSE),
      options = list(scrollX = TRUE)
    )


  output$rank_table <-
    renderDataTable(
      ranktable_function(
        yrs = input$variable1,
        ctry = input$variable2,
        var = input$variable3
      )
    )

  output$plot_country <-
    renderPlotly(plot(country_function(
      ctry = input$variable2,
      var = input$variable3
    )))

  output$plot_top_10 <-
    renderImage({
      # A temp file to save the output.
      # This file will be removed later by renderImage
      outfile <- tempfile(fileext = '.gif')

      # now make the animation
      gganimate::anim_save("outfile.gif",
                           animated_top10(var = input$variable4))

      # Return a list containing the filename
      list(
        src = "outfile.gif",
        contentType = 'image/gif',
        width = 500,
        height = 450,
        alt = "This is alternate text"
      )
    })


  output$plot_map <- renderImage({

    # Return a list containing the filename
    list(
      src = paste0(input$variable5,"_tmap.gif"),
      contentType = 'image/gif',
      width = 500,
      height = 450,
      alt = "This is alternate text"
    )
  },deleteFile = FALSE)


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



  output$plot_comparable1 <-
    renderLeaflet(
      comparison_function (
        yrs = input$variable6,
        eco_var  = input$variable8,
        pol_var = input$variable9

      )
    )

  output$plot_comparable2 <-
    renderLeaflet(
      comparison_function (
        yrs = input$variable7,
        eco_var  = input$variable8,
        pol_var = input$variable9

      )
    )


  output$plot_barchart1 <-
    renderPlotly(plot(
      barchart_function(
        eco_var = input$variable8,
        pol_var = input$variable9,
        yrs = input$variable6
      )
    ))

  output$plot_barchart2 <-
    renderPlotly(plot(
      barchart_function(
        eco_var = input$variable8,
        pol_var = input$variable9,
        yrs = input$variable7

      )
    ))



  output$plot_bubble <-
    renderPlotly((bubble_function(
            ctry = input$variable10,
      eco_var = input$variable11,
      pol_var = input$variable12)))



  output$plot_forecast <-
    dygraphs::renderDygraph(
      forecast_function(
        geo = input$variable13,
        variable = input$variable14,
        fc_length = input$variable15
      )[[3]]
    )


  observeEvent(input$resetAll, {
    reset("form")})


  output$us_photos1<- renderImage({
    list(
      src = c("Pierre.jpg"),
      contentType = 'image/gif',
      alt = "This is alternate text",
      width = "40%"
    )
  },deleteFile = FALSE)

  output$us_photos2 <- renderImage({
    list(
      src = c("Claudio.jpg"),
      contentType = 'image/gif',
      alt = "This is alternate text",
      width = "40%"
    )
  },deleteFile = FALSE)

  output$us_photos3 <- renderImage({
    list(
      src = c("Ana.jpeg"),
      contentType = 'image/gif',
      alt = "This is alternate text",
      width = "35%"
    )
  },deleteFile = FALSE)


  output$us_photos4 <- renderImage({
    list(
      src = c("Redwan.png"),
      contentType = 'image/gif',
      alt = "This is alternate text",
      width = "35%"
    )
  },deleteFile = FALSE)


  output$us_photos5 <- renderImage({
    list(
      src = c("Jeremy.jpeg"),
      contentType = 'image/gif',
      alt = "This is alternate text",
      width = "30%"
    )
  },deleteFile = FALSE)


}


shinyApp(ui, server)
